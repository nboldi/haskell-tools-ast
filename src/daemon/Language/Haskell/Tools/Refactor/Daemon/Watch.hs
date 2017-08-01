{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.Tools.Refactor.Daemon.Watch where

import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Control.Monad.State.Strict
import Control.Reference hiding (modifyMVarMasked_)
import qualified Data.Aeson as A ((.=))
import Data.Aeson hiding ((.=))
import Data.Algorithm.Diff
import qualified Data.ByteString.Char8 as StrictBS
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Either
import Data.IORef
import Data.List hiding (insert)
import qualified Data.Map as Map
import Data.Maybe
import Data.Tuple
import GHC.Generics
import Network.Socket hiding (send, sendTo, recv, recvFrom, KeepAlive)
import Network.Socket.ByteString.Lazy
import System.Directory
import System.Environment
import System.IO
import System.IO.Error
import System.IO.Strict as StrictIO (hGetContents)
import System.Process
import System.FilePath
import Data.Version

import Bag
import DynFlags
import ErrUtils
import FastString (unpackFS)
import GHC hiding (loadModule)
import GHC.Paths ( libdir )
import GhcMonad (GhcMonad(..), Session(..), reflectGhc, modifySession)
import HscTypes (hsc_mod_graph)
import Packages
import SrcLoc

import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.Perform
import Language.Haskell.Tools.Refactor.Prepare
import Language.Haskell.Tools.Refactor.RefactorBase
import Language.Haskell.Tools.Refactor.Session
import Language.Haskell.Tools.Refactor.Daemon.Protocol
import Language.Haskell.Tools.Refactor.Daemon.State
import Language.Haskell.Tools.Refactor.Daemon.PackageDB
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.PrettyPrint
import Paths_haskell_tools_daemon
import Language.Haskell.Tools.Refactor.Daemon.Update

createWatchProcess :: FilePath -> Session -> MVar DaemonSessionState -> (ResponseMsg -> IO ()) -> IO WatchProcess
createWatchProcess watchExePath ghcSess daemonSess upClient = do
    (Just _watchStdIn, Just _watchStdOut, _, _watchPHandle)
      <- createProcess (proc watchExePath ["slave"]) { std_in = CreatePipe, std_out = CreatePipe }
    hSetBuffering _watchStdIn NoBuffering
    hSetBuffering _watchStdOut NoBuffering
    hSetNewlineMode _watchStdIn (NewlineMode LF LF)
    hSetNewlineMode _watchStdOut (NewlineMode LF LF)
    -- collects changes that appear in a given timeframe
    store <- newEmptyMVar
    collectorThread <- forkIO $ forever $ void $ do
        str <- hGetLine _watchStdOut
        put <- tryPutMVar store [str] -- when the mvar is empty (this is the first change since last reload)
        when (not put) $ modifyMVar_ store (return . (++ [str])) -- otherwise append
    reloaderThread <- forkIO $ forever $ void $ do
        firstChanges <- readMVar store
        allChanges <- accumulateChanges store firstChanges
        changedFiles <- catMaybes <$> mapM getChangedFile allChanges
        let rel = ReLoad [] changedFiles []
        when (not $ null changedFiles)
          $ void $ modifyMVar daemonSess (\st -> swap <$> reflectGhc (runStateT (updateClient upClient rel) st) ghcSess)
    let _watchThreads = [collectorThread, reloaderThread]
    return WatchProcess { .. }
  where accumulateChanges store previous = do
          -- TODO: make this a parameter
          threadDelay 100000 -- wait for 0.1 seconds
          changes <- readMVar store
          if changes == previous then takeMVar store
                                 else accumulateChanges store changes
        getChangedFile str =
          case words str of
            (["Mod", fn]) -> return (Just ({- get rid of escapes and quotes -} read fn))
            (["Prt", _]) -> return Nothing -- package registered
            _ -> do putStrLn $ "watch_e: word str wrong param: " ++ show (words str)
                    return Nothing

stopWatch :: WatchProcess -> IO ()
stopWatch WatchProcess{..}
  = do hPutStrLn _watchStdIn $ "exit"
       forM _watchThreads killThread
       void $ waitForProcess _watchPHandle
