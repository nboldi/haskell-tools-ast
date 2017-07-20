{-# LANGUAGE LambdaCase
           , TupleSections
           , FlexibleContexts
           , TemplateHaskell
           , TypeFamilies
           , StandaloneDeriving
           #-}
module Language.Haskell.Tools.Refactor.CLI (refactorSession, tryOut) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Exception (displayException)
import Control.Monad.State.Strict
import Control.Reference
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char
import System.Directory
import System.Exit
import System.IO
import System.FilePath
import Data.Version (showVersion)

import DynFlags as GHC
import ErrUtils
import GHC
import GHC.Paths ( libdir )
import HscTypes as GHC
import Outputable
import Packages

import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor as HT
import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.Perform
import Language.Haskell.Tools.Refactor.Session
import Language.Haskell.Tools.Refactor.Daemon
import Paths_haskell_tools_cli (version)

tryOut :: IO ()
tryOut = void $ refactorSession stdin stdout
                  [ "-exec=OrganizeImports src\\ast\\Language\\Haskell\\Tools\\AST.hs"
                  , "src/ast", "src/backend-ghc", "src/prettyprint", "src/rewrite", "src/refactor"]

refactorSession :: Handle -> Handle -> [String] -> IO ()
refactorSession _ _ args
  | "-v" `elem` args = do
    putStrLn $ showVersion version
refactorSession input output args = do
  connStore <- newEmptyMVar
  isInteractive <- newEmptyMVar
  forkIO $ runDaemon channelMode connStore []
  (recv,send) <- takeMVar connStore -- wait for the server to establish connection
  wd <- getCurrentDirectory
  writeChan send (SetWorkingDir wd)
  -- TODO: separate cmd arguments here instead of in daemon
  writeChan send (SetGHCFlags args)
  forkIO $ forever $ do interactive <- takeMVar isInteractive
                        when interactive (processUserInput input output send)
  readFromSocket output isInteractive recv send

processUserInput :: Handle -> Handle -> Chan ClientMessage -> IO ()
processUserInput input output chan = do
  cmd <- hGetLine input
  continue <- processCommand output chan cmd
  when continue $ processUserInput input output chan

processCommand :: Handle -> Chan ClientMessage -> String -> IO Bool
processCommand output chan cmd =
  case splitOn " " cmd of
    ["Exit"] -> writeChan chan Disconnect >> return False
    ref : rest | let modPath:selection:details = rest ++ (replicate (2 - length rest) "")
               , ref `elem` refactorCommands
       -> writeChan chan (PerformRefactoring ref modPath selection details) >> return True
    _ -> do liftIO $ hPutStrLn output $ "'" ++ cmd ++ "' is not a known command. Commands are: Exit, "
                                            ++ intercalate ", " refactorCommands
            return True

readFromSocket :: Handle -> MVar Bool -> Chan ResponseMsg -> Chan ClientMessage -> IO ()
readFromSocket output isInteractive recv send = do
  continue <- readChan recv >>= processMessage output isInteractive send
  when continue (readFromSocket output isInteractive recv send)

processMessage :: Handle -> MVar Bool -> Chan ClientMessage -> ResponseMsg -> IO Bool
processMessage _ _ _ (ErrorMessage msg) = putStrLn msg >> return True
processMessage _ _ _ (CompilationProblem marks) = print marks >> return True
processMessage output _ _ (LoadedModules mods)
  = mapM (\(fp,name) -> hPutStrLn output $ "Loaded module: " ++ name ++ "( " ++ fp ++ ") ") mods >> return True
processMessage output isInteractive chan (UnusedFlags flags)
  = do loadModules chan flags
       performCmdOptions output isInteractive chan flags
processMessage _ _ _ Disconnected = return False
processMessage _ _ _ _ = return True

loadModules :: Chan ClientMessage -> [String] -> IO ()
loadModules chan flags = writeChan chan (AddPackages roots)
  where roots = filter (not . ("-" `isPrefixOf`)) flags

performCmdOptions :: Handle -> MVar Bool -> Chan ClientMessage -> [String] -> IO Bool
performCmdOptions output isInteractive chan flags = do
  mapM_ (processCommand output chan) cmds
  putMVar isInteractive (null cmds)
  when (not $ null cmds) $ writeChan chan Disconnect
  return True
  where cmds = catMaybes $ map (\f -> case splitOn "=" f of ["-exec", mod] -> Just mod; _ -> Nothing) flags
