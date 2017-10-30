{-# LANGUAGE RecordWildCards
           , ScopedTypeVariables
           #-}
-- | Controls the file system watching in the daemon. The file system watching must run in a
-- separate process to prevent blocking because of file operations interfering with watch.
module Language.Haskell.Tools.Daemon.Watch where

import Control.Concurrent
import Control.Exception (catches)
import Control.Monad
import Control.Monad.State.Strict
import qualified Data.Aeson as A ()
import Data.Maybe (Maybe(..), catMaybes)
import Data.Tuple (swap)
import GhcMonad (Session(..), reflectGhc)
import System.Environment (getExecutablePath)
import System.FSWatch.Repr (WatchProcess(..), PE(..))
import System.FSWatch.Slave (waitNotifies, createWatchProcess)
import System.FilePath
import System.IO (IO, FilePath)

import Language.Haskell.Tools.Daemon.ErrorHandling (userExceptionHandlers, exceptionHandlers)
import Language.Haskell.Tools.Daemon.Protocol (ResponseMsg(..))
import Language.Haskell.Tools.Daemon.State (DaemonSessionState)
import Language.Haskell.Tools.Daemon.Update (reloadModules)

-- | Starts the watch process and a thread that receives notifications from it. The notification
-- thread will invoke updates on the daemon state to re-load files.
createWatchProcess' :: Maybe FilePath -> Session -> MVar DaemonSessionState -> (ResponseMsg -> IO ())
                        -> IO (Maybe WatchProcess, [ThreadId])
createWatchProcess' watchExePath ghcSess daemonSess upClient = do
    exePath <- case watchExePath of Just exe -> return exe
                                    Nothing -> guessExePath
    process <- createWatchProcess exePath 100
    initProcess process
  where
    initProcess process = do
      reloaderThread <- forkIO $ forever $ void $ do
        changes <- waitNotifies process
        let changedFiles = catMaybes $ map getModifiedFile changes
            addedFiles = catMaybes $ map getAddedFile changes
            removedFiles = catMaybes $ map getRemovedFile changes
            reloadAction = reloadModules upClient addedFiles changedFiles removedFiles
            handlers = userExceptionHandlers
                           (upClient . ErrorMessage)
                           (\err hint -> upClient (CompilationProblem err hint))
                         ++ exceptionHandlers (return ()) (upClient . ErrorMessage)
        when (length changedFiles + length addedFiles + length removedFiles > 0)
          (void (modifyMVar daemonSess (\st -> swap <$> reflectGhc (runStateT reloadAction st) ghcSess))
             `catches` handlers)
      return $ (Just process, [reloaderThread])

    getModifiedFile (Mod file) | takeExtension file `elem` sourceExtensions = Just file
    getModifiedFile _ = Nothing

    getAddedFile (Add file) | takeExtension file `elem` sourceExtensions = Just file
    getAddedFile _ = Nothing

    getRemovedFile (Rem file) | takeExtension file `elem` sourceExtensions = Just file
    getRemovedFile _ = Nothing

    sourceExtensions = [ ".hs", ".hs-boot" ]

    guessExePath = do exePath <- getExecutablePath
                      return $ takeDirectory exePath </> "hfswatch"

-- | Stops the watch process and all threads associated with it.
stopWatch :: WatchProcess -> [ThreadId] -> IO ()
stopWatch WatchProcess{..} threads
  = do forM threads killThread
       wShutdown
