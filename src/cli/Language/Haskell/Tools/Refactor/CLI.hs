{-# LANGUAGE LambdaCase
           , TupleSections
           , FlexibleContexts
           , TypeFamilies
           , StandaloneDeriving
           , RecordWildCards
           #-}
module Language.Haskell.Tools.Refactor.CLI
  (refactorSession, normalRefactorSession, tryOut, CLIOptions(..)) where

import Control.Concurrent
import Control.Monad.State.Strict
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Version (showVersion)
import System.FilePath
import System.Directory
import System.IO

import Language.Haskell.Tools.Daemon
import Language.Haskell.Tools.Daemon.Mode (channelMode)
import Language.Haskell.Tools.Daemon.Protocol
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin
import Paths_haskell_tools_cli (version)

tryOut :: IO ()
tryOut = void $ normalRefactorSession builtinRefactorings stdin stdout
                  $ CLIOptions
                      False
                      (Just "OrganizeImports src\\ast\\Language\\Haskell\\Tools\\AST.hs")
                      Nothing
                      ["src/ast", "src/backend-ghc", "src/prettyprint", "src/rewrite", "src/refactor"]

type ServerInit = MVar (Chan ResponseMsg, Chan ClientMessage) -> IO ()

normalRefactorSession :: [RefactoringChoice IdDom] -> Handle -> Handle -> CLIOptions -> IO Bool
normalRefactorSession refactorings = refactorSession refactorings (\st -> void $ forkIO $ runDaemon refactorings channelMode st [])

data CLIOptions = CLIOptions { displayVersion :: Bool
                             , executeCommands :: Maybe String
                             , ghcFlags :: Maybe [String]
                             , packageRoots :: [FilePath]
                             }

refactorSession :: [RefactoringChoice IdDom] -> ServerInit -> Handle -> Handle -> CLIOptions -> IO Bool
refactorSession _ _ _ output CLIOptions{..} | displayVersion
  = do hPutStrLn output $ showVersion version
       return True
refactorSession refactorings init input output CLIOptions{..} = do
  connStore <- newEmptyMVar
  init connStore
  (recv,send) <- takeMVar connStore -- wait for the server to establish connection
  wd <- getCurrentDirectory
  writeChan send (SetWorkingDir wd)
  case ghcFlags of Just flags -> writeChan send (SetGHCFlags flags)
                   Nothing -> return ()
  writeChan send (AddPackages packageRoots)
  case executeCommands of
    Just cmds -> performCmdOptions refactorings output send (splitOn ";" cmds)
    Nothing -> return ()
  when (isNothing executeCommands) (void $ forkIO $ processUserInput refactorings input output send)
  readFromSocket output recv

processUserInput :: [RefactoringChoice IdDom] -> Handle -> Handle -> Chan ClientMessage -> IO ()
processUserInput refactorings input output chan = do
  cmd <- hGetLine input
  continue <- processCommand False refactorings output chan cmd
  when continue $ processUserInput refactorings input output chan

processCommand :: Bool -> [RefactoringChoice IdDom] -> Handle -> Chan ClientMessage -> String -> IO Bool
processCommand shutdown refactorings output chan cmd = do
  case splitOn " " cmd of
    ["Exit"] -> writeChan chan Disconnect >> return False
    ref : rest | let modPath:selection:details = rest ++ (replicate (2 - length rest) "")
               , ref `elem` refactorCommands refactorings
       -> do writeChan chan (PerformRefactoring ref modPath selection details shutdown) >> return (not shutdown)

    _ -> do liftIO $ hPutStrLn output $ "'" ++ cmd ++ "' is not a known command. Commands are: Exit, "
                                            ++ intercalate ", " (refactorCommands refactorings)
            return True

readFromSocket :: Handle -> Chan ResponseMsg -> IO Bool
readFromSocket output recv = do
  continue <- readChan recv >>= processMessage output
  maybe (readFromSocket output recv) return continue

-- | Returns Nothing if the execution should continue, Just False on erronous termination
-- and Just True on normal termination.
processMessage :: Handle -> ResponseMsg -> IO (Maybe Bool)
processMessage output (ErrorMessage msg) = hPutStrLn output msg >> return (Just False)
processMessage output (CompilationProblem marks) = hPutStrLn output (show marks) >> return Nothing
processMessage output (LoadedModules mods)
  = mapM (\(fp,name) -> hPutStrLn output $ "Loaded module: " ++ name ++ "( " ++ fp ++ ") ") mods >> return Nothing
processMessage output (UnusedFlags flags)
  = if not $ null flags
      then do hPutStrLn output $ "Error: The following ghc-flags are not recognized: "
                                    ++ intercalate " " flags
              return $ Just True
      else return Nothing
processMessage _ Disconnected = return (Just True)
processMessage _ _ = return Nothing

loadModules :: Chan ClientMessage -> [String] -> IO ()
loadModules chan flags = writeChan chan (AddPackages roots)
  where roots = filter (not . ("-" `isPrefixOf`)) flags

performCmdOptions :: [RefactoringChoice IdDom] -> Handle -> Chan ClientMessage -> [String] -> IO ()
performCmdOptions refactorings output chan cmds = do
  continue <- mapM (processCommand True refactorings output chan) cmds
  when (not $ and continue) $ writeChan chan Disconnect
