module Main where

import Control.Exception (finally, catch, SomeException)
import Control.Monad
import Data.List (intercalate)
import System.Directory
import Data.Knob (newKnob, newFileHandle)
import qualified Data.ByteString.Char8 as BS (pack)
import System.Environment (getArgs)
import System.FilePath (FilePath, (</>))
import System.IO

import Language.Haskell.Tools.Daemon.Options (SharedDaemonOptions(..))
import Language.Haskell.Tools.Refactor.Builtin (builtinRefactorings)
import Language.Haskell.Tools.Refactor.CLI (CLIOptions(..), normalRefactorSession)

main :: IO ()
main = mapM_ (uncurry makeCliTest) tests

projectDirs = map ((".." </> "..") </>)
                [ "demo"
                , "src" </> "ast"
                , "src" </> "backend-ghc"
                , "src" </> "builtin-refactorings"
                , "src" </> "cli"
                , "src" </> "daemon"
                , "src" </> "debug"
                , "src" </> "experimental-refactorings"
                , "src" </> "prettyprint"
                , "src" </> "refactor"
                , "src" </> "rewrite"
                ]

tests :: [(String, [String])]
tests = [ ("just-load"
          , [ "Exit"
            ] )
        ]

makeCliTest :: String -> [String] -> IO ()
makeCliTest name rfs
  = do putStrLn $ "running test " ++ name
       --mapM_ (\wd -> copyDir wd (wd ++ "_orig")) projectDirs
       putStrLn "starting cli"
       void $ normalRefactorSession builtinRefactorings stdin stdout
                (CLIOptions False True (Just $ intercalate ";" rfs)
                            (SharedDaemonOptions True Nothing False False Nothing) projectDirs)
       putStrLn "cli finished"
  -- `finally` (do mapM_ (\wd -> do removeDirectoryRecursive wd
  --                                renameDirectory (wd ++ "_orig") wd) projectDirs
  --               putStrLn "directories removed"
  --             `catch` \e -> print (e :: SomeException))

copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dst = do
  createDirectory dst
  content <- getDirectoryContents src
  let xs = filter (`notElem` [".", ".."]) content
  forM_ xs $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then copyDir srcPath dstPath
      else copyFile srcPath dstPath