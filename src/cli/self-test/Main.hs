module Main where

import Control.Exception (finally, catch, SomeException)
import Control.Monad
import Data.List (intercalate, find, isSuffixOf)
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
main = do dirs <- projectDirs
          mapM_ (uncurry (makeCliTest dirs)) (tests dirs)

suf = "_copy"

projectDirs :: IO [FilePath]
projectDirs = mapM (canonicalizePath . ((".." </> "..") </>))
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

tests :: [FilePath] -> [(String, [String])]
tests roots
  = [ ("just-load", [ "Exit" ] )
    , ("load-and-reload"
      , [ "ChangeFile " ++ (ast ++ suf </> "Language" </> "Haskell" </> "Tools" </> "AST" </> "Ann.hs")
        , "Exit"
        ] )
    ]
  where
   Just ast = find ("ast" `isSuffixOf`) roots

makeCliTest :: [FilePath] -> String -> [String] -> IO ()
makeCliTest dirs name rfs
  = do mapM_ (\wd -> copyDir wd (wd ++ suf)) dirs
       void $ normalRefactorSession builtinRefactorings stdin stdout
                (CLIOptions False True (Just $ intercalate ";" rfs)
                            (SharedDaemonOptions True Nothing False False Nothing) (map (++ suf) dirs))
       mapM_ (\wd -> removeDirectoryRecursive (wd ++ suf)) dirs

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