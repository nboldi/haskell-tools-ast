module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit (assertBool, testCase)

import Control.Monad ((=<<), when, forM_)
import Data.ByteString.Char8 (pack, unpack)
import Data.Knob (newKnob, newFileHandle, getContents)
import qualified Data.List as List
import System.Directory
import System.FilePath
import System.IO

import Language.Haskell.Tools.Refactor.Builtin (builtinRefactorings)
import Language.Haskell.Tools.Refactor.CLI (normalRefactorSession)

main :: IO ()
main = defaultMain allTests

allTests :: TestTree
allTests
  = testGroup "cli-tests" [
      makeCliTest ( "batch", ["examples"</>"example-project"]
                  , \s -> ["-exec=RenameDefinition " ++ "examples"</>("example-project"++s)</>"Demo.hs" ++ " 3:1 b"]
                  , \_ -> ""
                  , \s _ -> checkFileContent ("examples"</>("example-project"++s)</>"Demo.hs")
                                             ("b = ()" `List.isInfixOf`))
    , makeCliTest ( "session", ["examples"</>"example-project"], \_ -> []
                  , \s -> "RenameDefinition " ++ "examples"</>("example-project"++s)</>"Demo.hs" ++ " 3:1 b\nExit\n"
                  , \s _ -> checkFileContent ("examples"</>("example-project"++s)</>"Demo.hs")
                                             ("b = ()" `List.isInfixOf`))
    ]

makeCliTest :: (String, [FilePath], String -> [String], String -> String, String -> String -> IO Bool) -> TestTree
makeCliTest (name, dirs, args, input, outputCheck)
  = let dir = joinPath $ longestCommonPrefix $ map splitDirectories dirs
        testdirs = map (\d -> if d == dir then dir ++ suffix else (dir ++ suffix </> makeRelative dir d)) dirs
    in testCase name $ do
      putStrLn "makeCliTest 1"
      copyDir dir (dir ++ suffix)
      putStrLn "makeCliTest 2"
      inKnob <- newKnob (pack (input suffix))
      putStrLn "makeCliTest 3"
      inHandle <- newFileHandle inKnob "<input>" ReadMode
      putStrLn "makeCliTest 4"
      outKnob <- newKnob (pack [])
      putStrLn "makeCliTest 5"
      outHandle <- newFileHandle outKnob "<output>" WriteMode
      putStrLn "makeCliTest 6"
      res <- normalRefactorSession builtinRefactorings inHandle outHandle (args suffix ++ testdirs)
      putStrLn "makeCliTest 7"
      actualOut <- Data.Knob.getContents outKnob
      putStrLn "makeCliTest 8"
      assertBool ("The result is not what is expected. Output: " ++ (unpack actualOut))
        =<< outputCheck suffix (unpack actualOut)
  where suffix = "_test_" ++ name

checkFileContent :: FilePath -> (String -> Bool) -> IO Bool
checkFileContent fp check = check <$> readFile fp

copyDir :: FilePath -> FilePath -> IO ()
copyDir src dst = do
  exists <- doesDirectoryExist dst
  -- clear the target directory from possible earlier test runs
  when exists $ removeDirectoryRecursive dst
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

longestCommonPrefix :: (Eq a) => [[a]] -> [a]
longestCommonPrefix = foldl1 commonPrefix

commonPrefix :: (Eq e) => [e] -> [e] -> [e]
commonPrefix _ [] = []
commonPrefix [] _ = []
commonPrefix (x:xs) (y:ys)
  | x == y    = x : commonPrefix xs ys
  | otherwise = []
