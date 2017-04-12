{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import System.Directory
import System.IO
import System.Process
import System.Timeout
import System.Environment
import System.Exit
import Control.Concurrent
import Data.List
import Data.List.Split

data Result = GetFailure
            | BuildFailure
            | RefactError
            | WrongCodeError
            | OK
  deriving Show

main :: IO ()
main = do args <- getArgs
          testHackage args

testHackage :: [String] -> IO ()
testHackage args = do
  createDirectoryIfMissing False workDir
  withCurrentDirectory workDir $ do
    packages <- lines <$> readFile (last args)
    alreadyTested <- if noRetest then do appendFile resultFile ""
                                         map (head . splitOn ";") . filter (not . null) . lines
                                           <$> readFile resultFile
                                 else writeFile resultFile "" >> return []
    let filteredPackages = packages \\ alreadyTested
    createDirectoryIfMissing False "logs"
    mapM_ testAndEvaluate filteredPackages
  where workDir = "stackage-test"
        resultFile = "results.csv"

        noRetest = "-no-retest" `elem` args
        testAndEvaluate p = do
          (res, problem) <- testPackage p
          appendFile resultFile (p ++ ";" ++ show res ++ " ; " ++ problem ++ "\n")


testPackage :: String -> IO (Result, String)
testPackage pack = do
  res <- runCommands
           [ Left ("cabal get " ++ pack, GetFailure)
           , Right refreshDir
           , Left ("stack build --test --no-run-tests --bench --no-run-benchmarks > logs\\" ++ pack ++ "-build-log.txt 2>&1", BuildFailure)
           -- correct rts option handling (on windows) requires stack 1.4
           , Left ("stack exec ht-refact --stack-yaml=..\\stack.yaml --rts-options -M4G -- -one-shot -refactoring=ProjectOrganizeImports tested-package tested-package\\.stack-work\\dist\\" ++ snapshotId ++ "\\build\\autogen -package base > logs\\" ++ pack ++ "-refact-log.txt 2>&1", RefactError)
           , Left ("stack build > logs\\" ++ pack ++ "-reload-log.txt 2>&1", WrongCodeError)
           ]
  problem <- case res of
               RefactError -> map (\case '\n' -> ' '; c -> c) <$> readFile ("logs\\" ++ pack ++ "-refact-log.txt")
               WrongCodeError -> map (\case '\n' -> ' '; c -> c) <$> readFile ("logs\\" ++ pack ++ "-reload-log.txt")
               _ -> return ""
  return (res, problem)
  where testedDir = "tested-package"
        snapshotId = "ca59d0ab"
        refreshDir = refreshDir' 3
        refreshDir' n = do createDirectoryIfMissing False testedDir
                           removeDirectoryRecursive testedDir
                           renameDirectory pack testedDir
                         `catch` \e -> if n <= 0
                                         then throwIO (e :: IOException)
                                         else do threadDelay 500000
                                                 refreshDir' (n-1)

runCommands :: [Either (String, Result) (IO ())] -> IO Result
runCommands [] = return OK
runCommands (Left (cmd,failRes) : rest) = do
  pr <- runCommand cmd
  exitCode <- waitForProcess pr
  case exitCode of ExitSuccess -> runCommands rest
                   ExitFailure _ -> return failRes
runCommands (Right act : rest) = act >> runCommands rest
