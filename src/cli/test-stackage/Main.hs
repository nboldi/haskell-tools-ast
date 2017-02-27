{-# LANGUAGE LambdaCase
           #-}
module Main where

import Control.Applicative
import Control.Monad
import System.Directory
import System.Process
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
           , Right $ do threadDelay 500000
                        createDirectoryIfMissing False testedDir
                        removeDirectoryRecursive testedDir
                        threadDelay 500000
                        renameDirectory pack testedDir
           , Left ("stack build --test --no-run-tests --bench --no-run-benchmarks > logs\\" ++ pack ++ "-build-log.txt 2>&1", BuildFailure)
           , Left ("stack exec ht-refact --stack-yaml=..\\stack.yaml -- -one-shot -refactoring=ProjectOrganizeImports tested-package tested-package\\.stack-work\\dist\\" ++ snapshotId ++ "\\build\\autogen -package base +RTS -M6G -RTS > logs\\" ++ pack ++ "-refact-log.txt 2>&1", RefactError)
           , Left ("stack build > logs\\" ++ pack ++ "-reload-log.txt 2>&1", WrongCodeError)
           ]
  problem <- case res of
               RefactError -> map (\case '\n' -> ' '; c -> c) <$> readFile ("logs\\" ++ pack ++ "-refact-log.txt")
               WrongCodeError -> map (\case '\n' -> ' '; c -> c) <$> readFile ("logs\\" ++ pack ++ "-reload-log.txt")
               _ -> return ""
  return (res, problem)

  where testedDir = "tested-package"
        snapshotId = "ca59d0ab"

runCommands :: [Either (String, Result) (IO ())] -> IO Result
runCommands [] = return OK
runCommands (Left (cmd,failRes) : rest) = do
  exitCode <- waitForProcess =<< runCommand cmd
  case exitCode of ExitSuccess -> runCommands rest
                   ExitFailure _ -> return failRes
runCommands (Right act : rest) = act >> runCommands rest
