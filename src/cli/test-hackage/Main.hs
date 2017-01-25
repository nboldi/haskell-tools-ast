{-# LANGUAGE LambdaCase 
           #-}
module Main where

import Control.Applicative
import Control.Monad
import System.Directory
import System.Process
import System.Environment
import System.Exit
import Data.List
import Data.List.Split

data Result = DepInstallFailure
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
    unsetEnv "GHC_PACKAGE_PATH"
    callCommand "cabal update"
    callCommand "cabal list --simple > packages.txt 2>&1"
    packages <- map (map (\case ' ' -> '-'; c -> c)) . lines <$> readFile "packages.txt"
    alreadyTested <- if noRetest then do appendFile resultFile "" 
                                         map (head . splitOn ";") . filter (not . null) . lines 
                                           <$> readFile "results.csv"
                                 else writeFile resultFile "" >> return []
    putStrLn $ "Skipping " ++ show (length alreadyTested) ++ " already tested packages"
    let filteredPackages = packages \\ alreadyTested
    mapM_ testAndEvaluate filteredPackages
  where workDir = "hackage-test"
        resultFile = "results.csv"

        noRetest = "-no-retest" `elem` args
        testAndEvaluate p = do
          res <- testPackage p
          appendFile resultFile (p ++ ";" ++ show res ++ "\n")


testPackage :: String -> IO Result
testPackage pack = do
  downloaded <- doesDirectoryExist pack
  when (not downloaded) $ do
    callCommand ("cabal get " ++ pack)
  withCurrentDirectory pack $ do
    callCommand "cabal sandbox init"
    runCommands [ ("cabal install --only-dependencies > deps-log.txt 2>&1", DepInstallFailure)
                , ("cabal configure > config-log.txt 2>&1", BuildFailure)
                , ("cabal build > build-log.txt 2>&1", BuildFailure)
                , ("ht-refact -one-shot -refactoring=ProjectOrganizeImports -package-db .cabal-sandbox\\x86_64-windows-ghc-8.0.1-packages.conf.d . > refact-log.txt 2>&1", RefactError)
                , ("cabal build > reload-log.txt 2>&1", WrongCodeError)
                ]

runCommands :: [(String, Result)] -> IO Result
runCommands [] = return OK
runCommands ((cmd,failRes):rest) = do 
  exitCode <- waitForProcess =<< runCommand cmd
  case exitCode of ExitSuccess -> runCommands rest
                   ExitFailure _ -> return failRes