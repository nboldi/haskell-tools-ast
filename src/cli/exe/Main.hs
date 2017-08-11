module Main where

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (IO, stdout, stdin)
import Options.Applicative
import Options.Applicative.Types
import Control.Monad
import Control.Monad.Reader
import Data.Semigroup ((<>))
import Data.List
import Data.List.Split

import Language.Haskell.Tools.Refactor.Builtin (builtinRefactorings)
import Language.Haskell.Tools.Refactor.CLI (normalRefactorSession, CLIOptions(..))

main :: IO ()
main = exit =<< normalRefactorSession builtinRefactorings stdin stdout =<< execParser opts
  where exit :: Bool -> IO ()
        exit True = exitSuccess
        exit False = exitFailure
        opts = info (cliOptions <**> helper)
                    (fullDesc
                      <> progDesc "Run a refactoring or open a session"
                      <> header "ht-refact: a command-line interface for Haskell-tools")

cliOptions :: Parser CLIOptions
cliOptions
  = CLIOptions <$> version
               <*> oneShot
               <*> ghcFlags
               <*> packages
  where version = switch (long "version"
                            <> short 'v'
                            <> help "Show the version of this software")
        oneShot
          = optional $ strOption (long "execute"
                                   <> short 'e'
                                   <> metavar "COMMAND"
                                   <> help "Commands to execute in a one-shot refactoring run, separated by semicolons.")
        ghcFlags
          = optional $ option ghcFlagsParser
                         (long "ghc"
                           <> short 'g'
                           <> metavar "GHC_OPTIONS"
                           <> help "Flags passed to GHC when loading the packages.")
          where ghcFlagsParser :: ReadM [String]
                ghcFlagsParser
                  = ReadM $ do splitted <- splitOn " " <$> ask
                               let wrong = filter (not . isPrefixOf "-") splitted
                               when (not $ null wrong)
                                 $ fail ("The following arguments passed as ghc-options are not flags: " ++ intercalate " " wrong)
                               return splitted
        packages = many $ strArgument (metavar "PACKAGE_ROOT"
                                        <> help "The root folder of packages that are refactored")
