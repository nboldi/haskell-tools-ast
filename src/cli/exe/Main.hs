module Main where

import System.IO
import System.Environment
import System.Exit

import Language.Haskell.Tools.Refactor.CLI

main :: IO ()
main = normalRefactorSession stdin stdout =<< getArgs
