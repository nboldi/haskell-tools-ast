module Main where

import Test.HUnit
import System.Exit
import System.FilePath
import Data.Knob
import Data.ByteString.Char8 (pack, unpack)
import System.IO

import Language.Haskell.Tools.Refactor.CLI

main :: IO ()
main = run allTests

run :: [Test] -> IO ()
run tests = do results <- runTestTT $ TestList tests
               if errors results + failures results > 0 
                  then exitFailure
                  else exitSuccess

allTests :: [Test]
allTests = map makeCliTest cliTests

makeCliTest :: (String, [String], String, String) -> Test
makeCliTest (dir, args, input, output) = TestLabel dir $ TestCase $ do 
    inKnob <- newKnob (pack input)
    inHandle <- newFileHandle inKnob "<input>" ReadMode
    outKnob <- newKnob (pack [])
    outHandle <- newFileHandle outKnob "<output>" WriteMode
    res <- refactorSession inHandle outHandle (args ++ [dir])
    actualOut <- Data.Knob.getContents outKnob
    assertEqual "" (filter (/= '\r') output) (filter (/= '\r') $ unpack actualOut)

cliTests :: [(String, [String], String, String)]
cliTests 
  = [ ( ".." </> ".." </> "examples" </> "Project" </> "source-dir"
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"GenerateSignature 3:1-3:1\""] 
      , "", prefixText ["A"] ++ "### Module changed: A\n### new content:\nmodule A where\n\nx :: ()\nx = ()\n")
    , ( ".." </> ".." </> "examples" </> "Project" </> "source-dir-outside"
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"GenerateSignature 3:1-3:1\""] 
      , "", prefixText ["A"] ++ "### Module changed: A\n### new content:\nmodule A where\n\nx :: ()\nx = ()\n")
    , ( ".." </> ".." </> "examples" </> "Project" </> "no-cabal"
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"GenerateSignature 3:1-3:1\""] 
      , "", prefixText ["A"] ++ "### Module changed: A\n### new content:\nmodule A where\n\nx :: ()\nx = ()\n")
    , ( ".." </> ".." </> "examples" </> "Project" </> "has-cabal"
      , ["-dry-run", "-one-shot", "-module-name=A", "-refactoring=\"GenerateSignature 3:1-3:1\""] 
      , "", prefixText ["A"] ++ "### Module changed: A\n### new content:\nmodule A where\n\nx :: ()\nx = ()\n")
    -- , ( ".." </> ".." </> "examples" </> "Refactor" </> "RenameDefinition" </> "MultiModule", [] 
    --   , "", "")
    ]

prefixText :: [String] -> String
prefixText mods 
  = "Compiling modules. This may take some time. Please wait.\n" 
      ++ concatMap (\m -> "Loaded module: " ++ m ++ "\n") mods 
      ++ "All modules loaded. Use 'SelectModule module-name' to select a module\n"