{-# LANGUAGE LambdaCase
           , ViewPatterns
           , TypeFamilies
           #-}
module Main where

import GHC hiding (loadModule, ParsedModule)
import DynFlags
import GHC.Paths ( libdir )
import Module as GHC

import Control.Reference
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad
import Data.Maybe
import qualified Data.Map as Map
import Data.List
import Data.Either.Combinators
import Test.HUnit hiding (test)
import System.IO
import System.Exit
import System.FilePath
import Data.IntSet (member)
import Language.Haskell.TH.LanguageExtensions

import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.Rewrite as G
import Language.Haskell.Tools.AST.FromGHC
import Language.Haskell.Tools.Transform
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.Refactor.Perform
import Language.Haskell.Tools.Refactor.Prepare
import Language.Haskell.Tools.Refactor.GetModules
import Language.Haskell.Tools.Refactor.Predefined.OrganizeImports
import Language.Haskell.Tools.Refactor.Predefined.GenerateTypeSignature
import Language.Haskell.Tools.Refactor.Predefined.GenerateExports
import Language.Haskell.Tools.Refactor.Predefined.RenameDefinition
import Language.Haskell.Tools.Refactor.Predefined.ExtractBinding
import Language.Haskell.Tools.Refactor.RefactorBase
import Language.Haskell.Tools.Refactor.Session

import Language.Haskell.Tools.Refactor.Predefined.DataToNewtype
import Language.Haskell.Tools.Refactor.Predefined.IfToGuards
import Language.Haskell.Tools.Refactor.Predefined.DollarApp

main :: IO ()
main = run nightlyTests

run :: [Test] -> IO ()
run tests = do results <- runTestTT $ TestList tests
               if errors results + failures results > 0 
                  then exitFailure
                  else exitSuccess

nightlyTests :: [Test]
nightlyTests = unitTests 
                 ++ map makeCpphsTest cppHsTests
                 ++ map makeInstanceControlTest instanceControlTests

unitTests :: [Test]
unitTests = genTests ++ functionalTests

functionalTests :: [Test]
functionalTests = map makeReprintTest checkTestCases
              ++ map makeOrganizeImportsTest organizeImportTests
              ++ map makeGenerateSignatureTest generateSignatureTests
              ++ map makeWrongGenerateSigTest wrongGenerateSigTests
              ++ map makeGenerateExportsTest generateExportsTests
              ++ map makeRenameDefinitionTest renameDefinitionTests
              ++ map makeWrongRenameDefinitionTest wrongRenameDefinitionTests
              ++ map makeExtractBindingTest extractBindingTests
              ++ map makeWrongExtractBindingTest wrongExtractBindingTests
              ++ map makeInlineBindingTest inlineBindingTests
              ++ map makeWrongInlineBindingTest wrongInlineBindingTests
              ++ map (makeMultiModuleTest checkMultiResults) multiModuleTests
              ++ map (makeMultiModuleTest checkMultiFail) wrongMultiModuleTests
              ++ map makeMiscRefactorTest miscRefactorTests
  where checkTestCases = languageTests 
                          ++ organizeImportTests 
                          ++ map fst generateSignatureTests 
                          ++ generateExportsTests
                          ++ map (\(mod,_,_) -> mod) renameDefinitionTests
                          ++ map (\(mod,_,_) -> mod) wrongRenameDefinitionTests
                          ++ map (\(mod,_,_) -> mod) extractBindingTests
                          ++ map (\(mod,_,_) -> mod) wrongExtractBindingTests
                          ++ map (\(mod,_) -> mod) inlineBindingTests

rootDir = ".." </> ".." </> "examples"
        
languageTests =
  [ "Decl.AmbiguousFields"
  , "Decl.AnnPragma"
  , "Decl.ClosedTypeFamily"
  , "Decl.CtorOp"
  , "Decl.DataFamily"
  , "Decl.DataType"
  , "Decl.DataTypeDerivings"
  , "Decl.FunBind"
  , "Decl.FunctionalDeps"
  , "Decl.FunGuards"
  , "Decl.GADT"
  , "Decl.InjectiveTypeFamily"
  , "Decl.InlinePragma"
  , "Decl.InstanceOverlaps"
  , "Decl.InstanceSpec"
  , "Decl.LocalBindings"
  , "Decl.LocalFixity"
  , "Decl.MultipleFixity"
  , "Decl.MultipleSigs"
  , "Decl.OperatorBind"
  , "Decl.OperatorDecl"
  , "Decl.ParamDataType"
  , "Decl.PatternBind"
  , "Decl.PatternSynonym"
  , "Decl.RecordPatternSynonyms"
  , "Decl.RecordType"
  , "Decl.RewriteRule"
  , "Decl.SpecializePragma"
  , "Decl.StandaloneDeriving"
  , "Decl.TypeClass"
  , "Decl.TypeClassMinimal"
  , "Decl.TypeFamily"
  , "Decl.TypeFamilyKindSig"
  , "Decl.TypeInstance"
  , "Decl.TypeRole"
  , "Decl.TypeSynonym"
  , "Decl.ValBind"
  , "Expr.ArrowNotation"
  , "Expr.Case"
  , "Expr.DoNotation"
  , "Expr.GeneralizedListComp"
  , "Expr.EmptyCase"
  , "Expr.If"
  , "Expr.ImplicitParams"
  , "Expr.LambdaCase"
  , "Expr.ListComp"
  , "Expr.MultiwayIf"
  , "Expr.Negate"
  , "Expr.Operator"
  , "Expr.ParenName"
  , "Expr.ParListComp"
  , "Expr.RecordPuns"
  , "Expr.RecordWildcards"
  , "Expr.RecursiveDo"
  , "Expr.Sections"
  , "Expr.StaticPtr"
  , "Expr.TupleSections"
  , "Module.Simple"
  , "Module.GhcOptionsPragma"
  , "Module.Export"
  , "Module.NamespaceExport"
  , "Module.Import"
  , "Pattern.Backtick"
  , "Pattern.Constructor"
  , "Pattern.ImplicitParams"
  , "Pattern.Infix"
  , "Pattern.NPlusK"
  , "Pattern.Record"
  , "Type.Bang"
  , "Type.Builtin"
  , "Type.Ctx"
  , "Type.ExplicitTypeApplication"
  , "Type.Forall"
  , "Type.Primitives"
  , "Type.TypeOperators"
  , "Type.Unpack"
  , "Type.Wildcard"
  , "TH.Brackets"
  , "TH.QuasiQuote.Define"
  , "TH.QuasiQuote.Use"
  , "TH.Splice.Define"
  , "TH.Splice.Use"
  , "Refactor.CommentHandling.CommentTypes"
  , "Refactor.CommentHandling.BlockComments"
  , "Refactor.CommentHandling.Crosslinking"
  , "Refactor.CommentHandling.FunctionArgs"
  ]

cppHsTests = 
  [ "Language.Preprocessor.Cpphs"
  , "Language.Preprocessor.Unlit"
  , "Language.Preprocessor.Cpphs.CppIfdef"
  , "Language.Preprocessor.Cpphs.HashDefine"
  , "Language.Preprocessor.Cpphs.MacroPass"
  , "Language.Preprocessor.Cpphs.Options"
  , "Language.Preprocessor.Cpphs.Position"
  , "Language.Preprocessor.Cpphs.ReadFirst"
  , "Language.Preprocessor.Cpphs.RunCpphs"
  , "Language.Preprocessor.Cpphs.SymTab"
  , "Language.Preprocessor.Cpphs.Tokenise"
  ]

instanceControlTests = 
  [ "Control.Instances.Test"
  , "Control.Instances.Morph"
  , "Control.Instances.ShortestPath"
  , "Control.Instances.TypeLevelPrelude"
  ]
        
organizeImportTests = 
  [ "Refactor.OrganizeImports.Narrow"
  , "Refactor.OrganizeImports.Reorder"
  , "Refactor.OrganizeImports.Unused"
  , "Refactor.OrganizeImports.Ctor"
  , "Refactor.OrganizeImports.Class"
  , "Refactor.OrganizeImports.Operator"
  , "Refactor.OrganizeImports.SameName"
  , "Refactor.OrganizeImports.Removed"
  ]
  
generateSignatureTests = 
  [ ("Refactor.GenerateTypeSignature.Simple", "3:1-3:10")
  , ("Refactor.GenerateTypeSignature.Function", "3:1-3:15")
  , ("Refactor.GenerateTypeSignature.HigherOrder", "3:1-3:14")
  , ("Refactor.GenerateTypeSignature.Polymorph", "3:1-3:10")
  , ("Refactor.GenerateTypeSignature.PolymorphSub", "5:3-5:4")
  , ("Refactor.GenerateTypeSignature.PolymorphSubMulti", "5:3-5:4")
  , ("Refactor.GenerateTypeSignature.Placement", "4:1-4:10")
  , ("Refactor.GenerateTypeSignature.Tuple", "3:1-3:18")
  , ("Refactor.GenerateTypeSignature.Complex", "3:1-3:21")
  , ("Refactor.GenerateTypeSignature.Local", "4:3-4:12")
  , ("Refactor.GenerateTypeSignature.Let", "3:9-3:18")
  , ("Refactor.GenerateTypeSignature.TypeDefinedInModule", "3:1-3:1")
  , ("Refactor.GenerateTypeSignature.BringToScope.AlreadyQualImport", "6:1-6:2")
  , ("Refactor.GenerateTypeSignature.CanCaptureVariable", "8:10-8:10")
  , ("Refactor.GenerateTypeSignature.CanCaptureVariableHasOtherDef", "8:10-8:10")
  ]

wrongGenerateSigTests = 
  [ ("Refactor.GenerateTypeSignature.CannotCaptureVariable", "7:10-7:10")
  , ("Refactor.GenerateTypeSignature.CannotCaptureVariableNoExt", "8:10-8:10")
  ]

generateExportsTests = 
  [ "Refactor.GenerateExports.Normal"
  , "Refactor.GenerateExports.Operators"
  ]

renameDefinitionTests =
  [ ("Refactor.RenameDefinition.AmbiguousFields", "4:14-4:15", "xx")
  , ("Refactor.RenameDefinition.RecordField", "3:22-3:23", "xCoord")
  , ("Refactor.RenameDefinition.Constructor", "3:14-3:19", "Point2D")
  , ("Refactor.RenameDefinition.Type", "5:16-5:16", "Point2D")
  , ("Refactor.RenameDefinition.Function", "3:1-3:2", "q")
  , ("Refactor.RenameDefinition.QualName", "3:1-3:2", "q")
  , ("Refactor.RenameDefinition.BacktickName", "3:1-3:2", "g")
  , ("Refactor.RenameDefinition.ParenName", "4:3-4:5", "<->")
  , ("Refactor.RenameDefinition.RecordWildcards", "4:32-4:33", "yy")
  , ("Refactor.RenameDefinition.RecordPatternSynonyms", "4:16-4:17", "xx")
  , ("Refactor.RenameDefinition.ClassMember", "7:3-7:4", "q")
  , ("Refactor.RenameDefinition.LocalFunction", "4:5-4:6", "g")
  , ("Refactor.RenameDefinition.LayoutAware", "3:1-3:2", "main")
  , ("Refactor.RenameDefinition.Arg", "4:3-4:4", "y")
  , ("Refactor.RenameDefinition.FunTypeVar", "3:6-3:7", "x")
  , ("Refactor.RenameDefinition.FunTypeVarLocal", "5:10-5:11", "b")
  , ("Refactor.RenameDefinition.ClassTypeVar", "3:9-3:10", "f")
  , ("Refactor.RenameDefinition.TypeOperators", "4:13-4:15", "x1")
  , ("Refactor.RenameDefinition.NoPrelude", "4:1-4:2", "map")
  , ("Refactor.RenameDefinition.UnusedDef", "3:1-3:2", "map")
  , ("Refactor.RenameDefinition.ImplicitParams", "8:17-8:20", "compare")
  , ("Refactor.RenameDefinition.SameCtorAndType", "3:6-3:13", "P2D")
  , ("Refactor.RenameDefinition.RoleAnnotation", "4:11-4:12", "AA")
  , ("Refactor.RenameDefinition.TypeBracket", "6:6-6:7", "B")
  , ("Refactor.RenameDefinition.ValBracket", "8:11-8:12", "B")
  ]

wrongRenameDefinitionTests =
  [ ("Refactor.RenameDefinition.LibraryFunction", "4:5-4:7", "identity")
  , ("Refactor.RenameDefinition.NameClash", "5:9-5:10", "h")
  , ("Refactor.RenameDefinition.NameClash", "3:1-3:2", "map")
  , ("Refactor.RenameDefinition.WrongName", "4:1-4:2", "F")
  , ("Refactor.RenameDefinition.WrongName", "4:1-4:2", "++")
  , ("Refactor.RenameDefinition.WrongName", "7:6-7:7", "x")
  , ("Refactor.RenameDefinition.WrongName", "7:6-7:7", ":+:")
  , ("Refactor.RenameDefinition.WrongName", "7:10-7:11", "x")
  , ("Refactor.RenameDefinition.WrongName", "9:6-9:7", "A")
  , ("Refactor.RenameDefinition.WrongName", "9:19-9:19", ".+++.")
  , ("Refactor.RenameDefinition.WrongName", "11:3-11:3", ":+++:")
  , ("Refactor.RenameDefinition.IllegalQualRename", "4:30-4:34", "Bl")
  ]

extractBindingTests =
  [ ("Refactor.ExtractBinding.Simple", "3:19-3:27", "exaggerate")
  , ("Refactor.ExtractBinding.Parentheses", "3:23-3:62", "sqDistance")
  , ("Refactor.ExtractBinding.AddToExisting", "3:10-3:12", "b")
  , ("Refactor.ExtractBinding.LocalDefinition", "4:13-4:16", "y")
  , ("Refactor.ExtractBinding.ClassInstance", "6:30-6:35", "g")
  , ("Refactor.ExtractBinding.ListComprehension", "5:25-5:39", "notDivisible")
  , ("Refactor.ExtractBinding.Records", "5:5-5:39", "plus")
  , ("Refactor.ExtractBinding.RecordWildcards", "6:5-6:27", "plus")
  ]

wrongExtractBindingTests = 
  [ ("Refactor.ExtractBinding.TooSimple", "3:19-3:20", "x")
  , ("Refactor.ExtractBinding.NameConflict", "3:19-3:27", "stms")
  ]

inlineBindingTests =
  [ ("Refactor.InlineBinding.Simplest", "4:1-4:2")
  , ("Refactor.InlineBinding.Nested", "4:1-4:2")
  , ("Refactor.InlineBinding.Local", "4:9-4:10")
  , ("Refactor.InlineBinding.LocalNested", "5:17-5:18")
  , ("Refactor.InlineBinding.WithLocals", "4:1-4:2")
  , ("Refactor.InlineBinding.MultiMatch", "4:1-4:2")
  , ("Refactor.InlineBinding.SimpleMultiMatch", "4:1-4:2")
  , ("Refactor.InlineBinding.AlreadyApplied", "4:1-4:2")
  , ("Refactor.InlineBinding.PatternMatched", "4:1-4:2")
  , ("Refactor.InlineBinding.MultiApplied", "4:1-4:2")
  , ("Refactor.InlineBinding.Operator", "4:1-4:2")
  , ("Refactor.InlineBinding.MultiMatchGuarded", "4:1-4:2")
  , ("Refactor.InlineBinding.RemoveSignatures", "5:1-5:2")
  , ("Refactor.InlineBinding.FilterSignatures", "5:1-5:2")
  ]

wrongInlineBindingTests = 
  [ ("Refactor.InlineBinding.Recursive", "4:1-4:2")
  ]


multiModuleTests =
  [ ("RenameDefinition 5:5-5:6 bb", "A", "Refactor" </> "RenameDefinition" </> "MultiModule", [])
  , ("RenameDefinition 1:8-1:9 C", "B", "Refactor" </> "RenameDefinition" </> "RenameModule", ["B"])
  , ("RenameDefinition 3:8-3:9 C", "A", "Refactor" </> "RenameDefinition" </> "RenameModule", ["B"])
  , ("RenameDefinition 6:1-6:9 hello", "Use", "Refactor" </> "RenameDefinition" </> "SpliceDecls", [])
  , ("RenameDefinition 5:1-5:5 exprSplice", "Define", "Refactor" </> "RenameDefinition" </> "SpliceExpr", [])
  , ("RenameDefinition 6:1-6:4 spliceTyp", "Define", "Refactor" </> "RenameDefinition" </> "SpliceType", [])
  ]

wrongMultiModuleTests =
  [ ("InlineBinding 3:1-3:2", "A", "Refactor" </> "InlineBinding" </> "AppearsInAnother", [])
  ]

miscRefactorTests =
  [ ("Refactor.DataToNewtype.Cases", \m -> dataToNewtype)
  , ("Refactor.IfToGuards.Simple", \m -> ifToGuards (correctRefactorSpan m $ readSrcSpan "3:11-3:33"))
  , ("Refactor.DollarApp.FirstSingle", \m -> dollarApp (correctRefactorSpan m $ readSrcSpan "5:5-5:12"))
  , ("Refactor.DollarApp.FirstMulti", \m -> dollarApp (correctRefactorSpan m $ readSrcSpan "5:5-5:16"))
  , ("Refactor.DollarApp.InfixOperator", \m -> dollarApp (correctRefactorSpan m $ readSrcSpan "5:5-5:16"))
  , ("Refactor.DollarApp.AnotherOperator", \m -> dollarApp (correctRefactorSpan m $ readSrcSpan "5:5-5:15"))
  , ("Refactor.DollarApp.ImportDollar", \m -> dollarApp (correctRefactorSpan m $ readSrcSpan "6:5-6:12"))
  ]

makeMultiModuleTest :: ((String, String, String, [String]) -> Either String [(String, Maybe String)] -> IO ()) 
                         -> (String, String, String, [String]) -> Test
makeMultiModuleTest checker test@(refact, mod, root, removed)
  = TestLabel (root ++ ":" ++ mod) $ TestCase 
      $ do res <- performRefactors refact (rootDir </> root) [] mod
           checker test res
           
checkMultiResults :: (String, String, String, [String]) -> Either String [(String, Maybe String)] -> IO ()
checkMultiResults _ (Left err) = assertFailure $ "The transformation failed : " ++ err
checkMultiResults test@(_,_,root,removed) (Right ((name, Just mod):rest)) = 
  do expected <- loadExpected False ((rootDir </> root) ++ "_res") name
     assertEqual "The transformed result is not what is expected" (standardizeLineEndings expected)
                                                                  (standardizeLineEndings mod)
     checkMultiResults test (Right rest)
checkMultiResults (r,m,root,removed) (Right ((name, Nothing) : rest)) = checkMultiResults (r,m,root,delete name removed) (Right rest)
checkMultiResults (_,_,_,[]) (Right []) = return ()
checkMultiResults (_,_,_,removed) (Right []) 
  = assertFailure $ "Modules has not been marked as removed: " ++ concat (intersperse ", " removed)

checkMultiFail :: (String, String, String, [String]) -> Either String [(String, Maybe String)] -> IO ()
checkMultiFail _ (Left _) = return ()
checkMultiFail _ (Right _) = assertFailure "The transformation should fail."

createTest :: String -> [String] -> String -> Test
createTest refactoring args mod
  = TestLabel mod $ TestCase $ checkCorrectlyTransformed (refactoring ++ (concatMap (" "++) args)) rootDir mod

createFailTest :: String -> [String] -> String -> Test
createFailTest refactoring args mod
  = TestLabel mod $ TestCase $ checkTransformFails (refactoring ++ (concatMap (" "++) args)) rootDir mod

makeOrganizeImportsTest :: String -> Test
makeOrganizeImportsTest = createTest "OrganizeImports" []

makeGenerateSignatureTest :: (String, String) -> Test
makeGenerateSignatureTest (mod, rng) = createTest "GenerateSignature" [rng] mod

makeGenerateExportsTest :: String -> Test
makeGenerateExportsTest mod = createTest "GenerateExports" [] mod

makeRenameDefinitionTest :: (String, String, String) -> Test
makeRenameDefinitionTest (mod, rng, newName) = createTest "RenameDefinition" [rng, newName] mod

makeWrongRenameDefinitionTest :: (String, String, String) -> Test
makeWrongRenameDefinitionTest (mod, rng, newName) = createFailTest "RenameDefinition" [rng, newName] mod

makeWrongGenerateSigTest :: (String, String) -> Test
makeWrongGenerateSigTest (mod, rng) = createFailTest "GenerateSignature" [rng] mod

makeExtractBindingTest :: (String, String, String) -> Test
makeExtractBindingTest (mod, rng, newName) = createTest "ExtractBinding" [rng, newName] mod

makeWrongExtractBindingTest :: (String, String, String) -> Test
makeWrongExtractBindingTest (mod, rng, newName) = createFailTest "ExtractBinding" [rng, newName] mod
  
makeInlineBindingTest :: (String, String) -> Test
makeInlineBindingTest (mod, rng) = createTest "InlineBinding" [rng] mod
  
makeWrongInlineBindingTest :: (String, String) -> Test
makeWrongInlineBindingTest (mod, rng) = createFailTest "InlineBinding" [rng] mod

checkCorrectlyTransformed :: String -> String -> String -> IO ()
checkCorrectlyTransformed command workingDir moduleName
  = do expected <- loadExpected True workingDir moduleName
       res <- performRefactor command workingDir [] moduleName
       assertEqual "The transformed result is not what is expected" (Right (standardizeLineEndings expected)) 
                                                                    (mapRight standardizeLineEndings res)
makeMiscRefactorTest :: (String, UnnamedModule IdDom -> LocalRefactoring IdDom) -> Test
makeMiscRefactorTest (moduleName, refact)
  = TestLabel moduleName $ TestCase $
      do expected <- loadExpected True rootDir moduleName
         res <- testRefactor refact moduleName
         assertEqual "The transformed result is not what is expected" (Right (standardizeLineEndings expected)) 
                                                                      (mapRight standardizeLineEndings res)
        
testRefactor :: (UnnamedModule IdDom -> LocalRefactoring IdDom) -> String -> IO (Either String String)
testRefactor refact moduleName 
  = runGhc (Just libdir) $ do
      initGhcFlags
      useDirs [rootDir]
      mod <- loadModule rootDir moduleName >>= parseTyped
      res <- runRefactor (moduleName, mod) [] (localRefactoring $ refact mod) 
      case res of Right r -> return $ Right $ prettyPrint $ snd $ fromContentChanged $ head r
                  Left err -> return $ Left err

checkTransformFails :: String -> String -> String -> IO ()
checkTransformFails command workingDir moduleName
  = do res <- performRefactor command workingDir [] moduleName
       assertBool "The transform should fail for the given input" (isLeft res)
       
loadExpected :: Bool -> String -> String -> IO String
loadExpected resSuffix workingDir moduleName = 
  do -- need to use binary or line endings will be translated
     expectedHandle <- openBinaryFile (workingDir </> map (\case '.' -> pathSeparator; c -> c) moduleName ++ (if resSuffix then "_res" else "") ++ ".hs") ReadMode
     hGetContents expectedHandle

standardizeLineEndings = filter (/= '\r')
       
makeReprintTest :: String -> Test       
makeReprintTest mod = TestLabel mod $ TestCase (checkCorrectlyPrinted rootDir mod)

makeCpphsTest :: String -> Test       
makeCpphsTest mod = TestLabel mod $ TestCase (checkCorrectlyPrinted (rootDir </> "CppHs") mod)

makeInstanceControlTest :: String -> Test       
makeInstanceControlTest mod = TestLabel mod $ TestCase (checkCorrectlyPrinted (rootDir </> "InstanceControl") mod)

checkCorrectlyPrinted :: String -> String -> IO ()
checkCorrectlyPrinted workingDir moduleName 
  = do -- need to use binary or line endings will be translated
       expectedHandle <- openBinaryFile (workingDir </> map (\case '.' -> pathSeparator; c -> c) moduleName ++ ".hs") ReadMode
       expected <- hGetContents expectedHandle
       (actual, actual', actual'') <- runGhc (Just libdir) $ do
         parsed <- loadModule workingDir moduleName
         actual <- prettyPrint <$> parseAST parsed
         actual' <- prettyPrint <$> parseRenamed parsed
         actual'' <- prettyPrint <$> parseTyped parsed
         return (actual, actual', actual'')
       assertEqual "The original and the transformed source differ" expected actual
       assertEqual "The original and the transformed source differ" expected actual'
       assertEqual "The original and the transformed source differ" expected actual''

-- TODO: find out why the commented-out code doesn't work for the two Template Haskell tests. These work for CLI. 
-- performRefactors :: String -> String -> [String] -> String -> IO (Either String [(String, Maybe String)])
-- performRefactors command workingDir flags target = runGhc (Just libdir) $ flip evalStateT (initSession :: RefactorSessionState) $ do 
--   lift initGhcFlags
--   mods <- loadPackagesFrom ({- const $ return () -} \m -> liftIO $ (putStrLn ("Loaded module: " ++ workingDir ++ " " ++ m) >> hFlush stdout)) [workingDir]
--   (selectedMod, otherMods) <- getMods (Just $ SourceFileKey NormalHs target)
--   case selectedMod of 
--     Just (_, selMod) -> do 
--       res <- lift $ performCommand (readCommand command) (target, selMod) (map assocToNamedMod otherMods)
--       return $ (\case Right r -> Right $ (map (\case ContentChanged (n,m) -> (n, Just $ prettyPrint m)
--                                                      ModuleRemoved m -> (m, Nothing)
--                                               )) r
--                       Left l -> Left l) 
--              $ res
--     Nothing -> error "The selected module is not found"
    
performRefactors :: String -> String -> [String] -> String -> IO (Either String [(String, Maybe String)])
performRefactors command workingDir flags target = do 
    mods <- getAllModules [workingDir]
    runGhc (Just libdir) $ do
      initGhcFlagsForTest
      useFlags flags
      useDirs [workingDir]
      setTargets (map (\mod -> (Target (TargetModule (GHC.mkModuleName mod)) True Nothing)) (concatMap (map (^. sfkModuleName) . Map.keys . (^. mcModules)) mods))
      load LoadAllTargets
      allMods <- getModuleGraph
      selectedMod <- getModSummary (GHC.mkModuleName target)
      let otherModules = filter (not . (\ms -> ms_mod ms == ms_mod selectedMod && ms_hsc_src ms == ms_hsc_src selectedMod)) allMods 
      targetMod <- parseTyped selectedMod
      otherMods <- mapM parseTyped otherModules
      res <- performCommand (readCommand command) 
                            (target, targetMod) (zip (map (GHC.moduleNameString . moduleName . ms_mod) otherModules) otherMods)
      return $ (\case Right r -> Right $ (map (\case ContentChanged (n,m) -> (n, Just $ prettyPrint m)
                                                     ModuleRemoved m -> (m, Nothing)
                                              )) r
                      Left l -> Left l) 
             $ res

type ParsedModule = Ann AST.UModule (Dom RdrName) SrcTemplateStage

parseAST :: ModSummary -> Ghc ParsedModule
parseAST modSum = do
  let compExts = extensionFlags $ ms_hspp_opts modSum
      hasStaticFlags = fromEnum StaticPointers `member` compExts
      ms = if hasStaticFlags then forceAsmGen modSum else modSum
  p <- parseModule ms
  let annots = pm_annotations p
      srcBuffer = fromJust $ ms_hspp_buf $ pm_mod_summary p
  prepareAST srcBuffer . placeComments (snd annots) 
     <$> (runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule ms $ pm_parsed_source p)          

type RenamedModule = Ann AST.UModule (Dom GHC.Name) SrcTemplateStage

parseRenamed :: ModSummary -> Ghc RenamedModule
parseRenamed modSum = do
  let compExts = extensionFlags $ ms_hspp_opts modSum
      hasStaticFlags = fromEnum StaticPointers `member` compExts
      ms = if hasStaticFlags then forceAsmGen modSum else modSum
  p <- parseModule ms
  tc <- typecheckModule p
  let annots = pm_annotations p
      srcBuffer = fromJust $ ms_hspp_buf $ pm_mod_summary p
  prepareAST srcBuffer . placeComments (getNormalComments $ snd annots) 
    <$> (do parseTrf <- runTrf (fst annots) (getPragmaComments $ snd annots) $ trfModule ms (pm_parsed_source p)
            runTrf (fst annots) (getPragmaComments $ snd annots)
              $ trfModuleRename ms parseTrf
                  (fromJust $ tm_renamed_source tc) 
                  (pm_parsed_source p))

performRefactor :: String -> FilePath -> [String] -> String -> IO (Either String String)
performRefactor command workingDir flags target = 
  runGhc (Just libdir) $ do
    initGhcFlags
    useFlags flags
    useDirs [workingDir]
    ((\case Right r -> Right (newContent r); Left l -> Left l) <$> (refact =<< parseTyped =<< loadModule workingDir target))
  where refact m = performCommand (readCommand command) (target,m) []
        newContent (ContentChanged (_, newContent) : ress) = prettyPrint newContent
        newContent (_ : ress) = newContent ress

-- tests for ast-gen

genTests :: [Test]
genTests = testBase ++ map makeGenTest testExprs ++ map makeGenTest testPatterns ++ map makeGenTest testType 
             ++ map makeGenTest testBinds ++ map makeGenTest testDecls ++ map makeGenTest testModules

makeGenTest :: SourceInfoTraversal elem => (String, Ann elem dom SrcTemplateStage) -> Test
makeGenTest (expected, ast) = TestLabel expected $ TestCase $ assertEqual "The generated AST is not what is expected" expected (prettyPrint ast)

testBase
  = [ makeGenTest ("A.b", mkNormalName $ mkQualifiedName ["A"] "b")
    , makeGenTest ("A.+", mkQualOp ["A"] "+")
    , makeGenTest ("`mod`", mkBacktickOp [] "mod")
    , makeGenTest ("(+)", mkParenName $ mkSimpleName "+")
    ]

testExprs 
  = [ ("a + 3", mkInfixApp (mkVar (mkName "a")) (mkUnqualOp "+") (mkLit $ mkIntLit 3)) 
    , ("(\"xx\"++)", mkLeftSection (mkLit (mkStringLit "xx")) (mkUnqualOp "++"))
    , ("(1, [2, 3])", mkTuple [ mkLit (mkIntLit 1), mkList [ mkLit (mkIntLit 2), mkLit (mkIntLit 3) ] ])
    , ("P { x = 1 }", mkRecCon (mkName "P") [ mkFieldUpdate (mkName "x") (mkLit $ mkIntLit 1) ])
    , ("if f a then x else y", mkIf (mkApp (mkVar $ mkName "f") (mkVar $ mkName "a")) (mkVar $ mkName "x") (mkVar $ mkName "y"))
    , ("let nat = [0..] in !z", mkLet [mkLocalValBind $ mkSimpleBind' (mkName "nat") (mkEnum (mkLit (mkIntLit 0)) Nothing Nothing)] 
                                      (mkPrefixApp (mkUnqualOp "!") (mkVar $ mkName "z")) )
    , (    "case x of Just y -> y\n"
        ++ "          Nothing -> 0", mkCase (mkVar (mkName "x")) [ mkAlt (mkAppPat (mkName "Just") [mkVarPat (mkName "y")]) (mkCaseRhs $ mkVar (mkName "y")) Nothing
                                                                 , mkAlt (mkVarPat $ mkName "Nothing") (mkCaseRhs $ mkLit $ mkIntLit 0) Nothing
                                                                 ])
    , (    "if | x > y -> x\n"
        ++ "   | otherwise -> y", mkMultiIf [ mkGuardedCaseRhs [mkGuardCheck $ mkInfixApp (mkVar (mkName "x")) (mkUnqualOp ">") (mkVar (mkName "y"))] (mkVar (mkName "x"))
                                            , mkGuardedCaseRhs [mkGuardCheck $ mkVar (mkName "otherwise")] (mkVar (mkName "y"))
                                            ])
    , (    "do x <- a\n"
        ++ "   return x", mkDoBlock [ G.mkBindStmt (mkVarPat (mkName "x")) (mkVar (mkName "a"))
                                    , mkExprStmt (mkApp (mkVar $ mkName "return") (mkVar $ mkName "x"))
                                    ])
    ]

testPatterns
  = [ ("~[0, a]", mkIrrefutablePat $ mkListPat [ mkLitPat (mkIntLit 0), mkVarPat (mkName "a") ])
    , ("p@Point{ x = 1 }", mkAsPat (mkName "p") $ mkRecPat (mkName "Point") [ mkPatternField (mkName "x") (mkLitPat (mkIntLit 1)) ])
    , ("!(_, f -> 3)", mkBangPat $ mkTuplePat [mkWildPat, mkViewPat (mkVar $ mkName "f") (mkLitPat (mkIntLit 3))])
    ]

testType
  = [ ("forall x . Eq x => x -> ()", mkForallType [mkTypeVar (mkName "x")] 
                                       $ mkCtxType (mkContextOne (mkClassAssert (mkName "Eq") [mkVarType (mkName "x")])) 
                                       $ mkFunctionType (mkVarType (mkName "x")) (mkVarType (mkName "()")))
    , ("(A :+: B) (x, x)", mkTypeApp (mkParenType $ mkInfixTypeApp (mkVarType (mkName "A")) (mkUnqualOp ":+:") (mkVarType (mkName "B")))
                                  (mkTupleType [ mkVarType (mkName "x"), mkVarType (mkName "x") ]))
    ]

testBinds
  = [(    "x = (a, b) where a = 3\n"
       ++ "                 b = 4", mkSimpleBind (mkVarPat (mkName "x")) (mkUnguardedRhs (mkTuple [(mkVar (mkName "a")), (mkVar (mkName "b"))]))
                                                 (Just $ mkLocalBinds' [ mkLocalValBind $ mkSimpleBind' (mkName "a") (mkLit $ mkIntLit 3)
                                                                       , mkLocalValBind $ mkSimpleBind' (mkName "b") (mkLit $ mkIntLit 4)
                                                                       ]) )
    ,(    "f i 0 = i\n"
       ++ "f i x = x", mkFunctionBind' (mkName "f") [ ([mkVarPat $ mkName "i", mkLitPat $ mkIntLit 0], mkVar $ mkName "i")
                                                    , ([mkVarPat $ mkName "i", mkVarPat $ mkName "x"], mkVar $ mkName "x")
                                                    ])
    ]

testDecls
  = [ ("id :: a -> a", mkTypeSigDecl $ mkTypeSignature (mkName "id") (mkFunctionType (mkVarType (mkName "a")) (mkVarType (mkName "a"))))
    , ("id x = x", mkValueBinding $ mkFunctionBind' (mkName "id") [([mkVarPat $ mkName "x"], mkVar $ mkName "x")])
    , ("data A a = A a deriving Show", mkDataDecl mkDataKeyword Nothing (mkDeclHeadApp (mkNameDeclHead (mkName "A")) (mkTypeVar (mkName "a"))) 
                                         [mkConDecl (mkName "A") [mkVarType (mkName "a")]] (Just $ mkDeriving [mkInstanceHead (mkName "Show")]))
    , ("data A = A { x :: Int }", mkDataDecl mkDataKeyword Nothing (mkNameDeclHead (mkName "A")) 
                                    [mkRecordConDecl (mkName "A") [mkFieldDecl [mkName "x"] (mkVarType (mkName "Int"))]] Nothing)
    , (    "class A t => C t where f :: t\n"
        ++ "                       type T t :: *"
      , mkClassDecl (Just $ mkContextOne (mkClassAssert (mkName "A") [mkVarType (mkName "t")])) 
                    (mkDeclHeadApp (mkNameDeclHead (mkName "C")) (mkTypeVar (mkName "t"))) []
                    (Just $ mkClassBody [ mkClassElemSig $ mkTypeSignature (mkName "f") (mkVarType (mkName "t"))
                                        , mkClassElemTypeFam (mkDeclHeadApp (mkNameDeclHead (mkName "T")) (mkTypeVar (mkName "t"))) 
                                                             (Just $ mkTypeFamilyKindSpec $ mkKindConstraint $ mkKindStar)
                                        ])
      )
    , ("instance C Int where f = 0", mkInstanceDecl Nothing (mkInstanceRule Nothing $ mkAppInstanceHead (mkInstanceHead $ mkName "C") (mkVarType (mkName "Int"))) 
                                                    (Just $ mkInstanceBody [mkInstanceBind $ mkSimpleBind' (mkName "f") (mkLit $ mkIntLit 0)]))
    , ("infixl 6 +", mkFixityDecl $ mkInfixL 6 (mkUnqualOp "+"))
    ]

testModules
  = [ ("", G.mkModule [] Nothing [] [])
    , ("module Test(x, A(a), B(..)) where", G.mkModule [] (Just $ mkModuleHead (G.mkModuleName "Test") (Just $ mkExportSpecs [
                                                mkExportSpec $ mkIESpec (mkName "x") Nothing
                                              , mkExportSpec $ mkIESpec (mkName "A") (Just $ mkSubList [mkName "a"])
                                              , mkExportSpec $ mkIESpec (mkName "B") (Just mkSubAll)
                                            ]) Nothing) [] [])
    , ("\nimport qualified A\n"
      ++ "import B as BB(x)\n"
      ++ "import B hiding (x)", G.mkModule [] Nothing [ mkImportDecl False True False Nothing (G.mkModuleName "A") Nothing Nothing
                                                      , mkImportDecl False False False Nothing (G.mkModuleName "B") (Just $ G.mkModuleName "BB") (Just $ mkImportSpecList [mkIESpec (mkName "x") Nothing])
                                                      , mkImportDecl False False False Nothing (G.mkModuleName "B") Nothing (Just $ mkImportHidingList [mkIESpec (mkName "x") Nothing])
                                                      ] [])
    ]
