module Main where

import Test.Tasty (TestTree, testGroup, defaultMain)
import Test.Tasty.HUnit

import SrcLoc
import GHC hiding (loadModule, ModuleName)
import GHC.Paths (libdir)
import Language.Haskell.TH.LanguageExtensions

import Data.List (sort)
import qualified Data.Map.Strict as SMap
import System.FilePath

import Language.Haskell.Tools.Refactor hiding (ModuleName)
import Language.Haskell.Tools.Refactor.Builtin.OrganizeExtensions
import ExtensionOrganizerTest.AnnotationParser

import Control.Reference (_1, (.-))


{- NOTE:
  Exhaustive checks for Pattern type AST nodes are not given for each check.
  We only give exhaustive test cases for nested patterns in bangPatternsTest.
-}

main :: IO ()
main = defaultMain extensionOrganizerTestGroup

extensionOrganizerTestGroup = testGroup "ExtensionOrganizerTest"
  [ mkTests recordWildCardsTest
  , mkTests flexibleInstancesTest
  , mkTests derivingsTest
  , mkTests patternSynonymsTest
  , mkTests bangPatternsTest
  , mkTests templateHaskellTest
  , mkTests viewPatternsTest
  , mkTests lambdaCaseTest
  , mkTests tupleSectionsTest
  , mkNestedTests magicHashTest
  ]

testRoot = "test/ExtensionOrganizerTest"

mkModulePath :: FilePath -> ModuleName -> FilePath
mkModulePath testDir testName = testRoot </> testDir </> testName

type NestedTestSuite = (FilePath, [TestSuite])
type TestSuite       = (FilePath, [TestName])
type TestName        = String
type ModuleName      = String
type Line            = Int
type SimpleMap       = SMap.Map (LogicalRelation Extension) [Line]

spanToLine :: SrcSpan -> Line
spanToLine (RealSrcSpan s) = srcSpanEndLine s

simplifyExtMap :: ExtMap -> SimpleMap
simplifyExtMap = SMap.map (map spanToLine)

loadModuleAST :: FilePath -> ModuleName -> Ghc TypedModule
loadModuleAST dir moduleName = do
  useFlags ["-w"]
  modSummary <- loadModule (testRoot </> dir) moduleName
  parseTyped modSummary

getExtensionsFrom :: FilePath -> ModuleName -> IO SimpleMap
getExtensionsFrom dir moduleName = runGhc (Just libdir) $ do
  modAST <- loadModuleAST dir moduleName
  exts <- collectExtensions modAST
  return $! simplifyExtMap exts

getExtAnnotsFrom :: FilePath -> ModuleName -> IO SimpleMap
getExtAnnotsFrom dir moduleName = do
  s <- readFile $ addExtension (mkModulePath dir moduleName) ".hs"
  return $! getExtensionAnnotations s


mkTest :: FilePath -> ModuleName -> TestTree
mkTest dir moduleName = testCase moduleName $ mkAssertion dir moduleName

mkAssertion :: FilePath -> ModuleName -> IO ()
mkAssertion dir moduleName = do
  expected <- getExtAnnotsFrom  dir moduleName
  result   <- getExtensionsFrom dir moduleName
  assertEqual "Failure" (mapSort expected) (mapSort result)
  where mapSort = SMap.map sort

mkTests :: TestSuite -> TestTree
mkTests (testDir, tests) = testGroup testDir (map (mkTest testDir) tests)

mkNestedTests :: NestedTestSuite -> TestTree
mkNestedTests (parentDir, suites) = testGroup parentDir nestedTests
  where nestedSuites = map (_1 .- (parentDir </>)) suites
        nestedTests  = map mkTests nestedSuites




recordWildCardsTest :: TestSuite
recordWildCardsTest = (recordWildCardsRoot, recordWildCardsModules)
recordWildCardsRoot = "RecordWildCardsTest"
recordWildCardsModules = [ "InExpression"
                         , "InPattern"
                         ]

flexibleInstancesTest :: TestSuite
flexibleInstancesTest = (flexibleInstancesRoot, flexibleInstancesModules)
flexibleInstancesRoot = "FlexibleInstancesTest"
flexibleInstancesModules = [ "Combined"
                           , "NestedTypes"
                           , "NestedUnitTyCon"
                           , "NestedWiredInType"
                           , "NoOccurenceON"
                           , "NoOccurenceOFF"
                           , "SameTyVars"
                           , "TopLevelTyVar"
                           , "TopLevelWiredInType"
                           ]

derivingsTest :: TestSuite
derivingsTest = (derivingsRoot, derivingsModules)
derivingsRoot = "DerivingsTest"
derivingsModules = [ "DataDeriving"
                   , "DataDerivingStrategies"
                   , "NewtypeDeriving"
                   , "NewtypeDerivingStrategies"
                   , "StandaloneData"
                   , "StandaloneDataStrategies"
                   , "StandaloneDataSynonyms"
                   , "StandaloneDataSynonymsStrategies"
                   , "StandaloneNewtype"
                   , "StandaloneNewtypeStrategies"
                   , "StandaloneNewtypeAny"
                   , "StandaloneNewtypeSynonyms"
                   , "StandaloneNewtypeSynonymsStrategies"
                   , "StandaloneNewtypeSynonymsAny"
                   ]

patternSynonymsTest :: TestSuite
patternSynonymsTest = (patSynRoot, patSynModules)
patSynRoot = "PatternSynonymsTest"
patSynModules = [ "UniDirectional"
                , "BiDirectional"
                ]

bangPatternsTest :: TestSuite
bangPatternsTest = (bangPatternsRoot, bangPatternsModules)
bangPatternsRoot = "BangPatternsTest"
bangPatternsModules = [ "Combined"
                      , "InAlt"
                      , "InExpr"
                      , "InMatchLhs"
                      , "InPatSynRhs"
                      , "InPattern"
                      , "InRhsGuard"
                      , "InStmt"
                      , "InValueBind"
                      ]

templateHaskellTest :: TestSuite
templateHaskellTest = (thRoot, thModules)
thRoot = "TemplateHaskellTest"
thModules = [ "Quote"
            , "Splice"
            ]

viewPatternsTest :: TestSuite
viewPatternsTest = (vpRoot, vpModules)
vpRoot = "ViewPatternsTest"
vpModules = [ "InAlt"
            , "InExpr"
            , "InMatchLhs"
            , "InMatchLhsNested"
            ]

lambdaCaseTest :: TestSuite
lambdaCaseTest = (lcRoot, lcModules)
lcRoot = "LambdaCaseTest"
lcModules = [ "InCaseRhs"
            , "InCompStmt"
            , "InExpr"
            , "InFieldUpdate"
            , "InPattern"
            , "InRhs"
            , "InRhsGuard"
            , "InStmt"
            , "InTupSecElem"
            ]

tupleSectionsTest :: TestSuite
tupleSectionsTest = (tsRoot, tsModules)
tsRoot = "TupleSectionsTest"
tsModules = [ "InCaseRhs"
            , "InCompStmt"
            , "InExpr"
            , "InFieldUpdate"
            , "InPattern"
            , "InRhs"
            , "InRhsGuard"
            , "InStmt"
            , "InTupSecElem"
            , "NoTupleSections"
            ]


magicHashTest :: NestedTestSuite
magicHashTest = (mhRoot, [magicHashLiteralTest, magicHashNameTest])
mhRoot = "MagicHashTest"


magicHashNameTest :: TestSuite
magicHashNameTest = (mhNameRoot, mhNameModules)
mhNameRoot = "Name"
mhNameModules = [ "InAssertion"
                , "InClassElement"
                , "InDecl"
                , "InDeclHead"
                , "InExpr"
                , "InFieldDecl"
                , "InFieldUpdate"
                , "InFunDeps"
                , "InInstanceHead"
                --, "InKind"        NO PARSE
                , "InMatchLhs"
                , "InPatSynLhs"
                , "InPattern"
                , "InPatternField"
                , "InType"
                --, "InTypeFamily"  NO PARSE
                , "InTypeSig"
                ]

magicHashLiteralTest :: TestSuite
magicHashLiteralTest = (mhLiteralRoot, mhLiteralModules)
mhLiteralRoot = "Literal"
mhLiteralModules = [ "InExpr" ]
