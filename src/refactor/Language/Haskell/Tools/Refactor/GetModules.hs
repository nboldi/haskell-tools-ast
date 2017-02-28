{-# LANGUAGE TupleSections
           , NamedFieldPuns
           , LambdaCase
           , TemplateHaskell
           , FlexibleContexts
           , TypeApplications
           #-}
-- | Representation and operations for module collections (libraries, executables, ...) in the framework.
module Language.Haskell.Tools.Refactor.GetModules where

import Control.Reference
import Control.Monad
import Data.Function (on)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Distribution.Compiler
import Distribution.ModuleName (components)
import Distribution.ModuleName
import Distribution.Package (Dependency(..), PackageName(..), pkgName)
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.System
import Distribution.Verbosity (silent)
import Language.Haskell.Extension as Cabal
import System.Directory
import System.FilePath.Posix

import DynFlags
import qualified DynFlags as GHC
import GHC hiding (ModuleName)
import qualified Language.Haskell.TH.LanguageExtensions as GHC
import Name as GHC (Name)
import RdrName as GHC (RdrName)

import Language.Haskell.Tools.AST (Dom, IdDom)
import Language.Haskell.Tools.Refactor.RefactorBase

import Debug.Trace

-- | The modules of a library, executable, test or benchmark. A package contains one or more module collection.
data ModuleCollection
  = ModuleCollection { _mcId :: ModuleCollectionId
                     , _mcRoot :: FilePath
                     , _mcSourceDirs :: [FilePath]
                     , _mcModules :: Map.Map SourceFileKey ModuleRecord
                     , _mcFlagSetup :: DynFlags -> IO DynFlags -- ^ Sets up the ghc environment for compiling the modules of this collection
                     , _mcLoadFlagSetup :: DynFlags -> IO DynFlags -- ^ Sets up the ghc environment for dependency analysis
                     , _mcDependencies :: [ModuleCollectionId]
                     }

instance Eq ModuleCollection where
  (==) = (==) `on` _mcId

instance Show ModuleCollection where
  show (ModuleCollection id root srcDirs mods _ _ deps)
    = "ModuleCollection (" ++ show id ++ ") " ++ root ++ " " ++ show srcDirs ++ " (" ++ show mods ++ ") " ++ show deps

-- | The state of a module.
data ModuleRecord
       = ModuleNotLoaded { _recModuleWillNeedCode :: Bool }
       | ModuleParsed { _parsedRecModule :: UnnamedModule (Dom RdrName)
                      , _modRecMS :: ModSummary
                      }
       | ModuleRenamed { _renamedRecModule :: UnnamedModule (Dom Name)
                       , _modRecMS :: ModSummary
                       }
       | ModuleTypeChecked { _typedRecModule :: UnnamedModule IdDom
                           , _modRecMS :: ModSummary
                           }
       | ModuleCodeGenerated { _typedRecModule :: UnnamedModule IdDom
                             , _modRecMS :: ModSummary
                             }

-- | This data structure identifies a module collection.
data ModuleCollectionId = DirectoryMC FilePath
                        | LibraryMC String
                        | ExecutableMC String String
                        | TestSuiteMC String String
                        | BenchmarkMC String String
  deriving (Eq, Ord, Show)

moduleCollectionIdString :: ModuleCollectionId -> String
moduleCollectionIdString (DirectoryMC fp) = fp
moduleCollectionIdString (LibraryMC id) = id
moduleCollectionIdString (ExecutableMC _ id) = id
moduleCollectionIdString (TestSuiteMC _ id) = id
moduleCollectionIdString (BenchmarkMC _ id) = id

moduleCollectionPkgId :: ModuleCollectionId -> Maybe String
moduleCollectionPkgId (DirectoryMC _) = Nothing
moduleCollectionPkgId (LibraryMC id) = Just id
moduleCollectionPkgId (ExecutableMC id _) = Just id
moduleCollectionPkgId (TestSuiteMC id _) = Just id
moduleCollectionPkgId (BenchmarkMC id _) = Just id

makeReferences ''ModuleCollection
makeReferences ''ModuleRecord

instance Show ModuleRecord where
  show (ModuleNotLoaded code) = "ModuleNotLoaded " ++ show code
  show mr = GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod $ fromJust $ mr ^? modRecMS

-- | Find the module collection where the given module is.
lookupModuleColl :: String -> [ModuleCollection] -> Maybe (ModuleCollection)
lookupModuleColl moduleName = find (any ((moduleName ==) . (^. sfkModuleName)) . Map.keys . (^. mcModules))

lookupModInSCs :: SourceFileKey -> [ModuleCollection] -> Maybe (SourceFileKey, ModuleRecord)
lookupModInSCs moduleName = find ((moduleName ==) . fst) . concatMap (Map.assocs . (^. mcModules))

removeModule :: String -> [ModuleCollection] -> [ModuleCollection]
removeModule moduleName = map (mcModules .- Map.filterWithKey (\k _ -> moduleName /= (k ^. sfkModuleName)))

hasGeneratedCode :: SourceFileKey -> [ModuleCollection] -> Bool
hasGeneratedCode key = maybe False (\case (_, ModuleCodeGenerated {}) -> True; _ -> False)
                         . find ((key ==) . fst) . concatMap (Map.assocs . (^. mcModules))

needsGeneratedCode :: SourceFileKey -> [ModuleCollection] -> Bool
needsGeneratedCode key = maybe False (\case (_, ModuleCodeGenerated {}) -> True; (_, ModuleNotLoaded True) -> True; _ -> False)
                           . find ((key ==) . fst) . concatMap (Map.assocs . (^. mcModules))

codeGeneratedFor :: SourceFileKey -> [ModuleCollection] -> [ModuleCollection]
codeGeneratedFor key = map (mcModules .- Map.adjust (\case (ModuleTypeChecked mod ms) -> ModuleCodeGenerated mod ms
                                                           ModuleNotLoaded _ -> ModuleNotLoaded True
                                                           r -> r) key)

isAlreadyLoaded :: SourceFileKey -> [ModuleCollection] -> Bool
isAlreadyLoaded key = maybe False (\case (_, ModuleNotLoaded {}) -> False; _ -> True)
                         . find ((key ==) . fst) . concatMap (Map.assocs . (^. mcModules))

-- | Gets all ModuleCollections from a list of source directories. It also orders the source directories that are package roots so that
-- they can be loaded in the order they are defined (no backward imports). This matters in those cases because for them there can be
-- special compilation flags.
getAllModules :: [FilePath] -> IO [ModuleCollection]
getAllModules pathes = orderMCs . concat <$> mapM getModules pathes

-- | Sorts model collection in an order to remove all backward references.
-- Works because module collections defined by directories cannot be recursive.
orderMCs :: [ModuleCollection] -> [ModuleCollection]
orderMCs = sortBy compareMCs
  where compareMCs :: ModuleCollection -> ModuleCollection -> Ordering
        compareMCs mc _ | DirectoryMC _ <- (mc ^. mcId) = GT
        compareMCs _ mc | DirectoryMC _ <- (mc ^. mcId) = LT
        compareMCs mc1 mc2 | (mc2 ^. mcId) `elem` (mc1 ^. mcDependencies) = GT
        compareMCs mc1 mc2 | (mc1 ^. mcId) `elem` (mc2 ^. mcDependencies) = LT
        compareMCs _ _ = EQ


-- | Get modules of the project with the indicated root directory.
-- If there is a cabal file, it uses that, otherwise it just scans the directory recursively for haskell sourcefiles.
-- Only returns the non-boot haskell modules, the boot modules will be found during loading.
getModules :: FilePath -> IO [ModuleCollection]
getModules root
  = do files <- listDirectory root
       case find (\p -> takeExtension p == ".cabal") files of
          Just cabalFile -> modulesFromCabalFile root cabalFile
          Nothing        -> do mods <- modulesFromDirectory root root
                               return [ModuleCollection (DirectoryMC root) root [root] (Map.fromList $ map ((, ModuleNotLoaded False) . SourceFileKey NormalHs) mods) return return []]

-- | Load the module giving a directory. All modules loaded from the folder and subfolders.
modulesFromDirectory :: FilePath -> FilePath -> IO [String]
-- now recognizing only .hs files
modulesFromDirectory root searchRoot = concat <$> (mapM goOn =<< listDirectory searchRoot)
  where goOn fp = let path = searchRoot </> fp
                   in do isDir <- doesDirectoryExist path
                         if isDir
                           then modulesFromDirectory root path
                           else if takeExtension path == ".hs"
                                  then return [concat $ intersperse "." $ splitDirectories $ dropExtension $ makeRelative root path]
                                  else return []

srcDirFromRoot :: FilePath -> String -> FilePath
srcDirFromRoot fileName "" = fileName
srcDirFromRoot fileName moduleName
  = srcDirFromRoot (takeDirectory fileName) (dropWhile (/= '.') $ dropWhile (== '.') moduleName)

-- | Load the module using a cabal file. The modules described in the cabal file will be loaded.
-- The flags and extensions set in the cabal file will be used by default.
modulesFromCabalFile :: FilePath -> FilePath -> IO [ModuleCollection]
-- now adding all conditional entries, regardless of flags
modulesFromCabalFile root cabal = getModules . setupFlags <$> readPackageDescription silent (root </> cabal)
  where getModules pkg = maybe [] (maybe [] (:[]) . toModuleCollection pkg) (library pkg)
                           ++ catMaybes (map (toModuleCollection pkg) (executables pkg))
                           ++ catMaybes (map (toModuleCollection pkg) (testSuites pkg))
                           ++ catMaybes (map (toModuleCollection pkg) (benchmarks pkg))

        toModuleCollection :: ToModuleCollection tmc => PackageDescription -> tmc -> Maybe ModuleCollection
        toModuleCollection pkg tmc
          = let bi = getBuildInfo tmc
             in if buildable bi
                  then Just $ ModuleCollection (mkModuleCollKey (pkgName $ package pkg) tmc)
                                root
                                (map (normalise . (root </>)) $ hsSourceDirs bi)
                                (Map.fromList $ map ((, ModuleNotLoaded False) . SourceFileKey NormalHs . moduleName) (getModuleNames tmc))
                                (flagsFromBuildInfo bi)
                                (loadFlagsFromBuildInfo bi)
                                (map (\(Dependency pkgName _) -> LibraryMC (unPackageName pkgName)) (targetBuildDepends bi))
                  else Nothing

        moduleName = concat . intersperse "." . components
        setupFlags = either (\deps -> error $ "Missing dependencies: " ++ show deps) fst
                       . finalizePackageDescription [] (const True) buildPlatform
                                                    (unknownCompilerInfo buildCompilerId NoAbiTag) []

class ToModuleCollection t where
  mkModuleCollKey :: PackageName -> t -> ModuleCollectionId
  getBuildInfo :: t -> BuildInfo
  getModuleNames :: t -> [ModuleName]

instance ToModuleCollection Library where
  mkModuleCollKey pn _ = LibraryMC (unPackageName pn)
  getBuildInfo = libBuildInfo
  getModuleNames = libModules

instance ToModuleCollection Executable where
  mkModuleCollKey pn exe = ExecutableMC (unPackageName pn) (exeName exe)
  getBuildInfo = buildInfo
  getModuleNames e = {- fromString (toModuleName $ modulePath e) : -} exeModules e
    where toModuleName = map (\case c | c `elem` pathSeparators -> '.'; c -> c) . dropExtension

instance ToModuleCollection TestSuite where
  mkModuleCollKey pn test = TestSuiteMC (unPackageName pn) (testName test)
  getBuildInfo = testBuildInfo
  getModuleNames = testModules

instance ToModuleCollection Benchmark where
  mkModuleCollKey pn test = BenchmarkMC (unPackageName pn) (benchmarkName test)
  getBuildInfo = benchmarkBuildInfo
  getModuleNames = benchmarkModules

isDirectoryMC :: ModuleCollection -> Bool
isDirectoryMC mc = case mc ^. mcId of DirectoryMC{} -> True; _ -> False

compileInContext :: ModuleCollection -> [ModuleCollection] -> DynFlags -> IO DynFlags
compileInContext mc mcs dfs
  = (\dfs' -> applyDependencies mcs (mc ^. mcDependencies) (selectEnabled dfs'))
       <$> (mc ^. mcFlagSetup $ dfs)
  where selectEnabled = if isDirectoryMC mc then id else onlyUseEnabled

applyDependencies :: [ModuleCollection] -> [ModuleCollectionId] -> DynFlags -> DynFlags
applyDependencies mcs ids dfs
  = dfs { GHC.packageFlags = GHC.packageFlags dfs ++ (catMaybes $ map (dependencyToPkgFlag mcs) ids) }

onlyUseEnabled :: DynFlags -> DynFlags
onlyUseEnabled = GHC.setGeneralFlag' GHC.Opt_HideAllPackages

dependencyToPkgFlag :: [ModuleCollection] -> ModuleCollectionId -> Maybe (GHC.PackageFlag)
dependencyToPkgFlag mcs lib@(LibraryMC pkgName)
  = if isNothing $ find (\mc -> (mc ^. mcId) == lib) mcs
      then Just $ GHC.ExposePackage pkgName (GHC.PackageArg pkgName) (GHC.ModRenaming True [])
      else Nothing
dependencyToPkgFlag _ _ = Nothing

setupLoadFlags :: [ModuleCollection] -> DynFlags -> IO DynFlags
setupLoadFlags mcs dfs = applyDependencies mcs allDeps . selectEnabled <$> useSavedFlags dfs
  where allDeps = mcs ^? traversal & mcDependencies & traversal
        selectEnabled = if any (\(mc,rest) -> isDirectoryMC mc && isIndependentMc mc rest) (breaks mcs) then id else onlyUseEnabled
        useSavedFlags = foldl @[] (>=>) return (mcs ^? traversal & mcLoadFlagSetup)
        isIndependentMc mc rest = not $ any (`isPrefixOf` (mc ^. mcRoot)) (map (^. mcRoot) rest)

breaks :: [a] -> [(a,[a])]
breaks [] = []
breaks (e:rest) = (e,rest) : map (\(x,ls) -> (x,e:ls)) (breaks rest)

loadFlagsFromBuildInfo :: BuildInfo -> DynFlags -> IO DynFlags
loadFlagsFromBuildInfo bi@BuildInfo{ cppOptions } df
  = do (df',unused,warnings) <- parseDynamicFlags df (map (L noSrcSpan) $ cppOptions)
       mapM_ putStrLn (map unLoc warnings ++ map (("Flag is not used: " ++) . unLoc) unused)
       return (setupLoadExtensions df')
  where setupLoadExtensions = foldl (.) id (map translateExtension loadExtensions)
        loadExtensions = [PatternSynonyms | patternSynonymsNeeded] ++ [ExplicitNamespaces | explicitNamespacesNeeded]
        explicitNamespacesNeeded = not $ null $ map EnableExtension [ExplicitNamespaces, TypeFamilies, TypeOperators] `intersect` usedExtensions bi
        patternSynonymsNeeded = EnableExtension PatternSynonyms `elem` usedExtensions bi

flagsFromBuildInfo :: BuildInfo -> DynFlags -> IO DynFlags
-- the import pathes are already set globally
flagsFromBuildInfo bi@BuildInfo{ options } df
  = do (df',unused,warnings) <- parseDynamicFlags df (map (L noSrcSpan) $ concatMap snd options)
       mapM_ putStrLn (map unLoc warnings ++ map (("Flag is not used: " ++) . unLoc) unused)
       return $ (flip lang_set (toGhcLang =<< defaultLanguage bi))
         $ foldl (.) id (map (\case EnableExtension ext -> translateExtension ext
                                    _                   -> id
                        ) (usedExtensions bi ++ map EnableExtension (languageDefault (defaultLanguage bi))))
         $ df'
  where toGhcLang Cabal.Haskell98 = Just GHC.Haskell98
        toGhcLang Cabal.Haskell2010 = Just GHC.Haskell2010
        toGhcLang _ = Nothing

        languageDefault (Just Cabal.Haskell2010)
          = [ DatatypeContexts, DoAndIfThenElse, EmptyDataDecls, ForeignFunctionInterface
            , ImplicitPrelude, MonomorphismRestriction, PatternGuards, RelaxedPolyRec
            , TraditionalRecordSyntax ]
        -- Haskell 98 is the default
        languageDefault _
          = [ DatatypeContexts, ImplicitPrelude, MonomorphismRestriction
            , NondecreasingIndentation, NPlusKPatterns, TraditionalRecordSyntax ]

-- * Not imported from DynFlags.hs, so I copied it here
setExtensionFlag', unSetExtensionFlag' :: GHC.Extension -> DynFlags -> DynFlags
setExtensionFlag' f dflags = foldr ($) (xopt_set dflags f) deps
  where
    deps = [ if turn_on then setExtensionFlag'   d
                        else unSetExtensionFlag' d
           | (f', turn_on, d) <- impliedXFlags, f' == f ]
unSetExtensionFlag' f dflags = xopt_unset dflags f

turnOn = True
turnOff = False

impliedXFlags :: [(GHC.Extension, Bool, GHC.Extension)]
impliedXFlags
  = [ (GHC.RankNTypes,                turnOn, GHC.ExplicitForAll)
    , (GHC.ScopedTypeVariables,       turnOn, GHC.ExplicitForAll)
    , (GHC.LiberalTypeSynonyms,       turnOn, GHC.ExplicitForAll)
    , (GHC.ExistentialQuantification, turnOn, GHC.ExplicitForAll)
    , (GHC.FlexibleInstances,         turnOn, GHC.TypeSynonymInstances)
    , (GHC.FunctionalDependencies,    turnOn, GHC.MultiParamTypeClasses)
    , (GHC.MultiParamTypeClasses,     turnOn, GHC.ConstrainedClassMethods)
    , (GHC.TypeFamilyDependencies,    turnOn, GHC.TypeFamilies)
    , (GHC.RebindableSyntax, turnOff, GHC.ImplicitPrelude)
    , (GHC.GADTs,            turnOn, GHC.GADTSyntax)
    , (GHC.GADTs,            turnOn, GHC.MonoLocalBinds)
    , (GHC.TypeFamilies,     turnOn, GHC.MonoLocalBinds)
    , (GHC.TypeFamilies,     turnOn, GHC.KindSignatures)
    , (GHC.PolyKinds,        turnOn, GHC.KindSignatures)
    , (GHC.TypeInType,       turnOn, GHC.DataKinds)
    , (GHC.TypeInType,       turnOn, GHC.PolyKinds)
    , (GHC.TypeInType,       turnOn, GHC.KindSignatures)
    , (GHC.AutoDeriveTypeable, turnOn, GHC.DeriveDataTypeable)
    , (GHC.TypeFamilies,     turnOn, GHC.ExplicitNamespaces)
    , (GHC.TypeOperators, turnOn, GHC.ExplicitNamespaces)
    , (GHC.ImpredicativeTypes,  turnOn, GHC.RankNTypes)
    , (GHC.RecordWildCards,     turnOn, GHC.DisambiguateRecordFields)
    , (GHC.ParallelArrays, turnOn, GHC.ParallelListComp)
    , (GHC.JavaScriptFFI, turnOn, GHC.InterruptibleFFI)
    , (GHC.DeriveTraversable, turnOn, GHC.DeriveFunctor)
    , (GHC.DeriveTraversable, turnOn, GHC.DeriveFoldable)
    , (GHC.DuplicateRecordFields, turnOn, GHC.DisambiguateRecordFields)
    , (GHC.TemplateHaskell, turnOn, GHC.TemplateHaskellQuotes)
    , (GHC.Strict, turnOn, GHC.StrictData)
  ]

        -- * Mapping of Cabal haskell extensions to their GHC counterpart

-- | Map the cabal extensions to the ones that GHC recognizes
translateExtension AllowAmbiguousTypes = setExtensionFlag' GHC.AllowAmbiguousTypes
translateExtension ApplicativeDo = setExtensionFlag' GHC.ApplicativeDo
translateExtension Arrows = setExtensionFlag' GHC.Arrows
translateExtension AutoDeriveTypeable = setExtensionFlag' GHC.AutoDeriveTypeable
translateExtension BangPatterns = setExtensionFlag' GHC.BangPatterns
translateExtension BinaryLiterals = setExtensionFlag' GHC.BinaryLiterals
translateExtension CApiFFI = setExtensionFlag' GHC.CApiFFI
translateExtension ConstrainedClassMethods = setExtensionFlag' GHC.ConstrainedClassMethods
translateExtension ConstraintKinds = setExtensionFlag' GHC.ConstraintKinds
translateExtension CPP = setExtensionFlag' GHC.Cpp
translateExtension DataKinds = setExtensionFlag' GHC.DataKinds
translateExtension DatatypeContexts = setExtensionFlag' GHC.DatatypeContexts
translateExtension DefaultSignatures = setExtensionFlag' GHC.DefaultSignatures
translateExtension DeriveAnyClass = setExtensionFlag' GHC.DeriveAnyClass
translateExtension DeriveDataTypeable = setExtensionFlag' GHC.DeriveDataTypeable
translateExtension DeriveFoldable = setExtensionFlag' GHC.DeriveFoldable
translateExtension DeriveFunctor = setExtensionFlag' GHC.DeriveFunctor
translateExtension DeriveGeneric = setExtensionFlag' GHC.DeriveGeneric
translateExtension DeriveLift = setExtensionFlag' GHC.DeriveLift
translateExtension DeriveTraversable = setExtensionFlag' GHC.DeriveTraversable
translateExtension DisambiguateRecordFields = setExtensionFlag' GHC.DisambiguateRecordFields
translateExtension DoAndIfThenElse = setExtensionFlag' GHC.DoAndIfThenElse
translateExtension DoRec = setExtensionFlag' GHC.RecursiveDo
translateExtension DuplicateRecordFields = setExtensionFlag' GHC.DuplicateRecordFields
translateExtension EmptyCase = setExtensionFlag' GHC.EmptyCase
translateExtension EmptyDataDecls = setExtensionFlag' GHC.EmptyDataDecls
translateExtension ExistentialQuantification = setExtensionFlag' GHC.ExistentialQuantification
translateExtension ExplicitForAll = setExtensionFlag' GHC.ExplicitForAll
translateExtension ExplicitNamespaces = setExtensionFlag' GHC.ExplicitNamespaces
translateExtension ExtendedDefaultRules = setExtensionFlag' GHC.ExtendedDefaultRules
translateExtension FlexibleContexts = setExtensionFlag' GHC.FlexibleContexts
translateExtension FlexibleInstances = setExtensionFlag' GHC.FlexibleInstances
translateExtension ForeignFunctionInterface = setExtensionFlag' GHC.ForeignFunctionInterface
translateExtension FunctionalDependencies = setExtensionFlag' GHC.FunctionalDependencies
translateExtension GADTs = setExtensionFlag' GHC.GADTs
translateExtension GADTSyntax = setExtensionFlag' GHC.GADTSyntax
translateExtension GeneralizedNewtypeDeriving = setExtensionFlag' GHC.GeneralizedNewtypeDeriving
translateExtension GHCForeignImportPrim = setExtensionFlag' GHC.GHCForeignImportPrim
translateExtension ImplicitParams = setExtensionFlag' GHC.ImplicitParams
translateExtension ImplicitPrelude = setExtensionFlag' GHC.ImplicitPrelude
translateExtension ImpredicativeTypes = setExtensionFlag' GHC.ImpredicativeTypes
translateExtension IncoherentInstances = setExtensionFlag' GHC.IncoherentInstances
translateExtension InstanceSigs = setExtensionFlag' GHC.InstanceSigs
translateExtension InterruptibleFFI = setExtensionFlag' GHC.InterruptibleFFI
translateExtension JavaScriptFFI = setExtensionFlag' GHC.JavaScriptFFI
translateExtension KindSignatures = setExtensionFlag' GHC.KindSignatures
translateExtension LambdaCase = setExtensionFlag' GHC.LambdaCase
translateExtension LiberalTypeSynonyms = setExtensionFlag' GHC.LiberalTypeSynonyms
translateExtension MagicHash = setExtensionFlag' GHC.MagicHash
translateExtension MonadComprehensions = setExtensionFlag' GHC.MonadComprehensions
translateExtension MonadFailDesugaring = setExtensionFlag' GHC.MonadFailDesugaring
translateExtension MonoLocalBinds = setExtensionFlag' GHC.MonoLocalBinds
translateExtension MonomorphismRestriction = setExtensionFlag' GHC.MonomorphismRestriction
translateExtension MonoPatBinds = setExtensionFlag' GHC.MonoPatBinds
translateExtension MultiParamTypeClasses = setExtensionFlag' GHC.MultiParamTypeClasses
translateExtension MultiWayIf = setExtensionFlag' GHC.MultiWayIf
translateExtension NamedFieldPuns = setExtensionFlag' GHC.RecordPuns
translateExtension NamedWildCards = setExtensionFlag' GHC.NamedWildCards
translateExtension NegativeLiterals = setExtensionFlag' GHC.NegativeLiterals
translateExtension NondecreasingIndentation = setExtensionFlag' GHC.NondecreasingIndentation
translateExtension NPlusKPatterns = setExtensionFlag' GHC.NPlusKPatterns
translateExtension NullaryTypeClasses = setExtensionFlag' GHC.NullaryTypeClasses
translateExtension NumDecimals = setExtensionFlag' GHC.NumDecimals
translateExtension OverlappingInstances = setExtensionFlag' GHC.OverlappingInstances
translateExtension OverloadedLabels = setExtensionFlag' GHC.OverloadedLabels
translateExtension OverloadedLists = setExtensionFlag' GHC.OverloadedLists
translateExtension OverloadedStrings = setExtensionFlag' GHC.OverloadedStrings
translateExtension PackageImports = setExtensionFlag' GHC.PackageImports
translateExtension ParallelArrays = setExtensionFlag' GHC.ParallelArrays
translateExtension ParallelListComp = setExtensionFlag' GHC.ParallelListComp
translateExtension PartialTypeSignatures = setExtensionFlag' GHC.PartialTypeSignatures
translateExtension PatternGuards = setExtensionFlag' GHC.PatternGuards
translateExtension PatternSignatures = setExtensionFlag' GHC.PatternSynonyms
translateExtension PatternSynonyms = setExtensionFlag' GHC.PatternSynonyms
translateExtension PolyKinds = setExtensionFlag' GHC.PolyKinds
translateExtension PostfixOperators = setExtensionFlag' GHC.PostfixOperators
translateExtension QuasiQuotes = setExtensionFlag' GHC.QuasiQuotes
translateExtension RankNTypes = setExtensionFlag' GHC.RankNTypes
translateExtension RebindableSyntax = setExtensionFlag' GHC.RebindableSyntax
translateExtension RecordPuns = setExtensionFlag' GHC.RecordPuns
translateExtension RecordWildCards = setExtensionFlag' GHC.RecordWildCards
translateExtension RecursiveDo = setExtensionFlag' GHC.RecursiveDo
translateExtension RelaxedPolyRec = setExtensionFlag' GHC.RelaxedPolyRec
translateExtension RestrictedTypeSynonyms = flip xopt_unset GHC.LiberalTypeSynonyms
translateExtension RoleAnnotations = setExtensionFlag' GHC.RoleAnnotations
translateExtension ScopedTypeVariables = setExtensionFlag' GHC.ScopedTypeVariables
translateExtension StandaloneDeriving = setExtensionFlag' GHC.StandaloneDeriving
translateExtension StaticPointers = setExtensionFlag' GHC.StaticPointers
translateExtension Strict = setExtensionFlag' GHC.Strict
translateExtension StrictData = setExtensionFlag' GHC.StrictData
translateExtension TemplateHaskell = setExtensionFlag' GHC.TemplateHaskell
translateExtension TemplateHaskellQuotes = setExtensionFlag' GHC.TemplateHaskellQuotes
translateExtension TraditionalRecordSyntax = setExtensionFlag' GHC.TraditionalRecordSyntax
translateExtension TransformListComp = setExtensionFlag' GHC.TransformListComp
translateExtension TupleSections = setExtensionFlag' GHC.TupleSections
translateExtension TypeApplications = setExtensionFlag' GHC.TypeApplications
translateExtension TypeFamilies = setExtensionFlag' GHC.TypeFamilies
translateExtension TypeInType = setExtensionFlag' GHC.TypeInType
translateExtension TypeOperators = setExtensionFlag' GHC.TypeOperators
translateExtension TypeSynonymInstances = setExtensionFlag' GHC.TypeSynonymInstances
translateExtension UnboxedTuples = setExtensionFlag' GHC.UnboxedTuples
translateExtension UndecidableInstances = setExtensionFlag' GHC.UndecidableInstances
translateExtension UndecidableSuperClasses = setExtensionFlag' GHC.UndecidableSuperClasses
translateExtension UnicodeSyntax = setExtensionFlag' GHC.UnicodeSyntax
translateExtension UnliftedFFITypes = setExtensionFlag' GHC.UnliftedFFITypes
translateExtension ViewPatterns = setExtensionFlag' GHC.ViewPatterns

translateExtension Safe = \df -> df { GHC.safeHaskell = GHC.Sf_Safe }
translateExtension SafeImports = \df -> df { GHC.safeHaskell = GHC.Sf_Safe }
translateExtension Trustworthy = \df -> df { GHC.safeHaskell = GHC.Sf_Trustworthy }
translateExtension Unsafe = \df -> df { GHC.safeHaskell = GHC.Sf_Unsafe }

translateExtension Rank2Types = setExtensionFlag' GHC.RankNTypes
translateExtension PolymorphicComponents = setExtensionFlag' GHC.RankNTypes
translateExtension Generics = id -- it does nothing, deprecated extension
translateExtension NewQualifiedOperators = id -- it does nothing, deprecated extension
translateExtension ExtensibleRecords = id -- not in GHC
translateExtension XmlSyntax = id -- not in GHC
translateExtension HereDocuments = id -- not in GHC
translateExtension RegularPatterns = id -- not in GHC
