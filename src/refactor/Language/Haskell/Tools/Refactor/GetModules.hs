{-# LANGUAGE TupleSections
           , NamedFieldPuns
           , LambdaCase
           , TemplateHaskell
           , FlexibleContexts
           #-}
-- | Representation and operations for module collections (libraries, executables, ...) in the framework.
module Language.Haskell.Tools.Refactor.GetModules where

import Control.Reference
import Data.Function (on)
import Data.List (intersperse, find, sortBy)
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
import Language.Haskell.Extension
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

-- | The modules of a library, executable, test or benchmark. A package contains one or more module collection.
data ModuleCollection
  = ModuleCollection { _mcId :: ModuleCollectionId
                     , _mcRoot :: FilePath
                     , _mcSourceDirs :: [FilePath]
                     , _mcModules :: Map.Map SourceFileKey ModuleRecord
                     , _mcFlagSetup :: DynFlags -> IO DynFlags -- ^ Sets up the ghc environment for compiling the modules of this collection
                     , _mcDependencies :: [ModuleCollectionId]
                     }

instance Eq ModuleCollection where
  (==) = (==) `on` _mcId

instance Show ModuleCollection where
  show (ModuleCollection id root srcDirs mods _ deps)
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
                               return [ModuleCollection (DirectoryMC root) root [root] (Map.fromList $ map ((, ModuleNotLoaded False) . SourceFileKey NormalHs) mods) return []]

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
  = dfs { GHC.packageFlags = catMaybes $ map (dependencyToPkgFlag mcs) ids }

onlyUseEnabled :: DynFlags -> DynFlags
onlyUseEnabled = GHC.setGeneralFlag' GHC.Opt_HideAllPackages

dependencyToPkgFlag :: [ModuleCollection] -> ModuleCollectionId -> Maybe (GHC.PackageFlag)
dependencyToPkgFlag mcs lib@(LibraryMC pkgName)
  = if isNothing $ find (\mc -> (mc ^. mcId) == lib) mcs
      then Just $ GHC.ExposePackage pkgName (GHC.PackageArg pkgName) (GHC.ModRenaming True [])
      else Nothing
dependencyToPkgFlag _ _ = Nothing

enableAllPackages :: [ModuleCollection] -> DynFlags -> DynFlags
enableAllPackages mcs dfs = applyDependencies mcs allDeps (selectEnabled dfs)
  where allDeps = mcs ^? traversal & mcDependencies & traversal
        selectEnabled = if any isDirectoryMC mcs then id else onlyUseEnabled

flagsFromBuildInfo :: BuildInfo -> DynFlags -> IO DynFlags
-- the import pathes are already set globally
flagsFromBuildInfo bi@BuildInfo{ options } df
  = do (df,_,_) <- parseDynamicFlags df (map (L noSrcSpan) $ concatMap snd options)
       return $ foldl (.) id (map (\case EnableExtension ext -> translateExtension ext
                                         _                   -> id
                                  ) (usedExtensions bi))
                          $ df
  where -- | Map the cabal extensions to the ones that GHC recognizes
        translateExtension AllowAmbiguousTypes = flip xopt_set GHC.AllowAmbiguousTypes
        translateExtension ApplicativeDo = flip xopt_set GHC.ApplicativeDo
        translateExtension Arrows = flip xopt_set GHC.Arrows
        translateExtension AutoDeriveTypeable = flip xopt_set GHC.AutoDeriveTypeable
        translateExtension BangPatterns = flip xopt_set GHC.BangPatterns
        translateExtension BinaryLiterals = flip xopt_set GHC.BinaryLiterals
        translateExtension CApiFFI = flip xopt_set GHC.CApiFFI
        translateExtension ConstrainedClassMethods = flip xopt_set GHC.ConstrainedClassMethods
        translateExtension ConstraintKinds = flip xopt_set GHC.ConstraintKinds
        translateExtension CPP = flip xopt_set GHC.Cpp
        translateExtension DataKinds = flip xopt_set GHC.DataKinds
        translateExtension DatatypeContexts = flip xopt_set GHC.DatatypeContexts
        translateExtension DefaultSignatures = flip xopt_set GHC.DefaultSignatures
        translateExtension DeriveAnyClass = flip xopt_set GHC.DeriveAnyClass
        translateExtension DeriveDataTypeable = flip xopt_set GHC.DeriveDataTypeable
        translateExtension DeriveFoldable = flip xopt_set GHC.DeriveFoldable
        translateExtension DeriveFunctor = flip xopt_set GHC.DeriveFunctor
        translateExtension DeriveGeneric = flip xopt_set GHC.DeriveGeneric
        translateExtension DeriveLift = flip xopt_set GHC.DeriveLift
        translateExtension DeriveTraversable = flip xopt_set GHC.DeriveTraversable
        translateExtension DisambiguateRecordFields = flip xopt_set GHC.DisambiguateRecordFields
        translateExtension DoAndIfThenElse = flip xopt_set GHC.DoAndIfThenElse
        translateExtension DoRec = flip xopt_set GHC.RecursiveDo
        translateExtension DuplicateRecordFields = flip xopt_set GHC.DuplicateRecordFields
        translateExtension EmptyCase = flip xopt_set GHC.EmptyCase
        translateExtension EmptyDataDecls = flip xopt_set GHC.EmptyDataDecls
        translateExtension ExistentialQuantification = flip xopt_set GHC.ExistentialQuantification
        translateExtension ExplicitForAll = flip xopt_set GHC.ExplicitForAll
        translateExtension ExplicitNamespaces = flip xopt_set GHC.ExplicitNamespaces
        translateExtension ExtendedDefaultRules = flip xopt_set GHC.ExtendedDefaultRules
        translateExtension FlexibleContexts = flip xopt_set GHC.FlexibleContexts
        translateExtension FlexibleInstances = flip xopt_set GHC.FlexibleInstances
        translateExtension ForeignFunctionInterface = flip xopt_set GHC.ForeignFunctionInterface
        translateExtension FunctionalDependencies = flip xopt_set GHC.FunctionalDependencies
        translateExtension GADTs = flip xopt_set GHC.GADTs
        translateExtension GADTSyntax = flip xopt_set GHC.GADTSyntax
        translateExtension GeneralizedNewtypeDeriving = flip xopt_set GHC.GeneralizedNewtypeDeriving
        translateExtension GHCForeignImportPrim = flip xopt_set GHC.GHCForeignImportPrim
        translateExtension ImplicitParams = flip xopt_set GHC.ImplicitParams
        translateExtension ImplicitPrelude = flip xopt_set GHC.ImplicitPrelude
        translateExtension ImpredicativeTypes = flip xopt_set GHC.ImpredicativeTypes
        translateExtension IncoherentInstances = flip xopt_set GHC.IncoherentInstances
        translateExtension InstanceSigs = flip xopt_set GHC.InstanceSigs
        translateExtension InterruptibleFFI = flip xopt_set GHC.InterruptibleFFI
        translateExtension JavaScriptFFI = flip xopt_set GHC.JavaScriptFFI
        translateExtension KindSignatures = flip xopt_set GHC.KindSignatures
        translateExtension LambdaCase = flip xopt_set GHC.LambdaCase
        translateExtension LiberalTypeSynonyms = flip xopt_set GHC.LiberalTypeSynonyms
        translateExtension MagicHash = flip xopt_set GHC.MagicHash
        translateExtension MonadComprehensions = flip xopt_set GHC.MonadComprehensions
        translateExtension MonadFailDesugaring = flip xopt_set GHC.MonadFailDesugaring
        translateExtension MonoLocalBinds = flip xopt_set GHC.MonoLocalBinds
        translateExtension MonomorphismRestriction = flip xopt_set GHC.MonomorphismRestriction
        translateExtension MonoPatBinds = flip xopt_set GHC.MonoPatBinds
        translateExtension MultiParamTypeClasses = flip xopt_set GHC.MultiParamTypeClasses
        translateExtension MultiWayIf = flip xopt_set GHC.MultiWayIf
        translateExtension NamedFieldPuns = flip xopt_set GHC.RecordPuns
        translateExtension NamedWildCards = flip xopt_set GHC.NamedWildCards
        translateExtension NegativeLiterals = flip xopt_set GHC.NegativeLiterals
        translateExtension NondecreasingIndentation = flip xopt_set GHC.NondecreasingIndentation
        translateExtension NPlusKPatterns = flip xopt_set GHC.NPlusKPatterns
        translateExtension NullaryTypeClasses = flip xopt_set GHC.NullaryTypeClasses
        translateExtension NumDecimals = flip xopt_set GHC.NumDecimals
        translateExtension OverlappingInstances = flip xopt_set GHC.OverlappingInstances
        translateExtension OverloadedLabels = flip xopt_set GHC.OverloadedLabels
        translateExtension OverloadedLists = flip xopt_set GHC.OverloadedLists
        translateExtension OverloadedStrings = flip xopt_set GHC.OverloadedStrings
        translateExtension PackageImports = flip xopt_set GHC.PackageImports
        translateExtension ParallelArrays = flip xopt_set GHC.ParallelArrays
        translateExtension ParallelListComp = flip xopt_set GHC.ParallelListComp
        translateExtension PartialTypeSignatures = flip xopt_set GHC.PartialTypeSignatures
        translateExtension PatternGuards = flip xopt_set GHC.PatternGuards
        translateExtension PatternSignatures = flip xopt_set GHC.PatternSynonyms
        translateExtension PatternSynonyms = flip xopt_set GHC.PatternSynonyms
        translateExtension PolyKinds = flip xopt_set GHC.PolyKinds
        translateExtension PostfixOperators = flip xopt_set GHC.PostfixOperators
        translateExtension QuasiQuotes = flip xopt_set GHC.QuasiQuotes
        translateExtension RankNTypes = flip xopt_set GHC.RankNTypes
        translateExtension RebindableSyntax = flip xopt_set GHC.RebindableSyntax
        translateExtension RecordPuns = flip xopt_set GHC.RecordPuns
        translateExtension RecordWildCards = flip xopt_set GHC.RecordWildCards
        translateExtension RecursiveDo = flip xopt_set GHC.RecursiveDo
        translateExtension RelaxedPolyRec = flip xopt_set GHC.RelaxedPolyRec
        translateExtension RestrictedTypeSynonyms = flip xopt_unset GHC.LiberalTypeSynonyms
        translateExtension RoleAnnotations = flip xopt_set GHC.RoleAnnotations
        translateExtension ScopedTypeVariables = flip xopt_set GHC.ScopedTypeVariables
        translateExtension StandaloneDeriving = flip xopt_set GHC.StandaloneDeriving
        translateExtension StaticPointers = flip xopt_set GHC.StaticPointers
        translateExtension Strict = flip xopt_set GHC.Strict
        translateExtension StrictData = flip xopt_set GHC.StrictData
        translateExtension TemplateHaskell = flip xopt_set GHC.TemplateHaskell
        translateExtension TemplateHaskellQuotes = flip xopt_set GHC.TemplateHaskellQuotes
        translateExtension TraditionalRecordSyntax = flip xopt_set GHC.TraditionalRecordSyntax
        translateExtension TransformListComp = flip xopt_set GHC.TransformListComp
        translateExtension TupleSections = flip xopt_set GHC.TupleSections
        translateExtension TypeApplications = flip xopt_set GHC.TypeApplications
        translateExtension TypeFamilies = flip xopt_set GHC.TypeFamilies
        translateExtension TypeInType = flip xopt_set GHC.TypeInType
        translateExtension TypeOperators = flip xopt_set GHC.TypeOperators
        translateExtension TypeSynonymInstances = flip xopt_set GHC.TypeSynonymInstances
        translateExtension UnboxedTuples = flip xopt_set GHC.UnboxedTuples
        translateExtension UndecidableInstances = flip xopt_set GHC.UndecidableInstances
        translateExtension UndecidableSuperClasses = flip xopt_set GHC.UndecidableSuperClasses
        translateExtension UnicodeSyntax = flip xopt_set GHC.UnicodeSyntax
        translateExtension UnliftedFFITypes = flip xopt_set GHC.UnliftedFFITypes
        translateExtension ViewPatterns = flip xopt_set GHC.ViewPatterns

        translateExtension Safe = \df -> df { GHC.safeHaskell = GHC.Sf_Safe }
        translateExtension SafeImports = \df -> df { GHC.safeHaskell = GHC.Sf_Safe }
        translateExtension Trustworthy = \df -> df { GHC.safeHaskell = GHC.Sf_Trustworthy }
        translateExtension Unsafe = \df -> df { GHC.safeHaskell = GHC.Sf_Unsafe }

        translateExtension Rank2Types = flip xopt_set GHC.RankNTypes
        translateExtension PolymorphicComponents = flip xopt_set GHC.RankNTypes
        translateExtension Generics = id -- it does nothing, deprecated extension
        translateExtension NewQualifiedOperators = id -- it does nothing, deprecated extension
        translateExtension ExtensibleRecords = id -- not in GHC
        translateExtension XmlSyntax = id -- not in GHC
        translateExtension HereDocuments = id -- not in GHC
        translateExtension RegularPatterns = id -- not in GHC
