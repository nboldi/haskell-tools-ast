{-# LANGUAGE TupleSections
           , NamedFieldPuns
           , LambdaCase
           , TemplateHaskell
           #-}
module Language.Haskell.Tools.Refactor.GetModules where

import Control.Reference
import Data.List (intersperse, find, sortBy)
import qualified Data.Map as Map
import Distribution.Package (Dependency(..), PackageName(..), pkgName)
import Distribution.Verbosity (silent)
import Distribution.ModuleName (components)
import Distribution.ModuleName
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import System.FilePath.Posix
import System.Directory
import Language.Haskell.Extension

import DynFlags (DynFlags, xopt_set, xopt_unset)
import GHC (parseDynamicFlags)
import qualified DynFlags as GHC
import SrcLoc as GHC
import qualified Language.Haskell.TH.LanguageExtensions as GHC

import Language.Haskell.Tools.Refactor.RefactorBase
import Language.Haskell.Tools.AST (IdDom)

type AnalyzedModColl = ModuleCollection ()

-- | The modules of a library, executable, test or benchmark. A package contains one or more module collection.
data ModuleCollection mod
  = ModuleCollection { _mcId :: ModuleCollectionId
                     , _mcSourceDirs :: [FilePath]
                     , _mcModules :: Map.Map SourceFileKey mod
                     , _mcFlagSetup :: DynFlags -> IO DynFlags -- ^ Sets up the ghc environment for compiling the modules of this collection
                     , _mcDependencies :: [ModuleCollectionId]
                     }

-- | Module name and marker to separate .hs-boot module definitions. Specifies a source file in a working directory.
data SourceFileKey = SourceFileKey { _sfkIsBoot :: IsBoot
                                   , _sfkModuleName :: String
                                   }
  deriving (Eq, Ord, Show)

-- | This data structure identifies a module collection
data ModuleCollectionId = DirectoryMC FilePath
                        | LibraryMC String
                        | ExecutableMC String
                        | TestSuiteMC String
                        | BenchmarkMC String
  deriving (Eq, Ord, Show)

-- | Decides if a module is a .hs-boot file or a normal .hs file
data IsBoot = NormalHs | IsHsBoot deriving (Eq, Ord, Show)

makeReferences ''ModuleCollection
makeReferences ''SourceFileKey

lookupModuleColl :: String -> [ModuleCollection mod] -> Maybe (ModuleCollection mod)
lookupModuleColl moduleName = find (any ((moduleName ==) . (^. sfkModuleName)) . Map.keys . (^. mcModules))

lookupModInSCs :: SourceFileKey -> [ModuleCollection mod] -> Maybe (SourceFileKey, mod)
lookupModInSCs moduleName = find ((moduleName ==) . fst) . concatMap (Map.assocs . (^. mcModules))

removeModule :: String -> [ModuleCollection mod] -> [ModuleCollection mod]
removeModule moduleName = map (mcModules .- Map.filterWithKey (\k v -> moduleName /= (k ^. sfkModuleName)))

updateModule :: String -> IsBoot -> mod -> [ModuleCollection mod] -> [ModuleCollection mod]
updateModule moduleName boot mod = map (mcModules .- Map.insert (SourceFileKey boot moduleName) mod)

-- | Gets all ModuleCollections from a list of source directories. It also orders the source directories that are package roots so that
-- they can be loaded in the order they are defined (no backward imports). This matters in those cases because for them there can be
-- special compilation flags.
getAllModules :: [FilePath] -> IO [AnalyzedModColl]
getAllModules pathes = orderMCs . concat <$> mapM getModules pathes

-- | Sorts model collection in an order to remove all backward references.
-- Works because module collections defined by directories cannot be recursive.
orderMCs :: [ModuleCollection a] -> [ModuleCollection a]
orderMCs = sortBy compareMCs
  where compareMCs :: ModuleCollection a -> ModuleCollection a -> Ordering
        compareMCs mc _ | DirectoryMC _ <- (mc ^. mcId) = GT
        compareMCs _ mc | DirectoryMC _ <- (mc ^. mcId) = LT
        compareMCs mc1 mc2 | (mc2 ^. mcId) `elem` (mc1 ^. mcDependencies) = GT
        compareMCs mc1 mc2 | (mc1 ^. mcId) `elem` (mc2 ^. mcDependencies) = LT
        compareMCs _ _ = EQ


-- | Get modules of the project with the indicated root directory.
-- If there is a cabal file, it uses that, otherwise it just scans the directory recursively for haskell sourcefiles.
-- Only returns the non-boot haskell modules, the boot modules will be found during loading.
getModules :: FilePath -> IO [AnalyzedModColl]
getModules root
  = do files <- listDirectory root
       case find (\p -> takeExtension p == ".cabal") files of
          Just cabalFile -> modulesFromCabalFile root cabalFile
          Nothing        -> do mods <- modulesFromDirectory root root
                               return [ModuleCollection (DirectoryMC root) [root] (Map.fromList $ map ((, ()) . SourceFileKey NormalHs) mods) return []]

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

modulesFromCabalFile :: FilePath -> FilePath -> IO [AnalyzedModColl]
-- now adding all conditional entries, regardless of flags
modulesFromCabalFile root cabal = getModules . flattenPackageDescription <$> readPackageDescription silent (root </> cabal)
  where getModules pkg = maybe [] ((:[]) . toModuleCollection pkg) (library pkg) 
                           ++ map (toModuleCollection pkg) (executables pkg) 
                           ++ map (toModuleCollection pkg) (testSuites pkg) 
                           ++ map (toModuleCollection pkg) (benchmarks pkg)
           
        toModuleCollection :: ToModuleCollection tmc => PackageDescription -> tmc -> AnalyzedModColl
        toModuleCollection pkg tmc = let bi = getBuildInfo tmc 
                                      in ModuleCollection (mkModuleCollKey (pkgName $ package pkg) tmc) 
                                                          (map (normalise . (root </>)) $ hsSourceDirs bi) 
                                                          (Map.fromList $ map ((, ()) . SourceFileKey NormalHs . moduleName) (getModuleNames tmc)) 
                                                          (flagsFromBuildInfo bi)
                                                          (map (\(Dependency pkgName _) -> LibraryMC (unPackageName pkgName)) (targetBuildDepends bi))

        moduleName = concat . intersperse "." . components

class ToModuleCollection t where 
  mkModuleCollKey :: PackageName -> t -> ModuleCollectionId
  getBuildInfo :: t -> BuildInfo
  getModuleNames :: t -> [ModuleName]

instance ToModuleCollection Library where
  mkModuleCollKey pn _ = LibraryMC (unPackageName pn)
  getBuildInfo = libBuildInfo
  getModuleNames = libModules

instance ToModuleCollection Executable where
  mkModuleCollKey _ exe = ExecutableMC (exeName exe)
  getBuildInfo = buildInfo
  getModuleNames = exeModules

instance ToModuleCollection TestSuite where
  mkModuleCollKey _ test = TestSuiteMC (testName test)
  getBuildInfo = testBuildInfo
  getModuleNames = testModules

instance ToModuleCollection Benchmark where
  mkModuleCollKey _ test = BenchmarkMC (benchmarkName test)
  getBuildInfo = benchmarkBuildInfo
  getModuleNames = benchmarkModules



flagsFromBuildInfo :: BuildInfo -> DynFlags -> IO DynFlags
-- the import pathes are already set globally
flagsFromBuildInfo BuildInfo{ defaultExtensions, targetBuildDepends, options } df
  = do (df,_,_) <- parseDynamicFlags df (map (L noSrcSpan) $ concatMap snd options)
       return $ foldl (.) (\df -> df { GHC.packageFlags = map dependencyToPkgFlag targetBuildDepends }) 
                          (map (\case EnableExtension ext -> translateExtension ext
                                      _                   -> id                               
                               ) defaultExtensions) 
                          $ df
  where dependencyToPkgFlag (Dependency (PackageName pkgName) _) = GHC.ExposePackage pkgName (GHC.PackageArg pkgName) (GHC.ModRenaming True [])

        translateExtension OverlappingInstances = flip xopt_set GHC.OverlappingInstances
        translateExtension UndecidableInstances = flip xopt_set GHC.UndecidableInstances
        translateExtension IncoherentInstances = flip xopt_set GHC.IncoherentInstances
        translateExtension DoRec = flip xopt_set GHC.RecursiveDo
        translateExtension RecursiveDo = flip xopt_set GHC.RecursiveDo
        translateExtension ParallelListComp = flip xopt_set GHC.ParallelListComp
        translateExtension MultiParamTypeClasses = flip xopt_set GHC.MultiParamTypeClasses
        translateExtension MonomorphismRestriction = flip xopt_set GHC.MonomorphismRestriction
        translateExtension FunctionalDependencies = flip xopt_set GHC.FunctionalDependencies
        translateExtension RankNTypes = flip xopt_set GHC.RankNTypes
        translateExtension ExistentialQuantification = flip xopt_set GHC.ExistentialQuantification
        translateExtension ScopedTypeVariables = flip xopt_set GHC.ScopedTypeVariables
        translateExtension PatternSignatures = flip xopt_set GHC.PatternSynonyms
        translateExtension ImplicitParams = flip xopt_set GHC.ImplicitParams
        translateExtension FlexibleContexts = flip xopt_set GHC.FlexibleContexts
        translateExtension FlexibleInstances = flip xopt_set GHC.FlexibleInstances
        translateExtension EmptyDataDecls = flip xopt_set GHC.EmptyDataDecls
        translateExtension CPP = flip xopt_set GHC.Cpp
        translateExtension KindSignatures = flip xopt_set GHC.KindSignatures
        translateExtension BangPatterns = flip xopt_set GHC.BangPatterns
        translateExtension TypeSynonymInstances = flip xopt_set GHC.TypeSynonymInstances
        translateExtension TemplateHaskell = flip xopt_set GHC.TemplateHaskell
        translateExtension ForeignFunctionInterface = flip xopt_set GHC.ForeignFunctionInterface
        translateExtension Arrows = flip xopt_set GHC.Arrows
        translateExtension ImplicitPrelude = flip xopt_set GHC.ImplicitPrelude
        translateExtension NamedFieldPuns = flip xopt_set GHC.RecordPuns
        translateExtension PatternGuards = flip xopt_set GHC.PatternGuards
        translateExtension GeneralizedNewtypeDeriving = flip xopt_set GHC.GeneralizedNewtypeDeriving
        translateExtension RestrictedTypeSynonyms = flip xopt_unset GHC.LiberalTypeSynonyms
        translateExtension MagicHash = flip xopt_set GHC.MagicHash
        translateExtension TypeFamilies = flip xopt_set GHC.TypeFamilies
        translateExtension StandaloneDeriving = flip xopt_set GHC.StandaloneDeriving
        translateExtension UnicodeSyntax = flip xopt_set GHC.UnicodeSyntax
        translateExtension UnliftedFFITypes = flip xopt_set GHC.UnliftedFFITypes
        translateExtension InterruptibleFFI = flip xopt_set GHC.InterruptibleFFI
        translateExtension CApiFFI = flip xopt_set GHC.CApiFFI
        translateExtension LiberalTypeSynonyms = flip xopt_set GHC.LiberalTypeSynonyms
        translateExtension TypeOperators = flip xopt_set GHC.TypeOperators
        translateExtension RecordWildCards = flip xopt_set GHC.RecordWildCards
        translateExtension RecordPuns = flip xopt_set GHC.RecordPuns
        translateExtension DisambiguateRecordFields = flip xopt_set GHC.DisambiguateRecordFields
        translateExtension TraditionalRecordSyntax = flip xopt_set GHC.TraditionalRecordSyntax
        translateExtension OverloadedStrings = flip xopt_set GHC.OverloadedStrings
        translateExtension GADTs = flip xopt_set GHC.GADTs
        translateExtension GADTSyntax = flip xopt_set GHC.GADTSyntax
        translateExtension MonoPatBinds = flip xopt_set GHC.MonoPatBinds
        translateExtension RelaxedPolyRec = flip xopt_set GHC.RelaxedPolyRec
        translateExtension ExtendedDefaultRules = flip xopt_set GHC.ExtendedDefaultRules
        translateExtension UnboxedTuples = flip xopt_set GHC.UnboxedTuples
        translateExtension DeriveDataTypeable = flip xopt_set GHC.DeriveDataTypeable
        translateExtension DeriveGeneric = flip xopt_set GHC.DeriveGeneric
        translateExtension DefaultSignatures = flip xopt_set GHC.DefaultSignatures
        translateExtension InstanceSigs = flip xopt_set GHC.InstanceSigs
        translateExtension ConstrainedClassMethods = flip xopt_set GHC.ConstrainedClassMethods
        translateExtension PackageImports = flip xopt_set GHC.PackageImports
        translateExtension ImpredicativeTypes = flip xopt_set GHC.ImpredicativeTypes
        translateExtension PostfixOperators = flip xopt_set GHC.PostfixOperators
        translateExtension QuasiQuotes = flip xopt_set GHC.QuasiQuotes
        translateExtension TransformListComp = flip xopt_set GHC.TransformListComp
        translateExtension MonadComprehensions = flip xopt_set GHC.MonadComprehensions
        translateExtension ViewPatterns = flip xopt_set GHC.ViewPatterns
        translateExtension TupleSections = flip xopt_set GHC.TupleSections
        translateExtension GHCForeignImportPrim = flip xopt_set GHC.GHCForeignImportPrim
        translateExtension NPlusKPatterns = flip xopt_set GHC.NPlusKPatterns
        translateExtension DoAndIfThenElse = flip xopt_set GHC.DoAndIfThenElse
        translateExtension MultiWayIf = flip xopt_set GHC.MultiWayIf
        translateExtension LambdaCase = flip xopt_set GHC.LambdaCase
        translateExtension RebindableSyntax = flip xopt_set GHC.RebindableSyntax
        translateExtension ExplicitForAll = flip xopt_set GHC.ExplicitForAll
        translateExtension DatatypeContexts = flip xopt_set GHC.DatatypeContexts
        translateExtension MonoLocalBinds = flip xopt_set GHC.MonoLocalBinds
        translateExtension DeriveFunctor = flip xopt_set GHC.DeriveFunctor
        translateExtension DeriveTraversable = flip xopt_set GHC.DeriveTraversable
        translateExtension DeriveFoldable = flip xopt_set GHC.DeriveFoldable
        translateExtension NondecreasingIndentation = flip xopt_set GHC.NondecreasingIndentation
        translateExtension ConstraintKinds = flip xopt_set GHC.ConstraintKinds
        translateExtension PolyKinds = flip xopt_set GHC.PolyKinds
        translateExtension DataKinds = flip xopt_set GHC.DataKinds
        translateExtension ParallelArrays = flip xopt_set GHC.ParallelArrays
        translateExtension RoleAnnotations = flip xopt_set GHC.RoleAnnotations
        translateExtension OverloadedLists = flip xopt_set GHC.OverloadedLists
        translateExtension EmptyCase = flip xopt_set GHC.EmptyCase
        translateExtension AutoDeriveTypeable = flip xopt_set GHC.AutoDeriveTypeable
        translateExtension NegativeLiterals = flip xopt_set GHC.NegativeLiterals
        translateExtension BinaryLiterals = flip xopt_set GHC.BinaryLiterals
        translateExtension NumDecimals = flip xopt_set GHC.NumDecimals
        translateExtension NullaryTypeClasses = flip xopt_set GHC.NullaryTypeClasses
        translateExtension ExplicitNamespaces = flip xopt_set GHC.ExplicitNamespaces
        translateExtension AllowAmbiguousTypes = flip xopt_set GHC.AllowAmbiguousTypes
        translateExtension JavaScriptFFI = flip xopt_set GHC.JavaScriptFFI
        translateExtension PatternSynonyms = flip xopt_set GHC.PatternSynonyms
        translateExtension PartialTypeSignatures = flip xopt_set GHC.PartialTypeSignatures
        translateExtension NamedWildCards = flip xopt_set GHC.NamedWildCards
        translateExtension DeriveAnyClass = flip xopt_set GHC.DeriveAnyClass
        translateExtension DeriveLift = flip xopt_set GHC.DeriveLift
        translateExtension StaticPointers = flip xopt_set GHC.StaticPointers
        translateExtension StrictData = flip xopt_set GHC.StrictData
        translateExtension Strict = flip xopt_set GHC.Strict
        translateExtension ApplicativeDo = flip xopt_set GHC.ApplicativeDo
        translateExtension DuplicateRecordFields = flip xopt_set GHC.DuplicateRecordFields
        translateExtension TypeApplications = flip xopt_set GHC.TypeApplications
        translateExtension TypeInType = flip xopt_set GHC.TypeInType
        translateExtension UndecidableSuperClasses = flip xopt_set GHC.UndecidableSuperClasses
        translateExtension MonadFailDesugaring = flip xopt_set GHC.MonadFailDesugaring
        translateExtension TemplateHaskellQuotes = flip xopt_set GHC.TemplateHaskellQuotes
        translateExtension OverloadedLabels = flip xopt_set GHC.OverloadedLabels

        translateExtension Safe = \df -> df { GHC.safeHaskell = GHC.Sf_Safe }
        translateExtension SafeImports = \df -> df { GHC.safeHaskell = GHC.Sf_Safe }
        translateExtension Trustworthy = \df -> df { GHC.safeHaskell = GHC.Sf_Trustworthy }
        translateExtension Unsafe = \df -> df { GHC.safeHaskell = GHC.Sf_Unsafe }

        -- Couldn't find the equivalent of these extensions
        translateExtension Rank2Types = id
        translateExtension PolymorphicComponents = id
        translateExtension Generics = id
        translateExtension ExtensibleRecords = id
        translateExtension NewQualifiedOperators = id
        translateExtension XmlSyntax = id
        translateExtension HereDocuments = id
        translateExtension RegularPatterns = id
