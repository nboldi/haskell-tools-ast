{-# LANGUAGE TupleSections
           , NamedFieldPuns
           , LambdaCase
           , TemplateHaskell
           , FlexibleContexts
           , TypeApplications
           , RankNTypes
           #-}
-- | Representation and operations for module collections (libraries, executables, ...) in the framework.
module Language.Haskell.Tools.Refactor.GetModules where

import Control.Applicative
import Control.Monad
import Control.Reference
import Data.Function (on)
import Data.List
import qualified Data.Map as Map
import Data.Char
import Data.Maybe
import Distribution.Compiler
import Distribution.ModuleName
import Distribution.Package (Dependency(..), PackageName(..), pkgName)
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.PackageDescription.Parse
import Distribution.System
import Distribution.Verbosity (silent)
import Language.Haskell.Extension as Cabal
import System.Directory
import System.FilePath

import DynFlags
import Module (stringToUnitId)
import qualified DynFlags as GHC
import GHC hiding (ModuleName)
import qualified Language.Haskell.TH.LanguageExtensions as GHC
import Name as GHC (Name)
import RdrName as GHC (RdrName)

import Language.Haskell.Tools.AST (Dom, IdDom)
import Language.Haskell.Tools.Refactor.RefactorBase

import Debug.Trace

instance Eq (ModuleCollection k) where
  (==) = (==) `on` _mcId

instance Show k => Show (ModuleCollection k) where
  show (ModuleCollection id root srcDirs renames mods _ _ deps)
    = "ModuleCollection (" ++ show id ++ ") " ++ root ++ " " ++ show srcDirs
        ++ " " ++ show renames ++ " (" ++ show mods ++ ") " ++ show deps

containingMC :: FilePath -> Simple Traversal [ModuleCollection k] (ModuleCollection k)
containingMC fp = traversal & filtered (\mc -> _mcRoot mc `isPrefixOf` fp)


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

instance Show ModuleRecord where
  show (ModuleNotLoaded code exposed) = "ModuleNotLoaded " ++ show code ++ " " ++ show exposed
  show mr@(ModuleParsed {}) = "ModuleParsed (" ++ (GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod $ fromJust $ mr ^? modRecMS) ++ ")"
  show mr@(ModuleRenamed {}) = "ModuleRenamed (" ++ (GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod $ fromJust $ mr ^? modRecMS) ++ ")"
  show mr@(ModuleTypeChecked {}) = "ModuleTypeChecked (" ++ (GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod $ fromJust $ mr ^? modRecMS) ++ ")"
  show mr@(ModuleCodeGenerated {}) = "ModuleCodeGenerated (" ++ (GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod $ fromJust $ mr ^? modRecMS) ++ ")"

modRecLoaded :: ModuleRecord -> Bool
modRecLoaded ModuleTypeChecked{} = True
modRecLoaded ModuleCodeGenerated{} = True
modRecLoaded _ = False

-- | Find the module collection where the given module is.
lookupModuleColl :: String -> [ModuleCollection SourceFileKey] -> Maybe (ModuleCollection SourceFileKey)
lookupModuleColl moduleName = find (any ((moduleName ==) . (^. sfkModuleName)) . Map.keys . (^. mcModules))

lookupModuleInSCs :: String -> [ModuleCollection SourceFileKey] -> Maybe (SourceFileKey, ModuleRecord)
lookupModuleInSCs moduleName = find ((moduleName ==) . (^. sfkModuleName) . fst) . concatMap (Map.assocs . (^. mcModules))

lookupSourceFileColl :: FilePath -> [ModuleCollection SourceFileKey] -> Maybe (ModuleCollection SourceFileKey)
lookupSourceFileColl fp = find (any ((fp ==) . (^. sfkFileName)) . Map.keys . (^. mcModules))

lookupModInSCs :: SourceFileKey -> [ModuleCollection SourceFileKey] -> Maybe (SourceFileKey, ModuleRecord)
lookupModInSCs moduleName = find (((moduleName ^. sfkFileName) ==) . (^. sfkFileName) . fst) . concatMap (Map.assocs . (^. mcModules))

lookupSourceFileInSCs :: String -> [ModuleCollection SourceFileKey] -> Maybe (SourceFileKey, ModuleRecord)
lookupSourceFileInSCs fileName = find ((fileName ==) . (^. sfkFileName) . fst) . concatMap (Map.assocs . (^. mcModules))

removeModule :: String -> [ModuleCollection SourceFileKey] -> [ModuleCollection SourceFileKey]
removeModule moduleName = map (mcModules .- Map.filterWithKey (\k _ -> moduleName /= (k ^. sfkModuleName)))

hasGeneratedCode :: SourceFileKey -> [ModuleCollection SourceFileKey] -> Bool
hasGeneratedCode key = maybe False ((\case ModuleCodeGenerated {} -> True; _ -> False) . snd)
                         . lookupModInSCs key

needsGeneratedCode :: SourceFileKey -> [ModuleCollection SourceFileKey] -> Bool
needsGeneratedCode key mcs = maybe False ((\case ModuleCodeGenerated {} -> True; ModuleNotLoaded True _ -> True; _ -> False) . snd)
                              $ lookupModInSCs key mcs <|> lookupModInSCs (sfkFileName .= "" $ key) mcs

codeGeneratedFor :: SourceFileKey -> [ModuleCollection SourceFileKey] -> [ModuleCollection SourceFileKey]
codeGeneratedFor key = map (mcModules .- Map.adjust setCodeGen (sfkFileName .= "" $ key) . Map.adjust setCodeGen key)
  where setCodeGen (ModuleTypeChecked mod ms) = ModuleCodeGenerated mod ms
        setCodeGen (ModuleNotLoaded _ exp) = ModuleNotLoaded True exp
        setCodeGen m = m

isAlreadyLoaded :: SourceFileKey -> [ModuleCollection SourceFileKey] -> Bool
isAlreadyLoaded key = maybe False (\case (_, ModuleNotLoaded {}) -> False; _ -> True)
                         . find ((key ==) . fst) . concatMap (Map.assocs . (^. mcModules))

-- | Gets all ModuleCollections from a list of source directories. It also orders the source directories that are package roots so that
-- they can be loaded in the order they are defined (no backward imports). This matters in those cases because for them there can be
-- special compilation flags.
getAllModules :: [FilePath] -> IO [ModuleCollection ModuleNameStr]
getAllModules pathes = orderMCs . concat <$> mapM (getModules pathes) (map normalise pathes)

-- | Sorts model collection in an order to remove all backward references.
-- Works because module collections defined by directories cannot be recursive.
orderMCs :: [ModuleCollection k] -> [ModuleCollection k]
orderMCs = sortBy compareMCs
  where compareMCs :: ModuleCollection k -> ModuleCollection k -> Ordering
        compareMCs mc _ | DirectoryMC _ <- (mc ^. mcId) = GT
        compareMCs _ mc | DirectoryMC _ <- (mc ^. mcId) = LT
        compareMCs mc1 mc2 | (mc2 ^. mcId) `elem` (mc1 ^. mcDependencies) = GT
        compareMCs mc1 mc2 | (mc1 ^. mcId) `elem` (mc2 ^. mcDependencies) = LT
        compareMCs _ _ = EQ


-- | Get modules of the project with the indicated root directory.
-- If there is a cabal file, it uses that, otherwise it just scans the directory recursively for haskell sourcefiles.
-- Only returns the non-boot haskell modules, the boot modules will be found during loading.
getModules :: [FilePath] -> FilePath -> IO [ModuleCollection ModuleNameStr]
getModules allRoots root
  = do files <- listDirectory root
       case find (\p -> takeExtension p == ".cabal") files of
          Just cabalFile -> modulesFromCabalFile allRoots root cabalFile
          Nothing        -> do mods <- modulesFromDirectory root root
                               return [ModuleCollection (DirectoryMC root) root [root] [] (modKeys mods) return return []]
  where modKeys mods = Map.fromList $ map (, ModuleNotLoaded False True) mods

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
modulesFromCabalFile :: [FilePath] -> FilePath -> FilePath -> IO [ModuleCollection ModuleNameStr]
-- now adding all conditional entries, regardless of flags
modulesFromCabalFile allRoots root cabal = (getModules . setupFlags <$> readPackageDescription silent (root </> cabal))
  where getModules pkg = maybe [] (maybe [] (:[]) . toModuleCollection pkg) (library pkg)
                           ++ catMaybes (map (toModuleCollection pkg) (executables pkg))
                           ++ catMaybes (map (toModuleCollection pkg) (testSuites pkg))
                           ++ catMaybes (map (toModuleCollection pkg) (benchmarks pkg))

        toModuleCollection :: ToModuleCollection tmc => PackageDescription -> tmc -> Maybe (ModuleCollection ModuleNameStr)
        toModuleCollection PackageDescription{ buildType = Just Custom } tmc
          = error "While parsing cabal file \"build-type: custom\" is not supported"
        toModuleCollection pkg tmc
          = let bi = getBuildInfo tmc
                packageName = pkgName $ package pkg
             in if buildable bi
                  then Just $ ModuleCollection (mkModuleCollKey packageName tmc)
                                root
                                (map (normalise . (root </>)) $ hsSourceDirs bi)
                                (getModuleSourceFiles tmc)
                                (Map.fromList $ map modRecord $ getModuleNames tmc)
                                (flagsFromBuildInfo bi)
                                (loadFlagsFromBuildInfo bi)
                                (map (\(Dependency pkgName _) -> LibraryMC (unPackageName pkgName)) (targetBuildDepends bi))
                  else Nothing
          where modRecord mn = ( moduleName mn, ModuleNotLoaded False (needsToCompile tmc mn) )
        moduleName = concat . intersperse "." . components
        setupFlags = either (\deps -> error $ "Missing dependencies: " ++ show deps) fst
                       . finalizePackageDescription [] (const True) buildPlatform
                                                    (unknownCompilerInfo buildCompilerId NoAbiTag) []

class ToModuleCollection t where
  mkModuleCollKey :: PackageName -> t -> ModuleCollectionId
  getBuildInfo :: t -> BuildInfo
  getModuleNames :: t -> [ModuleName]
  getModuleSourceFiles :: t -> [(String, FilePath)]
  getModuleSourceFiles _ = []
  needsToCompile :: t -> ModuleName -> Bool
  getMain :: t -> String
  getMain l = getMain' (getBuildInfo l)

instance ToModuleCollection Library where
  mkModuleCollKey pn _ = LibraryMC (unPackageName pn)
  getBuildInfo = libBuildInfo
  getModuleNames = libModules
  needsToCompile l m = m `elem` exposedModules l

instance ToModuleCollection Executable where
  mkModuleCollKey pn exe = ExecutableMC (unPackageName pn) (exeName exe)
  getBuildInfo = buildInfo
  getModuleNames exe = fromString (getMain exe) : exeModules exe
  needsToCompile exe mn = components mn == [getMain exe]
  getModuleSourceFiles exe = [(getMain exe, modulePath exe)]

instance ToModuleCollection TestSuite where
  mkModuleCollKey pn test = TestSuiteMC (unPackageName pn) (testName test)
  getBuildInfo = testBuildInfo
  getModuleNames exe = fromString (getMain exe) : testModules exe
  needsToCompile exe mn = components mn == [getMain exe]
  getModuleSourceFiles exe
    = case testInterface exe of
        TestSuiteExeV10 _ fp -> [(getMain exe, fp)]
        _ -> []
  getMain t = case testInterface t of
        TestSuiteLibV09 _ mod -> intercalate "." $ components mod
        _ -> getMain' (getBuildInfo t)

instance ToModuleCollection Benchmark where
  mkModuleCollKey pn test = BenchmarkMC (unPackageName pn) (benchmarkName test)
  getBuildInfo = benchmarkBuildInfo
  getModuleNames exe = fromString (getMain exe) : benchmarkModules exe
  needsToCompile exe mn = components mn == [getMain exe]
  getModuleSourceFiles exe
    = case benchmarkInterface exe of
        BenchmarkExeV10 _ fp -> [(getMain exe, fp)]
        _ -> []

getMain' :: BuildInfo -> String
getMain' bi
  = case ls of _:e:_ -> intercalate "." $ filter (isUpper . head) $ groupBy ((==) `on` (== '.')) e
               _ -> "Main"
  where ls = dropWhile (/= "-main-is") (concatMap snd (options bi))

isDirectoryMC :: ModuleCollection k -> Bool
isDirectoryMC mc = case mc ^. mcId of DirectoryMC{} -> True; _ -> False

compileInContext :: ModuleCollection k -> [ModuleCollection k] -> DynFlags -> IO DynFlags
compileInContext mc mcs dfs
  = (\dfs' -> applyDependencies mcs (mc ^. mcDependencies) (selectEnabled dfs'))
       <$> (mc ^. mcFlagSetup $ dfs)
  where selectEnabled = if isDirectoryMC mc then id else onlyUseEnabled

applyDependencies :: [ModuleCollection k] -> [ModuleCollectionId] -> DynFlags -> DynFlags
applyDependencies mcs ids dfs
  = dfs { GHC.packageFlags = GHC.packageFlags dfs ++ (catMaybes $ map (dependencyToPkgFlag mcs) ids) }

onlyUseEnabled :: DynFlags -> DynFlags
onlyUseEnabled = GHC.setGeneralFlag' GHC.Opt_HideAllPackages

dependencyToPkgFlag :: [ModuleCollection k] -> ModuleCollectionId -> Maybe (GHC.PackageFlag)
dependencyToPkgFlag mcs lib@(LibraryMC pkgName)
  = if isNothing $ find (\mc -> (mc ^. mcId) == lib) mcs
      then Just $ GHC.ExposePackage pkgName (GHC.PackageArg pkgName) (GHC.ModRenaming True [])
      else Nothing
dependencyToPkgFlag _ _ = Nothing

setupLoadFlags :: [ModuleCollection k] -> DynFlags -> IO DynFlags
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
  where setupLoadExtensions = foldl (.) id (map setExtensionFlag' $ catMaybes $ map translateExtension loadExtensions)
        loadExtensions = [PatternSynonyms | patternSynonymsNeeded] ++ [ExplicitNamespaces | explicitNamespacesNeeded]
                           ++ [PackageImports | packageImportsNeeded] ++ [CPP | cppNeeded] ++ [MagicHash | magicHashNeeded]
        explicitNamespacesNeeded = not $ null $ map EnableExtension [ExplicitNamespaces, TypeFamilies, TypeOperators] `intersect` usedExtensions bi
        patternSynonymsNeeded = EnableExtension PatternSynonyms `elem` usedExtensions bi
        packageImportsNeeded = EnableExtension PackageImports `elem` usedExtensions bi
        cppNeeded = EnableExtension CPP `elem` usedExtensions bi
        magicHashNeeded = EnableExtension MagicHash `elem` usedExtensions bi

flagsFromBuildInfo :: BuildInfo -> DynFlags -> IO DynFlags
-- the import pathes are already set globally
flagsFromBuildInfo bi@BuildInfo{ options } df
  = do (df',unused,warnings) <- parseDynamicFlags df (map (L noSrcSpan) $ concatMap snd options)
       mapM_ putStrLn (map unLoc warnings ++ map (("Flag is not used: " ++) . unLoc) unused)
       return $ (flip lang_set (toGhcLang =<< defaultLanguage bi))
         $ foldl (.) id (map (\case EnableExtension ext -> setEnabled True ext
                                    DisableExtension ext -> setEnabled False ext
                        ) (usedExtensions bi))
         $ foldr (.) id (map (setEnabled True) (languageDefault (defaultLanguage bi)))
         $ df'
  where toGhcLang Cabal.Haskell98 = Just GHC.Haskell98
        toGhcLang Cabal.Haskell2010 = Just GHC.Haskell2010
        toGhcLang _ = Nothing

        -- We don't put the default settings (ImplicitPrelude, MonomorphismRestriction) here
        -- because that overrides the opposite extensions (NoImplicitPrelude, NoMonomorphismRestriction)
        -- enabled in modules.
        languageDefault (Just Cabal.Haskell2010)
          = [ DatatypeContexts, DoAndIfThenElse, EmptyDataDecls, ForeignFunctionInterface
            , PatternGuards, RelaxedPolyRec, TraditionalRecordSyntax ]
        -- Haskell 98 is the default
        languageDefault _
          = [ DatatypeContexts, NondecreasingIndentation, NPlusKPatterns, TraditionalRecordSyntax ]

        setEnabled enable ext
          = case translateExtension ext of
              Just e -> (if enable then setExtensionFlag' else unSetExtensionFlag') e
              Nothing -> id

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
translateExtension AllowAmbiguousTypes = Just GHC.AllowAmbiguousTypes
translateExtension ApplicativeDo = Just GHC.ApplicativeDo
translateExtension Arrows = Just GHC.Arrows
translateExtension AutoDeriveTypeable = Just GHC.AutoDeriveTypeable
translateExtension BangPatterns = Just GHC.BangPatterns
translateExtension BinaryLiterals = Just GHC.BinaryLiterals
translateExtension CApiFFI = Just GHC.CApiFFI
translateExtension ConstrainedClassMethods = Just GHC.ConstrainedClassMethods
translateExtension ConstraintKinds = Just GHC.ConstraintKinds
translateExtension CPP = Just GHC.Cpp
translateExtension DataKinds = Just GHC.DataKinds
translateExtension DatatypeContexts = Just GHC.DatatypeContexts
translateExtension DefaultSignatures = Just GHC.DefaultSignatures
translateExtension DeriveAnyClass = Just GHC.DeriveAnyClass
translateExtension DeriveDataTypeable = Just GHC.DeriveDataTypeable
translateExtension DeriveFoldable = Just GHC.DeriveFoldable
translateExtension DeriveFunctor = Just GHC.DeriveFunctor
translateExtension DeriveGeneric = Just GHC.DeriveGeneric
translateExtension DeriveLift = Just GHC.DeriveLift
translateExtension DeriveTraversable = Just GHC.DeriveTraversable
translateExtension DisambiguateRecordFields = Just GHC.DisambiguateRecordFields
translateExtension DoAndIfThenElse = Just GHC.DoAndIfThenElse
translateExtension DoRec = Just GHC.RecursiveDo
translateExtension DuplicateRecordFields = Just GHC.DuplicateRecordFields
translateExtension EmptyCase = Just GHC.EmptyCase
translateExtension EmptyDataDecls = Just GHC.EmptyDataDecls
translateExtension ExistentialQuantification = Just GHC.ExistentialQuantification
translateExtension ExplicitForAll = Just GHC.ExplicitForAll
translateExtension ExplicitNamespaces = Just GHC.ExplicitNamespaces
translateExtension ExtendedDefaultRules = Just GHC.ExtendedDefaultRules
translateExtension FlexibleContexts = Just GHC.FlexibleContexts
translateExtension FlexibleInstances = Just GHC.FlexibleInstances
translateExtension ForeignFunctionInterface = Just GHC.ForeignFunctionInterface
translateExtension FunctionalDependencies = Just GHC.FunctionalDependencies
translateExtension GADTs = Just GHC.GADTs
translateExtension GADTSyntax = Just GHC.GADTSyntax
translateExtension GeneralizedNewtypeDeriving = Just GHC.GeneralizedNewtypeDeriving
translateExtension GHCForeignImportPrim = Just GHC.GHCForeignImportPrim
translateExtension ImplicitParams = Just GHC.ImplicitParams
translateExtension ImplicitPrelude = Just GHC.ImplicitPrelude
translateExtension ImpredicativeTypes = Just GHC.ImpredicativeTypes
translateExtension IncoherentInstances = Just GHC.IncoherentInstances
translateExtension InstanceSigs = Just GHC.InstanceSigs
translateExtension InterruptibleFFI = Just GHC.InterruptibleFFI
translateExtension JavaScriptFFI = Just GHC.JavaScriptFFI
translateExtension KindSignatures = Just GHC.KindSignatures
translateExtension LambdaCase = Just GHC.LambdaCase
translateExtension LiberalTypeSynonyms = Just GHC.LiberalTypeSynonyms
translateExtension MagicHash = Just GHC.MagicHash
translateExtension MonadComprehensions = Just GHC.MonadComprehensions
translateExtension MonadFailDesugaring = Just GHC.MonadFailDesugaring
translateExtension MonoLocalBinds = Just GHC.MonoLocalBinds
translateExtension MonomorphismRestriction = Just GHC.MonomorphismRestriction
translateExtension MonoPatBinds = Just GHC.MonoPatBinds
translateExtension MultiParamTypeClasses = Just GHC.MultiParamTypeClasses
translateExtension MultiWayIf = Just GHC.MultiWayIf
translateExtension NamedFieldPuns = Just GHC.RecordPuns
translateExtension NamedWildCards = Just GHC.NamedWildCards
translateExtension NegativeLiterals = Just GHC.NegativeLiterals
translateExtension NondecreasingIndentation = Just GHC.NondecreasingIndentation
translateExtension NPlusKPatterns = Just GHC.NPlusKPatterns
translateExtension NullaryTypeClasses = Just GHC.NullaryTypeClasses
translateExtension NumDecimals = Just GHC.NumDecimals
translateExtension OverlappingInstances = Just GHC.OverlappingInstances
translateExtension OverloadedLabels = Just GHC.OverloadedLabels
translateExtension OverloadedLists = Just GHC.OverloadedLists
translateExtension OverloadedStrings = Just GHC.OverloadedStrings
translateExtension PackageImports = Just GHC.PackageImports
translateExtension ParallelArrays = Just GHC.ParallelArrays
translateExtension ParallelListComp = Just GHC.ParallelListComp
translateExtension PartialTypeSignatures = Just GHC.PartialTypeSignatures
translateExtension PatternGuards = Just GHC.PatternGuards
translateExtension PatternSignatures = Just GHC.PatternSynonyms
translateExtension PatternSynonyms = Just GHC.PatternSynonyms
translateExtension PolyKinds = Just GHC.PolyKinds
translateExtension PostfixOperators = Just GHC.PostfixOperators
translateExtension QuasiQuotes = Just GHC.QuasiQuotes
translateExtension RankNTypes = Just GHC.RankNTypes
translateExtension RebindableSyntax = Just GHC.RebindableSyntax
translateExtension RecordPuns = Just GHC.RecordPuns
translateExtension RecordWildCards = Just GHC.RecordWildCards
translateExtension RecursiveDo = Just GHC.RecursiveDo
translateExtension RelaxedPolyRec = Just GHC.RelaxedPolyRec
translateExtension RestrictedTypeSynonyms = Nothing -- flip xopt_unset GHC.LiberalTypeSynonyms
translateExtension RoleAnnotations = Just GHC.RoleAnnotations
translateExtension ScopedTypeVariables = Just GHC.ScopedTypeVariables
translateExtension StandaloneDeriving = Just GHC.StandaloneDeriving
translateExtension StaticPointers = Just GHC.StaticPointers
translateExtension Strict = Just GHC.Strict
translateExtension StrictData = Just GHC.StrictData
translateExtension TemplateHaskell = Just GHC.TemplateHaskell
translateExtension TemplateHaskellQuotes = Just GHC.TemplateHaskellQuotes
translateExtension TraditionalRecordSyntax = Just GHC.TraditionalRecordSyntax
translateExtension TransformListComp = Just GHC.TransformListComp
translateExtension TupleSections = Just GHC.TupleSections
translateExtension TypeApplications = Just GHC.TypeApplications
translateExtension TypeFamilies = Just GHC.TypeFamilies
translateExtension TypeInType = Just GHC.TypeInType
translateExtension TypeOperators = Just GHC.TypeOperators
translateExtension TypeSynonymInstances = Just GHC.TypeSynonymInstances
translateExtension UnboxedTuples = Just GHC.UnboxedTuples
translateExtension UndecidableInstances = Just GHC.UndecidableInstances
translateExtension UndecidableSuperClasses = Just GHC.UndecidableSuperClasses
translateExtension UnicodeSyntax = Just GHC.UnicodeSyntax
translateExtension UnliftedFFITypes = Just GHC.UnliftedFFITypes
translateExtension ViewPatterns = Just GHC.ViewPatterns

translateExtension Safe = Nothing -- \df -> df { GHC.safeHaskell = GHC.Sf_Safe }
translateExtension SafeImports = Nothing -- \df -> df { GHC.safeHaskell = GHC.Sf_Safe }
translateExtension Trustworthy = Nothing -- \df -> df { GHC.safeHaskell = GHC.Sf_Trustworthy }
translateExtension Unsafe = Nothing -- \df -> df { GHC.safeHaskell = GHC.Sf_Unsafe }

translateExtension Rank2Types = Just GHC.RankNTypes
translateExtension PolymorphicComponents = Just GHC.RankNTypes
translateExtension Generics = Nothing -- it does nothing, deprecated extension
translateExtension NewQualifiedOperators = Nothing -- it does nothing, deprecated extension
translateExtension ExtensibleRecords = Nothing -- not in GHC
translateExtension XmlSyntax = Nothing -- not in GHC
translateExtension HereDocuments = Nothing -- not in GHC
translateExtension RegularPatterns = Nothing -- not in GHC
