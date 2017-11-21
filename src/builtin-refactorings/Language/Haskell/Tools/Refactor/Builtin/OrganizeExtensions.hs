{-# LANGUAGE TupleSections,
             TypeFamilies,
             RankNTypes,
             ConstraintKinds,
             FlexibleContexts,
             LambdaCase
             #-}
module Language.Haskell.Tools.Refactor.Builtin.OrganizeExtensions
  ( module Language.Haskell.Tools.Refactor.Builtin.OrganizeExtensions
  , module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
  ) where

import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.TraverseAST

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Utils.Extensions (expandExtension)

import GHC

import Data.Maybe
import Data.Char
import qualified Data.Map.Strict as SMap
import Control.Reference

--import Debug.Trace (traceShow)

{-# ANN module "HLint: ignore Use mappend" #-}
{-# ANN module "HLint: ignore Redundant lambda" #-}

-- NOTE: When working on the entire AST, we should build a monad,
--       that will will avoid unnecessary checks.
--       For example if it already found a record wildcard, it won't check again

--       Pretty easy now. Chcek wheter it is already in the ExtMap.

type OrganizeExtensionsDomain dom = (HasModuleInfo dom, ExtDomain dom)

organizeExtensionsRefactoring :: OrganizeExtensionsDomain dom => RefactoringChoice dom
organizeExtensionsRefactoring = ModuleRefactoring "OrganizeExtensions" (localRefactoring organizeExtensions)

projectOrganizeExtensionsRefactoring :: OrganizeExtensionsDomain dom => RefactoringChoice dom
projectOrganizeExtensionsRefactoring = ProjectRefactoring "ProjectOrganizeExtensions" projectOrganizeExtensions

projectOrganizeExtensions :: forall dom . OrganizeExtensionsDomain dom => ProjectRefactoring dom
projectOrganizeExtensions mods
  = mapM (\(k, m) -> ContentChanged . (k,) <$> localRefactoringRes id m (organizeExtensions m)) mods

tryOut :: String -> String -> IO ()
tryOut = tryRefactor (localRefactoring . const organizeExtensions)

organizeExtensions :: ExtDomain dom => LocalRefactoring dom
organizeExtensions moduleAST = do
  exts <- liftGhc $ collectExtensions moduleAST
  let exts'      = calcExts exts
      isRedundant e = (e ^. langExt) `notElem` map show exts' 
                        && (e ^. langExt) `elem` fullyHandledExtensions
  
  -- remove unused extensions (only those that are fully handled)
  filePragmas & annList & lpPragmas !~ filterListSt (not . isRedundant)
        -- remove empty {-# LANGUAGE #-} pragmas
    >=> filePragmas !~ filterListSt (\case LanguagePragma (AnnList []) -> False; _ -> True)
    $ moduleAST
  where isLVar (LVar _) = True
        isLVar _        = False

        calcExts :: ExtMap -> [Extension]
        calcExts logRels
          | ks <- SMap.keys logRels
          , all isLVar ks 
          = map (\(LVar x) -> x) . SMap.keys $ logRels
          | otherwise     = []
          
        fullyHandledExtensions :: [String]
        fullyHandledExtensions 
          = [ "RecordWildCards", "TemplateHaskell", "BangPatterns", "PatternSynonyms"
            , "TupleSections", "LambdaCase", "QuasiQuotes", "ViewPatterns", "MagicHash"
            , "UnboxedTuples" ]

collectExtensions :: ExtDomain dom =>
                     UnnamedModule dom ->
                     Ghc ExtMap
collectExtensions = \moduleAST -> do
  let defaults = collectDefaultExtensions moduleAST
      expanded = concatMap expandExtension defaults
  flip execStateT SMap.empty . flip runReaderT expanded . traverseModule $ moduleAST


collectDefaultExtensions :: UnnamedModule dom -> [Extension]
collectDefaultExtensions = map toExt . getExtensions
  where
  getExtensions :: UnnamedModule dom -> [String]
  getExtensions = flip (^?) (filePragmas & annList & lpPragmas & annList & langExt)

toExt :: String -> Extension
toExt str = case map fst $ reads $ unregularExts $ takeWhile isAlpha str of
              e:_ -> e
              []  -> error $ "Extension '" ++ takeWhile isAlpha str ++ "' is not known."

unregularExts :: String -> String
unregularExts "CPP" = "Cpp"
unregularExts "NamedFieldPuns" = "RecordPuns"
unregularExts e = e
