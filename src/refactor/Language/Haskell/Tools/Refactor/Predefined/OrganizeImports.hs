{-# LANGUAGE LambdaCase 
           , ScopedTypeVariables
           , FlexibleContexts
           , TypeFamilies
           , ConstraintKinds
           #-}
module Language.Haskell.Tools.Refactor.Predefined.OrganizeImports (organizeImports, OrganizeImportsDomain) where

import Name hiding (Name)
import GHC (TyThing(..))
import qualified GHC
import TyCon
import DataCon

import Control.Reference hiding (element)
import Control.Monad
import Data.Function hiding ((&))
import Data.Maybe
import Data.List
import Data.Generics.Uniplate.Data

import Language.Haskell.Tools.Refactor as AST

type OrganizeImportsDomain dom = ( HasNameInfo dom, HasImportInfo dom )

organizeImports :: forall dom . OrganizeImportsDomain dom => LocalRefactoring dom
organizeImports mod
  = modImports !~ narrowImports usedNames . sortImports $ mod
  where usedNames = map getName $ catMaybes $ map semanticsName
                        -- obviously we don't want the names in the imports to be considered, but both from
                        -- the declarations (used), both from the module head (re-exported) will count as usage
                      $ (universeBi (mod ^. modHead) ++ universeBi (mod ^. modDecl) :: [QualifiedName dom])
        
-- | Sorts the imports in alphabetical order
sortImports :: forall dom . ImportDeclList dom -> ImportDeclList dom
sortImports ls = srcInfo & srcTmpSeparators .= filter (not . null) (concatMap (\(sep,elems) -> sep : map fst elems) reordered)
                   $ annListElems .= concatMap (map snd . snd) reordered
                   $ ls
  where reordered :: [(String, [(String, ImportDecl dom)])]
        reordered = map (_2 .- sortBy (compare `on` (^. _2 & importModule & AST.moduleNameString))) parts

        parts = map (_2 .- reverse) $ reverse $ breakApart [] imports
        
        breakApart :: [(String, [(String, ImportDecl dom)])] -> [(String, ImportDecl dom)] -> [(String, [(String, ImportDecl dom)])]
        breakApart res [] = res
        breakApart res ((sep, e) : rest) | length (filter ('\n' ==) sep) > 1 
          = breakApart ((sep, [("",e)]) : res) rest
        breakApart ((lastSep, lastRes) : res) (elem : rest)
          = breakApart ((lastSep, elem : lastRes) : res) rest
        breakApart [] ((sep, e) : rest)
          = breakApart [(sep, [("",e)])] rest

        imports = zipWithSeparators ls

-- | Modify an import to only import  names that are used.
narrowImports :: forall dom . OrganizeImportsDomain dom 
              => [GHC.Name] -> ImportDeclList dom -> LocalRefactor dom (ImportDeclList dom)
narrowImports usedNames imps 
  = annListElems & traversal !~ narrowImport usedNames 
      $ filterListIndexed (importIsNeeded usedNames (map semanticsImportedModule (imps ^. annListElems))) imps

-- | Reduces the number of definitions used from an import
narrowImport :: OrganizeImportsDomain dom
             => [GHC.Name] -> ImportDecl dom -> LocalRefactor dom (ImportDecl dom)
narrowImport usedNames imp
  | importIsExact imp
  = importSpec&annJust&importSpecList !~ narrowImportSpecs usedNames $ imp
  | null actuallyImported 
  = importSpec !- replaceWithJust (mkImportSpecList []) $ imp
  | otherwise
  = return imp
  where actuallyImported = semanticsImported imp `intersect` usedNames

-- | Check if the import is actually needed
importIsNeeded :: OrganizeImportsDomain dom
               => [GHC.Name] -> [GHC.Module] -> Int -> ImportDecl dom -> Bool
importIsNeeded usedNames allModules i imp = not (null actuallyImported) || importedMod `notElem` previousModules
  where actuallyImported = semanticsImported imp `intersect` usedNames
        importedMod = semanticsImportedModule imp
        previousModules = take i allModules

-- | Narrows the import specification (explicitely imported elements)
narrowImportSpecs :: forall dom . OrganizeImportsDomain dom
                  => [GHC.Name] -> IESpecList dom -> LocalRefactor dom (IESpecList dom)
narrowImportSpecs usedNames 
  = (annList !~ narrowSpecSubspec usedNames) 
       >=> return . filterList isNeededSpec
  where narrowSpecSubspec :: [GHC.Name] -> IESpec dom -> LocalRefactor dom (IESpec dom)
        narrowSpecSubspec usedNames spec 
          = do let Just specName = semanticsName =<< (spec ^? ieName&simpleName)
               Just tt <- GHC.lookupName (getName specName)
               let subspecsInScope = case tt of ATyCon tc | not (isClassTyCon tc) 
                                                  -> (map getName (tyConDataCons tc) ++ map flSelector (tyConFieldLabels tc)) `intersect` usedNames
                                                _ -> usedNames
               ieSubspec&annJust !- narrowImportSubspecs subspecsInScope $ spec
  
        isNeededSpec :: IESpec dom -> Bool
        isNeededSpec ie = 
          -- if the name is used, it is needed
          fmap getName (semanticsName =<< (ie ^? ieName&simpleName)) `elem` map Just usedNames
          -- if the name is not used, but some of its constructors are used, it is needed
            || ((ie ^? ieSubspec&annJust&essList&annList) /= [])
            || (case ie ^? ieSubspec&annJust of Just SubAll -> True; _ -> False)     

-- | Reduces the number of definitions imported from a sub-specifier.
narrowImportSubspecs :: OrganizeImportsDomain dom => [GHC.Name] -> SubSpec dom -> SubSpec dom
narrowImportSubspecs [] SubAll = mkSubList []
narrowImportSubspecs _ ss@SubAll = ss
narrowImportSubspecs usedNames ss@(SubList {}) 
  = essList .- filterList (\n -> fmap getName (semanticsName =<< (n ^? simpleName)) `elem` map Just usedNames) $ ss
