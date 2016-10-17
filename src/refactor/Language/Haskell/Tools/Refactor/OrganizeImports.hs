{-# LANGUAGE LambdaCase 
           , ScopedTypeVariables
           , FlexibleContexts
           , TypeFamilies
           , ConstraintKinds
           #-}
module Language.Haskell.Tools.Refactor.OrganizeImports (organizeImports, OrganizeImportsDomain) where

import SrcLoc
import Name hiding (Name)
import GHC (Ghc, GhcMonad, lookupGlobalName, TyThing(..), moduleNameString, moduleName)
import qualified GHC
import TyCon
import ConLike
import DataCon
import Outputable (Outputable(..), ppr, showSDocUnsafe)

import Control.Reference hiding (element)
import Control.Monad
import Control.Monad.IO.Class
import Data.Function hiding ((&))
import Data.String
import Data.Maybe
import Data.Data
import Data.List
import Data.Generics.Uniplate.Data
import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.FromGHC
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers
import Language.Haskell.Tools.PrettyPrint
import Language.Haskell.Tools.AST.Rewrite
import Language.Haskell.Tools.Refactor.RefactorBase
import Debug.Trace

type OrganizeImportsDomain dom = ( Domain dom, HasNameInfo dom, HasImportInfo dom )

organizeImports :: forall dom . OrganizeImportsDomain dom => LocalRefactoring dom
organizeImports mod
  = modImports&annListElems !~ narrowImports usedNames . sortImports $ mod
  where usedNames = map getName $ catMaybes
                                $ map (semanticsName . (^. (annotation&semanticInfo)))
                                $ (universeBi (mod ^. modHead) ++ universeBi (mod ^. modDecl) :: [Ann QualifiedName dom SrcTemplateStage])
        
-- | Sorts the imports in alphabetical order
sortImports :: [Ann ImportDecl dom SrcTemplateStage] -> [Ann ImportDecl dom SrcTemplateStage]
sortImports = sortBy (compare `on` (^. importModule&AST.moduleNameString))

-- | Modify an import to only import  names that are used.
narrowImports :: forall dom . OrganizeImportsDomain dom 
              => [GHC.Name] -> [Ann ImportDecl dom SrcTemplateStage] -> LocalRefactor dom [Ann ImportDecl dom SrcTemplateStage]
narrowImports usedNames imps = foldM (narrowOneImport usedNames) imps imps 
  where narrowOneImport :: [GHC.Name] -> [Ann ImportDecl dom SrcTemplateStage] -> Ann ImportDecl dom SrcTemplateStage -> LocalRefactor dom [Ann ImportDecl dom SrcTemplateStage]
        narrowOneImport names all one =
          (\case Just x -> map (\e -> if e == one then x else e) all
                 Nothing -> delete one all) <$> narrowImport names (map (semanticsImportedModule . (^. semantics)) all) one 
        
narrowImport :: OrganizeImportsDomain dom
             => [GHC.Name] -> [GHC.Module] -> Ann ImportDecl dom SrcTemplateStage 
                           -> LocalRefactor dom (Maybe (Ann ImportDecl dom SrcTemplateStage))
narrowImport usedNames otherModules imp
  | importIsExact imp
  = Just <$> (importSpec&annJust&importSpecList !~ narrowImportSpecs usedNames $ imp)
  | otherwise 
  = if null actuallyImported
      then if length (filter (== importedMod) otherModules) > 1 
              then pure Nothing
              else Just <$> (importSpec !- replaceWithJust (mkImportSpecList []) $ imp)
      else pure (Just imp)
  where actuallyImported = semanticsImported (imp ^. semantics) `intersect` usedNames
        importedMod = semanticsImportedModule $ imp ^. semantics
    
-- | Narrows the import specification (explicitely imported elements)
narrowImportSpecs :: forall dom . OrganizeImportsDomain dom
                  => [GHC.Name] -> AnnList IESpec dom SrcTemplateStage -> LocalRefactor dom (AnnList IESpec dom SrcTemplateStage)
narrowImportSpecs usedNames 
  = (annList !~ narrowSpecSubspec usedNames) 
       >=> return . filterList isNeededSpec
  where narrowSpecSubspec :: [GHC.Name] -> Ann IESpec dom SrcTemplateStage -> LocalRefactor dom (Ann IESpec dom SrcTemplateStage)
        narrowSpecSubspec usedNames spec 
          = do let Just specName = semanticsName =<< (spec ^? ieName&simpleName&annotation&semanticInfo)
               Just tt <- GHC.lookupName (getName specName)
               let subspecsInScope = case tt of ATyCon tc | not (isClassTyCon tc) 
                                                  -> map getName (tyConDataCons tc) `intersect` usedNames
                                                _ -> usedNames
               ieSubspec&annJust !- narrowImportSubspecs subspecsInScope $ spec
  
        isNeededSpec :: Ann IESpec dom SrcTemplateStage -> Bool
        isNeededSpec ie = 
          -- if the name is used, it is needed
          fmap getName (semanticsName =<< (ie ^? ieName&simpleName&annotation&semanticInfo)) `elem` map Just usedNames
          -- if the name is not used, but some of its constructors are used, it is needed
            || ((ie ^? ieSubspec&annJust&essList&annList) /= [])
            || (case ie ^? ieSubspec&annJust of Just SubAll -> True; _ -> False)     
  
narrowImportSubspecs :: OrganizeImportsDomain dom => [GHC.Name] -> Ann SubSpec dom SrcTemplateStage -> Ann SubSpec dom SrcTemplateStage
narrowImportSubspecs [] SubAll = mkSubList []
narrowImportSubspecs _ ss@SubAll = ss
narrowImportSubspecs usedNames ss@(SubList {}) 
  = essList .- filterList (\n -> fmap getName (semanticsName =<< (n ^? simpleName&annotation&semanticInfo)) `elem` map Just usedNames) $ ss
