{-# LANGUAGE ViewPatterns
           , FlexibleContexts
           , ScopedTypeVariables
           , RankNTypes 
           , TypeApplications
           , TypeFamilies
           , ConstraintKinds
           #-}
module Language.Haskell.Tools.Refactor.GenerateTypeSignature (generateTypeSignature, generateTypeSignature', GenerateSignatureDomain) where

import GHC hiding (UModule)
import Type as GHC
import TyCon as GHC
import OccName as GHC
import Outputable as GHC
import TysWiredIn as GHC
import Id as GHC

import Data.List
import Data.Maybe
import Data.Data
import Data.Generics.Uniplate.Data
import Control.Monad
import Control.Monad.State
import Control.Reference hiding (element)
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers
import Language.Haskell.Tools.AST.Rewrite
import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.Refactor.RefactorBase

type GenerateSignatureDomain dom = ( HasModuleInfo dom, HasIdInfo dom, HasImportInfo dom ) 

generateTypeSignature' :: GenerateSignatureDomain dom => RealSrcSpan -> LocalRefactoring dom
generateTypeSignature' sp = generateTypeSignature (nodesContaining sp) (nodesContaining sp) (getValBindInList sp) 

-- | Perform the refactoring on either local or top-level definition
generateTypeSignature :: GenerateSignatureDomain dom => Simple Traversal (Ann UModule dom SrcTemplateStage) (AnnList UDecl dom SrcTemplateStage) 
                                -- ^ Access for a top-level definition if it is the selected definition
                           -> Simple Traversal (Ann UModule dom SrcTemplateStage) (AnnList ULocalBind dom SrcTemplateStage) 
                                -- ^ Access for a definition list if it contains the selected definition
                           -> (forall d . (Show (Ann d dom SrcTemplateStage), Data (Ann d dom SrcTemplateStage), Typeable d, BindingElem d) 
                                => AnnList d dom SrcTemplateStage -> Maybe (Ann UValueBind dom SrcTemplateStage)) 
                                -- ^ Selector for either local or top-level declaration in the definition list
                           -> LocalRefactoring dom
generateTypeSignature topLevelRef localRef vbAccess
  = flip evalStateT False .
     (topLevelRef !~ genTypeSig vbAccess
        <=< localRef !~ genTypeSig vbAccess)
  
genTypeSig :: (GenerateSignatureDomain dom, BindingElem d) => (AnnList d dom SrcTemplateStage -> Maybe (Ann UValueBind dom SrcTemplateStage))  
                -> AnnList d dom SrcTemplateStage -> StateT Bool (LocalRefactor dom) (AnnList d dom SrcTemplateStage)
genTypeSig vbAccess ls 
  | Just vb <- vbAccess ls 
  , not (typeSignatureAlreadyExist ls vb)
    = do let id = getBindingName vb
             isTheBind (Just decl) 
               = isBinding decl && map semanticsId (decl ^? bindName) == map semanticsId (vb ^? bindingName)
             isTheBind _ = False
             
         alreadyGenerated <- get
         if alreadyGenerated 
           then return ls
           else do put True
                   typeSig <- lift $ generateTSFor (getName id) (idType id)
                   return $ insertWhere (createTypeSig typeSig) (const True) isTheBind ls
  | otherwise = return ls


generateTSFor :: GenerateSignatureDomain dom => GHC.Name -> GHC.Type -> LocalRefactor dom (Ann UTypeSignature dom SrcTemplateStage)
generateTSFor n t = mkTypeSignature (mkUnqualName' n) <$> generateTypeFor (-1) (dropForAlls t)

-- | Generates the source-level type for a GHC internal type
generateTypeFor :: GenerateSignatureDomain dom => Int -> GHC.Type -> LocalRefactor dom (Ann AST.UType dom SrcTemplateStage) 
generateTypeFor prec t 
  -- context
  | (break (not . isPredTy) -> (preds, other), rt) <- splitFunTys t
  , not (null preds)
  = do ctx <- case preds of [pred] -> mkContextOne <$> generateAssertionFor pred
                            _ -> mkContextMulti <$> mapM generateAssertionFor preds
       wrapParen 0 <$> (mkTyCtx ctx <$> generateTypeFor 0 (mkFunTys other rt))
  -- function
  | Just (at, rt) <- splitFunTy_maybe t
  = wrapParen 0 <$> (mkTyFun <$> generateTypeFor 10 at <*> generateTypeFor 0 rt)
  -- type operator (we don't know the precedences, so always use parentheses)
  | (op, [at,rt]) <- splitAppTys t
  , Just tc <- tyConAppTyCon_maybe op
  , isSymOcc (getOccName (getName tc))
  = wrapParen 0 <$> (mkTyInfix <$> generateTypeFor 10 at <*> referenceOperator (idName $ getTCId tc) <*> generateTypeFor 10 rt)
  -- tuple types
  | Just (tc, tas) <- splitTyConApp_maybe t
  , isTupleTyCon tc
  = mkTyTuple <$> mapM (generateTypeFor (-1)) tas
  -- string type
  | Just (ls, [et]) <- splitTyConApp_maybe t
  , Just ch <- tyConAppTyCon_maybe et
  , listTyCon == ls
  , charTyCon == ch
  = return $ mkTyVar (mkNormalName $ mkSimpleName "String")
  -- list types
  | Just (tc, [et]) <- splitTyConApp_maybe t
  , listTyCon == tc
  = mkTyList <$> generateTypeFor (-1) et
  -- type application
  | Just (tf, ta) <- splitAppTy_maybe t
  = wrapParen 10 <$> (mkTyApp <$> generateTypeFor 10 tf <*> generateTypeFor 11 ta)
  -- type constructor
  | Just tc <- tyConAppTyCon_maybe t
  = mkTyVar <$> referenceName (idName $ getTCId tc)
  -- type variable
  | Just tv <- getTyVar_maybe t
  = mkTyVar <$> referenceName (idName tv)
  -- forall type
  | (tvs@(_:_), t') <- splitForAllTys t
  = wrapParen (-1) <$> (mkTyForall (map (mkTypeVar' . getName) tvs) <$> generateTypeFor 0 t')
  | otherwise = error ("Cannot represent type: " ++ showSDocUnsafe (ppr t))
  where wrapParen :: Int -> Ann AST.UType dom SrcTemplateStage -> Ann AST.UType dom SrcTemplateStage
        wrapParen prec' node = if prec' < prec then mkTyParen node else node

        getTCId :: GHC.TyCon -> GHC.Id
        getTCId tc = GHC.mkVanillaGlobal (GHC.tyConName tc) (tyConKind tc)

        generateAssertionFor :: GenerateSignatureDomain dom => GHC.Type -> LocalRefactor dom (Ann AST.UAssertion dom SrcTemplateStage)
        generateAssertionFor t 
          | Just (tc, types) <- splitTyConApp_maybe t
          = mkClassAssert <$> referenceName (idName $ getTCId tc) <*> mapM (generateTypeFor 0) types
        -- TODO: infix things
    
-- | Check whether the definition already has a type signature
typeSignatureAlreadyExist :: (GenerateSignatureDomain dom, BindingElem d) => AnnList d dom SrcTemplateStage -> Ann UValueBind dom SrcTemplateStage -> Bool
typeSignatureAlreadyExist ls vb = 
  getBindingName vb `elem` (map semanticsId $ concatMap (^? bindName) (filter isTypeSig $ ls ^? annList))
  
getBindingName :: GenerateSignatureDomain dom => Ann UValueBind dom SrcTemplateStage -> GHC.Id
getBindingName vb = case nub $ map semanticsId $ vb ^? bindingName of 
  [n] -> n
  [] -> error "Trying to generate a signature for a binding with no name"
  _ -> error "Trying to generate a signature for a binding with multiple names"


-- TODO: put these into a utility module

-- | A type class for transformations that work on both top-level and local definitions
class BindingElem d where
  sigBind :: Simple Partial (Ann d dom stage) (Ann UTypeSignature dom stage)
  valBind :: Simple Partial (Ann d dom stage) (Ann UValueBind dom stage)
  createTypeSig :: Ann UTypeSignature dom SrcTemplateStage -> Ann d dom SrcTemplateStage
  createBinding :: Ann UValueBind dom SrcTemplateStage -> Ann d dom SrcTemplateStage
  isTypeSig :: Ann d dom stage -> Bool
  isBinding :: Ann d dom stage -> Bool
  
instance BindingElem UDecl where
  sigBind = declTypeSig
  valBind = declValBind
  createTypeSig = mkTypeSigDecl
  createBinding = mkValueBinding
  isTypeSig TypeSigDecl {} = True
  isTypeSig _ = False
  isBinding ValueBinding {} = True
  isBinding _ = False

instance BindingElem ULocalBind where
  sigBind = localSig
  valBind = localVal
  createTypeSig = mkLocalTypeSig
  createBinding = mkLocalValBind
  isTypeSig LocalTypeSig {} = True
  isTypeSig _ = False
  isBinding LocalValBind {} = True
  isBinding _ = False

bindName :: (BindingElem d, SemanticInfo dom UQualifiedName ~ k) => Simple Traversal (Ann d dom stage) k
bindName = valBind&bindingName &+& sigBind&tsName&annList&simpleName&semantics

valBindsInList :: BindingElem d => Simple Traversal (AnnList d dom stage) (Ann UValueBind dom stage)
valBindsInList = annList & valBind
     
getValBindInList :: (BindingElem d, SourceInfo stage) => RealSrcSpan -> AnnList d dom stage -> Maybe (Ann UValueBind dom stage)
getValBindInList sp ls = case ls ^? valBindsInList & filtered (isInside sp) of
  [] -> Nothing
  [n] -> Just n
  _ -> error "getValBindInList: Multiple nodes"
