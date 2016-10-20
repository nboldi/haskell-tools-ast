{-# LANGUAGE FlexibleContexts
           , TypeFamilies
           #-}
-- | Utilities for transformations that work on both top-level and local definitions
module Language.Haskell.Tools.Refactor.BindingElem where

import Control.Reference
import Language.Haskell.Tools.AST.Rewrite
import Language.Haskell.Tools.Refactor.ASTElements
import Language.Haskell.Tools.AST
import SrcLoc

-- | A type class for handling definitions that can appear as both top-level and local definitions
class BindingElem d where
  sigBind :: Simple Partial (Ann d dom SrcTemplateStage) (TypeSignature dom)
  valBind :: Simple Partial (Ann d dom SrcTemplateStage) (ValueBind dom)
  createTypeSig :: TypeSignature dom -> Ann d dom SrcTemplateStage
  createBinding :: ValueBind dom -> Ann d dom SrcTemplateStage
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

bindName :: (BindingElem d, SemanticInfo dom UQualifiedName ~ k) => Simple Traversal (Ann d dom SrcTemplateStage) k
bindName = valBind&bindingName &+& sigBind&tsName&annList&simpleName&semantics
     
getValBindInList :: (BindingElem d) => RealSrcSpan -> AnnList d dom SrcTemplateStage -> Maybe (ValueBind dom)
getValBindInList sp ls = case ls ^? valBindsInList & filtered (isInside sp) of
  [] -> Nothing
  [n] -> Just n
  _ -> error "getValBindInList: Multiple nodes"

valBindsInList :: BindingElem d => Simple Traversal (AnnList d dom SrcTemplateStage) (ValueBind dom)
valBindsInList = annList & valBind
