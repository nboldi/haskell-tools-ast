-- | Generation of type-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkTyForall@ creates the annotated version of the @TyForall@ AST constructor.
{-# LANGUAGE OverloadedStrings
           , TypeFamilies 
           #-}
module Language.Haskell.Tools.AST.Gen.Types where

import qualified Name as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen.Names
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

-- * Generation of types

mkTyForall :: [Ann UTyVar dom SrcTemplateStage] -> Ann UType dom SrcTemplateStage -> Ann UType dom SrcTemplateStage
mkTyForall vars t = mkAnn ("forall " <> child <> " . " <> child) (UTyForall (mkAnnList (listSep " ") vars) t)

mkTypeVar' :: GHC.Name -> Ann UTyVar dom SrcTemplateStage
mkTypeVar' = mkTypeVar . mkUnqualName'

mkTyCtx :: Ann UContext dom SrcTemplateStage -> Ann UType dom SrcTemplateStage -> Ann UType dom SrcTemplateStage
mkTyCtx ctx t = mkAnn (child <> " " <> child) (UTyCtx ctx t)

mkTyFun :: Ann UType dom SrcTemplateStage -> Ann UType dom SrcTemplateStage -> Ann UType dom SrcTemplateStage
mkTyFun at rt = mkAnn (child <> " -> " <> child) (UTyFun at rt)

mkTyTuple :: [Ann UType dom SrcTemplateStage] -> Ann UType dom SrcTemplateStage
mkTyTuple args = mkAnn ("(" <> child <> ")") (UTyTuple (mkAnnList (listSep ", ") args))

mkTyUnbTuple :: [Ann UType dom SrcTemplateStage] -> Ann UType dom SrcTemplateStage
mkTyUnbTuple args = mkAnn ("(#" <> child <> "#)") (UTyUnbTuple (mkAnnList (listSep ", ") args))

mkTyList :: Ann UType dom SrcTemplateStage -> Ann UType dom SrcTemplateStage
mkTyList = mkAnn ("[" <> child <> "]") . UTyList

mkTyParArray :: Ann UType dom SrcTemplateStage -> Ann UType dom SrcTemplateStage
mkTyParArray = mkAnn ("[:" <> child <> ":]") . UTyParArray

mkTyApp :: Ann UType dom SrcTemplateStage -> Ann UType dom SrcTemplateStage -> Ann UType dom SrcTemplateStage
mkTyApp ft at = mkAnn (child <> " " <> child) (UTyApp ft at)

mkTyInfix :: Ann UType dom SrcTemplateStage -> Ann UOperator dom SrcTemplateStage -> Ann UType dom SrcTemplateStage -> Ann UType dom SrcTemplateStage
mkTyInfix left op right = mkAnn (child <> " " <> child <> " " <> child) (UTyInfix left op right)
             
mkTyParen :: Ann UType dom SrcTemplateStage -> Ann UType dom SrcTemplateStage
mkTyParen = mkAnn ("(" <> child <> ")") . UTyParen
           
mkTypeVar :: Ann UName dom SrcTemplateStage -> Ann UTyVar dom SrcTemplateStage
mkTypeVar n = mkAnn (child <> child) (UTyVarDecl n noth)

mkTyVar :: Ann UName dom SrcTemplateStage -> Ann UType dom SrcTemplateStage
mkTyVar = wrapperAnn . UTyVar

mkTyKinded :: Ann UType dom SrcTemplateStage -> Ann UKind dom SrcTemplateStage -> Ann UType dom SrcTemplateStage
mkTyKinded t k = mkAnn (child <> " :: " <> child) (UTyKinded t k)

mkTyBang :: Ann UType dom SrcTemplateStage -> Ann UType dom SrcTemplateStage
mkTyBang = mkAnn ("!" <> child) . UTyBang

mkTyLazy :: Ann UType dom SrcTemplateStage -> Ann UType dom SrcTemplateStage
mkTyLazy = mkAnn ("~" <> child) . UTyLazy

mkTyUnpack :: Ann UType dom SrcTemplateStage -> Ann UType dom SrcTemplateStage
mkTyUnpack = mkAnn ("{-# UNPACK #-} " <> child) . UTyUnpack

mkTyWildcard :: Ann UType dom SrcTemplateStage
mkTyWildcard = mkAnn "_" UTyWildcard

mkTyNamedWildcard :: Ann UName dom SrcTemplateStage -> Ann UType dom SrcTemplateStage
mkTyNamedWildcard = mkAnn ("_" <> child) . UTyNamedWildc

-- * Generation of contexts

mkContextOne :: Ann UAssertion dom SrcTemplateStage -> Ann UContext dom SrcTemplateStage
mkContextOne = mkAnn (child <> " =>") . UContextOne

mkContextMulti :: [Ann UAssertion dom SrcTemplateStage] -> Ann UContext dom SrcTemplateStage
mkContextMulti = mkAnn ("(" <> child <> ") =>") . UContextMulti . mkAnnList (listSep ", ")

-- * Generation of assertions

mkClassAssert :: Ann UName dom SrcTemplateStage -> [Ann UType dom SrcTemplateStage] -> Ann UAssertion dom SrcTemplateStage
-- fixme: class assertion without parameters should not have the last space
mkClassAssert n args = mkAnn (child <> " " <> child) $ UClassAssert n (mkAnnList (listSep " ") args)

mkInfixAssert :: Ann UType dom SrcTemplateStage -> Ann UOperator dom SrcTemplateStage -> Ann UType dom SrcTemplateStage -> Ann UAssertion dom SrcTemplateStage
mkInfixAssert left op right = mkAnn (child <> " " <> child <> " " <> child) $ UInfixAssert left op right
