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
import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.AST.Gen.Names
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

-- * Generation of types

mkTyForall :: [TyVar dom] -> Type dom -> Type dom
mkTyForall vars t = mkAnn ("forall " <> child <> " . " <> child) (UTyForall (mkAnnList (listSep " ") vars) t)

mkTypeVar' :: GHC.Name -> TyVar dom
mkTypeVar' = mkTypeVar . mkUnqualName'

mkTyCtx :: Context dom -> Type dom -> Type dom
mkTyCtx ctx t = mkAnn (child <> " " <> child) (UTyCtx ctx t)

mkTyFun :: Type dom -> Type dom -> Type dom
mkTyFun at rt = mkAnn (child <> " -> " <> child) (UTyFun at rt)

mkTyTuple :: [Type dom] -> Type dom
mkTyTuple args = mkAnn ("(" <> child <> ")") (UTyTuple (mkAnnList (listSep ", ") args))

mkTyUnbTuple :: [Type dom] -> Type dom
mkTyUnbTuple args = mkAnn ("(#" <> child <> "#)") (UTyUnbTuple (mkAnnList (listSep ", ") args))

mkTyList :: Type dom -> Type dom
mkTyList = mkAnn ("[" <> child <> "]") . UTyList

mkTyParArray :: Type dom -> Type dom
mkTyParArray = mkAnn ("[:" <> child <> ":]") . UTyParArray

mkTyApp :: Type dom -> Type dom -> Type dom
mkTyApp ft at = mkAnn (child <> " " <> child) (UTyApp ft at)

mkTyInfix :: Type dom -> Operator dom -> Type dom -> Type dom
mkTyInfix left op right = mkAnn (child <> " " <> child <> " " <> child) (UTyInfix left op right)
             
mkTyParen :: Type dom -> Type dom
mkTyParen = mkAnn ("(" <> child <> ")") . UTyParen
           
mkTypeVar :: Name dom -> TyVar dom
mkTypeVar n = mkAnn (child <> child) (UTyVarDecl n noth)

mkTyVar :: Name dom -> Type dom
mkTyVar = wrapperAnn . UTyVar

mkTyKinded :: Type dom -> Kind dom -> Type dom
mkTyKinded t k = mkAnn (child <> " :: " <> child) (UTyKinded t k)

mkTyBang :: Type dom -> Type dom
mkTyBang = mkAnn ("!" <> child) . UTyBang

mkTyLazy :: Type dom -> Type dom
mkTyLazy = mkAnn ("~" <> child) . UTyLazy

mkTyUnpack :: Type dom -> Type dom
mkTyUnpack = mkAnn ("{-# UNPACK #-} " <> child) . UTyUnpack

mkTyWildcard :: Type dom
mkTyWildcard = mkAnn "_" UTyWildcard

mkTyNamedWildcard :: Name dom -> Type dom
mkTyNamedWildcard = mkAnn ("_" <> child) . UTyNamedWildc

-- * Generation of contexts

mkContextOne :: Assertion dom -> Context dom
mkContextOne = mkAnn (child <> " =>") . UContextOne

mkContextMulti :: [Assertion dom] -> Context dom
mkContextMulti = mkAnn ("(" <> child <> ") =>") . UContextMulti . mkAnnList (listSep ", ")

-- * Generation of assertions

mkClassAssert :: Name dom -> [Type dom] -> Assertion dom
-- fixme: class assertion without parameters should not have the last space
mkClassAssert n args = mkAnn (child <> " " <> child) $ UClassAssert n (mkAnnList (listSep " ") args)

mkInfixAssert :: Type dom -> Operator dom -> Type dom -> Assertion dom
mkInfixAssert left op right = mkAnn (child <> " " <> child <> " " <> child) $ UInfixAssert left op right
