-- | UPattern matching on type-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms
           #-}
module Language.Haskell.Tools.AST.Match.Types where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes

-- * Types

pattern TyForall :: TyVarList dom -> Type dom -> Type dom
pattern TyForall vars t <- Ann _ (UTyForall vars t)

pattern TyCtx :: Context dom -> Type dom -> Type dom
pattern TyCtx ctx t <- Ann _ (UTyCtx ctx t)

pattern TyFun :: Type dom -> Type dom -> Type dom
pattern TyFun at rt <- Ann _ (UTyFun at rt)

pattern TyTuple :: TypeList dom -> Type dom
pattern TyTuple args <- Ann _ (UTyTuple args)

pattern TyUnbTuple :: TypeList dom -> Type dom
pattern TyUnbTuple args <- Ann _ (UTyUnbTuple args)

pattern TyList :: Type dom -> Type dom
pattern TyList t <- Ann _ (UTyList t)

pattern TyParArray :: Type dom -> Type dom
pattern TyParArray t <- Ann _ (UTyParArray t)

pattern TyApp :: Type dom -> Type dom -> Type dom
pattern TyApp ft at <- Ann _ (UTyApp ft at)

pattern TyInfix :: Type dom -> Operator dom -> Type dom -> Type dom
pattern TyInfix left op right <- Ann _ (UTyInfix left op right)

pattern TyParen :: Type dom -> Type dom
pattern TyParen t <- Ann _ (UTyParen t)

pattern TyVar :: Name dom -> Type dom
pattern TyVar n <- Ann _ (UTyVar n)

pattern TyKinded :: Type dom -> Kind dom -> Type dom
pattern TyKinded t k <- Ann _ (UTyKinded t k)

pattern TyBang :: Type dom -> Type dom
pattern TyBang n <- Ann _ (UTyBang n)

pattern TyLazy :: Type dom -> Type dom
pattern TyLazy n <- Ann _ (UTyLazy n)

pattern TyUnpack :: Type dom -> Type dom
pattern TyUnpack n <- Ann _ (UTyUnpack n)

pattern TyWildcard :: Type dom
pattern TyWildcard <- Ann _ UTyWildcard

pattern TyNamedWildcard :: Name dom -> Type dom
pattern TyNamedWildcard n <- Ann _ (UTyNamedWildc n)

-- * UType variable

pattern TyVarDecl :: Name dom -> TyVar dom
pattern TyVarDecl n <- Ann _ (UTyVarDecl n _)

-- * Contexts

pattern ContextOne :: Assertion dom -> Context dom
pattern ContextOne n <- Ann _ (UContextOne n)

pattern ContextMulti :: AssertionList dom -> Context dom
pattern ContextMulti n <- Ann _ (UContextMulti n)

-- * Assertions

pattern ClassAssert :: Name dom -> TypeList dom -> Assertion dom
pattern ClassAssert n args <- Ann _ (UClassAssert n args)

pattern InfixAssert :: Type dom -> Operator dom -> Type dom -> Assertion dom
pattern InfixAssert left op right <- Ann _ (UInfixAssert left op right)
