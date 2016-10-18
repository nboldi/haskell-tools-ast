-- | Pattern matching on type-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms
           #-}
module Language.Haskell.Tools.AST.Match.Types where

import Language.Haskell.Tools.AST

-- * Types

pattern TyForall :: AnnList TyVar dom stage -> Ann Type dom stage -> Ann Type dom stage
pattern TyForall vars t <- Ann _ (UTyForall vars t)

pattern TyCtx :: Ann Context dom stage -> Ann Type dom stage -> Ann Type dom stage
pattern TyCtx ctx t <- Ann _ (UTyCtx ctx t)

pattern TyFun :: Ann Type dom stage -> Ann Type dom stage -> Ann Type dom stage
pattern TyFun at rt <- Ann _ (UTyFun at rt)

pattern TyTuple :: AnnList Type dom stage -> Ann Type dom stage
pattern TyTuple args <- Ann _ (UTyTuple args)

pattern TyUnbTuple :: AnnList Type dom stage -> Ann Type dom stage
pattern TyUnbTuple args <- Ann _ (UTyUnbTuple args)

pattern TyList :: Ann Type dom stage -> Ann Type dom stage
pattern TyList t <- Ann _ (UTyList t)

pattern TyParArray :: Ann Type dom stage -> Ann Type dom stage
pattern TyParArray t <- Ann _ (UTyParArray t)

pattern TyApp :: Ann Type dom stage -> Ann Type dom stage -> Ann Type dom stage
pattern TyApp ft at <- Ann _ (UTyApp ft at)

pattern TyInfix :: Ann Type dom stage -> Ann UOperator dom stage -> Ann Type dom stage -> Ann Type dom stage
pattern TyInfix left op right <- Ann _ (UTyInfix left op right)

pattern TyParen :: Ann Type dom stage -> Ann Type dom stage
pattern TyParen t <- Ann _ (UTyParen t)

pattern TyVar :: Ann UName dom stage -> Ann Type dom stage
pattern TyVar n <- Ann _ (UTyVar n)

pattern TyKinded :: Ann Type dom stage -> Ann Kind dom stage -> Ann Type dom stage
pattern TyKinded t k <- Ann _ (UTyKinded t k)

pattern TyBang :: Ann Type dom stage -> Ann Type dom stage
pattern TyBang n <- Ann _ (UTyBang n)

pattern TyLazy :: Ann Type dom stage -> Ann Type dom stage
pattern TyLazy n <- Ann _ (UTyLazy n)

pattern TyUnpack :: Ann Type dom stage -> Ann Type dom stage
pattern TyUnpack n <- Ann _ (UTyUnpack n)

pattern TyWildcard :: Ann Type dom stage
pattern TyWildcard <- Ann _ UTyWildcard

pattern TyNamedWildcard :: Ann UName dom stage -> Ann Type dom stage
pattern TyNamedWildcard n <- Ann _ (UTyNamedWildc n)

-- * Type variable

pattern TyVarDecl :: Ann UName dom stage -> Ann TyVar dom stage
pattern TyVarDecl n <- Ann _ (UTyVarDecl n _)

-- * Contexts

pattern ContextOne :: Ann Assertion dom stage -> Ann Context dom stage
pattern ContextOne n <- Ann _ (UContextOne n)

pattern ContextMulti :: AnnList Assertion dom stage -> Ann Context dom stage
pattern ContextMulti n <- Ann _ (UContextMulti n)

-- * Assertions

pattern ClassAssert :: Ann UName dom stage -> AnnList Type dom stage -> Ann Assertion dom stage
pattern ClassAssert n args <- Ann _ (UClassAssert n args)

pattern InfixAssert :: Ann Type dom stage -> Ann UOperator dom stage -> Ann Type dom stage -> Ann Assertion dom stage
pattern InfixAssert left op right <- Ann _ (UInfixAssert left op right)
