-- | UPattern matching on type-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms
           #-}
module Language.Haskell.Tools.AST.Match.Types where

import Language.Haskell.Tools.AST

-- * Types

pattern TyForall :: AnnListG UTyVar dom stage -> Ann UType dom stage -> Ann UType dom stage
pattern TyForall vars t <- Ann _ (UTyForall vars t)

pattern TyCtx :: Ann UContext dom stage -> Ann UType dom stage -> Ann UType dom stage
pattern TyCtx ctx t <- Ann _ (UTyCtx ctx t)

pattern TyFun :: Ann UType dom stage -> Ann UType dom stage -> Ann UType dom stage
pattern TyFun at rt <- Ann _ (UTyFun at rt)

pattern TyTuple :: AnnListG UType dom stage -> Ann UType dom stage
pattern TyTuple args <- Ann _ (UTyTuple args)

pattern TyUnbTuple :: AnnListG UType dom stage -> Ann UType dom stage
pattern TyUnbTuple args <- Ann _ (UTyUnbTuple args)

pattern TyList :: Ann UType dom stage -> Ann UType dom stage
pattern TyList t <- Ann _ (UTyList t)

pattern TyParArray :: Ann UType dom stage -> Ann UType dom stage
pattern TyParArray t <- Ann _ (UTyParArray t)

pattern TyApp :: Ann UType dom stage -> Ann UType dom stage -> Ann UType dom stage
pattern TyApp ft at <- Ann _ (UTyApp ft at)

pattern TyInfix :: Ann UType dom stage -> Ann UOperator dom stage -> Ann UType dom stage -> Ann UType dom stage
pattern TyInfix left op right <- Ann _ (UTyInfix left op right)

pattern TyParen :: Ann UType dom stage -> Ann UType dom stage
pattern TyParen t <- Ann _ (UTyParen t)

pattern TyVar :: Ann UName dom stage -> Ann UType dom stage
pattern TyVar n <- Ann _ (UTyVar n)

pattern TyKinded :: Ann UType dom stage -> Ann UKind dom stage -> Ann UType dom stage
pattern TyKinded t k <- Ann _ (UTyKinded t k)

pattern TyBang :: Ann UType dom stage -> Ann UType dom stage
pattern TyBang n <- Ann _ (UTyBang n)

pattern TyLazy :: Ann UType dom stage -> Ann UType dom stage
pattern TyLazy n <- Ann _ (UTyLazy n)

pattern TyUnpack :: Ann UType dom stage -> Ann UType dom stage
pattern TyUnpack n <- Ann _ (UTyUnpack n)

pattern TyWildcard :: Ann UType dom stage
pattern TyWildcard <- Ann _ UTyWildcard

pattern TyNamedWildcard :: Ann UName dom stage -> Ann UType dom stage
pattern TyNamedWildcard n <- Ann _ (UTyNamedWildc n)

-- * UType variable

pattern TyVarDecl :: Ann UName dom stage -> Ann UTyVar dom stage
pattern TyVarDecl n <- Ann _ (UTyVarDecl n _)

-- * Contexts

pattern ContextOne :: Ann UAssertion dom stage -> Ann UContext dom stage
pattern ContextOne n <- Ann _ (UContextOne n)

pattern ContextMulti :: AnnListG UAssertion dom stage -> Ann UContext dom stage
pattern ContextMulti n <- Ann _ (UContextMulti n)

-- * Assertions

pattern ClassAssert :: Ann UName dom stage -> AnnListG UType dom stage -> Ann UAssertion dom stage
pattern ClassAssert n args <- Ann _ (UClassAssert n args)

pattern InfixAssert :: Ann UType dom stage -> Ann UOperator dom stage -> Ann UType dom stage -> Ann UAssertion dom stage
pattern InfixAssert left op right <- Ann _ (UInfixAssert left op right)
