-- | UPattern matching on statement-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Stmts where

import Language.Haskell.Tools.AST

pattern BindStmt :: Ann UPattern dom stage -> Ann UExpr dom stage -> Ann UStmt dom stage
pattern BindStmt bound expr <- Ann _ (UBindStmt bound expr)

pattern ExprStmt :: Ann UExpr dom stage -> Ann UStmt dom stage
pattern ExprStmt expr <- Ann _ (UExprStmt expr)

pattern LetStmt :: AnnList ULocalBind dom stage -> Ann UStmt dom stage
pattern LetStmt binds <- Ann _ (ULetStmt binds)

pattern ListCompBody :: AnnList UCompStmt dom stage -> Ann UListCompBody dom stage
pattern ListCompBody stmts <- Ann _ (UListCompBody stmts)

pattern CompStmt :: Ann UStmt dom stage -> Ann UCompStmt dom stage
pattern CompStmt stmt <- Ann _ (UCompStmt stmt)

pattern DoKeyword :: Ann UDoKind dom stage
pattern DoKeyword <- Ann _ UDoKeyword

pattern MDoKeyword :: Ann UDoKind dom stage
pattern MDoKeyword <- Ann _ UMDoKeyword