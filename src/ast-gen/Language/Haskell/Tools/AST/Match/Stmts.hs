-- | Pattern matching on statement-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Stmts where

import Language.Haskell.Tools.AST

pattern BindStmt :: Ann Pattern dom stage -> Ann UExpr dom stage -> Ann Stmt dom stage
pattern BindStmt bound expr <- Ann _ (UBindStmt bound expr)

pattern ExprStmt :: Ann UExpr dom stage -> Ann Stmt dom stage
pattern ExprStmt expr <- Ann _ (UExprStmt expr)

pattern LetStmt :: AnnList ULocalBind dom stage -> Ann Stmt dom stage
pattern LetStmt binds <- Ann _ (ULetStmt binds)

pattern ListCompBody :: AnnList CompStmt dom stage -> Ann ListCompBody dom stage
pattern ListCompBody stmts <- Ann _ (UListCompBody stmts)

pattern CompStmt :: Ann Stmt dom stage -> Ann CompStmt dom stage
pattern CompStmt stmt <- Ann _ (UCompStmt stmt)