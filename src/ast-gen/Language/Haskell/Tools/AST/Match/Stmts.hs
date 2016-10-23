-- | UPattern matching on statement-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Stmts where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes

pattern BindStmt :: Pattern dom -> Expr dom -> Stmt dom
pattern BindStmt bound expr <- Ann _ (UBindStmt bound expr)

pattern ExprStmt :: Expr dom -> Stmt dom
pattern ExprStmt expr <- Ann _ (UExprStmt expr)

pattern LetStmt :: LocalBindList dom -> Stmt dom
pattern LetStmt binds <- Ann _ (ULetStmt binds)

pattern ListCompBody :: CompStmtList dom -> ListCompBody dom
pattern ListCompBody stmts <- Ann _ (UListCompBody stmts)

pattern CompStmt :: Stmt dom -> CompStmt dom
pattern CompStmt stmt <- Ann _ (UCompStmt stmt)

pattern DoKeyword :: DoKind dom
pattern DoKeyword <- Ann _ UDoKeyword

pattern MDoKeyword :: DoKind dom
pattern MDoKeyword <- Ann _ UMDoKeyword