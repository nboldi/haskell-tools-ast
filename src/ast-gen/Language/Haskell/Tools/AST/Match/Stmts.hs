-- | Pattern matching on statement-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Stmts where

import Language.Haskell.Tools.AST

-- mkBindStmt :: Ann Pattern dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann Stmt dom SrcTemplateStage
-- mkBindStmt bound expr = mkAnn (child <> " <- " <> child) $ BindStmt bound expr

-- mkExprStmt :: Ann Expr dom SrcTemplateStage -> Ann Stmt dom SrcTemplateStage
-- mkExprStmt = mkAnn child . ExprStmt

-- mkLetStmt :: [Ann LocalBind dom SrcTemplateStage] -> Ann Stmt dom SrcTemplateStage
-- mkLetStmt = mkAnn ("let " <> child) . LetStmt . mkAnnList indentedList

-- mkListCompBody :: [Ann CompStmt dom SrcTemplateStage] -> Ann ListCompBody dom SrcTemplateStage
-- mkListCompBody = mkAnn child . ListCompBody . mkAnnList (listSep " ")

-- mkCompStmt :: Ann Stmt dom SrcTemplateStage -> Ann CompStmt dom SrcTemplateStage
-- mkCompStmt = mkAnn child . CompStmt
