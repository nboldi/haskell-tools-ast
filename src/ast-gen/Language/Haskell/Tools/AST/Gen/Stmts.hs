-- | Generation of statement-level AST fragments for refactorings.
-- The bindings defined here are the AST constructor names with an "mk" prefix.
{-# LANGUAGE OverloadedStrings 
           , TypeFamilies 
           #-}
module Language.Haskell.Tools.AST.Gen.Stmts where

import qualified Name as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST.Gen.Base
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkBindStmt :: Ann UPattern dom SrcTemplateStage -> Ann UExpr dom SrcTemplateStage -> Ann UStmt dom SrcTemplateStage
mkBindStmt bound expr = mkAnn (child <> " <- " <> child) $ UBindStmt bound expr

mkExprStmt :: Ann UExpr dom SrcTemplateStage -> Ann UStmt dom SrcTemplateStage
mkExprStmt = mkAnn child . UExprStmt

mkLetStmt :: [Ann ULocalBind dom SrcTemplateStage] -> Ann UStmt dom SrcTemplateStage
mkLetStmt = mkAnn ("let " <> child) . ULetStmt . mkAnnList indentedList

mkListCompBody :: [Ann UCompStmt dom SrcTemplateStage] -> Ann UListCompBody dom SrcTemplateStage
mkListCompBody = mkAnn child . UListCompBody . mkAnnList (listSep " ")

mkCompStmt :: Ann UStmt dom SrcTemplateStage -> Ann UCompStmt dom SrcTemplateStage
mkCompStmt = mkAnn child . UCompStmt
