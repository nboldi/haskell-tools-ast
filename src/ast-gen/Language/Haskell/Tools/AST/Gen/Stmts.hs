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
import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST.Gen.Names
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkBindStmt :: Pattern dom -> Expr dom -> Stmt dom
mkBindStmt bound expr = mkAnn (child <> " <- " <> child) $ UBindStmt bound expr

mkExprStmt :: Expr dom -> Stmt dom
mkExprStmt = mkAnn child . UExprStmt

mkLetStmt :: [LocalBind dom] -> Stmt dom
mkLetStmt = mkAnn ("let " <> child) . ULetStmt . mkAnnList indentedList

mkListCompBody :: [CompStmt dom] -> ListCompBody dom
mkListCompBody = mkAnn child . UListCompBody . mkAnnList (listSep " ")

mkCompStmt :: Stmt dom -> CompStmt dom
mkCompStmt = mkAnn child . UCompStmt
