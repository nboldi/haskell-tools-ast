-- | Generation of expression-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkApp@ creates the annotated version of the @App@ AST constructor.
{-# LANGUAGE OverloadedStrings 
           , TypeFamilies
           #-}
module Language.Haskell.Tools.AST.Gen.Exprs where

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

-- * Expressions

mkVar :: Name dom -> Expr dom
mkVar = mkAnn child . UVar

mkLit :: Literal dom -> Expr dom
mkLit = mkAnn child . ULit

mkInfixApp :: Expr dom -> Operator dom -> Expr dom -> Expr dom
mkInfixApp lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ UInfixApp lhs op rhs

mkPrefixApp :: Operator dom -> Expr dom -> Expr dom
mkPrefixApp op rhs = mkAnn (child <> child) $ UPrefixApp op rhs

mkApp :: Expr dom -> Expr dom -> Expr dom
mkApp f e = mkAnn (child <> " " <> child) (UApp f e)

mkLambda :: [Pattern dom] -> Expr dom -> Expr dom
mkLambda pats rhs = mkAnn ("\\" <> child <> " -> " <> child) $ ULambda (mkAnnList (listSep " ") pats) rhs

mkLet :: [LocalBind dom] -> Expr dom -> Expr dom
mkLet pats expr = mkAnn ("let " <> child <> " in " <> child) $ ULet (mkAnnList indentedList pats) expr

mkIf :: Expr dom -> Expr dom -> Expr dom -> Expr dom
mkIf cond then_ else_ = mkAnn ("if " <> child <> " then " <> child <> " else " <> child) $ UIf cond then_ else_

mkMultiIf :: [GuardedCaseRhs dom] -> Expr dom
mkMultiIf cases = mkAnn ("if" <> child) $ UMultiIf (mkAnnList indentedList cases)

mkCase :: Expr dom -> [Alt dom] -> Expr dom
mkCase expr cases = mkAnn ("case " <> child <> " of " <> child) $ UCase expr (mkAnnList indentedList cases)

mkDoBlock :: [Stmt dom] -> Expr dom
mkDoBlock stmts = mkAnn (child <> " " <> child) $ UDo (mkAnn "do" UDoKeyword) (mkAnnList indentedList stmts)

mkTuple :: [Expr dom] -> Expr dom
mkTuple exprs = mkAnn ("(" <> child <> ")") $ UTuple (mkAnnList (listSep ", ") exprs)

mkUnboxedTuple :: [Expr dom] -> Expr dom
mkUnboxedTuple exprs = mkAnn ("(# " <> child <> " #)") $ UTuple (mkAnnList (listSep ", ") exprs)

mkList :: [Expr dom] -> Expr dom
mkList exprs = mkAnn ("[" <> child <> "]") $ UList (mkAnnList (listSep ", ") exprs)

mkParen :: Expr dom -> Expr dom
mkParen = mkAnn ("(" <> child <> ")") . UParen

mkLeftSection :: Expr dom -> Operator dom -> Expr dom
mkLeftSection lhs op = mkAnn ("(" <> child <> child <> ")") $ ULeftSection lhs op

mkRightSection :: Operator dom -> Expr dom -> Expr dom
mkRightSection op rhs = mkAnn ("(" <> child <> child <> ")") $ URightSection op rhs

mkRecCon :: Name dom -> [FieldUpdate dom] -> Expr dom
mkRecCon name flds = mkAnn (child <> " { " <> child <> " }") $ URecCon name (mkAnnList (listSep ", ") flds)

mkRecUpdate :: Expr dom -> [FieldUpdate dom] -> Expr dom
mkRecUpdate expr flds = mkAnn (child <> " { " <> child <> " }") $ URecUpdate expr (mkAnnList (listSep ", ") flds)

mkEnum :: Expr dom -> Maybe (Expr dom) -> Maybe (Expr dom) -> Expr dom
mkEnum from step to = mkAnn ("[" <> child <> child <> ".." <> child <> "]") $ UEnum from (mkAnnMaybe (optBefore ",") step) (mkAnnMaybe (optBefore ",") to)

mkExprTypeSig :: Expr dom -> Type dom -> Expr dom
mkExprTypeSig lhs typ = mkAnn (child <> " :: " <> child) $ UExplTypeApp lhs typ

-- * Field updates

mkFieldUpdate :: Name dom -> Expr dom -> FieldUpdate dom
mkFieldUpdate name val = mkAnn (child <> " = " <> child) $ UNormalFieldUpdate name val

mkFieldPun :: Name dom -> FieldUpdate dom
mkFieldPun name = mkAnn child $ UFieldPun name

mkFieldWildcard :: FieldWildcard dom -> FieldUpdate dom
mkFieldWildcard name = mkAnn child $ UFieldWildcard name


-- * UPattern matching and guards

mkAlt :: Pattern dom -> CaseRhs dom -> Maybe (LocalBinds dom) -> Alt dom
mkAlt pat rhs locals = mkAnn (child <> child <> child) $ UAlt pat rhs (mkAnnMaybe (optBefore " where ") locals)

mkCaseRhs :: Expr dom -> CaseRhs dom
mkCaseRhs = mkAnn (" -> " <> child) . UUnguardedCaseRhs

mkGuardedCaseRhss :: [GuardedCaseRhs dom] -> CaseRhs dom
mkGuardedCaseRhss = mkAnn child . UGuardedCaseRhss . mkAnnList indentedList

mkGuardedCaseRhs :: [RhsGuard dom] -> Expr dom -> GuardedCaseRhs dom
mkGuardedCaseRhs guards expr = mkAnn (" | " <> child <> " -> " <> child) $ UGuardedCaseRhs (mkAnnList (listSep ", ") guards) expr


