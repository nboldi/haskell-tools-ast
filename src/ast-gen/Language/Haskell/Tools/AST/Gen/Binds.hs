-- | Generation of binding-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkMatch@ creates the annotated version of the @UMatch@ constructor.
{-# LANGUAGE OverloadedStrings
           , TypeFamilies
           #-}
module Language.Haskell.Tools.AST.Gen.Binds where

import qualified Name as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.AST.Gen.Patterns
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST.Gen.Names
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkSimpleBind' :: Name dom -> Expr dom -> ValueBind dom
mkSimpleBind' n e = mkSimpleBind (mkVarPat n) (mkUnguardedRhs e) Nothing

mkSimpleBind :: Pattern dom -> Rhs dom -> Maybe (LocalBinds dom) -> ValueBind dom
mkSimpleBind p r l = mkAnn (child <> child <> child) (USimpleBind p r (mkAnnMaybe opt l))

mkFunctionBind :: [Match dom] -> ValueBind dom
mkFunctionBind = mkAnn child . UFunBind . mkAnnList indentedList

mkFunctionBind' :: Name dom -> [([Pattern dom], Expr dom)] -> ValueBind dom
mkFunctionBind' name matches = mkFunctionBind $ map (\(args, rhs) -> mkMatch (mkMatchLhs name args) (mkUnguardedRhs rhs) Nothing) matches

mkMatch :: MatchLhs dom -> Rhs dom -> Maybe (LocalBinds dom) -> Match dom
mkMatch lhs rhs locs 
  = mkAnn (child <> child <> child) 
      $ UMatch lhs rhs (mkAnnMaybe (optBefore " ") locs)

mkMatchLhs :: Name dom -> [Pattern dom] -> MatchLhs dom
mkMatchLhs n pats = mkAnn (child <> child) $ UNormalLhs n (mkAnnList (listSepBefore " " " ") pats)

mkInfixLhs :: Pattern dom -> Operator dom -> Pattern dom -> [Pattern dom] -> MatchLhs dom
mkInfixLhs lhs op rhs pats = mkAnn (child <> child <> child <> child) $ UInfixLhs lhs op rhs (mkAnnList (listSepBefore " " " ") pats)

mkLocalBinds :: Int -> [LocalBind dom] -> MaybeLocalBinds dom
mkLocalBinds col = mkAnnMaybe (optBefore ("\n" ++ replicate (col - 1) ' ' ++ "where ")) 
                     . Just . mkAnn child . ULocalBinds . mkAnnList indentedList

mkLocalBinds' :: [LocalBind dom] -> LocalBinds dom
mkLocalBinds' = mkAnn (" where " <> child) . ULocalBinds . mkAnnList indentedList

mkLocalValBind :: ValueBind dom -> LocalBind dom
mkLocalValBind = mkAnn child . ULocalValBind

mkLocalTypeSig :: TypeSignature dom -> LocalBind dom
mkLocalTypeSig = mkAnn child . ULocalSignature

mkLocalFixity :: FixitySignature dom -> LocalBind dom
mkLocalFixity = mkAnn child . ULocalFixity

mkTypeSignature :: Name dom -> Type dom -> TypeSignature dom
mkTypeSignature n t = mkAnn (child <> " :: " <> child) (UTypeSignature (mkAnnList (listSep ", ") [n]) t)

mkInfixL :: Int -> Operator dom -> FixitySignature dom
mkInfixL prec op = mkAnn (child <> " " <> child <> " " <> child) 
                     $ UFixitySignature (mkAnn "infixl" AssocLeft) (mkAnn (fromString (show prec)) (Precedence prec)) (mkAnnList (listSep ", ") [op])

mkInfixR :: Int -> Operator dom -> FixitySignature dom
mkInfixR prec op = mkAnn (child <> " " <> child <> " " <> child) 
                     $ UFixitySignature (mkAnn "infixr" AssocRight) (mkAnn (fromString (show prec)) (Precedence prec)) (mkAnnList (listSep ", ") [op])

mkInfix :: Int -> Operator dom -> FixitySignature dom
mkInfix prec op = mkAnn (child <> " " <> child <> " " <> child) 
                    $ UFixitySignature (mkAnn "infix" AssocNone) (mkAnn (fromString (show prec)) (Precedence prec)) (mkAnnList (listSep ", ") [op])

mkUnguardedRhs :: Expr dom -> Rhs dom
mkUnguardedRhs = mkAnn (" = " <> child) . UUnguardedRhs

mkGuardedRhss :: [GuardedRhs dom] -> Rhs dom
mkGuardedRhss = mkAnn child . UGuardedRhss . mkAnnList indentedList

mkGuardedRhs :: [RhsGuard dom] -> Expr dom -> GuardedRhs dom
mkGuardedRhs guards expr = mkAnn ("| " <> child <> " = " <> child) $ UGuardedRhs (mkAnnList (listSep ", ") guards) expr

mkGuardBind :: Pattern dom -> Expr dom -> RhsGuard dom
mkGuardBind pat expr = mkAnn (child <> " <- " <> child) $ UGuardBind pat expr

mkGuardLet :: [LocalBind dom] -> RhsGuard dom
mkGuardLet = mkAnn ("let " <> child) . UGuardLet . mkAnnList indentedList

mkGuardCheck :: Expr dom -> RhsGuard dom
mkGuardCheck = mkAnn child . UGuardCheck

-- pragmas are omitted
