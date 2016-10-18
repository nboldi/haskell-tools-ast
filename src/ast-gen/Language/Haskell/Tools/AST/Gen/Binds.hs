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
import Language.Haskell.Tools.AST.Gen.Patterns
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST.Gen.Base
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkSimpleBind' :: Ann UName dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann UValueBind dom SrcTemplateStage
mkSimpleBind' n e = mkSimpleBind (mkVarPat n) (mkUnguardedRhs e) Nothing

mkSimpleBind :: Ann Pattern dom SrcTemplateStage -> Ann URhs dom SrcTemplateStage -> Maybe (Ann ULocalBinds dom SrcTemplateStage) -> Ann UValueBind dom SrcTemplateStage
mkSimpleBind p r l = mkAnn (child <> child <> child) (USimpleBind p r (mkAnnMaybe opt l))

mkFunctionBind :: [Ann UMatch dom SrcTemplateStage] -> Ann UValueBind dom SrcTemplateStage
mkFunctionBind = mkAnn child . UFunBind . mkAnnList indentedList

mkFunctionBind' :: Ann UName dom SrcTemplateStage -> [([Ann Pattern dom SrcTemplateStage], Ann Expr dom SrcTemplateStage)] -> Ann UValueBind dom SrcTemplateStage
mkFunctionBind' name matches = mkFunctionBind $ map (\(args, rhs) -> mkMatch (mkMatchLhs name args) (mkUnguardedRhs rhs) Nothing) matches

mkMatch :: Ann UMatchLhs dom SrcTemplateStage -> Ann URhs dom SrcTemplateStage -> Maybe (Ann ULocalBinds dom SrcTemplateStage) -> Ann UMatch dom SrcTemplateStage
mkMatch lhs rhs locs 
  = mkAnn (child <> child <> child) 
      $ UMatch lhs rhs (mkAnnMaybe (optBefore " ") locs)

mkMatchLhs :: Ann UName dom SrcTemplateStage -> [Ann Pattern dom SrcTemplateStage] -> Ann UMatchLhs dom SrcTemplateStage
mkMatchLhs n pats = mkAnn (child <> child) $ UNormalLhs n (mkAnnList (listSepBefore " " " ") pats)

mkInfixLhs :: Ann Pattern dom SrcTemplateStage -> Ann UOperator dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage 
                -> [Ann Pattern dom SrcTemplateStage] -> Ann UMatchLhs dom SrcTemplateStage
mkInfixLhs lhs op rhs pats = mkAnn (child <> child <> child <> child) $ UInfixLhs lhs op rhs (mkAnnList (listSepBefore " " " ") pats)

mkLocalBinds :: Int -> [Ann ULocalBind dom SrcTemplateStage] -> AnnMaybe ULocalBinds dom SrcTemplateStage
mkLocalBinds col = mkAnnMaybe (optBefore ("\n" ++ replicate (col - 1) ' ' ++ "where ")) 
                     . Just . mkAnn child . ULocalBinds . mkAnnList indentedList

mkLocalBinds' :: [Ann ULocalBind dom SrcTemplateStage] -> Ann ULocalBinds dom SrcTemplateStage
mkLocalBinds' = mkAnn (" where " <> child) . ULocalBinds . mkAnnList indentedList

mkLocalValBind :: Ann UValueBind dom SrcTemplateStage -> Ann ULocalBind dom SrcTemplateStage
mkLocalValBind = mkAnn child . ULocalValBind

mkLocalTypeSig :: Ann UTypeSignature dom SrcTemplateStage -> Ann ULocalBind dom SrcTemplateStage
mkLocalTypeSig = mkAnn child . ULocalSignature

mkLocalFixity :: Ann UFixitySignature dom SrcTemplateStage -> Ann ULocalBind dom SrcTemplateStage
mkLocalFixity = mkAnn child . ULocalFixity

mkTypeSignature :: Ann UName dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann UTypeSignature dom SrcTemplateStage
mkTypeSignature n t = mkAnn (child <> " :: " <> child) (UTypeSignature (mkAnnList (listSep ", ") [n]) t)

mkInfixL :: Int -> Ann UOperator dom SrcTemplateStage -> Ann UFixitySignature dom SrcTemplateStage
mkInfixL prec op = mkAnn (child <> " " <> child <> " " <> child) 
                     $ UFixitySignature (mkAnn "infixl" AssocLeft) (mkAnn (fromString (show prec)) (Precedence prec)) (mkAnnList (listSep ", ") [op])

mkInfixR :: Int -> Ann UOperator dom SrcTemplateStage -> Ann UFixitySignature dom SrcTemplateStage
mkInfixR prec op = mkAnn (child <> " " <> child <> " " <> child) 
                     $ UFixitySignature (mkAnn "infixr" AssocRight) (mkAnn (fromString (show prec)) (Precedence prec)) (mkAnnList (listSep ", ") [op])

mkInfix :: Int -> Ann UOperator dom SrcTemplateStage -> Ann UFixitySignature dom SrcTemplateStage
mkInfix prec op = mkAnn (child <> " " <> child <> " " <> child) 
                    $ UFixitySignature (mkAnn "infix" AssocNone) (mkAnn (fromString (show prec)) (Precedence prec)) (mkAnnList (listSep ", ") [op])

mkUnguardedRhs :: Ann Expr dom SrcTemplateStage -> Ann URhs dom SrcTemplateStage
mkUnguardedRhs = mkAnn (" = " <> child) . UUnguardedRhs

mkGuardedRhss :: [Ann UGuardedRhs dom SrcTemplateStage] -> Ann URhs dom SrcTemplateStage
mkGuardedRhss = mkAnn child . UGuardedRhss . mkAnnList indentedList

mkGuardedRhs :: [Ann URhsGuard dom SrcTemplateStage] -> Ann Expr dom SrcTemplateStage -> Ann UGuardedRhs dom SrcTemplateStage
mkGuardedRhs guards expr = mkAnn ("| " <> child <> " = " <> child) $ UGuardedRhs (mkAnnList (listSep ", ") guards) expr

mkGuardBind :: Ann Pattern dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann URhsGuard dom SrcTemplateStage
mkGuardBind pat expr = mkAnn (child <> " <- " <> child) $ UGuardBind pat expr

mkGuardLet :: [Ann ULocalBind dom SrcTemplateStage] -> Ann URhsGuard dom SrcTemplateStage
mkGuardLet = mkAnn ("let " <> child) . UGuardLet . mkAnnList indentedList

mkGuardCheck :: Ann Expr dom SrcTemplateStage -> Ann URhsGuard dom SrcTemplateStage
mkGuardCheck = mkAnn child . UGuardCheck

-- pragmas are omitted
