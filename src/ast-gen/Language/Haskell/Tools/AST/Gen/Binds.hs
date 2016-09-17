-- | Generation of binding-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkMatch@ creates the annotated version of the @Match@ constructor.
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
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST.Gen.Base
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkSimpleBind :: Ann Pattern dom SrcTemplateStage -> Ann Rhs dom SrcTemplateStage -> Maybe (Ann LocalBinds dom SrcTemplateStage) -> Ann ValueBind dom SrcTemplateStage
mkSimpleBind p r l = mkAnn (child <> child <> child) (SimpleBind p r (mkAnnMaybe (optBefore " ") l))

mkFunctionBind :: [Ann Match dom SrcTemplateStage] -> Ann ValueBind dom SrcTemplateStage
mkFunctionBind = mkAnn child . FunBind . mkAnnList indentedList

mkMatch :: Ann MatchLhs dom SrcTemplateStage -> Ann Rhs dom SrcTemplateStage -> Maybe (Ann LocalBinds dom SrcTemplateStage) -> Ann Match dom SrcTemplateStage
mkMatch lhs rhs locs 
  = mkAnn (child <> child <> child) 
      $ Match lhs rhs (mkAnnMaybe (optBefore " ") locs)

mkNormalMatchLhs :: Ann Name dom SrcTemplateStage -> [Ann Pattern dom SrcTemplateStage] -> Ann MatchLhs dom SrcTemplateStage
mkNormalMatchLhs n pats = mkAnn (child <> child) $ NormalLhs n (mkAnnList (listSepBefore " " " ") pats)

mkInfixMatchLhs :: Ann Pattern dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage 
                                                    -> [Ann Pattern dom SrcTemplateStage] -> Ann MatchLhs dom SrcTemplateStage
mkInfixMatchLhs lhs op rhs pats = mkAnn (child <> child <> child <> child) $ InfixLhs lhs op rhs (mkAnnList (listSepBefore " " " ") pats)

mkLocalBinds :: Int -> [Ann LocalBind dom SrcTemplateStage] -> AnnMaybe LocalBinds dom SrcTemplateStage
mkLocalBinds col = mkAnnMaybe (optBefore ("\n" ++ replicate (col - 1) ' ' ++ "where ")) 
                     . Just . mkAnn child . LocalBinds . mkAnnList indentedList

mkLocalValBind :: Ann ValueBind dom SrcTemplateStage -> Ann LocalBind dom SrcTemplateStage
mkLocalValBind = mkAnn child . LocalValBind

mkLocalTypeSig :: Ann TypeSignature dom SrcTemplateStage -> Ann LocalBind dom SrcTemplateStage
mkLocalTypeSig = mkAnn child . LocalSignature

mkLocalFixity :: Ann FixitySignature dom SrcTemplateStage -> Ann LocalBind dom SrcTemplateStage
mkLocalFixity = mkAnn child . LocalFixity

mkTypeSignature :: Ann Name dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann TypeSignature dom SrcTemplateStage
mkTypeSignature n t = mkAnn (child <> " :: " <> child) (TypeSignature (mkAnnList (listSep ", ") [n]) t)

mkInfixL :: Int -> Ann Operator dom SrcTemplateStage -> Ann FixitySignature dom SrcTemplateStage
mkInfixL prec op = mkAnn (child <> " " <> child <> " " <> child) 
                     $ FixitySignature (mkAnn "infixl" AssocLeft) (mkAnn (fromString (show prec)) (Precedence prec)) (mkAnnList (listSep ", ") [op])

mkInfixR :: Int -> Ann Operator dom SrcTemplateStage -> Ann FixitySignature dom SrcTemplateStage
mkInfixR prec op = mkAnn (child <> " " <> child <> " " <> child) 
                     $ FixitySignature (mkAnn "infixr" AssocRight) (mkAnn (fromString (show prec)) (Precedence prec)) (mkAnnList (listSep ", ") [op])

mkInfix :: Int -> Ann Operator dom SrcTemplateStage -> Ann FixitySignature dom SrcTemplateStage
mkInfix prec op = mkAnn (child <> " " <> child <> " " <> child) 
                    $ FixitySignature (mkAnn "infix" AssocNone) (mkAnn (fromString (show prec)) (Precedence prec)) (mkAnnList (listSep ", ") [op])

mkUnguardedRhs :: Ann Expr dom SrcTemplateStage -> Ann Rhs dom SrcTemplateStage
mkUnguardedRhs = mkAnn (" = " <> child) . UnguardedRhs

mkGuardedRhss :: [Ann GuardedRhs dom SrcTemplateStage] -> Ann Rhs dom SrcTemplateStage
mkGuardedRhss = mkAnn child . GuardedRhss . mkAnnList indentedList

mkGuardedRhs :: [Ann RhsGuard dom SrcTemplateStage] -> Ann Expr dom SrcTemplateStage -> Ann GuardedRhs dom SrcTemplateStage
mkGuardedRhs guards expr = mkAnn ("| " <> child <> child) $ GuardedRhs (mkAnnList (listSep ", ") guards) expr

mkGuardBind :: Ann Pattern dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage -> Ann RhsGuard dom SrcTemplateStage
mkGuardBind pat expr = mkAnn (child <> " <- " <> child) $ GuardBind pat expr

mkGuardLet :: [Ann LocalBind dom SrcTemplateStage] -> Ann RhsGuard dom SrcTemplateStage
mkGuardLet = mkAnn ("let " <> child) . GuardLet . mkAnnList indentedList

mkGuardCheck :: Ann Expr dom SrcTemplateStage -> Ann RhsGuard dom SrcTemplateStage
mkGuardCheck = mkAnn child . GuardCheck

-- pragmas are omitted
