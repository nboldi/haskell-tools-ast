-- | Generation of pattern-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkVarPat@ creates the annotated version of the @VarPat@ AST constructor.
{-# LANGUAGE OverloadedStrings 
           , TypeFamilies
           #-}
module Language.Haskell.Tools.AST.Gen.Patterns where

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

mkVarPat :: Ann UName dom SrcTemplateStage -> Ann UPattern dom SrcTemplateStage
mkVarPat = mkAnn child . UVarPat

mkLitPat :: Ann ULiteral dom SrcTemplateStage -> Ann UPattern dom SrcTemplateStage
mkLitPat = mkAnn child . ULitPat

mkInfixAppPat :: Ann UPattern dom SrcTemplateStage -> Ann UOperator dom SrcTemplateStage -> Ann UPattern dom SrcTemplateStage -> Ann UPattern dom SrcTemplateStage
mkInfixAppPat lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ UInfixAppPat lhs op rhs

mkAppPat :: Ann UName dom SrcTemplateStage -> [Ann UPattern dom SrcTemplateStage] -> Ann UPattern dom SrcTemplateStage
mkAppPat n pat = mkAnn (child <> child) $ UAppPat n (mkAnnList (listSepBefore " " " ") pat)

mkTuplePat :: [Ann UPattern dom SrcTemplateStage] -> Ann UPattern dom SrcTemplateStage
mkTuplePat pats = mkAnn ("(" <> child <> ")") $ UTuplePat (mkAnnList (listSep ", ") pats)

mkUnboxTuplePat :: [Ann UPattern dom SrcTemplateStage] -> Ann UPattern dom SrcTemplateStage
mkUnboxTuplePat pats = mkAnn ("(# " <> child <> " #)") $ UUnboxTuplePat (mkAnnList (listSep ", ") pats)

mkListPat :: [Ann UPattern dom SrcTemplateStage] -> Ann UPattern dom SrcTemplateStage
mkListPat pats = mkAnn ("[" <> child <> "]") $ UListPat (mkAnnList (listSep ", ") pats)

mkParenPat :: Ann UPattern dom SrcTemplateStage -> Ann UPattern dom SrcTemplateStage
mkParenPat = mkAnn ("(" <> child <> ")") . UParenPat

mkRecPat :: Ann UName dom SrcTemplateStage -> [Ann UPatternField dom SrcTemplateStage] -> Ann UPattern dom SrcTemplateStage
mkRecPat name flds = mkAnn (child <> "{ " <> child <> " }") $ URecPat name (mkAnnList (listSep ", ") flds)

mkAsPat :: Ann UName dom SrcTemplateStage -> Ann UPattern dom SrcTemplateStage -> Ann UPattern dom SrcTemplateStage
mkAsPat name pat = mkAnn (child <> "@" <> child) $ UAsPat name pat

mkWildPat :: Ann UPattern dom SrcTemplateStage
mkWildPat = mkAnn "_" UWildPat

mkIrrefutablePat :: Ann UPattern dom SrcTemplateStage -> Ann UPattern dom SrcTemplateStage
mkIrrefutablePat = mkAnn ("~" <> child) . UIrrefutablePat

mkBangPat :: Ann UPattern dom SrcTemplateStage -> Ann UPattern dom SrcTemplateStage
mkBangPat = mkAnn ("!" <> child) . UBangPat

mkTypeSigPat :: Ann UPattern dom SrcTemplateStage -> Ann UType dom SrcTemplateStage -> Ann UPattern dom SrcTemplateStage
mkTypeSigPat pat typ = mkAnn (child <> " :: " <> child) $ UTypeSigPat pat typ

mkViewPat :: Ann UExpr dom SrcTemplateStage -> Ann UPattern dom SrcTemplateStage -> Ann UPattern dom SrcTemplateStage
mkViewPat name pat = mkAnn (child <> " -> " <> child) $ UViewPat name pat

mkPatternField :: Ann UName dom SrcTemplateStage -> Ann UPattern dom SrcTemplateStage -> Ann UPatternField dom SrcTemplateStage
mkPatternField name pat = mkAnn (child <> " = " <> child) $ UNormalFieldPattern name pat
