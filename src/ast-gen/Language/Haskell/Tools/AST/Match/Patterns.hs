-- | Pattern matching on pattern-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Patterns where

import Language.Haskell.Tools.AST

-- mkVarPat :: Ann Name dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
-- mkVarPat = mkAnn child . VarPat

-- mkLitPat :: Ann Literal dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
-- mkLitPat = mkAnn child . LitPat

-- mkInfixAppPat :: Ann Pattern dom SrcTemplateStage -> Ann Operator dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
-- mkInfixAppPat lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ InfixPat lhs op rhs

-- mkAppPat :: Ann Name dom SrcTemplateStage -> [Ann Pattern dom SrcTemplateStage] -> Ann Pattern dom SrcTemplateStage
-- mkAppPat n pat = mkAnn (child <> child) $ AppPat n (mkAnnList (listSepBefore " " " ") pat)

-- mkTuplePat :: [Ann Pattern dom SrcTemplateStage] -> Ann Pattern dom SrcTemplateStage
-- mkTuplePat pats = mkAnn ("(" <> child <> ")") $ TuplePat (mkAnnList (listSep ", ") pats)

-- mkUnboxedTuplePat :: [Ann Pattern dom SrcTemplateStage] -> Ann Pattern dom SrcTemplateStage
-- mkUnboxedTuplePat pats = mkAnn ("(# " <> child <> " #)") $ UnboxTuplePat (mkAnnList (listSep ", ") pats)

-- mkListPat :: [Ann Pattern dom SrcTemplateStage] -> Ann Pattern dom SrcTemplateStage
-- mkListPat pats = mkAnn ("[" <> child <> "]") $ ListPat (mkAnnList (listSep ", ") pats)

-- mkParenPat :: Ann Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
-- mkParenPat = mkAnn ("(" <> child <> ")") . ParenPat

-- mkRecPat :: Ann Name dom SrcTemplateStage -> [Ann PatternField dom SrcTemplateStage] -> Ann Pattern dom SrcTemplateStage
-- mkRecPat name flds = mkAnn (child <> "{ " <> child <> " }") $ RecPat name (mkAnnList (listSep ", ") flds)

-- mkAsPat :: Ann Name dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
-- mkAsPat name pat = mkAnn (child <> "@" <> child) $ AsPat name pat

-- mkWildPat :: Ann Pattern dom SrcTemplateStage
-- mkWildPat = mkAnn "_" WildPat

-- mkIrrefutablePat :: Ann Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
-- mkIrrefutablePat = mkAnn ("~" <> child) . IrrPat

-- mkBangPat :: Ann Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
-- mkBangPat = mkAnn ("!" <> child) . BangPat

-- mkTypeSignPat :: Ann Pattern dom SrcTemplateStage -> Ann Type dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
-- mkTypeSignPat pat typ = mkAnn (child <> " :: " <> child) $ TypeSigPat pat typ

-- mkViewPat :: Ann Expr dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage
-- mkViewPat name pat = mkAnn (child <> " -> " <> child) $ ViewPat name pat

-- mkPatternField :: Ann Name dom SrcTemplateStage -> Ann Pattern dom SrcTemplateStage -> Ann PatternField dom SrcTemplateStage
-- mkPatternField name pat = mkAnn (child <> " = " <> child) $ NormalFieldPattern name pat
