-- | Generation of statement-level AST fragments for refactorings.
-- The bindings defined here are the AST constructor names with an "mk" prefix.
{-# LANGUAGE OverloadedStrings 
           , TypeFamilies 
           #-}
module Language.Haskell.Tools.AST.Gen.Kinds where

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

mkKindConstraint :: Ann UKind dom SrcTemplateStage -> Ann UKindConstraint dom SrcTemplateStage
mkKindConstraint = mkAnn (" :: " <> child) . UKindConstraint

mkKindStar :: Ann UKind dom SrcTemplateStage
mkKindStar = mkAnn "*" UStarKind

mkKindUnbox :: Ann UKind dom SrcTemplateStage
mkKindUnbox = mkAnn "#" UUnboxKind

mkKindFun :: Ann UKind dom SrcTemplateStage -> Ann UKind dom SrcTemplateStage -> Ann UKind dom SrcTemplateStage
mkKindFun lhs rhs = mkAnn (child <> " -> " <> child) $ UFunKind lhs rhs

mkKindParen :: Ann UKind dom SrcTemplateStage -> Ann UKind dom SrcTemplateStage
mkKindParen = mkAnn ("(" <> child <> ")") . UParenKind

mkKindVar :: Ann UName dom SrcTemplateStage -> Ann UKind dom SrcTemplateStage
mkKindVar = mkAnn child . UVarKind

mkKindApp :: Ann UKind dom SrcTemplateStage -> Ann UKind dom SrcTemplateStage -> Ann UKind dom SrcTemplateStage
mkKindApp lhs rhs = mkAnn (child <> " " <> child) $ UAppKind lhs rhs

mkKindList :: Ann UKind dom SrcTemplateStage -> Ann UKind dom SrcTemplateStage
mkKindList = mkAnn ("[" <> child <> "]") . UListKind

mkIntKind :: Integer -> Ann UKind dom SrcTemplateStage
mkIntKind i = mkAnn child $ UPromotedKind $ mkAnn (fromString $ show i) (UPromotedInt i)

mkStringKind :: String -> Ann UKind dom SrcTemplateStage
mkStringKind i = mkAnn child $ UPromotedKind $ mkAnn (fromString $ show i) (UPromotedString i)

mkConKind :: Ann UName dom SrcTemplateStage -> Ann UKind dom SrcTemplateStage
mkConKind = mkAnn child . UPromotedKind . mkAnn child . UPromotedCon

mkListKind :: [Ann UKind dom SrcTemplateStage] -> Ann UKind dom SrcTemplateStage
mkListKind = mkAnn child . UPromotedKind . mkAnn ("[" <> child <> "]") . UPromotedList . mkAnnList (listSep ", ")

mkTupleKind :: [Ann UKind dom SrcTemplateStage] -> Ann UKind dom SrcTemplateStage
mkTupleKind = mkAnn child . UPromotedKind . mkAnn ("(" <> child <> ")") . UPromotedTuple . mkAnnList (listSep ", ")

mkUnitKind :: Ann UKind dom SrcTemplateStage
mkUnitKind = mkAnn child $ UPromotedKind $ mkAnn "()" UPromotedUnit