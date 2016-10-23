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
import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST.Gen.Names
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkKindConstraint :: Kind dom -> KindConstraint dom
mkKindConstraint = mkAnn (" :: " <> child) . UKindConstraint

mkKindStar :: Kind dom
mkKindStar = mkAnn "*" UStarKind

mkKindUnbox :: Kind dom
mkKindUnbox = mkAnn "#" UUnboxKind

mkKindFun :: Kind dom -> Kind dom -> Kind dom
mkKindFun lhs rhs = mkAnn (child <> " -> " <> child) $ UFunKind lhs rhs

mkKindParen :: Kind dom -> Kind dom
mkKindParen = mkAnn ("(" <> child <> ")") . UParenKind

mkKindVar :: Name dom -> Kind dom
mkKindVar = mkAnn child . UVarKind

mkKindApp :: Kind dom -> Kind dom -> Kind dom
mkKindApp lhs rhs = mkAnn (child <> " " <> child) $ UAppKind lhs rhs

mkKindList :: Kind dom -> Kind dom
mkKindList = mkAnn ("[" <> child <> "]") . UListKind

mkIntKind :: Integer -> Kind dom
mkIntKind i = mkAnn child $ UPromotedKind $ mkAnn (fromString $ show i) (UPromotedInt i)

mkStringKind :: String -> Kind dom
mkStringKind i = mkAnn child $ UPromotedKind $ mkAnn (fromString $ show i) (UPromotedString i)

mkConKind :: Name dom -> Kind dom
mkConKind = mkAnn child . UPromotedKind . mkAnn child . UPromotedCon

mkListKind :: [Kind dom] -> Kind dom
mkListKind = mkAnn child . UPromotedKind . mkAnn ("[" <> child <> "]") . UPromotedList . mkAnnList (listSep ", ")

mkTupleKind :: [Kind dom] -> Kind dom
mkTupleKind = mkAnn child . UPromotedKind . mkAnn ("(" <> child <> ")") . UPromotedTuple . mkAnnList (listSep ", ")

mkUnitKind :: Kind dom
mkUnitKind = mkAnn child $ UPromotedKind $ mkAnn "()" UPromotedUnit