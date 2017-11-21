-- | Generation of statement-level AST fragments for refactorings.
-- The bindings defined here are the AST constructor names with an "mk" prefix.
{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Language.Haskell.Tools.Rewrite.Create.Kinds where

import Data.String (IsString(..), String)
import Language.Haskell.Tools.AST (UPromoted(..), UKind(..), UKindConstraint(..))
import Language.Haskell.Tools.PrettyPrint.Prepare
import Language.Haskell.Tools.Rewrite.Create.Utils (mkAnn, mkAnnList)
import Language.Haskell.Tools.Rewrite.ElementTypes (Name, Kind, KindConstraint)

-- | Kind constraint (@ :: * -> * @)
mkKindConstraint :: Kind dom -> KindConstraint dom
mkKindConstraint = mkAnn (" :: " <> child) . UKindConstraint

-- | @*@, the kind of types
mkKindStar :: Kind dom
mkKindStar = mkAnn "*" UStarKind

-- | @#@, the kind of unboxed types
mkKindUnbox :: Kind dom
mkKindUnbox = mkAnn "#" UUnboxKind

-- | @->@, the kind of type constructor
mkKindFun :: Kind dom -> Kind dom -> Kind dom
mkKindFun lhs rhs = mkAnn (child <> " -> " <> child) $ UFunKind lhs rhs

-- | A parenthesised kind
mkKindParen :: Kind dom -> Kind dom
mkKindParen = mkAnn ("(" <> child <> ")") . UParenKind

-- | Kind variable (using @PolyKinds@ extension)
mkKindVar :: Name dom -> Kind dom
mkKindVar = mkAnn child . UVarKind

-- | Kind application (@ k1 k2 @)
mkKindApp :: Kind dom -> Kind dom -> Kind dom
mkKindApp lhs rhs = mkAnn (child <> " " <> child) $ UAppKind lhs rhs

-- | A list kind (@ [k] @)
mkKindList :: Kind dom -> Kind dom
mkKindList = mkAnn ("[" <> child <> "]") . UListKind

-- | Numeric value promoted to the kind level.
mkIntKind :: Integer -> Kind dom
mkIntKind i = mkAnn child $ UPromotedKind $ mkAnn (fromString $ show i) (UPromotedInt i)

-- | String value promoted to the kind level.
mkStringKind :: String -> Kind dom
mkStringKind i = mkAnn child $ UPromotedKind $ mkAnn (fromString $ show i) (UPromotedString i)

-- | A data constructor value promoted to the kind level.
mkConKind :: Name dom -> Kind dom
mkConKind = mkAnn child . UPromotedKind . mkAnn child . UPromotedCon

-- | A list of elements as a kind.
mkListKind :: [Kind dom] -> Kind dom
mkListKind = mkAnn child . UPromotedKind . mkAnn ("[" <> child <> "]") . UPromotedList . mkAnnList (separatedBy ", " list)

-- | A tuple of elements as a kind.
mkTupleKind :: [Kind dom] -> Kind dom
mkTupleKind = mkAnn child . UPromotedKind . mkAnn ("(" <> child <> ")") . UPromotedTuple . mkAnnList (separatedBy ", " list)

-- | Kind of the unit value @()@. 
mkUnitKind :: Kind dom
mkUnitKind = mkAnn child $ UPromotedKind $ mkAnn "()" UPromotedUnit