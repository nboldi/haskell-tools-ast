-- | UPattern matching on statement-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Kinds where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes

pattern KindConstraint :: Kind dom -> KindConstraint dom
pattern KindConstraint k <- Ann _ (UKindConstraint k)

pattern StarKind :: Kind dom
pattern StarKind <- Ann _ UStarKind

pattern UnboxKind :: Kind dom
pattern UnboxKind <- Ann _ UUnboxKind

pattern FunKind :: Kind dom -> Kind dom -> Kind dom
pattern FunKind a r <- Ann _ (UFunKind a r)

pattern ParenKind :: Kind dom -> Kind dom
pattern ParenKind k <- Ann _ (UParenKind k)

pattern VarKind :: Name dom -> Kind dom
pattern VarKind v <- Ann _ (UVarKind v)

pattern AppKind :: Kind dom -> Kind dom -> Kind dom
pattern AppKind f a <- Ann _ (UAppKind f a)

pattern ListKind :: Kind dom -> Kind dom
pattern ListKind k <- Ann _ (UListKind k)

pattern IntKind :: Integer -> Kind dom
pattern IntKind i <- Ann _ (UPromotedKind (Ann _ (UPromotedInt i)))

pattern StringKind :: String -> Kind dom
pattern StringKind s <- Ann _ (UPromotedKind (Ann _ (UPromotedString s)))

pattern ConKind :: Name dom -> Kind dom
pattern ConKind s <- Ann _ (UPromotedKind (Ann _ (UPromotedCon s)))

pattern ListKindPromoted :: KindList dom -> Kind dom
pattern ListKindPromoted elems <- Ann _ (UPromotedKind (Ann _ (UPromotedList elems)))

pattern TupleKind :: KindList dom -> Kind dom
pattern TupleKind elems <- Ann _ (UPromotedKind (Ann _ (UPromotedTuple elems)))

pattern UnitKind :: Kind dom
pattern UnitKind <- Ann _ (UPromotedKind (Ann _ UPromotedUnit))
