-- | Pattern matching on statement-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Kinds where

import Language.Haskell.Tools.AST

pattern KindConstraint :: Ann UKind dom stage -> Ann UKindConstraint dom stage
pattern KindConstraint k <- Ann _ (UKindConstraint k)

pattern StarKind :: Ann UKind dom stage
pattern StarKind <- Ann _ UStarKind

pattern UnboxKind :: Ann UKind dom stage
pattern UnboxKind <- Ann _ UUnboxKind

pattern FunKind :: Ann UKind dom stage -> Ann UKind dom stage -> Ann UKind dom stage
pattern FunKind a r <- Ann _ (UFunKind a r)

pattern ParenKind :: Ann UKind dom stage -> Ann UKind dom stage
pattern ParenKind k <- Ann _ (UParenKind k)

pattern VarKind :: Ann UName dom stage -> Ann UKind dom stage
pattern VarKind v <- Ann _ (UVarKind v)

pattern AppKind :: Ann UKind dom stage -> Ann UKind dom stage -> Ann UKind dom stage
pattern AppKind f a <- Ann _ (UAppKind f a)

pattern ListKind :: Ann UKind dom stage -> Ann UKind dom stage
pattern ListKind k <- Ann _ (UListKind k)

pattern IntKind :: Integer -> Ann UKind dom stage
pattern IntKind i <- Ann _ (UPromotedKind (Ann _ (UPromotedInt i)))

pattern StringKind :: String -> Ann UKind dom stage
pattern StringKind s <- Ann _ (UPromotedKind (Ann _ (UPromotedString s)))

pattern ConKind :: Ann UName dom stage -> Ann UKind dom stage
pattern ConKind s <- Ann _ (UPromotedKind (Ann _ (UPromotedCon s)))

pattern ListKindPromoted :: AnnList UKind dom stage -> Ann UKind dom stage
pattern ListKindPromoted elems <- Ann _ (UPromotedKind (Ann _ (UPromotedList elems)))

pattern TupleKind :: AnnList UKind dom stage -> Ann UKind dom stage
pattern TupleKind elems <- Ann _ (UPromotedKind (Ann _ (UPromotedTuple elems)))

pattern UnitKind :: Ann UKind dom stage
pattern UnitKind <- Ann _ (UPromotedKind (Ann _ UPromotedUnit))
