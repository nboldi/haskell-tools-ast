-- | UPattern matching on pattern-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Patterns where

import Language.Haskell.Tools.AST

pattern VarPat :: Ann UName dom stage -> Ann UPattern dom stage
pattern VarPat var <- Ann _ (UVarPat var)

pattern LitPat :: Ann ULiteral dom stage -> Ann UPattern dom stage
pattern LitPat lit <- Ann _ (ULitPat lit)

pattern InfixAppPat :: Ann UPattern dom stage -> Ann UOperator dom stage -> Ann UPattern dom stage -> Ann UPattern dom stage
pattern InfixAppPat lhs op rhs <- Ann _ (UInfixAppPat lhs op rhs)

pattern AppPat :: Ann UName dom stage -> AnnList UPattern dom stage -> Ann UPattern dom stage
pattern AppPat n pat <- Ann _ (UAppPat n pat)

pattern TuplePat :: AnnList UPattern dom stage -> Ann UPattern dom stage
pattern TuplePat pats <- Ann _ (UTuplePat pats)

pattern UnboxTuplePat :: AnnList UPattern dom stage -> Ann UPattern dom stage
pattern UnboxTuplePat pats <- Ann _ (UUnboxTuplePat pats)

pattern ListPat :: AnnList UPattern dom stage -> Ann UPattern dom stage
pattern ListPat pats <- Ann _ (UListPat pats)

pattern ParenPat :: Ann UPattern dom stage -> Ann UPattern dom stage
pattern ParenPat pat <- Ann _ (UParenPat pat)

pattern RecPat :: Ann UName dom stage -> AnnList UPatternField dom stage -> Ann UPattern dom stage
pattern RecPat name flds <- Ann _ (URecPat name flds)

pattern AsPat :: Ann UName dom stage -> Ann UPattern dom stage -> Ann UPattern dom stage
pattern AsPat name pat <- Ann _ (UAsPat name pat)

pattern WildPat :: Ann UPattern dom stage
pattern WildPat <- Ann _ UWildPat

pattern IrrefutablePat :: Ann UPattern dom stage -> Ann UPattern dom stage
pattern IrrefutablePat pat <- Ann _ (UIrrefutablePat pat)

pattern BangPat :: Ann UPattern dom stage -> Ann UPattern dom stage
pattern BangPat pat <- Ann _ (UBangPat pat)

pattern TypeSigPat :: Ann UPattern dom stage -> Ann Type dom stage -> Ann UPattern dom stage
pattern TypeSigPat pat typ <- Ann _ (UTypeSigPat pat typ)

pattern ViewPat :: Ann UExpr dom stage -> Ann UPattern dom stage -> Ann UPattern dom stage
pattern ViewPat name pat <- Ann _ (UViewPat name pat)

pattern UPatternField :: Ann UName dom stage -> Ann UPattern dom stage -> Ann UPatternField dom stage
pattern UPatternField name pat <- Ann _ (UNormalFieldPattern name pat)
