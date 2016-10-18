-- | Pattern matching on pattern-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Patterns where

import Language.Haskell.Tools.AST

pattern VarPat :: Ann UName dom stage -> Ann Pattern dom stage
pattern VarPat var <- Ann _ (UVarPat var)

pattern LitPat :: Ann ULiteral dom stage -> Ann Pattern dom stage
pattern LitPat lit <- Ann _ (ULitPat lit)

pattern InfixAppPat :: Ann Pattern dom stage -> Ann UOperator dom stage -> Ann Pattern dom stage -> Ann Pattern dom stage
pattern InfixAppPat lhs op rhs <- Ann _ (UInfixAppPat lhs op rhs)

pattern AppPat :: Ann UName dom stage -> AnnList Pattern dom stage -> Ann Pattern dom stage
pattern AppPat n pat <- Ann _ (UAppPat n pat)

pattern TuplePat :: AnnList Pattern dom stage -> Ann Pattern dom stage
pattern TuplePat pats <- Ann _ (UTuplePat pats)

pattern UnboxTuplePat :: AnnList Pattern dom stage -> Ann Pattern dom stage
pattern UnboxTuplePat pats <- Ann _ (UUnboxTuplePat pats)

pattern ListPat :: AnnList Pattern dom stage -> Ann Pattern dom stage
pattern ListPat pats <- Ann _ (UListPat pats)

pattern ParenPat :: Ann Pattern dom stage -> Ann Pattern dom stage
pattern ParenPat pat <- Ann _ (UParenPat pat)

pattern RecPat :: Ann UName dom stage -> AnnList PatternField dom stage -> Ann Pattern dom stage
pattern RecPat name flds <- Ann _ (URecPat name flds)

pattern AsPat :: Ann UName dom stage -> Ann Pattern dom stage -> Ann Pattern dom stage
pattern AsPat name pat <- Ann _ (UAsPat name pat)

pattern WildPat :: Ann Pattern dom stage
pattern WildPat <- Ann _ UWildPat

pattern IrrefutablePat :: Ann Pattern dom stage -> Ann Pattern dom stage
pattern IrrefutablePat pat <- Ann _ (UIrrefutablePat pat)

pattern BangPat :: Ann Pattern dom stage -> Ann Pattern dom stage
pattern BangPat pat <- Ann _ (UBangPat pat)

pattern TypeSigPat :: Ann Pattern dom stage -> Ann Type dom stage -> Ann Pattern dom stage
pattern TypeSigPat pat typ <- Ann _ (UTypeSigPat pat typ)

pattern ViewPat :: Ann UExpr dom stage -> Ann Pattern dom stage -> Ann Pattern dom stage
pattern ViewPat name pat <- Ann _ (UViewPat name pat)

pattern PatternField :: Ann UName dom stage -> Ann Pattern dom stage -> Ann PatternField dom stage
pattern PatternField name pat <- Ann _ (UNormalFieldPattern name pat)
