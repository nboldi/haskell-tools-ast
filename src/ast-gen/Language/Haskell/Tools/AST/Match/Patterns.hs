-- | UPattern matching on pattern-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Patterns where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes

pattern VarPat :: Name dom -> Pattern dom
pattern VarPat var <- Ann _ (UVarPat var)

pattern LitPat :: Literal dom -> Pattern dom
pattern LitPat lit <- Ann _ (ULitPat lit)

pattern InfixAppPat :: Pattern dom -> Operator dom -> Pattern dom -> Pattern dom
pattern InfixAppPat lhs op rhs <- Ann _ (UInfixAppPat lhs op rhs)

pattern AppPat :: Name dom -> PatternList dom -> Pattern dom
pattern AppPat n pat <- Ann _ (UAppPat n pat)

pattern TuplePat :: PatternList dom -> Pattern dom
pattern TuplePat pats <- Ann _ (UTuplePat pats)

pattern UnboxTuplePat :: PatternList dom -> Pattern dom
pattern UnboxTuplePat pats <- Ann _ (UUnboxTuplePat pats)

pattern ListPat :: PatternList dom -> Pattern dom
pattern ListPat pats <- Ann _ (UListPat pats)

pattern ParenPat :: Pattern dom -> Pattern dom
pattern ParenPat pat <- Ann _ (UParenPat pat)

pattern RecPat :: Name dom -> PatternFieldList dom -> Pattern dom
pattern RecPat name flds <- Ann _ (URecPat name flds)

pattern AsPat :: Name dom -> Pattern dom -> Pattern dom
pattern AsPat name pat <- Ann _ (UAsPat name pat)

pattern WildPat :: Pattern dom
pattern WildPat <- Ann _ UWildPat

pattern IrrefutablePat :: Pattern dom -> Pattern dom
pattern IrrefutablePat pat <- Ann _ (UIrrefutablePat pat)

pattern BangPat :: Pattern dom -> Pattern dom
pattern BangPat pat <- Ann _ (UBangPat pat)

pattern TypeSigPat :: Pattern dom -> Type dom -> Pattern dom
pattern TypeSigPat pat typ <- Ann _ (UTypeSigPat pat typ)

pattern ViewPat :: Expr dom -> Pattern dom -> Pattern dom
pattern ViewPat name pat <- Ann _ (UViewPat name pat)

pattern UPatternField :: Name dom -> Pattern dom -> PatternField dom
pattern UPatternField name pat <- Ann _ (UNormalFieldPattern name pat)
