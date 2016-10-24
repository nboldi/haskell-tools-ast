-- | UPattern matching on binding-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Binds where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes

pattern SimpleBind :: Pattern dom -> Rhs dom -> MaybeLocalBinds dom -> ValueBind dom
pattern SimpleBind p r l <- Ann _ (USimpleBind p r l)

pattern FunctionBind :: MatchList dom -> ValueBind dom
pattern FunctionBind matches <- Ann _ (UFunBind matches)

pattern Match :: MatchLhs dom -> Rhs dom -> MaybeLocalBinds dom -> Match dom
pattern Match lhs rhs locs <- Ann _ (UMatch lhs rhs locs)

pattern MatchLhs :: Name dom -> PatternList dom -> MatchLhs dom
pattern MatchLhs n pats <- Ann _ (UNormalLhs n pats)

pattern InfixLhs :: Pattern dom -> Operator dom -> Pattern dom -> PatternList dom -> MatchLhs dom
pattern InfixLhs lhs op rhs pats <- Ann _ (UInfixLhs lhs op rhs pats)

pattern LocalBinds :: LocalBindList dom -> LocalBinds dom
pattern LocalBinds binds <- Ann _ (ULocalBinds binds)

pattern LocalValBind :: ValueBind dom -> LocalBind dom
pattern LocalValBind bind <- Ann _ (ULocalValBind bind)

pattern LocalTypeSig :: TypeSignature dom -> LocalBind dom
pattern LocalTypeSig typeSig <- Ann _ (ULocalSignature typeSig)

pattern LocalFixity :: FixitySignature dom -> LocalBind dom
pattern LocalFixity fixity <- Ann _ (ULocalFixity fixity)

pattern TypeSignature :: NameList dom -> Type dom -> TypeSignature dom
pattern TypeSignature n t <- Ann _ (UTypeSignature n t)

pattern InfixL :: Int -> OperatorList dom -> FixitySignature dom
pattern InfixL prec op <- Ann _ (UFixitySignature (Ann _ AssocLeft) (Ann _ (Precedence prec)) op)

pattern InfixR :: Int -> OperatorList dom -> FixitySignature dom
pattern InfixR prec op <- Ann _ (UFixitySignature (Ann _ AssocRight) (Ann _ (Precedence prec)) op)

pattern Infix :: Int -> OperatorList dom -> FixitySignature dom
pattern Infix prec op <- Ann _ (UFixitySignature (Ann _ AssocNone) (Ann _ (Precedence prec)) op)

pattern UnguardedRhs :: Expr dom -> Rhs dom
pattern UnguardedRhs expr <- Ann _ (UUnguardedRhs expr)

pattern GuardedRhss :: GuardedRhsList dom -> Rhs dom
pattern GuardedRhss rhss <- Ann _ (UGuardedRhss rhss)

pattern GuardedRhs :: RhsGuardList dom -> Expr dom -> GuardedRhs dom
pattern GuardedRhs guards expr <- Ann _ (UGuardedRhs guards expr)

pattern GuardBind :: Pattern dom -> Expr dom -> RhsGuard dom
pattern GuardBind pat expr <- Ann _ (UGuardBind pat expr)

pattern GuardLet :: LocalBindList dom -> RhsGuard dom
pattern GuardLet binds <- Ann _ (UGuardLet binds)

pattern GuardCheck :: Expr dom -> RhsGuard dom
pattern GuardCheck expr <- Ann _ (UGuardCheck expr)
