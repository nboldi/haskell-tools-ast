-- | Pattern matching on binding-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Binds where

import Language.Haskell.Tools.AST

pattern SimpleBind :: Ann Pattern dom stage -> Ann Rhs dom stage 
                        -> AnnMaybe LocalBinds dom stage -> Ann ValueBind dom stage
pattern SimpleBind p r l <- Ann _ (USimpleBind p r l)

pattern FunctionBind :: AnnList Match dom stage -> Ann ValueBind dom stage
pattern FunctionBind matches <- Ann _ (UFunBind matches)

pattern Match :: Ann MatchLhs dom stage -> Ann Rhs dom stage 
                   -> AnnMaybe LocalBinds dom stage -> Ann Match dom stage
pattern Match lhs rhs locs <- Ann _ (UMatch lhs rhs locs)

pattern MatchLhs :: Ann Name dom stage -> AnnList Pattern dom stage 
                       -> Ann MatchLhs dom stage
pattern MatchLhs n pats <- Ann _ (UNormalLhs n pats)

pattern InfixLhs :: Ann Pattern dom stage -> Ann Operator dom stage 
                      -> Ann Pattern dom stage -> AnnList Pattern dom stage
                      -> Ann MatchLhs dom stage
pattern InfixLhs lhs op rhs pats <- Ann _ (UInfixLhs lhs op rhs pats)

pattern LocalBinds :: AnnList LocalBind dom stage -> Ann LocalBinds dom stage
pattern LocalBinds binds <- Ann _ (ULocalBinds binds)

pattern LocalValBind :: Ann ValueBind dom stage -> Ann LocalBind dom stage
pattern LocalValBind bind <- Ann _ (ULocalValBind bind)

pattern LocalTypeSig :: Ann TypeSignature dom stage -> Ann LocalBind dom stage
pattern LocalTypeSig typeSig <- Ann _ (ULocalSignature typeSig)

pattern LocalFixity :: Ann FixitySignature dom stage -> Ann LocalBind dom stage
pattern LocalFixity fixity <- Ann _ (ULocalFixity fixity)

pattern TypeSignature :: AnnList Name dom stage -> Ann Type dom stage 
                          -> Ann TypeSignature dom stage
pattern TypeSignature n t <- Ann _ (UTypeSignature n t)

pattern InfixL :: Ann Precedence dom stage -> AnnList Operator dom stage -> Ann FixitySignature dom stage
pattern InfixL prec op <- Ann _ (UFixitySignature (Ann _ AssocLeft) prec op)

pattern InfixR :: Ann Precedence dom stage -> AnnList Operator dom stage -> Ann FixitySignature dom stage
pattern InfixR prec op <- Ann _ (UFixitySignature (Ann _ AssocRight) prec op)

pattern Infix :: Ann Precedence dom stage -> AnnList Operator dom stage -> Ann FixitySignature dom stage
pattern Infix prec op <- Ann _ (UFixitySignature (Ann _ AssocNone) prec op)

pattern UnguardedRhs :: Ann Expr dom stage -> Ann Rhs dom stage
pattern UnguardedRhs expr <- Ann _ (UUnguardedRhs expr)

pattern GuardedRhss :: AnnList GuardedRhs dom stage -> Ann Rhs dom stage
pattern GuardedRhss rhss <- Ann _ (UGuardedRhss rhss)

pattern GuardedRhs :: AnnList RhsGuard dom stage -> Ann Expr dom stage -> Ann GuardedRhs dom stage
pattern GuardedRhs guards expr <- Ann _ (UGuardedRhs guards expr)

pattern GuardBind :: Ann Pattern dom stage -> Ann Expr dom stage -> Ann RhsGuard dom stage
pattern GuardBind pat expr <- Ann _ (UGuardBind pat expr)

pattern GuardLet :: AnnList LocalBind dom stage -> Ann RhsGuard dom stage
pattern GuardLet binds <- Ann _ (UGuardLet binds)

pattern GuardCheck :: Ann Expr dom stage -> Ann RhsGuard dom stage
pattern GuardCheck expr <- Ann _ (UGuardCheck expr)
