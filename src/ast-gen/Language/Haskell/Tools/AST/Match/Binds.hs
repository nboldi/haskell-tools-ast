-- | UPattern matching on binding-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Binds where

import Language.Haskell.Tools.AST

pattern SimpleBind :: Ann UPattern dom stage -> Ann URhs dom stage 
                        -> AnnMaybeG ULocalBinds dom stage -> Ann UValueBind dom stage
pattern SimpleBind p r l <- Ann _ (USimpleBind p r l)

pattern FunctionBind :: AnnListG UMatch dom stage -> Ann UValueBind dom stage
pattern FunctionBind matches <- Ann _ (UFunBind matches)

pattern Match :: Ann UMatchLhs dom stage -> Ann URhs dom stage 
                   -> AnnMaybeG ULocalBinds dom stage -> Ann UMatch dom stage
pattern Match lhs rhs locs <- Ann _ (UMatch lhs rhs locs)

pattern MatchLhs :: Ann UName dom stage -> AnnListG UPattern dom stage 
                       -> Ann UMatchLhs dom stage
pattern MatchLhs n pats <- Ann _ (UNormalLhs n pats)

pattern InfixLhs :: Ann UPattern dom stage -> Ann UOperator dom stage 
                      -> Ann UPattern dom stage -> AnnListG UPattern dom stage
                      -> Ann UMatchLhs dom stage
pattern InfixLhs lhs op rhs pats <- Ann _ (UInfixLhs lhs op rhs pats)

pattern LocalBinds :: AnnListG ULocalBind dom stage -> Ann ULocalBinds dom stage
pattern LocalBinds binds <- Ann _ (ULocalBinds binds)

pattern LocalValBind :: Ann UValueBind dom stage -> Ann ULocalBind dom stage
pattern LocalValBind bind <- Ann _ (ULocalValBind bind)

pattern LocalTypeSig :: Ann UTypeSignature dom stage -> Ann ULocalBind dom stage
pattern LocalTypeSig typeSig <- Ann _ (ULocalSignature typeSig)

pattern LocalFixity :: Ann UFixitySignature dom stage -> Ann ULocalBind dom stage
pattern LocalFixity fixity <- Ann _ (ULocalFixity fixity)

pattern TypeSignature :: AnnListG UName dom stage -> Ann UType dom stage 
                          -> Ann UTypeSignature dom stage
pattern TypeSignature n t <- Ann _ (UTypeSignature n t)

pattern InfixL :: Ann Precedence dom stage -> AnnListG UOperator dom stage -> Ann UFixitySignature dom stage
pattern InfixL prec op <- Ann _ (UFixitySignature (Ann _ AssocLeft) prec op)

pattern InfixR :: Ann Precedence dom stage -> AnnListG UOperator dom stage -> Ann UFixitySignature dom stage
pattern InfixR prec op <- Ann _ (UFixitySignature (Ann _ AssocRight) prec op)

pattern Infix :: Ann Precedence dom stage -> AnnListG UOperator dom stage -> Ann UFixitySignature dom stage
pattern Infix prec op <- Ann _ (UFixitySignature (Ann _ AssocNone) prec op)

pattern UnguardedRhs :: Ann UExpr dom stage -> Ann URhs dom stage
pattern UnguardedRhs expr <- Ann _ (UUnguardedRhs expr)

pattern GuardedRhss :: AnnListG UGuardedRhs dom stage -> Ann URhs dom stage
pattern GuardedRhss rhss <- Ann _ (UGuardedRhss rhss)

pattern GuardedRhs :: AnnListG URhsGuard dom stage -> Ann UExpr dom stage -> Ann UGuardedRhs dom stage
pattern GuardedRhs guards expr <- Ann _ (UGuardedRhs guards expr)

pattern GuardBind :: Ann UPattern dom stage -> Ann UExpr dom stage -> Ann URhsGuard dom stage
pattern GuardBind pat expr <- Ann _ (UGuardBind pat expr)

pattern GuardLet :: AnnListG ULocalBind dom stage -> Ann URhsGuard dom stage
pattern GuardLet binds <- Ann _ (UGuardLet binds)

pattern GuardCheck :: Ann UExpr dom stage -> Ann URhsGuard dom stage
pattern GuardCheck expr <- Ann _ (UGuardCheck expr)
