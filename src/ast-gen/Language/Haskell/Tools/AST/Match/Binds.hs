-- | Pattern matching on binding-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Binds where

import Language.Haskell.Tools.AST

pattern SimpleBind :: Ann Pattern dom stage -> Ann URhs dom stage 
                        -> AnnMaybe ULocalBinds dom stage -> Ann UValueBind dom stage
pattern SimpleBind p r l <- Ann _ (USimpleBind p r l)

pattern FunctionBind :: AnnList UMatch dom stage -> Ann UValueBind dom stage
pattern FunctionBind matches <- Ann _ (UFunBind matches)

pattern Match :: Ann UMatchLhs dom stage -> Ann URhs dom stage 
                   -> AnnMaybe ULocalBinds dom stage -> Ann UMatch dom stage
pattern Match lhs rhs locs <- Ann _ (UMatch lhs rhs locs)

pattern MatchLhs :: Ann UName dom stage -> AnnList Pattern dom stage 
                       -> Ann UMatchLhs dom stage
pattern MatchLhs n pats <- Ann _ (UNormalLhs n pats)

pattern InfixLhs :: Ann Pattern dom stage -> Ann UOperator dom stage 
                      -> Ann Pattern dom stage -> AnnList Pattern dom stage
                      -> Ann UMatchLhs dom stage
pattern InfixLhs lhs op rhs pats <- Ann _ (UInfixLhs lhs op rhs pats)

pattern LocalBinds :: AnnList ULocalBind dom stage -> Ann ULocalBinds dom stage
pattern LocalBinds binds <- Ann _ (ULocalBinds binds)

pattern LocalValBind :: Ann UValueBind dom stage -> Ann ULocalBind dom stage
pattern LocalValBind bind <- Ann _ (ULocalValBind bind)

pattern LocalTypeSig :: Ann UTypeSignature dom stage -> Ann ULocalBind dom stage
pattern LocalTypeSig typeSig <- Ann _ (ULocalSignature typeSig)

pattern LocalFixity :: Ann UFixitySignature dom stage -> Ann ULocalBind dom stage
pattern LocalFixity fixity <- Ann _ (ULocalFixity fixity)

pattern TypeSignature :: AnnList UName dom stage -> Ann Type dom stage 
                          -> Ann UTypeSignature dom stage
pattern TypeSignature n t <- Ann _ (UTypeSignature n t)

pattern InfixL :: Ann Precedence dom stage -> AnnList UOperator dom stage -> Ann UFixitySignature dom stage
pattern InfixL prec op <- Ann _ (UFixitySignature (Ann _ AssocLeft) prec op)

pattern InfixR :: Ann Precedence dom stage -> AnnList UOperator dom stage -> Ann UFixitySignature dom stage
pattern InfixR prec op <- Ann _ (UFixitySignature (Ann _ AssocRight) prec op)

pattern Infix :: Ann Precedence dom stage -> AnnList UOperator dom stage -> Ann UFixitySignature dom stage
pattern Infix prec op <- Ann _ (UFixitySignature (Ann _ AssocNone) prec op)

pattern UnguardedRhs :: Ann Expr dom stage -> Ann URhs dom stage
pattern UnguardedRhs expr <- Ann _ (UUnguardedRhs expr)

pattern GuardedRhss :: AnnList UGuardedRhs dom stage -> Ann URhs dom stage
pattern GuardedRhss rhss <- Ann _ (UGuardedRhss rhss)

pattern GuardedRhs :: AnnList URhsGuard dom stage -> Ann Expr dom stage -> Ann UGuardedRhs dom stage
pattern GuardedRhs guards expr <- Ann _ (UGuardedRhs guards expr)

pattern GuardBind :: Ann Pattern dom stage -> Ann Expr dom stage -> Ann URhsGuard dom stage
pattern GuardBind pat expr <- Ann _ (UGuardBind pat expr)

pattern GuardLet :: AnnList ULocalBind dom stage -> Ann URhsGuard dom stage
pattern GuardLet binds <- Ann _ (UGuardLet binds)

pattern GuardCheck :: Ann Expr dom stage -> Ann URhsGuard dom stage
pattern GuardCheck expr <- Ann _ (UGuardCheck expr)
