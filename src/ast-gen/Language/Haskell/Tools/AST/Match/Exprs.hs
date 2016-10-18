-- | Pattern matching expression-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Exprs where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Match.Base

-- * Expressions

pattern Var :: Ann UName dom stage -> Ann UExpr dom stage
pattern Var name <- Ann _ (UVar name)

pattern Lit :: Ann ULiteral dom stage -> Ann UExpr dom stage
pattern Lit lit <- Ann _ (ULit lit)

pattern InfixApp :: Ann UExpr dom stage -> Ann UOperator dom stage -> Ann UExpr dom stage -> Ann UExpr dom stage
pattern InfixApp lhs op rhs <- Ann _ (UInfixApp lhs op rhs)

pattern PrefixApp :: Ann UOperator dom stage -> Ann UExpr dom stage -> Ann UExpr dom stage
pattern PrefixApp op rhs <- Ann _ (UPrefixApp op rhs)

pattern App :: Ann UExpr dom stage -> Ann UExpr dom stage -> Ann UExpr dom stage
pattern App f e <- Ann _ (UApp f e)

pattern Lambda :: AnnList Pattern dom stage -> Ann UExpr dom stage -> Ann UExpr dom stage
pattern Lambda pats rhs <- Ann _ (ULambda pats rhs)

pattern Let :: AnnList ULocalBind dom stage -> Ann UExpr dom stage -> Ann UExpr dom stage
pattern Let pats expr <- Ann _ (ULet pats expr)

pattern If :: Ann UExpr dom stage -> Ann UExpr dom stage -> Ann UExpr dom stage -> Ann UExpr dom stage
pattern If cond then_ else_ <- Ann _ (UIf cond then_ else_)

pattern MultiIf :: AnnList UGuardedCaseRhs dom stage -> Ann UExpr dom stage
pattern MultiIf cases <- Ann _ (UMultiIf cases)

pattern Case :: Ann UExpr dom stage -> AnnList UAlt dom stage -> Ann UExpr dom stage
pattern Case expr cases <- Ann _ (UCase expr cases)

pattern Do :: AnnList Stmt dom stage -> Ann UExpr dom stage
pattern Do stmts <- Ann _ (UDo DoKeyword stmts)

pattern ListComp :: Ann UExpr dom stage -> AnnList ListCompBody dom stage -> Ann UExpr dom stage
pattern ListComp expr stmts <- Ann _ (UListComp expr stmts)

pattern ParArrayComp :: Ann UExpr dom stage -> AnnList ListCompBody dom stage -> Ann UExpr dom stage
pattern ParArrayComp expr stmts <- Ann _ (UParArrayComp expr stmts)

pattern Tuple :: AnnList UExpr dom stage -> Ann UExpr dom stage
pattern Tuple exprs <-  Ann _ (UTuple exprs)

pattern UnboxedTuple :: AnnList UExpr dom stage -> Ann UExpr dom stage
pattern UnboxedTuple exprs <-  Ann _ (UUnboxedTuple exprs)

pattern List :: AnnList UExpr dom stage -> Ann UExpr dom stage
pattern List exprs <-  Ann _ (UList exprs)

pattern ParArray :: AnnList UExpr dom stage -> Ann UExpr dom stage
pattern ParArray exprs <-  Ann _ (UParArray exprs)

pattern Paren :: Ann UExpr dom stage -> Ann UExpr dom stage
pattern Paren expr <- Ann _ (UParen expr)

pattern LeftSection :: Ann UExpr dom stage -> Ann UOperator dom stage -> Ann UExpr dom stage
pattern LeftSection lhs op <- Ann _ (ULeftSection lhs op)

pattern RightSection :: Ann UOperator dom stage -> Ann UExpr dom stage -> Ann UExpr dom stage
pattern RightSection op lhs <- Ann _ (URightSection op lhs)

pattern RecCon :: Ann UName dom stage -> AnnList UFieldUpdate dom stage -> Ann UExpr dom stage
pattern RecCon name flds <- Ann _ (URecCon name flds)

pattern RecUpdate :: Ann UExpr dom stage -> AnnList UFieldUpdate dom stage -> Ann UExpr dom stage
pattern RecUpdate expr flds <- Ann _ (URecUpdate expr flds)

pattern Enum :: Ann UExpr dom stage -> AnnMaybe UExpr dom stage -> AnnMaybe UExpr dom stage -> Ann UExpr dom stage
pattern Enum from step to <- Ann _ (UEnum from step to)

pattern ParArrayEnum :: Ann UExpr dom stage -> AnnMaybe UExpr dom stage -> Ann UExpr dom stage -> Ann UExpr dom stage
pattern ParArrayEnum from step to <- Ann _ (UParArrayEnum from step to)

pattern TypeSig :: Ann UExpr dom stage -> Ann Type dom stage -> Ann UExpr dom stage
pattern TypeSig lhs typ <- Ann _ (UTypeSig lhs typ)

pattern BracketExpr :: Ann Bracket dom stage -> Ann UExpr dom stage
pattern BracketExpr brack <- Ann _ (UBracketExpr brack)

pattern Splice :: Ann Splice dom stage -> Ann UExpr dom stage
pattern Splice splice <- Ann _ (USplice splice)

pattern QuasiQuoteExpr :: Ann QuasiQuote dom stage -> Ann UExpr dom stage
pattern QuasiQuoteExpr qq <- Ann _ (UQuasiQuoteExpr qq)

-- * Field updates

pattern NormalFieldUpdate :: Ann UName dom stage -> Ann UExpr dom stage -> Ann UFieldUpdate dom stage
pattern NormalFieldUpdate n e <- Ann _ (UNormalFieldUpdate n e)

pattern FieldPun :: Ann UName dom stage -> Ann UFieldUpdate dom stage
pattern FieldPun n <- Ann _ (UFieldPun n)

pattern FieldWildcard :: Ann UFieldWildcard dom stage -> Ann UFieldUpdate dom stage
pattern FieldWildcard wc <- Ann _ (UFieldWildcard wc)

-- * Pattern matching and guards

pattern Alt :: Ann Pattern dom stage -> Ann UCaseRhs dom stage -> AnnMaybe ULocalBinds dom stage -> Ann UAlt dom stage
pattern Alt pat rhs locals <- Ann _ (UAlt pat rhs locals) 

pattern CaseRhs :: Ann UExpr dom stage -> Ann UCaseRhs dom stage
pattern CaseRhs e <- Ann _ (UUnguardedCaseRhs e)

pattern GuardedCaseRhss :: AnnList UGuardedCaseRhs dom stage -> Ann UCaseRhs dom stage
pattern GuardedCaseRhss cases <- Ann _ (UGuardedCaseRhss cases)

pattern GuardedCaseRhs :: AnnList URhsGuard dom stage -> Ann UExpr dom stage -> Ann UGuardedCaseRhs dom stage
pattern GuardedCaseRhs guards expr <- Ann _ (UGuardedCaseRhs guards expr)

