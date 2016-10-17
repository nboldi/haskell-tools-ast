-- | Pattern matching expression-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Exprs where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Match.Base

-- * Expressions

pattern Var :: Ann Name dom stage -> Ann Expr dom stage
pattern Var name <- Ann _ (UVar name)

pattern Lit :: Ann Literal dom stage -> Ann Expr dom stage
pattern Lit lit <- Ann _ (ULit lit)

pattern InfixApp :: Ann Expr dom stage -> Ann Operator dom stage -> Ann Expr dom stage -> Ann Expr dom stage
pattern InfixApp lhs op rhs <- Ann _ (UInfixApp lhs op rhs)

pattern PrefixApp :: Ann Operator dom stage -> Ann Expr dom stage -> Ann Expr dom stage
pattern PrefixApp op rhs <- Ann _ (UPrefixApp op rhs)

pattern App :: Ann Expr dom stage -> Ann Expr dom stage -> Ann Expr dom stage
pattern App f e <- Ann _ (UApp f e)

pattern Lambda :: AnnList Pattern dom stage -> Ann Expr dom stage -> Ann Expr dom stage
pattern Lambda pats rhs <- Ann _ (ULambda pats rhs)

pattern Let :: AnnList LocalBind dom stage -> Ann Expr dom stage -> Ann Expr dom stage
pattern Let pats expr <- Ann _ (ULet pats expr)

pattern If :: Ann Expr dom stage -> Ann Expr dom stage -> Ann Expr dom stage -> Ann Expr dom stage
pattern If cond then_ else_ <- Ann _ (UIf cond then_ else_)

pattern MultiIf :: AnnList GuardedCaseRhs dom stage -> Ann Expr dom stage
pattern MultiIf cases <- Ann _ (UMultiIf cases)

pattern Case :: Ann Expr dom stage -> AnnList Alt dom stage -> Ann Expr dom stage
pattern Case expr cases <- Ann _ (UCase expr cases)

pattern Do :: AnnList Stmt dom stage -> Ann Expr dom stage
pattern Do stmts <- Ann _ (UDo DoKeyword stmts)

pattern ListComp :: Ann Expr dom stage -> AnnList ListCompBody dom stage -> Ann Expr dom stage
pattern ListComp expr stmts <- Ann _ (UListComp expr stmts)

pattern ParArrayComp :: Ann Expr dom stage -> AnnList ListCompBody dom stage -> Ann Expr dom stage
pattern ParArrayComp expr stmts <- Ann _ (UParArrayComp expr stmts)

pattern Tuple :: AnnList Expr dom stage -> Ann Expr dom stage
pattern Tuple exprs <-  Ann _ (UTuple exprs)

pattern UnboxedTuple :: AnnList Expr dom stage -> Ann Expr dom stage
pattern UnboxedTuple exprs <-  Ann _ (UUnboxedTuple exprs)

pattern List :: AnnList Expr dom stage -> Ann Expr dom stage
pattern List exprs <-  Ann _ (UList exprs)

pattern ParArray :: AnnList Expr dom stage -> Ann Expr dom stage
pattern ParArray exprs <-  Ann _ (UParArray exprs)

pattern Paren :: Ann Expr dom stage -> Ann Expr dom stage
pattern Paren expr <- Ann _ (UParen expr)

pattern LeftSection :: Ann Expr dom stage -> Ann Operator dom stage -> Ann Expr dom stage
pattern LeftSection lhs op <- Ann _ (ULeftSection lhs op)

pattern RightSection :: Ann Operator dom stage -> Ann Expr dom stage -> Ann Expr dom stage
pattern RightSection op lhs <- Ann _ (URightSection op lhs)

pattern RecCon :: Ann Name dom stage -> AnnList FieldUpdate dom stage -> Ann Expr dom stage
pattern RecCon name flds <- Ann _ (URecCon name flds)

pattern RecUpdate :: Ann Expr dom stage -> AnnList FieldUpdate dom stage -> Ann Expr dom stage
pattern RecUpdate expr flds <- Ann _ (URecUpdate expr flds)

pattern Enum :: Ann Expr dom stage -> AnnMaybe Expr dom stage -> AnnMaybe Expr dom stage -> Ann Expr dom stage
pattern Enum from step to <- Ann _ (UEnum from step to)

pattern ParArrayEnum :: Ann Expr dom stage -> AnnMaybe Expr dom stage -> Ann Expr dom stage -> Ann Expr dom stage
pattern ParArrayEnum from step to <- Ann _ (UParArrayEnum from step to)

pattern TypeSig :: Ann Expr dom stage -> Ann Type dom stage -> Ann Expr dom stage
pattern TypeSig lhs typ <- Ann _ (UTypeSig lhs typ)

pattern BracketExpr :: Ann Bracket dom stage -> Ann Expr dom stage
pattern BracketExpr brack <- Ann _ (UBracketExpr brack)

pattern Splice :: Ann Splice dom stage -> Ann Expr dom stage
pattern Splice splice <- Ann _ (USplice splice)

pattern QuasiQuoteExpr :: Ann QuasiQuote dom stage -> Ann Expr dom stage
pattern QuasiQuoteExpr qq <- Ann _ (UQuasiQuoteExpr qq)

-- * Field updates

pattern NormalFieldUpdate :: Ann Name dom stage -> Ann Expr dom stage -> Ann FieldUpdate dom stage
pattern NormalFieldUpdate n e <- Ann _ (UNormalFieldUpdate n e)

pattern FieldPun :: Ann Name dom stage -> Ann FieldUpdate dom stage
pattern FieldPun n <- Ann _ (UFieldPun n)

pattern FieldWildcard :: Ann FieldWildcard dom stage -> Ann FieldUpdate dom stage
pattern FieldWildcard wc <- Ann _ (UFieldWildcard wc)

-- * Pattern matching and guards

pattern Alt :: Ann Pattern dom stage -> Ann CaseRhs dom stage -> AnnMaybe LocalBinds dom stage -> Ann Alt dom stage
pattern Alt pat rhs locals <- Ann _ (UAlt pat rhs locals) 

pattern CaseRhs :: Ann Expr dom stage -> Ann CaseRhs dom stage
pattern CaseRhs e <- Ann _ (UUnguardedCaseRhs e)

pattern GuardedCaseRhss :: AnnList GuardedCaseRhs dom stage -> Ann CaseRhs dom stage
pattern GuardedCaseRhss cases <- Ann _ (UGuardedCaseRhss cases)

pattern GuardedCaseRhs :: AnnList RhsGuard dom stage -> Ann Expr dom stage -> Ann GuardedCaseRhs dom stage
pattern GuardedCaseRhs guards expr <- Ann _ (UGuardedCaseRhs guards expr)

