-- | UPattern matching expression-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Exprs where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.AST.Match.Stmts
import Language.Haskell.Tools.AST.Match.Names

-- * Expressions

pattern Var :: Name dom -> Expr dom
pattern Var name <- Ann _ (UVar name)

pattern Lit :: Literal dom -> Expr dom
pattern Lit lit <- Ann _ (ULit lit)

pattern InfixApp :: Expr dom -> Operator dom -> Expr dom -> Expr dom
pattern InfixApp lhs op rhs <- Ann _ (UInfixApp lhs op rhs)

pattern PrefixApp :: Operator dom -> Expr dom -> Expr dom
pattern PrefixApp op rhs <- Ann _ (UPrefixApp op rhs)

pattern App :: Expr dom -> Expr dom -> Expr dom
pattern App f e <- Ann _ (UApp f e)

pattern Lambda :: PatternList dom -> Expr dom -> Expr dom
pattern Lambda pats rhs <- Ann _ (ULambda pats rhs)

pattern Let :: LocalBindList dom -> Expr dom -> Expr dom
pattern Let pats expr <- Ann _ (ULet pats expr)

pattern If :: Expr dom -> Expr dom -> Expr dom -> Expr dom
pattern If cond then_ else_ <- Ann _ (UIf cond then_ else_)

pattern MultiIf :: GuardedCaseRhsList dom -> Expr dom
pattern MultiIf cases <- Ann _ (UMultiIf cases)

pattern Case :: Expr dom -> AltList dom -> Expr dom
pattern Case expr cases <- Ann _ (UCase expr cases)

pattern Do :: StmtList dom -> Expr dom
pattern Do stmts <- Ann _ (UDo DoKeyword stmts)

pattern ListComp :: Expr dom -> ListCompBodyList dom -> Expr dom
pattern ListComp expr stmts <- Ann _ (UListComp expr stmts)

pattern ParArrayComp :: Expr dom -> ListCompBodyList dom -> Expr dom
pattern ParArrayComp expr stmts <- Ann _ (UParArrayComp expr stmts)

pattern Tuple :: ExprList dom -> Expr dom
pattern Tuple exprs <-  Ann _ (UTuple exprs)

pattern UnboxedTuple :: ExprList dom -> Expr dom
pattern UnboxedTuple exprs <-  Ann _ (UUnboxedTuple exprs)

pattern List :: ExprList dom -> Expr dom
pattern List exprs <-  Ann _ (UList exprs)

pattern ParArray :: ExprList dom -> Expr dom
pattern ParArray exprs <-  Ann _ (UParArray exprs)

pattern Paren :: Expr dom -> Expr dom
pattern Paren expr <- Ann _ (UParen expr)

pattern LeftSection :: Expr dom -> Operator dom -> Expr dom
pattern LeftSection lhs op <- Ann _ (ULeftSection lhs op)

pattern RightSection :: Operator dom -> Expr dom -> Expr dom
pattern RightSection op lhs <- Ann _ (URightSection op lhs)

pattern RecCon :: Name dom -> FieldUpdateList dom -> Expr dom
pattern RecCon name flds <- Ann _ (URecCon name flds)

pattern RecUpdate :: Expr dom -> FieldUpdateList dom -> Expr dom
pattern RecUpdate expr flds <- Ann _ (URecUpdate expr flds)

pattern Enum :: Expr dom -> MaybeExpr dom -> MaybeExpr dom -> Expr dom
pattern Enum from step to <- Ann _ (UEnum from step to)

pattern ParArrayEnum :: Expr dom -> MaybeExpr dom -> Expr dom -> Expr dom
pattern ParArrayEnum from step to <- Ann _ (UParArrayEnum from step to)

pattern TypeSig :: Expr dom -> Type dom -> Expr dom
pattern TypeSig lhs typ <- Ann _ (UTypeSig lhs typ)

pattern BracketExpr :: Ann Bracket dom stage -> Expr dom
pattern BracketExpr brack <- Ann _ (UBracketExpr brack)

pattern SpliceExpr :: Ann USplice dom stage -> Expr dom
pattern SpliceExpr splice <- Ann _ (USplice splice)

pattern QuasiQuoteExpr :: Ann QuasiQuote dom stage -> Expr dom
pattern QuasiQuoteExpr qq <- Ann _ (UQuasiQuoteExpr qq)

-- * Field updates

pattern NormalFieldUpdate :: Name dom -> Expr dom -> FieldUpdate dom
pattern NormalFieldUpdate n e <- Ann _ (UNormalFieldUpdate n e)

pattern FieldPun :: Name dom -> FieldUpdate dom
pattern FieldPun n <- Ann _ (UFieldPun n)

pattern FieldWildcard :: FieldWildcard dom -> FieldUpdate dom
pattern FieldWildcard wc <- Ann _ (UFieldWildcard wc)

-- * UPattern matching and guards

pattern Alt :: Pattern dom -> CaseRhs dom -> MaybeLocalBinds dom -> Alt dom
pattern Alt pat rhs locals <- Ann _ (UAlt pat rhs locals) 

pattern CaseRhs :: Expr dom -> CaseRhs dom
pattern CaseRhs e <- Ann _ (UUnguardedCaseRhs e)

pattern GuardedCaseRhss :: GuardedCaseRhsList dom -> CaseRhs dom
pattern GuardedCaseRhss cases <- Ann _ (UGuardedCaseRhss cases)

pattern GuardedCaseRhs :: RhsGuardList dom -> Expr dom -> GuardedCaseRhs dom
pattern GuardedCaseRhs guards expr <- Ann _ (UGuardedCaseRhs guards expr)

