{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
-- Generated references for handling the custom AST
module Language.Haskell.Tools.AST.References where

import Control.Reference hiding (element)
import Language.Haskell.Tools.AST.MakeASTReferences

import Language.Haskell.Tools.AST.Modules
import Language.Haskell.Tools.AST.TH
import Language.Haskell.Tools.AST.Decls
import Language.Haskell.Tools.AST.Binds
import Language.Haskell.Tools.AST.Exprs
import Language.Haskell.Tools.AST.Stmts
import Language.Haskell.Tools.AST.Patterns
import Language.Haskell.Tools.AST.Types
import Language.Haskell.Tools.AST.Kinds
import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Ann

-- Modules
$(toASTReferences (makeReferences ''Module))
$(toASTReferences (makeReferences ''ModuleHead))
$(toASTReferences (makeReferences ''ExportSpecList))
$(toASTReferences (makeReferences ''ExportSpec))
$(toASTReferences (makeReferences ''IESpec))
$(toASTReferences (makeReferences ''SubSpec))
$(toASTReferences (makeReferences ''ModulePragma))
$(toASTReferences (makeReferences ''ImportDecl))
$(toASTReferences (makeReferences ''ImportSpec))
$(toASTReferences (makeReferences ''ImportQualified))
$(toASTReferences (makeReferences ''ImportSource))
$(toASTReferences (makeReferences ''ImportSafe))
$(toASTReferences (makeReferences ''TypeNamespace))
$(toASTReferences (makeReferences ''ImportRenaming))

-- Declarations
$(toASTReferences (makeReferences ''UDecl))
$(toASTReferences (makeReferences ''UClassBody))
$(toASTReferences (makeReferences ''UClassElement))
$(toASTReferences (makeReferences ''UDeclHead))
$(toASTReferences (makeReferences ''UInstBody))
$(toASTReferences (makeReferences ''UInstBodyDecl))
$(toASTReferences (makeReferences ''UGadtConDecl))
$(toASTReferences (makeReferences ''UGadtConType))
$(toASTReferences (makeReferences ''UFunDeps))
$(toASTReferences (makeReferences ''UFunDep))
$(toASTReferences (makeReferences ''UConDecl))
$(toASTReferences (makeReferences ''UFieldDecl))
$(toASTReferences (makeReferences ''UDeriving))
$(toASTReferences (makeReferences ''UInstanceRule))
$(toASTReferences (makeReferences ''UInstanceHead))
$(toASTReferences (makeReferences ''UTypeEqn))
$(toASTReferences (makeReferences ''KindConstraint))
$(toASTReferences (makeReferences ''TyVar))
$(toASTReferences (makeReferences ''Type))
$(toASTReferences (makeReferences ''Kind))
$(toASTReferences (makeReferences ''Context))
$(toASTReferences (makeReferences ''Assertion))
$(toASTReferences (makeReferences ''UExpr))
$(toASTReferences (makeReferences ''Stmt'))
$(toASTReferences (makeReferences ''CompStmt))
$(toASTReferences (makeReferences ''UValueBind))
$(toASTReferences (makeReferences ''Pattern))
$(toASTReferences (makeReferences ''PatternField))
$(toASTReferences (makeReferences ''Splice))
$(toASTReferences (makeReferences ''QQString))
$(toASTReferences (makeReferences ''UMatch))
$(toASTReferences (makeReferences ''UAlt'))
$(toASTReferences (makeReferences ''URhs))
$(toASTReferences (makeReferences ''UGuardedRhs))
$(toASTReferences (makeReferences ''UFieldUpdate))
$(toASTReferences (makeReferences ''Bracket))
$(toASTReferences (makeReferences ''TopLevelPragma))
$(toASTReferences (makeReferences ''Rule))
$(toASTReferences (makeReferences ''AnnotationSubject))
$(toASTReferences (makeReferences ''MinimalFormula))
$(toASTReferences (makeReferences ''ExprPragma))
$(toASTReferences (makeReferences ''SourceRange))
$(toASTReferences (makeReferences ''Number))
$(toASTReferences (makeReferences ''QuasiQuote))
$(toASTReferences (makeReferences ''URhsGuard))
$(toASTReferences (makeReferences ''ULocalBind))
$(toASTReferences (makeReferences ''ULocalBinds))
$(toASTReferences (makeReferences ''UFixitySignature))
$(toASTReferences (makeReferences ''UTypeSignature))
$(toASTReferences (makeReferences ''ListCompBody))
$(toASTReferences (makeReferences ''UTupSecElem))
$(toASTReferences (makeReferences ''UTypeFamily))
$(toASTReferences (makeReferences ''UTypeFamilySpec))
$(toASTReferences (makeReferences ''UInjectivityAnn))
$(toASTReferences (makeReferences ''UCaseRhs'))
$(toASTReferences (makeReferences ''UGuardedCaseRhs'))
$(toASTReferences (makeReferences ''UPatternSynonym))
$(toASTReferences (makeReferences ''UPatSynRhs))
$(toASTReferences (makeReferences ''UPatSynLhs))
$(toASTReferences (makeReferences ''UPatSynWhere))
$(toASTReferences (makeReferences ''UPatternTypeSignature))
$(toASTReferences (makeReferences ''Role))
$(toASTReferences (makeReferences ''LanguageExtension))
$(toASTReferences (makeReferences ''UMatchLhs))

-- Literal
$(toASTReferences (makeReferences ''Literal))
$(toASTReferences (makeReferences ''Promoted))

-- Base
$(toASTReferences (makeReferences ''UOperator))
$(toASTReferences (makeReferences ''UName))
$(toASTReferences (makeReferences ''UQualifiedName))
$(toASTReferences (makeReferences ''UModuleName))
$(toASTReferences (makeReferences ''UNamePart))
$(toASTReferences (makeReferences ''UStringNode))
$(toASTReferences (makeReferences ''UDataOrNewtypeKeyword))
$(toASTReferences (makeReferences ''UDoKind))
$(toASTReferences (makeReferences ''TypeKeyword))
$(toASTReferences (makeReferences ''OverlapPragma))
$(toASTReferences (makeReferences ''CallConv))
$(toASTReferences (makeReferences ''ArrowAppl))
$(toASTReferences (makeReferences ''Safety))
$(toASTReferences (makeReferences ''Assoc))
$(toASTReferences (makeReferences ''Precedence))
$(toASTReferences (makeReferences ''PhaseControl))
$(toASTReferences (makeReferences ''PhaseNumber))
$(toASTReferences (makeReferences ''PhaseInvert))
