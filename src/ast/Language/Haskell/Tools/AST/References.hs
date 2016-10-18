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
$(toASTReferences (makeReferences ''Decl))
$(toASTReferences (makeReferences ''ClassBody))
$(toASTReferences (makeReferences ''ClassElement))
$(toASTReferences (makeReferences ''DeclHead))
$(toASTReferences (makeReferences ''InstBody))
$(toASTReferences (makeReferences ''InstBodyDecl))
$(toASTReferences (makeReferences ''GadtConDecl))
$(toASTReferences (makeReferences ''GadtConType))
$(toASTReferences (makeReferences ''FunDeps))
$(toASTReferences (makeReferences ''FunDep))
$(toASTReferences (makeReferences ''ConDecl))
$(toASTReferences (makeReferences ''FieldDecl))
$(toASTReferences (makeReferences ''Deriving))
$(toASTReferences (makeReferences ''InstanceRule))
$(toASTReferences (makeReferences ''InstanceHead))
$(toASTReferences (makeReferences ''TypeEqn))
$(toASTReferences (makeReferences ''KindConstraint))
$(toASTReferences (makeReferences ''TyVar))
$(toASTReferences (makeReferences ''Type))
$(toASTReferences (makeReferences ''Kind))
$(toASTReferences (makeReferences ''Context))
$(toASTReferences (makeReferences ''Assertion))
$(toASTReferences (makeReferences ''Expr))
$(toASTReferences (makeReferences ''Stmt'))
$(toASTReferences (makeReferences ''CompStmt))
$(toASTReferences (makeReferences ''UValueBind))
$(toASTReferences (makeReferences ''Pattern))
$(toASTReferences (makeReferences ''PatternField))
$(toASTReferences (makeReferences ''Splice))
$(toASTReferences (makeReferences ''QQString))
$(toASTReferences (makeReferences ''UMatch))
$(toASTReferences (makeReferences ''Alt'))
$(toASTReferences (makeReferences ''URhs))
$(toASTReferences (makeReferences ''UGuardedRhs))
$(toASTReferences (makeReferences ''FieldUpdate))
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
$(toASTReferences (makeReferences ''TupSecElem))
$(toASTReferences (makeReferences ''TypeFamily))
$(toASTReferences (makeReferences ''TypeFamilySpec))
$(toASTReferences (makeReferences ''InjectivityAnn))
$(toASTReferences (makeReferences ''CaseRhs'))
$(toASTReferences (makeReferences ''GuardedCaseRhs'))
$(toASTReferences (makeReferences ''PatternSynonym))
$(toASTReferences (makeReferences ''PatSynRhs))
$(toASTReferences (makeReferences ''PatSynLhs))
$(toASTReferences (makeReferences ''PatSynWhere))
$(toASTReferences (makeReferences ''PatternTypeSignature))
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
