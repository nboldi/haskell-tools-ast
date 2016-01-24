{-# LANGUAGE FlexibleContexts, TemplateHaskell #-}
module Language.Haskell.Tools.AST.Instances.Lenses where

import Control.Lens hiding (Context)

import Language.Haskell.Tools.AST.Module
import Language.Haskell.Tools.AST.Decl
import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Ann

-- Annotations
makeLenses ''Ann
makeLenses ''AnnList
makeLenses ''AnnMaybe

-- Modules
makeLenses ''Module
makeLenses ''ModuleHead
makeLenses ''ExportSpecList
makeLenses ''ExportSpec
makeLenses ''IESpec
makeLenses ''SubSpec
makeLenses ''ModulePragma
makeLenses ''ImportDecl
makeLenses ''ImportSpec
makeLenses ''ImportQualified
makeLenses ''ImportSource
makeLenses ''ImportSafe
makeLenses ''TypeNamespace
makeLenses ''ImportRenaming

-- Declarations
makeLenses ''Decl
makeLenses ''ClassBody
makeLenses ''GadtDeclList
makeLenses ''ClassElement
makeLenses ''DeclHead
makeLenses ''InstBody
makeLenses ''InstBodyDecl
makeLenses ''GadtDecl
makeLenses ''GadtField
makeLenses ''FunDeps
makeLenses ''FunDep
makeLenses ''ConDecl
makeLenses ''FieldDecl
makeLenses ''Deriving
makeLenses ''InstanceRule
makeLenses ''InstanceHead
makeLenses ''TypeEqn
makeLenses ''KindConstraint
makeLenses ''TyVar
makeLenses ''Type
makeLenses ''Kind
makeLenses ''Context
makeLenses ''Assertion
makeLenses ''Expr
makeLenses ''Stmt
makeLenses ''CompStmt
makeLenses ''ValueBind
makeLenses ''Pattern
makeLenses ''PatternField
makeLenses ''Splice
makeLenses ''QQString
makeLenses ''Match
makeLenses ''Alt
makeLenses ''Rhs
makeLenses ''GuardedRhs
makeLenses ''FieldUpdate
makeLenses ''Bracket
makeLenses ''TopLevelPragma
makeLenses ''Rule
makeLenses ''Annotation
makeLenses ''MinimalFormula
makeLenses ''ExprPragma
makeLenses ''SourceRange
makeLenses ''Number
makeLenses ''QuasiQuote
makeLenses ''RhsGuard
makeLenses ''LocalBind
makeLenses ''LocalBinds
makeLenses ''FixitySignature
makeLenses ''TypeSignature
makeLenses ''ListCompBody
makeLenses ''TupSecElem
makeLenses ''TypeFamily

-- Literal
makeLenses ''Literal
makeLenses ''Promoted

-- Base
makeLenses ''Name
makeLenses ''SimpleName
makeLenses ''StringNode
makeLenses ''DataOrNewtypeKeyword
makeLenses ''DoKind
makeLenses ''TypeKeyword
makeLenses ''OverlapPragma
makeLenses ''CallConv
makeLenses ''ArrowAppl
makeLenses ''Safety
makeLenses ''Assoc
makeLenses ''Precedence
makeLenses ''PhaseControl
makeLenses ''PhaseNumber
makeLenses ''PhaseInvert