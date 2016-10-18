-- | Generic instance for Haskell AST representation
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, DeriveGeneric #-}
module Language.Haskell.Tools.AST.Instances.Generic where

import GHC.Generics

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

-- Annotations
deriving instance (Domain dom, SourceInfo stage, Generic (e dom stage)) => Generic (Ann e dom stage)
deriving instance (Domain dom, SourceInfo stage, Generic (e dom stage)) => Generic (AnnMaybe e dom stage)
deriving instance (Domain dom, SourceInfo stage, Generic (e dom stage)) => Generic (AnnList e dom stage)

-- Modules
deriving instance (Domain dom, SourceInfo stage) => Generic (Module dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ModuleHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ExportSpecList dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ExportSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (IESpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (SubSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ModulePragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (FilePragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ImportDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ImportSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ImportQualified dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ImportSource dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ImportSafe dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (TypeNamespace dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ImportRenaming dom stage)

-- Declarations
deriving instance (Domain dom, SourceInfo stage) => Generic (UDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UClassBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UClassElement dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UDeclHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UInstBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UInstBodyDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UGadtConDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UGadtConType dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (FieldWildcard dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UFunDeps dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UFunDep dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UConDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UFieldDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UDeriving dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UInstanceRule dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UInstanceHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UTypeEqn dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (KindConstraint dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (TyVar dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Type dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Kind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Context dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Assertion dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Expr dom stage)
deriving instance (Domain dom, SourceInfo stage, Generic (expr dom stage)) => Generic (Stmt' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (CompStmt dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UValueBind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Pattern dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (PatternField dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Splice dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (QQString dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UMatch dom stage)
deriving instance (Domain dom, SourceInfo stage, Generic (expr dom stage)) => Generic (Alt' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (URhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UGuardedRhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (FieldUpdate dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Bracket dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (TopLevelPragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Rule dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (AnnotationSubject dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (MinimalFormula dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ExprPragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (SourceRange dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Number dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (QuasiQuote dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (URhsGuard dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ULocalBind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ULocalBinds dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UFixitySignature dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UTypeSignature dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ListCompBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (TupSecElem dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UTypeFamily dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UTypeFamilySpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UInjectivityAnn dom stage)
deriving instance (Domain dom, SourceInfo stage, Generic (expr dom stage)) => Generic (CaseRhs' expr dom stage)
deriving instance (Domain dom, SourceInfo stage, Generic (expr dom stage)) => Generic (GuardedCaseRhs' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UPatternSynonym dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UPatSynRhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UPatSynLhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UPatSynWhere dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UPatternTypeSignature dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Role dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Cmd dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (LanguageExtension dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UMatchLhs dom stage)


-- Literal
deriving instance (Domain dom, SourceInfo stage) => Generic (Literal dom stage)
deriving instance (Domain dom, SourceInfo stage, Generic (k dom stage)) => Generic (Promoted k dom stage)

-- Base
deriving instance (Domain dom, SourceInfo stage) => Generic (UOperator dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UName dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UQualifiedName dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UModuleName dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UNamePart dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UStringNode dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UDataOrNewtypeKeyword dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UDoKind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (TypeKeyword dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (OverlapPragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (CallConv dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ArrowAppl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Safety dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ConlikeAnnot dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Assoc dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Precedence dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (LineNumber dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (PhaseControl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (PhaseNumber dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (PhaseInvert dom stage)