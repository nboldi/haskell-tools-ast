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
deriving instance (Domain dom, SourceInfo stage) => Generic (UModule dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UModuleHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UExportSpecList dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UExportSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UIESpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (USubSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UModulePragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UFilePragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UImportDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UImportSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UImportQualified dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UImportSource dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UImportSafe dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UTypeNamespace dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UImportRenaming dom stage)

-- Declarations
deriving instance (Domain dom, SourceInfo stage) => Generic (UDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UClassBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UClassElement dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UDeclHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UInstBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UInstBodyDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UGadtConDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UGadtConType dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UFieldWildcard dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UFunDeps dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UFunDep dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UConDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UFieldDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UDeriving dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UInstanceRule dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UInstanceHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UTypeEqn dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UKindConstraint dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (TyVar dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Type dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UKind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Context dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Assertion dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UExpr dom stage)
deriving instance (Domain dom, SourceInfo stage, Generic (expr dom stage)) => Generic (UStmt' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UCompStmt dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UValueBind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UPattern dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UPatternField dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Splice dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (QQString dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UMatch dom stage)
deriving instance (Domain dom, SourceInfo stage, Generic (expr dom stage)) => Generic (UAlt' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (URhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UGuardedRhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UFieldUpdate dom stage)
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
deriving instance (Domain dom, SourceInfo stage) => Generic (UListCompBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UTupSecElem dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UTypeFamily dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UTypeFamilySpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UInjectivityAnn dom stage)
deriving instance (Domain dom, SourceInfo stage, Generic (expr dom stage)) => Generic (UCaseRhs' expr dom stage)
deriving instance (Domain dom, SourceInfo stage, Generic (expr dom stage)) => Generic (UGuardedCaseRhs' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UPatternSynonym dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UPatSynRhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UPatSynLhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UPatSynWhere dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UPatternTypeSignature dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Role dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (Cmd dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (ULanguageExtension dom stage)
deriving instance (Domain dom, SourceInfo stage) => Generic (UMatchLhs dom stage)


-- ULiteral
deriving instance (Domain dom, SourceInfo stage) => Generic (ULiteral dom stage)
deriving instance (Domain dom, SourceInfo stage, Generic (k dom stage)) => Generic (UPromoted k dom stage)

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