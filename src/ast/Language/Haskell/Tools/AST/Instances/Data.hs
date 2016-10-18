-- | Data instances for Haskell AST (used for generics)
{-# LANGUAGE FlexibleContexts, StandaloneDeriving, DeriveDataTypeable #-}
module Language.Haskell.Tools.AST.Instances.Data where

import Data.Data

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
deriving instance (DomainWith e dom, SourceInfo stage, Typeable e, Data (e dom stage)) => Data (Ann e dom stage)
deriving instance (DomainWith e dom, SourceInfo stage, Typeable e, Data (e dom stage)) => Data (AnnMaybe e dom stage)
deriving instance (DomainWith e dom, SourceInfo stage, Typeable e, Data (e dom stage)) => Data (AnnList e dom stage)

-- Modules
deriving instance (Domain dom, SourceInfo stage) => Data (Module dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ModuleHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ExportSpecList dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ExportSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (IESpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (SubSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ModulePragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (FilePragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ImportDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ImportSpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ImportQualified dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ImportSource dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ImportSafe dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (TypeNamespace dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ImportRenaming dom stage)

-- Declarations
deriving instance (Domain dom, SourceInfo stage) => Data (UDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UClassBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UClassElement dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UDeclHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UInstBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UInstBodyDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UGadtConDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UGadtConType dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (FieldWildcard dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UFunDeps dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UFunDep dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UConDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UFieldDecl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UDeriving dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UInstanceRule dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UInstanceHead dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UTypeEqn dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (KindConstraint dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (TyVar dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Type dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Kind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Context dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Assertion dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Expr dom stage)
deriving instance (DomainWith expr dom, SourceInfo stage, Typeable expr, Data (expr dom stage)) => Data (Stmt' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (CompStmt dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UValueBind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Pattern dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (PatternField dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Splice dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (QQString dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UMatch dom stage)
deriving instance (DomainWith expr dom, SourceInfo stage, Typeable expr, Data (expr dom stage)) => Data (Alt' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (URhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UGuardedRhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (FieldUpdate dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Bracket dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (TopLevelPragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Rule dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (AnnotationSubject dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (MinimalFormula dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ExprPragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (SourceRange dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Number dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (QuasiQuote dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (URhsGuard dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ULocalBind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ULocalBinds dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UFixitySignature dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UTypeSignature dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ListCompBody dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (TupSecElem dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UTypeFamily dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UTypeFamilySpec dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UInjectivityAnn dom stage)
deriving instance (DomainWith expr dom, SourceInfo stage, Typeable expr, Data (expr dom stage)) => Data (CaseRhs' expr dom stage)
deriving instance (DomainWith expr dom, SourceInfo stage, Typeable expr, Data (expr dom stage))=> Data (GuardedCaseRhs' expr dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UPatternSynonym dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UPatSynRhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UPatSynLhs dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UPatSynWhere dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UPatternTypeSignature dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Role dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Cmd dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (LanguageExtension dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UMatchLhs dom stage)

-- Literal
deriving instance (Domain dom, SourceInfo stage) => Data (Literal dom stage)
deriving instance (DomainWith k dom, SourceInfo stage, Typeable k, Data (k dom stage)) => Data (Promoted k dom stage)

-- Base
deriving instance (Domain dom, SourceInfo stage) => Data (UOperator dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UName dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UQualifiedName dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UModuleName dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UNamePart dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UStringNode dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UDataOrNewtypeKeyword dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (UDoKind dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (TypeKeyword dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (OverlapPragma dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (CallConv dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ArrowAppl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Safety dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (ConlikeAnnot dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Assoc dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (Precedence dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (LineNumber dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (PhaseControl dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (PhaseNumber dom stage)
deriving instance (Domain dom, SourceInfo stage) => Data (PhaseInvert dom stage)