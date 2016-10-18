-- | Show instance for Haskell AST representation ignoring source and semantic information
{-# LANGUAGE FlexibleContexts, StandaloneDeriving #-}
module Language.Haskell.Tools.AST.Instances.Show where

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
instance (Show (e dom stage)) => Show (Ann e dom stage) where
  show (Ann _ e) = show e

instance (Show (e dom stage)) => Show (AnnMaybe e dom stage) where
  show (AnnMaybe _ e) = show e
  
instance (Show (e dom stage)) => Show (AnnList e dom stage) where
  show (AnnList e) = show e

-- Modules
deriving instance Show (UModule dom stage)
deriving instance Show (UModuleHead dom stage)
deriving instance Show (UExportSpecList dom stage)
deriving instance Show (UExportSpec dom stage)
deriving instance Show (UIESpec dom stage)
deriving instance Show (USubSpec dom stage)
deriving instance Show (UModulePragma dom stage)
deriving instance Show (UFilePragma dom stage)
deriving instance Show (UImportDecl dom stage)
deriving instance Show (UImportSpec dom stage)
deriving instance Show (UImportQualified dom stage)
deriving instance Show (UImportSource dom stage)
deriving instance Show (UImportSafe dom stage)
deriving instance Show (UTypeNamespace dom stage)
deriving instance Show (UImportRenaming dom stage)

-- Declarations
deriving instance Show (UDecl dom stage)
deriving instance Show (UClassBody dom stage)
deriving instance Show (UClassElement dom stage)
deriving instance Show (UDeclHead dom stage)
deriving instance Show (UInstBody dom stage)
deriving instance Show (UInstBodyDecl dom stage)
deriving instance Show (UGadtConDecl dom stage)
deriving instance Show (UGadtConType dom stage)
deriving instance Show (UFieldWildcard dom stage)
deriving instance Show (UFunDeps dom stage)
deriving instance Show (UFunDep dom stage)
deriving instance Show (UConDecl dom stage)
deriving instance Show (UFieldDecl dom stage)
deriving instance Show (UDeriving dom stage)
deriving instance Show (UInstanceRule dom stage)
deriving instance Show (UInstanceHead dom stage)
deriving instance Show (UTypeEqn dom stage)
deriving instance Show (UKindConstraint dom stage)
deriving instance Show (TyVar dom stage)
deriving instance Show (Type dom stage)
deriving instance Show (UKind dom stage)
deriving instance Show (Context dom stage)
deriving instance Show (Assertion dom stage)
deriving instance Show (UExpr dom stage)
deriving instance Show (expr dom stage) => Show (UStmt' expr dom stage)
deriving instance Show (UCompStmt dom stage)
deriving instance Show (UValueBind dom stage)
deriving instance Show (UPattern dom stage)
deriving instance Show (UPatternField dom stage)
deriving instance Show (Splice dom stage)
deriving instance Show (QQString dom stage)
deriving instance Show (UMatch dom stage)
deriving instance Show (expr dom stage) => Show (UAlt' expr dom stage)
deriving instance Show (URhs dom stage)
deriving instance Show (UGuardedRhs dom stage)
deriving instance Show (UFieldUpdate dom stage)
deriving instance Show (Bracket dom stage)
deriving instance Show (TopLevelPragma dom stage)
deriving instance Show (Rule dom stage)
deriving instance Show (AnnotationSubject dom stage)
deriving instance Show (MinimalFormula dom stage)
deriving instance Show (ExprPragma dom stage)
deriving instance Show (SourceRange dom stage)
deriving instance Show (Number dom stage)
deriving instance Show (QuasiQuote dom stage)
deriving instance Show (URhsGuard dom stage)
deriving instance Show (ULocalBind dom stage)
deriving instance Show (ULocalBinds dom stage)
deriving instance Show (UFixitySignature dom stage)
deriving instance Show (UTypeSignature dom stage)
deriving instance Show (UListCompBody dom stage)
deriving instance Show (UTupSecElem dom stage)
deriving instance Show (UTypeFamily dom stage)
deriving instance Show (UTypeFamilySpec dom stage)
deriving instance Show (UInjectivityAnn dom stage)
deriving instance Show (expr dom stage) => Show (UCaseRhs' expr dom stage)
deriving instance Show (expr dom stage) => Show (UGuardedCaseRhs' expr dom stage)
deriving instance Show (UPatternSynonym dom stage)
deriving instance Show (UPatSynRhs dom stage)
deriving instance Show (UPatSynLhs dom stage)
deriving instance Show (UPatSynWhere dom stage)
deriving instance Show (UPatternTypeSignature dom stage)
deriving instance Show (Role dom stage)
deriving instance Show (Cmd dom stage)
deriving instance Show (ULanguageExtension dom stage)
deriving instance Show (UMatchLhs dom stage)


-- ULiteral
deriving instance Show (ULiteral dom stage)
deriving instance Show (k dom stage) => Show (UPromoted k dom stage)

-- Base
deriving instance Show (UOperator dom stage)
deriving instance Show (UName dom stage)
deriving instance Show (UQualifiedName dom stage)
deriving instance Show (UModuleName dom stage)
deriving instance Show (UNamePart dom stage)
deriving instance Show (UStringNode dom stage)
deriving instance Show (UDataOrNewtypeKeyword dom stage)
deriving instance Show (UDoKind dom stage)
deriving instance Show (TypeKeyword dom stage)
deriving instance Show (OverlapPragma dom stage)
deriving instance Show (CallConv dom stage)
deriving instance Show (ArrowAppl dom stage)
deriving instance Show (Safety dom stage)
deriving instance Show (ConlikeAnnot dom stage)
deriving instance Show (Assoc dom stage)
deriving instance Show (Precedence dom stage)
deriving instance Show (LineNumber dom stage)
deriving instance Show (PhaseControl dom stage)
deriving instance Show (PhaseNumber dom stage)
deriving instance Show (PhaseInvert dom stage)