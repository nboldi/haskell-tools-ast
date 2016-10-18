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
deriving instance Show (Module dom stage)
deriving instance Show (ModuleHead dom stage)
deriving instance Show (ExportSpecList dom stage)
deriving instance Show (ExportSpec dom stage)
deriving instance Show (IESpec dom stage)
deriving instance Show (SubSpec dom stage)
deriving instance Show (ModulePragma dom stage)
deriving instance Show (FilePragma dom stage)
deriving instance Show (ImportDecl dom stage)
deriving instance Show (ImportSpec dom stage)
deriving instance Show (ImportQualified dom stage)
deriving instance Show (ImportSource dom stage)
deriving instance Show (ImportSafe dom stage)
deriving instance Show (TypeNamespace dom stage)
deriving instance Show (ImportRenaming dom stage)

-- Declarations
deriving instance Show (Decl dom stage)
deriving instance Show (ClassBody dom stage)
deriving instance Show (ClassElement dom stage)
deriving instance Show (DeclHead dom stage)
deriving instance Show (InstBody dom stage)
deriving instance Show (InstBodyDecl dom stage)
deriving instance Show (GadtConDecl dom stage)
deriving instance Show (GadtConType dom stage)
deriving instance Show (FieldWildcard dom stage)
deriving instance Show (FunDeps dom stage)
deriving instance Show (FunDep dom stage)
deriving instance Show (ConDecl dom stage)
deriving instance Show (FieldDecl dom stage)
deriving instance Show (Deriving dom stage)
deriving instance Show (InstanceRule dom stage)
deriving instance Show (InstanceHead dom stage)
deriving instance Show (TypeEqn dom stage)
deriving instance Show (KindConstraint dom stage)
deriving instance Show (TyVar dom stage)
deriving instance Show (Type dom stage)
deriving instance Show (Kind dom stage)
deriving instance Show (Context dom stage)
deriving instance Show (Assertion dom stage)
deriving instance Show (Expr dom stage)
deriving instance Show (expr dom stage) => Show (Stmt' expr dom stage)
deriving instance Show (CompStmt dom stage)
deriving instance Show (UValueBind dom stage)
deriving instance Show (Pattern dom stage)
deriving instance Show (PatternField dom stage)
deriving instance Show (Splice dom stage)
deriving instance Show (QQString dom stage)
deriving instance Show (UMatch dom stage)
deriving instance Show (expr dom stage) => Show (Alt' expr dom stage)
deriving instance Show (URhs dom stage)
deriving instance Show (UGuardedRhs dom stage)
deriving instance Show (FieldUpdate dom stage)
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
deriving instance Show (ListCompBody dom stage)
deriving instance Show (TupSecElem dom stage)
deriving instance Show (TypeFamily dom stage)
deriving instance Show (TypeFamilySpec dom stage)
deriving instance Show (InjectivityAnn dom stage)
deriving instance Show (expr dom stage) => Show (CaseRhs' expr dom stage)
deriving instance Show (expr dom stage) => Show (GuardedCaseRhs' expr dom stage)
deriving instance Show (PatternSynonym dom stage)
deriving instance Show (PatSynRhs dom stage)
deriving instance Show (PatSynLhs dom stage)
deriving instance Show (PatSynWhere dom stage)
deriving instance Show (PatternTypeSignature dom stage)
deriving instance Show (Role dom stage)
deriving instance Show (Cmd dom stage)
deriving instance Show (LanguageExtension dom stage)
deriving instance Show (UMatchLhs dom stage)


-- Literal
deriving instance Show (Literal dom stage)
deriving instance Show (k dom stage) => Show (Promoted k dom stage)

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