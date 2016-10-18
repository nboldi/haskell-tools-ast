-- | Equality check of AST nodes that ignore the source and semantic information.
{-# LANGUAGE FlexibleContexts, StandaloneDeriving #-}
module Language.Haskell.Tools.AST.Instances.Eq where

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
instance (Eq (e dom stage)) => Eq (Ann e dom stage) where
  Ann _ e1 == Ann _ e2 = e1 == e2

instance (Eq (e dom stage)) => Eq (AnnMaybe e dom stage) where
  AnnMaybe _ e1 == AnnMaybe _ e2 = e1 == e2

instance (Eq (e dom stage)) => Eq (AnnList e dom stage) where
  AnnList e1 == AnnList e2 = e1 == e2

-- Modules
deriving instance Eq (Module dom stage)
deriving instance Eq (ModuleHead dom stage)
deriving instance Eq (ExportSpecList dom stage)
deriving instance Eq (ExportSpec dom stage)
deriving instance Eq (IESpec dom stage)
deriving instance Eq (SubSpec dom stage)
deriving instance Eq (ModulePragma dom stage)
deriving instance Eq (FilePragma dom stage)
deriving instance Eq (ImportDecl dom stage)
deriving instance Eq (ImportSpec dom stage)
deriving instance Eq (ImportQualified dom stage)
deriving instance Eq (ImportSource dom stage)
deriving instance Eq (ImportSafe dom stage)
deriving instance Eq (TypeNamespace dom stage)
deriving instance Eq (ImportRenaming dom stage)

-- Declarations
deriving instance Eq (UDecl dom stage)
deriving instance Eq (UClassBody dom stage)
deriving instance Eq (UClassElement dom stage)
deriving instance Eq (UDeclHead dom stage)
deriving instance Eq (UInstBody dom stage)
deriving instance Eq (UInstBodyDecl dom stage)
deriving instance Eq (UGadtConDecl dom stage)
deriving instance Eq (UGadtConType dom stage)
deriving instance Eq (UFieldWildcard dom stage)
deriving instance Eq (UFunDeps dom stage)
deriving instance Eq (UFunDep dom stage)
deriving instance Eq (UConDecl dom stage)
deriving instance Eq (UFieldDecl dom stage)
deriving instance Eq (UDeriving dom stage)
deriving instance Eq (UInstanceRule dom stage)
deriving instance Eq (UInstanceHead dom stage)
deriving instance Eq (UTypeEqn dom stage)
deriving instance Eq (KindConstraint dom stage)
deriving instance Eq (TyVar dom stage)
deriving instance Eq (Type dom stage)
deriving instance Eq (Kind dom stage)
deriving instance Eq (Context dom stage)
deriving instance Eq (Assertion dom stage)
deriving instance Eq (UExpr dom stage)
deriving instance Eq (expr dom stage) => Eq (Stmt' expr dom stage)
deriving instance Eq (CompStmt dom stage)
deriving instance Eq (UValueBind dom stage)
deriving instance Eq (Pattern dom stage)
deriving instance Eq (PatternField dom stage)
deriving instance Eq (Splice dom stage)
deriving instance Eq (QQString dom stage)
deriving instance Eq (UMatch dom stage)
deriving instance Eq (expr dom stage) => Eq (UAlt' expr dom stage)
deriving instance Eq (URhs dom stage)
deriving instance Eq (UGuardedRhs dom stage)
deriving instance Eq (UFieldUpdate dom stage)
deriving instance Eq (Bracket dom stage)
deriving instance Eq (TopLevelPragma dom stage)
deriving instance Eq (Rule dom stage)
deriving instance Eq (AnnotationSubject dom stage)
deriving instance Eq (MinimalFormula dom stage)
deriving instance Eq (ExprPragma dom stage)
deriving instance Eq (SourceRange dom stage)
deriving instance Eq (Number dom stage)
deriving instance Eq (QuasiQuote dom stage)
deriving instance Eq (URhsGuard dom stage)
deriving instance Eq (ULocalBind dom stage)
deriving instance Eq (ULocalBinds dom stage)
deriving instance Eq (UFixitySignature dom stage)
deriving instance Eq (UTypeSignature dom stage)
deriving instance Eq (ListCompBody dom stage)
deriving instance Eq (UTupSecElem dom stage)
deriving instance Eq (UTypeFamily dom stage)
deriving instance Eq (UTypeFamilySpec dom stage)
deriving instance Eq (UInjectivityAnn dom stage)
deriving instance Eq (expr dom stage) => Eq (UCaseRhs' expr dom stage)
deriving instance Eq (expr dom stage) => Eq (UGuardedCaseRhs' expr dom stage)
deriving instance Eq (UPatternSynonym dom stage)
deriving instance Eq (UPatSynRhs dom stage)
deriving instance Eq (UPatSynLhs dom stage)
deriving instance Eq (UPatSynWhere dom stage)
deriving instance Eq (UPatternTypeSignature dom stage)
deriving instance Eq (Role dom stage)
deriving instance Eq (Cmd dom stage)
deriving instance Eq (LanguageExtension dom stage)
deriving instance Eq (UMatchLhs dom stage)

-- Literal
deriving instance Eq (Literal dom stage)
deriving instance Eq (k dom stage) => Eq (Promoted k dom stage)

-- Base
deriving instance Eq (UOperator dom stage)
deriving instance Eq (UName dom stage)
deriving instance Eq (UQualifiedName dom stage)
deriving instance Eq (UModuleName dom stage)
deriving instance Eq (UNamePart dom stage)
deriving instance Eq (UStringNode dom stage)
deriving instance Eq (UDataOrNewtypeKeyword dom stage)
deriving instance Eq (UDoKind dom stage)
deriving instance Eq (TypeKeyword dom stage)
deriving instance Eq (OverlapPragma dom stage)
deriving instance Eq (CallConv dom stage)
deriving instance Eq (ArrowAppl dom stage)
deriving instance Eq (Safety dom stage)
deriving instance Eq (ConlikeAnnot dom stage)
deriving instance Eq (Assoc dom stage)
deriving instance Eq (Precedence dom stage)
deriving instance Eq (LineNumber dom stage)
deriving instance Eq (PhaseControl dom stage)
deriving instance Eq (PhaseNumber dom stage)
deriving instance Eq (PhaseInvert dom stage)