-- | Generic instance for Haskell AST representation
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Haskell.Tools.AST.Instances.Generic () where

import GHC.Generics (Generic(..))

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Representation.Binds
import Language.Haskell.Tools.AST.Representation.Decls
import Language.Haskell.Tools.AST.Representation.Exprs
import Language.Haskell.Tools.AST.Representation.Kinds (UPromoted(..), UKind(..), UKindConstraint(..))
import Language.Haskell.Tools.AST.Representation.Literals (ULiteral(..))
import Language.Haskell.Tools.AST.Representation.Modules
import Language.Haskell.Tools.AST.Representation.Names
import Language.Haskell.Tools.AST.Representation.Patterns (UPatternField(..), UPattern(..))
import Language.Haskell.Tools.AST.Representation.Stmts
import Language.Haskell.Tools.AST.Representation.TH
import Language.Haskell.Tools.AST.Representation.Types

-- Annotations
deriving instance Domain dom => Generic (Ann e dom stage)
deriving instance Domain dom => Generic (AnnMaybeG e dom stage)
deriving instance Domain dom => Generic (AnnListG e dom stage)

-- Modules
deriving instance Domain dom => Generic (UModule dom stage)
deriving instance Domain dom => Generic (UModuleHead dom stage)
deriving instance Domain dom => Generic (UExportSpecs dom stage)
deriving instance Domain dom => Generic (UExportSpec dom stage)
deriving instance Domain dom => Generic (UIESpec dom stage)
deriving instance Domain dom => Generic (USubSpec dom stage)
deriving instance Domain dom => Generic (UModulePragma dom stage)
deriving instance Domain dom => Generic (UFilePragma dom stage)
deriving instance Domain dom => Generic (UImportDecl dom stage)
deriving instance Domain dom => Generic (UImportSpec dom stage)
deriving instance Domain dom => Generic (UImportModifier dom stage)
deriving instance Domain dom => Generic (UImportQualified dom stage)
deriving instance Domain dom => Generic (UImportSource dom stage)
deriving instance Domain dom => Generic (UImportSafe dom stage)
deriving instance Domain dom => Generic (UTypeNamespace dom stage)
deriving instance Domain dom => Generic (UImportRenaming dom stage)

-- Declarations
deriving instance Domain dom => Generic (UDecl dom stage)
deriving instance Domain dom => Generic (UClassBody dom stage)
deriving instance Domain dom => Generic (UClassElement dom stage)
deriving instance Domain dom => Generic (UDeclHead dom stage)
deriving instance Domain dom => Generic (UInstBody dom stage)
deriving instance Domain dom => Generic (UInstBodyDecl dom stage)
deriving instance Domain dom => Generic (UGadtConDecl dom stage)
deriving instance Domain dom => Generic (UGadtConType dom stage)
deriving instance Domain dom => Generic (UFieldWildcard dom stage)
deriving instance Domain dom => Generic (UFunDeps dom stage)
deriving instance Domain dom => Generic (UFunDep dom stage)
deriving instance Domain dom => Generic (UConDecl dom stage)
deriving instance Domain dom => Generic (UFieldDecl dom stage)
deriving instance Domain dom => Generic (UDeriving dom stage)
deriving instance Domain dom => Generic (UDeriveStrategy dom stage)
deriving instance Domain dom => Generic (UInstanceRule dom stage)
deriving instance Domain dom => Generic (UInstanceHead dom stage)
deriving instance Domain dom => Generic (UTypeEqn dom stage)
deriving instance Domain dom => Generic (UKindConstraint dom stage)
deriving instance Domain dom => Generic (UTyVar dom stage)
deriving instance Domain dom => Generic (UType dom stage)
deriving instance Domain dom => Generic (UKind dom stage)
deriving instance Domain dom => Generic (UContext dom stage)
deriving instance Domain dom => Generic (UAssertion dom stage)
deriving instance Domain dom => Generic (UExpr dom stage)
deriving instance Domain dom => Generic (UStmt' expr dom stage)
deriving instance Domain dom => Generic (UCompStmt dom stage)
deriving instance Domain dom => Generic (UValueBind dom stage)
deriving instance Domain dom => Generic (UPattern dom stage)
deriving instance Domain dom => Generic (UPatternField dom stage)
deriving instance Domain dom => Generic (USplice dom stage)
deriving instance Domain dom => Generic (QQString dom stage)
deriving instance Domain dom => Generic (UMatch dom stage)
deriving instance Domain dom => Generic (UAlt' expr dom stage)
deriving instance Domain dom => Generic (URhs dom stage)
deriving instance Domain dom => Generic (UGuardedRhs dom stage)
deriving instance Domain dom => Generic (UFieldUpdate dom stage)
deriving instance Domain dom => Generic (UBracket dom stage)
deriving instance Domain dom => Generic (UTopLevelPragma dom stage)
deriving instance Domain dom => Generic (URule dom stage)
deriving instance Domain dom => Generic (URuleVar dom stage)
deriving instance Domain dom => Generic (UAnnotationSubject dom stage)
deriving instance Domain dom => Generic (UMinimalFormula dom stage)
deriving instance Domain dom => Generic (UExprPragma dom stage)
deriving instance Domain dom => Generic (USourceRange dom stage)
deriving instance Domain dom => Generic (Number dom stage)
deriving instance Domain dom => Generic (UQuasiQuote dom stage)
deriving instance Domain dom => Generic (URhsGuard dom stage)
deriving instance Domain dom => Generic (ULocalBind dom stage)
deriving instance Domain dom => Generic (ULocalBinds dom stage)
deriving instance Domain dom => Generic (UFixitySignature dom stage)
deriving instance Domain dom => Generic (UTypeSignature dom stage)
deriving instance Domain dom => Generic (UListCompBody dom stage)
deriving instance Domain dom => Generic (UTupSecElem dom stage)
deriving instance Domain dom => Generic (UTypeFamily dom stage)
deriving instance Domain dom => Generic (UTypeFamilySpec dom stage)
deriving instance Domain dom => Generic (UInjectivityAnn dom stage)
deriving instance Domain dom => Generic (UCaseRhs' expr dom stage)
deriving instance Domain dom => Generic (UGuardedCaseRhs' expr dom stage)
deriving instance Domain dom => Generic (UPatternSynonym dom stage)
deriving instance Domain dom => Generic (UPatSynRhs dom stage)
deriving instance Domain dom => Generic (UPatSynLhs dom stage)
deriving instance Domain dom => Generic (UPatSynWhere dom stage)
deriving instance Domain dom => Generic (UPatternTypeSignature dom stage)
deriving instance Domain dom => Generic (URole dom stage)
deriving instance Domain dom => Generic (UCmd dom stage)
deriving instance Domain dom => Generic (ULanguageExtension dom stage)
deriving instance Domain dom => Generic (UMatchLhs dom stage)
deriving instance Domain dom => Generic (UInlinePragma dom stage)
deriving instance Domain dom => Generic (USpecializePragma dom stage)
deriving instance Domain dom => Generic (UUnboxedSumPlaceHolder dom stage)


-- Literal
deriving instance Domain dom => Generic (ULiteral dom stage)
deriving instance Domain dom => Generic (UPromoted k dom stage)

-- Base
deriving instance Domain dom => Generic (UOperator dom stage)
deriving instance Domain dom => Generic (UName dom stage)
deriving instance Domain dom => Generic (UQualifiedName dom stage)
deriving instance Domain dom => Generic (UModuleName dom stage)
deriving instance Domain dom => Generic (UNamePart dom stage)
deriving instance Domain dom => Generic (UStringNode dom stage)
deriving instance Domain dom => Generic (UDataOrNewtypeKeyword dom stage)
deriving instance Domain dom => Generic (UDoKind dom stage)
deriving instance Domain dom => Generic (TypeKeyword dom stage)
deriving instance Domain dom => Generic (UOverlapPragma dom stage)
deriving instance Domain dom => Generic (UCallConv dom stage)
deriving instance Domain dom => Generic (UArrowAppl dom stage)
deriving instance Domain dom => Generic (USafety dom stage)
deriving instance Domain dom => Generic (UConlikeAnnot dom stage)
deriving instance Domain dom => Generic (Assoc dom stage)
deriving instance Domain dom => Generic (Precedence dom stage)
deriving instance Domain dom => Generic (LineNumber dom stage)
deriving instance Domain dom => Generic (UPhaseControl dom stage)
deriving instance Domain dom => Generic (PhaseNumber dom stage)
deriving instance Domain dom => Generic (PhaseInvert dom stage)
