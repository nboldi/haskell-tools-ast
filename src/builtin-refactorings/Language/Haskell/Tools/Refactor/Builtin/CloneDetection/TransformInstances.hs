{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Haskell.Tools.Refactor.Builtin.CloneDetection.TransformInstances where

import Language.Haskell.Tools.Refactor.Builtin.CloneDetection.Transform
import GHC.Generics (Generic(..))
import Data.Data
import Language.Haskell.Tools.AST

-- Annotations
instance {-# OVERLAPPABLE #-} (CCSerialize e dom st, ShowSrcInfo st, DomainWith e dom, Typeable e, HasNameInfo dom) => CCSerialize (Ann e) dom st where
  put ann@(Ann _ e) = mkNode (Info (getRange ann) (toConstr e) Nothing) [put e]
--
instance {-# OVERLAPPING #-} (CCSerialize UQualifiedName dom st, ShowSrcInfo st, DomainWith UQualifiedName dom, HasNameInfo dom) => CCSerialize (Ann UQualifiedName) dom st where
  put ann@(Ann _ e) = mkNode (Info (getRange ann) (toConstr e) (fmap Left (semanticsName ann))) [put e]

instance (CCSerialize e dom st, ShowSrcInfo st, DomainWith e dom, Typeable e, HasNameInfo dom) => CCSerialize (AnnListG e) dom st where
  put (AnnListG _ ls) = mkNode NoInfo (map put ls)

instance (CCSerialize e dom st, ShowSrcInfo st, DomainWith e dom, Typeable e, HasNameInfo dom) => CCSerialize (AnnMaybeG e) dom st where
  put (AnnMaybeG _ e) = mkNode NoInfo (maybe [] ((:[]) . put) e)

-- Modules
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UModule dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UModuleHead dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UExportSpecs dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UExportSpec dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UIESpec dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize USubSpec dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UModulePragma dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UFilePragma dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UImportDecl dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UImportSpec dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UImportModifier dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UImportQualified dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UImportSource dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UImportSafe dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UTypeNamespace dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UImportRenaming dom st

-- Declarations
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UDecl dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UClassBody dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UClassElement dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UDeclHead dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UInstBody dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UInstBodyDecl dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UGadtConDecl dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UGadtConType dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UFieldWildcard dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UFunDeps dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UFunDep dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UConDecl dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UFieldDecl dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UDeriving dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UDeriveStrategy dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UInstanceRule dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UInstanceHead dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UTypeEqn dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UKindConstraint dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UTyVar dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UType dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UKind dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UContext dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UAssertion dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UExpr dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, CCSerialize expr dom st, Generic (expr dom st), DomainWith expr dom, Typeable expr, HasNameInfo dom) => CCSerialize (UStmt' expr) dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UCompStmt dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UValueBind dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UPattern dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UPatternField dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize USplice dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize QQString dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UMatch dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, CCSerialize expr dom st, Generic (expr dom st), DomainWith expr dom, Typeable expr, HasNameInfo dom) => CCSerialize (UAlt' expr) dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize URhs dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UGuardedRhs dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UFieldUpdate dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UBracket dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UTopLevelPragma dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize URule dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize URuleVar dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UAnnotationSubject dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UMinimalFormula dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UExprPragma dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize USourceRange dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize Number dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UQuasiQuote dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize URhsGuard dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize ULocalBind dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize ULocalBinds dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UFixitySignature dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UTypeSignature dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UListCompBody dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UTupSecElem dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UTypeFamily dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UTypeFamilySpec dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UInjectivityAnn dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, CCSerialize expr dom st, Generic (expr dom st), DomainWith expr dom, Typeable expr, HasNameInfo dom) => CCSerialize (UCaseRhs' expr) dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, CCSerialize expr dom st, Generic (expr dom st), DomainWith expr dom, Typeable expr, HasNameInfo dom) => CCSerialize (UGuardedCaseRhs' expr) dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UPatternSynonym dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UPatSynRhs dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UPatSynLhs dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UPatSynWhere dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UPatternTypeSignature dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize URole dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UCmd dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize ULanguageExtension dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UMatchLhs dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UInlinePragma dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize USpecializePragma dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UUnboxedSumPlaceHolder dom st

-- ULiteral
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize ULiteral dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, CCSerialize k dom st, Generic (k dom st), DomainWith k dom, Typeable k, HasNameInfo dom) => CCSerialize (UPromoted k) dom st

-- Base
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UOperator dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UName dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UQualifiedName dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UModuleName dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UNamePart dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UStringNode dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UDataOrNewtypeKeyword dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UDoKind dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize TypeKeyword dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UOverlapPragma dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UCallConv dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UArrowAppl dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize USafety dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UConlikeAnnot dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize Assoc dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize Precedence dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize LineNumber dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize UPhaseControl dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize PhaseNumber dom st
instance (SourceInfo st, ShowSrcInfo st, Domain dom, HasNameInfo dom) => CCSerialize PhaseInvert dom st
