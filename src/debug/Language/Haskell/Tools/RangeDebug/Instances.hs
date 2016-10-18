{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , StandaloneDeriving
           , DeriveGeneric
           , UndecidableInstances 
           #-}
module Language.Haskell.Tools.RangeDebug.Instances where

import Language.Haskell.Tools.RangeDebug

import GHC.Generics
import Control.Reference

import Language.Haskell.Tools.AST

-- Annotations
instance TreeDebug e dom st => TreeDebug (Ann e) dom st where
  treeDebug' i (Ann a e) = identLine i ++ show (a ^. sourceInfo) ++ " " ++ take 40 (show e) ++ "..." ++ treeDebug' (i+1) e
  
identLine :: Int -> String
identLine i = "\n" ++ replicate (i*2) ' '
  
instance TreeDebug e dom st => TreeDebug (AnnList e) dom st where
  treeDebug' i (AnnListC a ls) = identLine i ++ show (a ^. sourceInfo) ++ " <*>" ++ concatMap (treeDebug' (i + 1)) ls 
  
instance TreeDebug e dom st => TreeDebug (AnnMaybe e) dom st where
  treeDebug' i (AnnMaybe a e) = identLine i ++ show (a ^. sourceInfo) ++ " <?>" ++ maybe "" (\e -> treeDebug' (i + 1) e) e
  
-- Modules
instance (SourceInfo st, Domain dom) => TreeDebug UModule dom st
instance (SourceInfo st, Domain dom) => TreeDebug UModuleHead dom st
instance (SourceInfo st, Domain dom) => TreeDebug UExportSpecList dom st
instance (SourceInfo st, Domain dom) => TreeDebug UExportSpec dom st
instance (SourceInfo st, Domain dom) => TreeDebug UIESpec dom st
instance (SourceInfo st, Domain dom) => TreeDebug USubSpec dom st
instance (SourceInfo st, Domain dom) => TreeDebug UModulePragma dom st
instance (SourceInfo st, Domain dom) => TreeDebug UFilePragma dom st
instance (SourceInfo st, Domain dom) => TreeDebug UImportDecl dom st
instance (SourceInfo st, Domain dom) => TreeDebug UImportSpec dom st
instance (SourceInfo st, Domain dom) => TreeDebug UImportQualified dom st
instance (SourceInfo st, Domain dom) => TreeDebug UImportSource dom st
instance (SourceInfo st, Domain dom) => TreeDebug UImportSafe dom st
instance (SourceInfo st, Domain dom) => TreeDebug UTypeNamespace dom st
instance (SourceInfo st, Domain dom) => TreeDebug UImportRenaming dom st

-- Declarations
instance (SourceInfo st, Domain dom) => TreeDebug UDecl dom st
instance (SourceInfo st, Domain dom) => TreeDebug UClassBody dom st
instance (SourceInfo st, Domain dom) => TreeDebug UClassElement dom st
instance (SourceInfo st, Domain dom) => TreeDebug UDeclHead dom st
instance (SourceInfo st, Domain dom) => TreeDebug UInstBody dom st
instance (SourceInfo st, Domain dom) => TreeDebug UInstBodyDecl dom st
instance (SourceInfo st, Domain dom) => TreeDebug UGadtConDecl dom st
instance (SourceInfo st, Domain dom) => TreeDebug UGadtConType dom st
instance (SourceInfo st, Domain dom) => TreeDebug UFieldWildcard dom st
instance (SourceInfo st, Domain dom) => TreeDebug UFunDeps dom st
instance (SourceInfo st, Domain dom) => TreeDebug UFunDep dom st
instance (SourceInfo st, Domain dom) => TreeDebug UConDecl dom st
instance (SourceInfo st, Domain dom) => TreeDebug UFieldDecl dom st
instance (SourceInfo st, Domain dom) => TreeDebug UDeriving dom st
instance (SourceInfo st, Domain dom) => TreeDebug UInstanceRule dom st
instance (SourceInfo st, Domain dom) => TreeDebug UInstanceHead dom st
instance (SourceInfo st, Domain dom) => TreeDebug UTypeEqn dom st
instance (SourceInfo st, Domain dom) => TreeDebug UKindConstraint dom st
instance (SourceInfo st, Domain dom) => TreeDebug TyVar dom st
instance (SourceInfo st, Domain dom) => TreeDebug Type dom st
instance (SourceInfo st, Domain dom) => TreeDebug UKind dom st
instance (SourceInfo st, Domain dom) => TreeDebug Context dom st
instance (SourceInfo st, Domain dom) => TreeDebug Assertion dom st
instance (SourceInfo st, Domain dom) => TreeDebug UExpr dom st
instance (SourceInfo st, Domain dom, TreeDebug expr dom st, Generic (expr dom st)) => TreeDebug (Stmt' expr) dom st
instance (SourceInfo st, Domain dom) => TreeDebug CompStmt dom st
instance (SourceInfo st, Domain dom) => TreeDebug UValueBind dom st
instance (SourceInfo st, Domain dom) => TreeDebug Pattern dom st
instance (SourceInfo st, Domain dom) => TreeDebug PatternField dom st
instance (SourceInfo st, Domain dom) => TreeDebug Splice dom st
instance (SourceInfo st, Domain dom) => TreeDebug QQString dom st
instance (SourceInfo st, Domain dom) => TreeDebug UMatch dom st
instance (SourceInfo st, Domain dom, TreeDebug expr dom st, Generic (expr dom st)) => TreeDebug (UAlt' expr) dom st
instance (SourceInfo st, Domain dom) => TreeDebug URhs dom st
instance (SourceInfo st, Domain dom) => TreeDebug UGuardedRhs dom st
instance (SourceInfo st, Domain dom) => TreeDebug UFieldUpdate dom st
instance (SourceInfo st, Domain dom) => TreeDebug Bracket dom st
instance (SourceInfo st, Domain dom) => TreeDebug TopLevelPragma dom st
instance (SourceInfo st, Domain dom) => TreeDebug Rule dom st
instance (SourceInfo st, Domain dom) => TreeDebug AnnotationSubject dom st
instance (SourceInfo st, Domain dom) => TreeDebug MinimalFormula dom st
instance (SourceInfo st, Domain dom) => TreeDebug ExprPragma dom st
instance (SourceInfo st, Domain dom) => TreeDebug SourceRange dom st
instance (SourceInfo st, Domain dom) => TreeDebug Number dom st
instance (SourceInfo st, Domain dom) => TreeDebug QuasiQuote dom st
instance (SourceInfo st, Domain dom) => TreeDebug URhsGuard dom st
instance (SourceInfo st, Domain dom) => TreeDebug ULocalBind dom st
instance (SourceInfo st, Domain dom) => TreeDebug ULocalBinds dom st
instance (SourceInfo st, Domain dom) => TreeDebug UFixitySignature dom st
instance (SourceInfo st, Domain dom) => TreeDebug UTypeSignature dom st
instance (SourceInfo st, Domain dom) => TreeDebug ListCompBody dom st
instance (SourceInfo st, Domain dom) => TreeDebug UTupSecElem dom st
instance (SourceInfo st, Domain dom) => TreeDebug UTypeFamily dom st
instance (SourceInfo st, Domain dom) => TreeDebug UTypeFamilySpec dom st
instance (SourceInfo st, Domain dom) => TreeDebug UInjectivityAnn dom st
instance (SourceInfo st, Domain dom, TreeDebug expr dom st, Generic (expr dom st)) => TreeDebug (UCaseRhs' expr) dom st
instance (SourceInfo st, Domain dom, TreeDebug expr dom st, Generic (expr dom st)) => TreeDebug (UGuardedCaseRhs' expr) dom st
instance (SourceInfo st, Domain dom) => TreeDebug UPatternSynonym dom st
instance (SourceInfo st, Domain dom) => TreeDebug UPatSynRhs dom st
instance (SourceInfo st, Domain dom) => TreeDebug UPatSynLhs dom st
instance (SourceInfo st, Domain dom) => TreeDebug UPatSynWhere dom st
instance (SourceInfo st, Domain dom) => TreeDebug UPatternTypeSignature dom st
instance (SourceInfo st, Domain dom) => TreeDebug Role dom st
instance (SourceInfo st, Domain dom) => TreeDebug Cmd dom st
instance (SourceInfo st, Domain dom) => TreeDebug ULanguageExtension dom st
instance (SourceInfo st, Domain dom) => TreeDebug UMatchLhs dom st

-- ULiteral
instance (SourceInfo st, Domain dom) => TreeDebug ULiteral dom st
instance (SourceInfo st, Domain dom, TreeDebug k dom st, Generic (k dom st)) => TreeDebug (UPromoted k) dom st

-- Base
instance (SourceInfo st, Domain dom) => TreeDebug UOperator dom st
instance (SourceInfo st, Domain dom) => TreeDebug UName dom st
instance (SourceInfo st, Domain dom) => TreeDebug UQualifiedName dom st
instance (SourceInfo st, Domain dom) => TreeDebug UModuleName dom st
instance (SourceInfo st, Domain dom) => TreeDebug UNamePart dom st
instance (SourceInfo st, Domain dom) => TreeDebug UStringNode dom st
instance (SourceInfo st, Domain dom) => TreeDebug UDataOrNewtypeKeyword dom st
instance (SourceInfo st, Domain dom) => TreeDebug UDoKind dom st
instance (SourceInfo st, Domain dom) => TreeDebug TypeKeyword dom st
instance (SourceInfo st, Domain dom) => TreeDebug OverlapPragma dom st
instance (SourceInfo st, Domain dom) => TreeDebug CallConv dom st
instance (SourceInfo st, Domain dom) => TreeDebug ArrowAppl dom st
instance (SourceInfo st, Domain dom) => TreeDebug Safety dom st
instance (SourceInfo st, Domain dom) => TreeDebug ConlikeAnnot dom st
instance (SourceInfo st, Domain dom) => TreeDebug Assoc dom st
instance (SourceInfo st, Domain dom) => TreeDebug Precedence dom st
instance (SourceInfo st, Domain dom) => TreeDebug LineNumber dom st
instance (SourceInfo st, Domain dom) => TreeDebug PhaseControl dom st
instance (SourceInfo st, Domain dom) => TreeDebug PhaseNumber dom st
instance (SourceInfo st, Domain dom) => TreeDebug PhaseInvert dom st