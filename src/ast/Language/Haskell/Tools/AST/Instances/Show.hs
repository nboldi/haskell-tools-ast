{-# LANGUAGE FlexibleContexts
           , StandaloneDeriving
           , FlexibleInstances
           #-}
module Language.Haskell.Tools.AST.Instances.Show where

import Language.Haskell.Tools.AST.Module
import Language.Haskell.Tools.AST.Decl
import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Ann

-- Annotations
deriving instance (Show a, Show (e wt a)) => Show (Ann (e wt) a)
deriving instance (Show a, Show (e wt a)) => Show (AnnMaybe (e wt) a)
deriving instance (Show a, Show (e wt a)) => Show (AnnList (e wt) a)

instance (Functor elem) => Functor (Ann elem) where
  fmap f ann = ann { annotation = f (annotation ann), element = fmap f (element ann) }
instance (Functor elem) => Functor (AnnList elem) where
  fmap f annList = annList { fromAnnList = fmap (fmap f) (fromAnnList annList) }
instance (Functor elem) => Functor (AnnMaybe elem) where
  fmap f annMaybe = annMaybe { fromAnnMaybe = fmap (fmap f) (fromAnnMaybe annMaybe) }
{-
-- Modules
deriving instance Show a => Show (Module wt a)
deriving instance Show a => Show (ModuleHead wt a)
deriving instance Show a => Show (ExportSpecList wt a)
deriving instance Show a => Show (ExportSpec wt a)
deriving instance Show a => Show (IESpec wt a)
deriving instance Show a => Show (SubSpec wt a)
deriving instance Show a => Show (ModulePragma wt a)
deriving instance Show a => Show (ImportDecl wt a)
deriving instance Show a => Show (ImportSpec wt a)
deriving instance Show a => Show (ImportQualified wt a)
deriving instance Show a => Show (ImportSource wt a)
deriving instance Show a => Show (ImportSafe wt a)
deriving instance Show a => Show (TypeNamespace wt a)
deriving instance Show a => Show (ImportRenaming wt a)

-- Declarations
deriving instance Show a => Show (Decl wt a)
deriving instance Show a => Show (ClassBody wt a)
deriving instance Show a => Show (GadtDeclList wt a)
deriving instance Show a => Show (ClassElement wt a)
deriving instance Show a => Show (DeclHead wt a)
deriving instance Show a => Show (InstBody wt a)
deriving instance Show a => Show (InstBodyDecl wt a)
deriving instance Show a => Show (GadtDecl wt a)
deriving instance Show a => Show (GadtField wt a)
deriving instance Show a => Show (FunDeps wt a)
deriving instance Show a => Show (FunDep wt a)
deriving instance Show a => Show (ConDecl wt a)
deriving instance Show a => Show (FieldDecl wt a)
deriving instance Show a => Show (Deriving wt a)
deriving instance Show a => Show (InstanceRule wt a)
deriving instance Show a => Show (InstanceHead wt a)
deriving instance Show a => Show (TypeEqn wt a)
deriving instance Show a => Show (KindConstraint wt a)
deriving instance Show a => Show (TyVar wt a)
deriving instance Show a => Show (Type wt a)
deriving instance Show a => Show (Kind wt a)
deriving instance Show a => Show (Context wt a)
deriving instance Show a => Show (Assertion wt a)
deriving instance Show a => Show (Expr wt a)
deriving instance Show a => Show (Stmt wt a)
deriving instance Show a => Show (CompStmt wt a)
deriving instance Show a => Show (FunBind wt a)
deriving instance Show a => Show (Pattern wt a)
deriving instance Show a => Show (PatternField wt a)
deriving instance Show a => Show (Splice wt a)
deriving instance Show a => Show (QQString wt a)
deriving instance Show a => Show (Match wt a)
deriving instance Show a => Show (Alt wt a)
deriving instance Show a => Show (Binds wt a)
deriving instance Show a => Show (Rhs wt a)
deriving instance Show a => Show (GuardedRhs wt a)
deriving instance Show a => Show (FieldUpdate wt a)
deriving instance Show a => Show (Bracket wt a)
deriving instance Show a => Show (TopLevelPragma wt a)
deriving instance Show a => Show (Rule wt a)
deriving instance Show a => Show (Annotation wt a)
deriving instance Show a => Show (MinimalFormula wt a)
deriving instance Show a => Show (ExprPragma wt a)
deriving instance Show a => Show (SourceRange wt a)
deriving instance Show a => Show (Number wt a)

-- Literal
deriving instance Show a => Show (Literal wt a)
deriving instance Show a => Show (Promoted wt a)

-- Base
deriving instance Show a => Show (Name wt a)
deriving instance Show a => Show (SimpleName wt a)
deriving instance Show a => Show (StringNode wt a)
deriving instance Show a => Show (DataOrNewtypeKeyword wt a)
deriving instance Show a => Show (DoKind wt a)
deriving instance Show a => Show (TypeKeyword wt a)
deriving instance Show a => Show (OverlapPragma wt a)
deriving instance Show a => Show (CallConv wt a)
deriving instance Show a => Show (ArrowAppl wt a)
deriving instance Show a => Show (Safety wt a)
deriving instance Show a => Show (Assoc wt a)
deriving instance Show a => Show (Precedence wt a)
deriving instance Show a => Show (PhaseControl wt a)
deriving instance Show a => Show (PhaseNumber wt a)
deriving instance Show a => Show (PhaseInvert wt a)
-}
