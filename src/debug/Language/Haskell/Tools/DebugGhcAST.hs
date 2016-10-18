{-# LANGUAGE StandaloneDeriving
           , TypeSynonymInstances 
           , FlexibleInstances 
           #-}
-- | A module for showing GHC's syntax tree representation.
module Language.Haskell.Tools.DebugGhcAST where

import Language.Haskell.Tools.RangeDebug
import Language.Haskell.Tools.AST.FromGHC.GHCUtils
import Language.Haskell.Tools.AST (shortShowSpan)

import GHC
import HsSyn
import HsDecls
import Module
import Coercion
import SrcLoc
import RdrName
import BasicTypes
import Outputable
import TyCon
import PlaceHolder
import ForeignCall
import Var
import ConLike
import PatSyn
import TcEvidence
import Bag
import BooleanFormula
import FieldLabel
import CoreSyn
import UniqFM
import OccName

instance Show a => Show (Located a) where
  show (L l a) = "L(" ++ shortShowSpan l ++ ") (" ++ show a ++ ")"

deriving instance Show (ABExport RdrName)
deriving instance Show (AmbiguousFieldOcc RdrName)
deriving instance Show (AnnDecl RdrName)
deriving instance Show (AnnProvenance RdrName)
deriving instance Show (ApplicativeArg RdrName RdrName)
deriving instance Show (ArithSeqInfo RdrName)
deriving instance Show (BooleanFormula (Located RdrName))
deriving instance Show (ClsInstDecl RdrName)
deriving instance Show (UConDecl RdrName)
deriving instance Show (ConDeclField RdrName)
deriving instance Show (DataFamInstDecl RdrName)
deriving instance Show (DefaultDecl RdrName)
deriving instance Show (DerivDecl RdrName)
deriving instance Show (FamilyDecl RdrName)
deriving instance Show (FamilyInfo RdrName)
deriving instance Show (FamilyResultSig RdrName)
deriving instance Show (FieldLbl RdrName)
deriving instance Show (FieldOcc RdrName)
deriving instance Show (FixitySig RdrName)
deriving instance Show (ForeignDecl RdrName)
deriving instance Show a => Show (GRHS RdrName a)
deriving instance Show a => Show (GRHSs RdrName a)
deriving instance Show (UInjectivityAnn RdrName)
deriving instance Show (HsAppType RdrName)
deriving instance Show (HsBindLR RdrName RdrName)
deriving instance Show (HsBracket RdrName)
deriving instance Show (HsCmd RdrName)
deriving instance Show (HsCmdTop RdrName)
deriving instance Show (HsConDeclDetails RdrName)
deriving instance Show (HsConPatDetails RdrName)
deriving instance Show (HsDataDefn RdrName)
deriving instance Show (HsDecl RdrName)
deriving instance Show (HsExpr RdrName)
deriving instance Show (HsGroup RdrName)
deriving instance Show (HsLocalBindsLR RdrName RdrName)
deriving instance Show (HsMatchContext RdrName)
deriving instance Show (HsModule RdrName)
deriving instance Show (HsOverLit RdrName)
deriving instance Show (HsPatSynDetails (Located RdrName))
deriving instance Show (HsPatSynDir RdrName)
deriving instance Show (HsRecFields RdrName (LPat RdrName))
deriving instance Show (HsRecordBinds RdrName)
deriving instance Show (HsSplice RdrName)
deriving instance Show (HsStmtContext RdrName)
deriving instance Show (HsTupArg RdrName)
deriving instance Show (HsTyVarBndr RdrName)
deriving instance Show (HsType RdrName)
deriving instance Show (HsValBindsLR RdrName RdrName)
deriving instance Show (HsWildCardInfo RdrName)
deriving instance Show (IE RdrName)
deriving instance Show (UImportDecl RdrName)
deriving instance Show (InstDecl RdrName)
deriving instance Show (LHsQTyVars RdrName)
deriving instance Show a => Show (UMatch RdrName a)
deriving instance Show (MatchFixity RdrName)
deriving instance Show a => Show (MatchGroup RdrName a)
deriving instance Show (ParStmtBlock RdrName RdrName)
deriving instance Show (Pat RdrName)
deriving instance Show (PatSynBind RdrName RdrName)
deriving instance Show (RecordPatSynField (Located RdrName))
deriving instance Show (RoleAnnotDecl RdrName)
deriving instance Show (RuleBndr RdrName)
deriving instance Show (RuleDecl RdrName)
deriving instance Show (RuleDecls RdrName)
deriving instance Show (Sig RdrName)
deriving instance Show (SpliceDecl RdrName)
deriving instance Show (SyntaxExpr RdrName)
deriving instance Show a => Show (StmtLR RdrName RdrName a)
deriving instance Show (TyClDecl RdrName)
deriving instance Show (TyClGroup RdrName)
deriving instance Show a => Show (TyFamEqn RdrName a)
deriving instance Show (TyFamInstDecl RdrName)
deriving instance Show (VectDecl RdrName)
deriving instance Show (WarnDecl RdrName)
deriving instance Show (WarnDecls RdrName)


deriving instance Show (ABExport UName)
deriving instance Show (AmbiguousFieldOcc UName)
deriving instance Show (AnnDecl UName)
deriving instance Show (AnnProvenance UName)
deriving instance Show (ApplicativeArg UName UName)
deriving instance Show (ArithSeqInfo UName)
deriving instance Show (BooleanFormula (Located UName))
deriving instance Show (ClsInstDecl UName)
deriving instance Show (UConDecl UName)
deriving instance Show (ConDeclField UName)
deriving instance Show (DataFamInstDecl UName)
deriving instance Show (DefaultDecl UName)
deriving instance Show (DerivDecl UName)
deriving instance Show (FamilyDecl UName)
deriving instance Show (FamilyInfo UName)
deriving instance Show (FamilyResultSig UName)
deriving instance Show (FieldLbl UName)
deriving instance Show (FieldOcc UName)
deriving instance Show (FixitySig UName)
deriving instance Show (ForeignDecl UName)
deriving instance Show a => Show (GRHS UName a)
deriving instance Show a => Show (GRHSs UName a)
deriving instance Show (UInjectivityAnn UName)
deriving instance Show (HsAppType UName)
deriving instance Show (HsBindLR UName UName)
deriving instance Show (HsBracket UName)
deriving instance Show (HsCmd UName)
deriving instance Show (HsCmdTop UName)
deriving instance Show (HsConDeclDetails UName)
deriving instance Show (HsConPatDetails UName)
deriving instance Show (HsDataDefn UName)
deriving instance Show (HsDecl UName)
deriving instance Show (HsExpr UName)
deriving instance Show (HsGroup UName)
deriving instance Show (HsLocalBindsLR UName UName)
deriving instance Show (HsMatchContext UName)
deriving instance Show (HsModule UName)
deriving instance Show (HsOverLit UName)
deriving instance Show (HsPatSynDetails (Located UName))
deriving instance Show (HsPatSynDir UName)
deriving instance Show (HsRecFields UName (LPat UName))
deriving instance Show (HsRecordBinds UName)
deriving instance Show (HsSplice UName)
deriving instance Show (HsStmtContext UName)
deriving instance Show (HsTupArg UName)
deriving instance Show (HsTyVarBndr UName)
deriving instance Show (HsType UName)
deriving instance Show (HsValBindsLR UName UName)
deriving instance Show (HsWildCardInfo UName)
deriving instance Show (IE UName)
deriving instance Show (UImportDecl UName)
deriving instance Show (InstDecl UName)
deriving instance Show (LHsQTyVars UName)
deriving instance Show a => Show (UMatch UName a)
deriving instance Show (MatchFixity UName)
deriving instance Show a => Show (MatchGroup UName a)
deriving instance Show (ParStmtBlock UName UName)
deriving instance Show (Pat UName)
deriving instance Show (PatSynBind UName UName)
deriving instance Show (RecordPatSynField (Located UName))
deriving instance Show (RoleAnnotDecl UName)
deriving instance Show (RuleBndr UName)
deriving instance Show (RuleDecl UName)
deriving instance Show (RuleDecls UName)
deriving instance Show (Sig UName)
deriving instance Show (SpliceDecl UName)
deriving instance Show (SyntaxExpr UName)
deriving instance Show a => Show (StmtLR UName UName a)
deriving instance Show (TyClDecl UName)
deriving instance Show (TyClGroup UName)
deriving instance Show a => Show (TyFamEqn UName a)
deriving instance Show (TyFamInstDecl UName)
deriving instance Show (VectDecl UName)
deriving instance Show (WarnDecl UName)
deriving instance Show (WarnDecls UName)


deriving instance Show (ABExport Id)
deriving instance Show (AmbiguousFieldOcc Id)
deriving instance Show (AnnDecl Id)
deriving instance Show (AnnProvenance Id)
deriving instance Show (ApplicativeArg Id Id)
deriving instance Show (ArithSeqInfo Id)
deriving instance Show (BooleanFormula (Located Id))
deriving instance Show (ClsInstDecl Id)
deriving instance Show (UConDecl Id)
deriving instance Show (ConDeclField Id)
deriving instance Show (DataFamInstDecl Id)
deriving instance Show (DefaultDecl Id)
deriving instance Show (DerivDecl Id)
deriving instance Show (FamilyDecl Id)
deriving instance Show (FamilyInfo Id)
deriving instance Show (FamilyResultSig Id)
deriving instance Show (FieldLbl Id)
deriving instance Show (FieldOcc Id)
deriving instance Show (FixitySig Id)
deriving instance Show (ForeignDecl Id)
deriving instance Show a => Show (GRHS Id a)
deriving instance Show a => Show (GRHSs Id a)
deriving instance Show (UInjectivityAnn Id)
deriving instance Show (HsAppType Id)
deriving instance Show (HsBindLR Id Id)
deriving instance Show (HsBracket Id)
deriving instance Show (HsCmd Id)
deriving instance Show (HsCmdTop Id)
deriving instance Show (HsConDeclDetails Id)
deriving instance Show (HsConPatDetails Id)
deriving instance Show (HsDataDefn Id)
deriving instance Show (HsDecl Id)
deriving instance Show (HsExpr Id)
deriving instance Show (HsGroup Id)
deriving instance Show (HsLocalBindsLR Id Id)
deriving instance Show (HsMatchContext Id)
deriving instance Show (HsModule Id)
deriving instance Show (HsOverLit Id)
deriving instance Show (HsPatSynDetails (Located Id))
deriving instance Show (HsPatSynDir Id)
deriving instance Show (HsRecFields Id (LPat Id))
deriving instance Show (HsRecordBinds Id)
deriving instance Show (HsSplice Id)
deriving instance Show (HsStmtContext Id)
deriving instance Show (HsTupArg Id)
deriving instance Show (HsTyVarBndr Id)
deriving instance Show (HsType Id)
deriving instance Show (HsValBindsLR Id Id)
deriving instance Show (HsWildCardInfo Id)
deriving instance Show (IE Id)
deriving instance Show (UImportDecl Id)
deriving instance Show (InstDecl Id)
deriving instance Show (LHsQTyVars Id)
deriving instance Show a => Show (UMatch Id a)
deriving instance Show (MatchFixity Id)
deriving instance Show a => Show (MatchGroup Id a)
deriving instance Show (ParStmtBlock Id Id)
deriving instance Show (Pat Id)
deriving instance Show (PatSynBind Id Id)
deriving instance Show (RecordPatSynField (Located Id))
deriving instance Show (RoleAnnotDecl Id)
deriving instance Show (RuleBndr Id)
deriving instance Show (RuleDecl Id)
deriving instance Show (RuleDecls Id)
deriving instance Show (Sig Id)
deriving instance Show (SpliceDecl Id)
deriving instance Show (SyntaxExpr Id)
deriving instance Show a => Show (StmtLR Id Id a)
deriving instance Show (TyClDecl Id)
deriving instance Show (TyClGroup Id)
deriving instance Show a => Show (TyFamEqn Id a)
deriving instance Show (TyFamInstDecl Id)
deriving instance Show (VectDecl Id)
deriving instance Show (WarnDecl Id)
deriving instance Show (WarnDecls Id)

deriving instance Show Activation
deriving instance Show HsArrAppType
deriving instance Show Boxity
deriving instance Show CType
deriving instance Show CImportSpec
deriving instance Show CExportSpec
deriving instance Show CCallConv
deriving instance Show CCallTarget
deriving instance Show ConLike
deriving instance Show DocDecl
deriving instance Show Fixity
deriving instance Show FixityDirection
deriving instance Show ForeignImport
deriving instance Show ForeignExport
deriving instance Show Header
deriving instance Show HsIPName
deriving instance Show HsLit
deriving instance Show HsTupleSort
deriving instance Show HsSrcBang
deriving instance Show InlinePragma
deriving instance Show NewOrData
deriving instance Show Origin
deriving instance Show OverLitVal
deriving instance Show OverlapMode
deriving instance Show PlaceHolder
deriving instance Show Role
deriving instance Show RecFlag
deriving instance Show SpliceExplicitFlag
deriving instance Show TcSpecPrag
deriving instance Show TcSpecPrags
deriving instance Show TransForm
deriving instance Show WarningTxt
deriving instance Show PendingRnSplice
deriving instance Show PendingTcSplice

instance Show UnboundVar where
  show (OutOfScope n _) = "OutOfScope " ++ show n
  show (TrueExprHole n) = "TrueExprHole " ++ show n



instance Show UModuleName where
  show = showSDocUnsafe . ppr
instance Show TyCon where
  show = showSDocUnsafe . ppr
instance Show ClsInst where
  show = showSDocUnsafe . ppr
instance Show UType where
  show = showSDocUnsafe . ppr
instance Show OccName where
  show = showSDocUnsafe . ppr
-- instance Show RdrName where
  -- show = showSDocUnsafe . ppr
  
deriving instance Show RdrName
deriving instance Show UModule
deriving instance Show StringLiteral
deriving instance Show UntypedSpliceFlavour
deriving instance Show SrcUnpackedness
deriving instance Show SrcStrictness
deriving instance Show IEWildcard

deriving instance Show t => Show (HsImplicitBndrs RdrName t)
deriving instance Show t => Show (HsImplicitBndrs UName t)
deriving instance Show t => Show (HsImplicitBndrs Id t)
deriving instance Show t => Show (HsWildCardBndrs RdrName t)
deriving instance Show t => Show (HsWildCardBndrs UName t)
deriving instance Show t => Show (HsWildCardBndrs Id t)
deriving instance (Show a, Show b) => Show (HsRecField' a b)


instance Show UnitId where
  show = showSDocUnsafe . ppr
instance Show UName where
  show = showSDocUnsafe . ppr
instance Show HsTyLit where
  show = showSDocUnsafe . ppr
instance Show Var where
  show = showSDocUnsafe . ppr
instance Show DataCon where
  show = showSDocUnsafe . ppr
instance Show PatSyn where
  show = showSDocUnsafe . ppr
instance Show TcEvBinds where
  show = showSDocUnsafe . ppr
instance Show HsWrapper where
  show = showSDocUnsafe . ppr
instance Show Class where
  show = showSDocUnsafe . ppr
instance Show TcCoercion where
  show = showSDocUnsafe . ppr
instance Outputable a => Show (UniqFM a) where
  show = showSDocUnsafe . ppr
instance Outputable a => Show (Tickish a) where
  show = showSDocUnsafe . ppr
instance OutputableBndr a => Show (HsIPBinds a) where
  show = showSDocUnsafe . ppr
  
instance Show a => Show (Bag a) where
  show = show . bagToList

