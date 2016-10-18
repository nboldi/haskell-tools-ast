-- | UPattern matching on declaration-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Decls where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Match.Base

-- * Declarations

pattern TypeDecl :: Ann UDeclHead dom stage -> Ann Type dom stage -> Ann UDecl dom stage 
pattern TypeDecl dh typ <- Ann _ (UTypeDecl dh typ)

pattern TypeFamily :: Ann UDeclHead dom stage -> AnnMaybe UTypeFamilySpec dom stage -> Ann UDecl dom stage
pattern TypeFamily dh famSpec <- Ann _ (UTypeFamilyDecl (Ann _ (UTypeFamily dh famSpec)))

pattern DataFamily :: Ann UDeclHead dom stage -> AnnMaybe UKindConstraint dom stage -> Ann UDecl dom stage
pattern DataFamily dh kind <- Ann _ (UTypeFamilyDecl (Ann _ (UDataFamily dh kind)))

pattern ClosedTypeFamily :: Ann UDeclHead dom stage -> AnnMaybe UKindConstraint dom stage -> AnnList UTypeEqn dom stage -> Ann UDecl dom stage
pattern ClosedTypeFamily dh kind typeqs <- Ann _ (UClosedTypeFamilyDecl dh kind typeqs)

pattern DataDecl :: AnnMaybe Context dom stage -> Ann UDeclHead dom stage -> AnnList UConDecl dom stage -> AnnMaybe UDeriving dom stage -> Ann UDecl dom stage
pattern DataDecl ctx dh cons derivs <- Ann _ (UDataDecl DataKeyword ctx dh cons derivs)

pattern NewtypeDecl :: AnnMaybe Context dom stage -> Ann UDeclHead dom stage -> AnnList UConDecl dom stage -> AnnMaybe UDeriving dom stage -> Ann UDecl dom stage
pattern NewtypeDecl ctx dh cons derivs <- Ann _ (UDataDecl NewtypeKeyword ctx dh cons derivs)

pattern GADTDataDecl :: AnnMaybe Context dom stage -> Ann UDeclHead dom stage -> AnnMaybe UKindConstraint dom stage -> AnnList UGadtConDecl dom stage -> AnnMaybe UDeriving dom stage -> Ann UDecl dom stage
pattern GADTDataDecl ctx dh kind cons derivs  <- Ann _ (UGDataDecl DataKeyword ctx dh kind cons derivs )

pattern GADTNewtypeDecl :: AnnMaybe Context dom stage -> Ann UDeclHead dom stage -> AnnMaybe UKindConstraint dom stage -> AnnList UGadtConDecl dom stage -> AnnMaybe UDeriving dom stage -> Ann UDecl dom stage
pattern GADTNewtypeDecl ctx dh kind cons derivs  <- Ann _ (UGDataDecl NewtypeKeyword ctx dh kind cons derivs )

pattern TypeInstance :: Ann UInstanceRule dom stage -> Ann Type dom stage -> Ann UDecl dom stage
pattern TypeInstance instRule typ <- Ann _ (UTypeInstDecl instRule typ)

pattern DataInstance :: Ann UInstanceRule dom stage -> AnnList UConDecl dom stage -> AnnMaybe UDeriving dom stage
                    -> Ann UDecl dom stage
pattern DataInstance instRule cons derivs  <- Ann _ (UDataInstDecl DataKeyword instRule cons derivs )

pattern NewtypeInstance :: Ann UInstanceRule dom stage -> AnnList UConDecl dom stage -> AnnMaybe UDeriving dom stage
                       -> Ann UDecl dom stage
pattern NewtypeInstance instRule cons derivs  <- Ann _ (UDataInstDecl NewtypeKeyword instRule cons derivs )

pattern GadtDataInstance :: Ann UInstanceRule dom stage -> AnnMaybe UKindConstraint dom stage -> AnnList UGadtConDecl dom stage
                       -> Ann UDecl dom stage
pattern GadtDataInstance instRule kind cons  <- Ann _ (UGDataInstDecl DataKeyword instRule kind cons )

pattern ClassDecl :: AnnMaybe Context dom stage -> Ann UDeclHead dom stage -> AnnMaybe UClassBody dom stage -> Ann UDecl dom stage
pattern ClassDecl ctx dh body <- Ann _ (UClassDecl ctx dh _ body)

pattern InstanceDecl :: Ann UInstanceRule dom stage -> AnnMaybe UInstBody dom stage -> Ann UDecl dom stage
pattern InstanceDecl instRule body <- Ann _ (UInstDecl _ instRule body)

pattern StandaloneDeriving :: Ann UInstanceRule dom stage -> Ann UDecl dom stage
pattern StandaloneDeriving instRule <- Ann _ (UDerivDecl _ instRule)

pattern FixityDecl :: Ann UFixitySignature dom stage -> Ann UDecl dom stage
pattern FixityDecl fixity <- Ann _ (UFixityDecl fixity)

pattern TypeSigDecl :: Ann UTypeSignature dom stage -> Ann UDecl dom stage
pattern TypeSigDecl typeSig <- Ann _ (UTypeSigDecl typeSig)

pattern ValueBinding :: Ann UValueBind dom stage -> Ann UDecl dom stage
pattern ValueBinding bind <- Ann _ (UValueBinding bind)

pattern ForeignImport :: Ann CallConv dom stage -> Ann UName dom stage -> Ann Type dom stage -> Ann UDecl dom stage
pattern ForeignImport cc name typ <- Ann _ (UForeignImport cc _ name typ)

pattern PatternSynonym :: Ann UPatSynLhs dom stage -> Ann UPatSynRhs dom stage -> Ann UDecl dom stage
pattern PatternSynonym lhs rhs <- Ann _ (UPatternSynonymDecl (Ann _ (UPatternSynonym lhs rhs)))

-- * UPattern synonyms

pattern ConPatSyn :: Ann UName dom stage -> AnnList UName dom stage -> Ann UPatSynLhs dom stage
pattern ConPatSyn con args <- Ann _ (UNormalPatSyn con args)

pattern InfixPatSyn :: Ann UName dom stage -> Ann UOperator dom stage -> Ann UName dom stage -> Ann UPatSynLhs dom stage
pattern InfixPatSyn lhs op rhs <- Ann _ (UInfixPatSyn lhs op rhs)

pattern RecordPatSyn :: Ann UName dom stage -> AnnList UName dom stage -> Ann UPatSynLhs dom stage
pattern RecordPatSyn con args <- Ann _ (URecordPatSyn con args)

pattern SymmetricPatSyn :: Ann UPattern dom stage -> Ann UPatSynRhs dom stage
pattern SymmetricPatSyn pat <- Ann _ (UBidirectionalPatSyn pat AnnNothing)

pattern OneWayPatSyn :: Ann UPattern dom stage -> Ann UPatSynRhs dom stage
pattern OneWayPatSyn pat <- Ann _ (UOneDirectionalPatSyn pat)

pattern TwoWayPatSyn :: Ann UPattern dom stage -> AnnList UMatch dom stage -> Ann UPatSynRhs dom stage
pattern TwoWayPatSyn pat match <- Ann _ (UBidirectionalPatSyn pat (AnnJust (Ann _ (UPatSynWhere match))))

-- * Type families

pattern TypeFamilyKindSpec :: Ann UKindConstraint dom stage -> Ann UTypeFamilySpec dom stage
pattern TypeFamilyKindSpec kind <- Ann _ (UTypeFamilyKind kind)

pattern TypeFamilyInjectivitySpec :: Ann UName dom stage -> AnnList UName dom stage -> Ann UTypeFamilySpec dom stage
pattern TypeFamilyInjectivitySpec res dependent <- Ann _ (UTypeFamilyInjectivity (Ann _ (UInjectivityAnn res dependent)))

-- * Elements of type classes

pattern ClassBody :: AnnList UClassElement dom stage -> Ann UClassBody dom stage
pattern ClassBody body <- Ann _ (UClassBody body)

pattern ClassElemSig :: Ann UTypeSignature dom stage -> Ann UClassElement dom stage
pattern ClassElemSig typeSig <- Ann _ (UClsSig typeSig)

pattern ClassElemDef :: Ann UValueBind dom stage -> Ann UClassElement dom stage
pattern ClassElemDef def <- Ann _ (UClsDef def)

pattern ClassElemTypeFam :: Ann UDeclHead dom stage -> AnnMaybe UTypeFamilySpec dom stage -> Ann UClassElement dom stage
pattern ClassElemTypeFam dh tfSpec <- Ann _ (UClsTypeFam (Ann _ (UTypeFamily dh tfSpec)))

pattern ClassElemDataFam :: Ann UDeclHead dom stage -> AnnMaybe UKindConstraint dom stage -> Ann UClassElement dom stage
pattern ClassElemDataFam dh kind <- Ann _ (UClsTypeFam (Ann _ (UDataFamily dh kind)))

-- * Declaration heads

pattern NameDeclHead :: Ann UName dom stage -> Ann UDeclHead dom stage
pattern NameDeclHead name <- Ann _ (UDeclHead name)

pattern ParenDeclHead :: Ann UDeclHead dom stage -> Ann UDeclHead dom stage
pattern ParenDeclHead dh <- Ann _ (UDHParen dh)

pattern DeclHeadApp :: Ann UDeclHead dom stage -> Ann TyVar dom stage -> Ann UDeclHead dom stage
pattern DeclHeadApp dh tv <- Ann _ (UDHApp dh tv)

pattern InfixDeclHead :: Ann TyVar dom stage -> Ann UOperator dom stage -> Ann TyVar dom stage -> Ann UDeclHead dom stage
pattern InfixDeclHead lhs op rhs <- Ann _ (UDHInfix lhs op rhs)

-- * Elements of class instances

pattern InstanceBody :: AnnList UInstBodyDecl dom stage -> Ann UInstBody dom stage
pattern InstanceBody defs <- Ann _ (UInstBody defs)

pattern InstanceElemDef :: Ann UValueBind dom stage -> Ann UInstBodyDecl dom stage
pattern InstanceElemDef bind <- Ann _ (UInstBodyNormalDecl bind)

pattern InstanceElemTypeDef :: Ann UTypeEqn dom stage -> Ann UInstBodyDecl dom stage
pattern InstanceElemTypeDef typeEq <- Ann _ (UInstBodyTypeDecl typeEq)

pattern InstanceElemDataDef :: Ann UInstanceRule dom stage -> AnnList UConDecl dom stage -> AnnMaybe UDeriving dom stage 
                           -> Ann UInstBodyDecl dom stage
pattern InstanceElemDataDef instRule cons derivs  <- Ann _ (UInstBodyDataDecl DataKeyword instRule cons derivs )

pattern InstanceElemNewtypeDef :: Ann UInstanceRule dom stage -> AnnList UConDecl dom stage -> AnnMaybe UDeriving dom stage 
                           -> Ann UInstBodyDecl dom stage
pattern InstanceElemNewtypeDef instRule cons derivs  <- Ann _ (UInstBodyDataDecl NewtypeKeyword instRule cons derivs )

pattern InstanceElemGadtDataDef :: Ann UInstanceRule dom stage -> AnnMaybe UKindConstraint dom stage -> AnnList UGadtConDecl dom stage 
                               -> AnnMaybe UDeriving dom stage -> Ann UInstBodyDecl dom stage
pattern InstanceElemGadtDataDef instRule kind cons derivs  <- Ann _ (UInstBodyGadtDataDecl _ instRule kind cons derivs )

-- * Data type definitions

pattern GadtConDecl :: AnnList UName dom stage -> Ann Type dom stage -> Ann UGadtConDecl dom stage
pattern GadtConDecl names typ <- Ann _ (UGadtConDecl names (Ann _ (UGadtNormalType typ)))

pattern ConDecl :: Ann UName dom stage -> AnnList Type dom stage -> Ann UConDecl dom stage
pattern ConDecl name args <- Ann _ (UConDecl name args)

pattern RecordConDecl :: Ann UName dom stage -> AnnList UFieldDecl dom stage -> Ann UConDecl dom stage
pattern RecordConDecl name fields <- Ann _ (URecordDecl name fields)

pattern InfixConDecl :: Ann Type dom stage -> Ann UOperator dom stage -> Ann Type dom stage -> Ann UConDecl dom stage
pattern InfixConDecl lhs op rhs <- Ann _ (UInfixConDecl lhs op rhs)

pattern FieldDecl :: AnnList UName dom stage -> Ann Type dom stage -> Ann UFieldDecl dom stage
pattern FieldDecl names typ <- Ann _ (UFieldDecl names typ)

pattern DerivingOne :: Ann UInstanceHead dom stage -> Ann UDeriving dom stage
pattern DerivingOne deriv <- Ann _ (UDerivingOne deriv)

pattern DerivingMulti :: AnnList UInstanceHead dom stage -> Ann UDeriving dom stage
pattern DerivingMulti derivs <- Ann _ (UDerivings derivs)

pattern InstanceRule :: AnnMaybe (AnnList TyVar) dom stage -> AnnMaybe Context dom stage -> Ann UInstanceHead dom stage -> Ann UInstanceRule dom stage
pattern InstanceRule tvs ctx ih <- Ann _ (UInstanceRule tvs ctx ih)

pattern InstanceHead :: Ann UName dom stage -> Ann UInstanceHead dom stage
pattern InstanceHead name <- Ann _ (UInstanceHeadCon name)

pattern InfixInstanceHead :: Ann Type dom stage -> Ann UName dom stage -> Ann UInstanceHead dom stage
pattern InfixInstanceHead typ n <- Ann _ (UInstanceHeadInfix typ n)

pattern ParenInstanceHead :: Ann UInstanceHead dom stage -> Ann UInstanceHead dom stage
pattern ParenInstanceHead ih <- Ann _ (UInstanceHeadParen ih)

pattern AppInstanceHead :: Ann UInstanceHead dom stage -> Ann Type dom stage -> Ann UInstanceHead dom stage
pattern AppInstanceHead fun arg <- Ann _ (UInstanceHeadApp fun arg)

pattern TypeEqn :: Ann Type dom stage -> Ann Type dom stage -> Ann UTypeEqn dom stage
pattern TypeEqn lhs rhs <- Ann _ (UTypeEqn lhs rhs)
