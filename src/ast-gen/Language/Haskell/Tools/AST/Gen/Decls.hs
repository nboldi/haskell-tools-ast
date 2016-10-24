-- | Generation of declaration-level AST fragments for refactorings.
-- The bindings defined here create a the annotated version of the AST constructor with the same name.
-- For example, @mkTypeSignature@ creates the annotated version of the @UTypeSignature@ AST constructor.
{-# LANGUAGE OverloadedStrings
           , TypeFamilies
           #-}
module Language.Haskell.Tools.AST.Gen.Decls where

import qualified Name as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST.Gen.Names
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkTypeDecl :: DeclHead dom -> Type dom -> Decl dom 
mkTypeDecl dh typ = mkAnn (child <> " :: " <> child) $ UTypeDecl dh typ

mkTypeFamily :: DeclHead dom -> Maybe (TypeFamilySpec dom) -> Decl dom
mkTypeFamily dh famSpec = mkAnn child $ UTypeFamilyDecl (mkAnn (child <> child) $ UTypeFamily dh (mkAnnMaybe (optBefore " ") famSpec))

mkDataFamily :: DeclHead dom -> Maybe (KindConstraint dom) -> Decl dom
mkDataFamily dh kind = mkAnn child $ UTypeFamilyDecl (mkAnn (child <> child) $ UDataFamily dh (mkAnnMaybe (optBefore " ") kind))

mkClosedTypeFamily :: DeclHead dom -> Maybe (KindConstraint dom) -> [TypeEqn dom] -> Decl dom
mkClosedTypeFamily dh kind typeqs = mkAnn (child <> child <> " where " <> child) 
                                      $ UClosedTypeFamilyDecl dh (mkAnnMaybe (optBefore " ") kind) (mkAnnList indentedList typeqs)

mkDataDecl :: DataOrNewtypeKeyword dom -> Maybe (Context dom) -> DeclHead dom -> [ConDecl dom] -> Maybe (Deriving dom) -> Decl dom
mkDataDecl keyw ctx dh cons derivs 
  = mkAnn (child <> " " <> child <> child <> child <> child) 
      $ UDataDecl keyw (mkAnnMaybe (optBefore " ") ctx) dh 
                 (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkGADTDataDecl :: DataOrNewtypeKeyword dom -> Maybe (Context dom) -> DeclHead dom -> Maybe (KindConstraint dom)
                    -> [GadtConDecl dom] -> Maybe (Deriving dom) -> Decl dom
mkGADTDataDecl keyw ctx dh kind cons derivs 
  = mkAnn (child <> " " <> child <> child <> child <> child <> child) 
      $ UGDataDecl keyw (mkAnnMaybe (optBefore " ") ctx) dh 
                  (mkAnnMaybe (optBefore " ") kind) (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkTypeInstance :: InstanceRule dom -> Type dom -> Decl dom
mkTypeInstance instRule typ = mkAnn ("type instance " <> child <> " = " <> child) $ UTypeInstDecl instRule typ

mkDataInstance :: DataOrNewtypeKeyword dom -> InstanceRule dom -> [ConDecl dom] -> Maybe (Deriving dom) -> Decl dom
mkDataInstance keyw instRule cons derivs 
  = mkAnn (child <> " instance " <> child <> " = " <> child <> child) 
      $ UDataInstDecl keyw instRule (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkGadtDataInstance :: DataOrNewtypeKeyword dom -> InstanceRule dom -> Maybe (KindConstraint dom) -> [GadtConDecl dom] -> Decl dom
mkGadtDataInstance keyw instRule kind cons 
  = mkAnn (child <> " instance " <> child <> child <> " where " <> child) 
      $ UGDataInstDecl keyw instRule (mkAnnMaybe (optBefore " ") kind) (mkAnnList indentedList cons)

mkStandaloneDeriving :: InstanceRule dom -> Decl dom
mkStandaloneDeriving instRule = mkAnn ("deriving instance" <> child <> child) $ UDerivDecl (mkAnnMaybe (optBefore " ") Nothing) instRule

mkFixityDecl :: FixitySignature dom -> Decl dom
mkFixityDecl = mkAnn child . UFixityDecl

mkDefaultDecl :: [Type dom] -> Decl dom
mkDefaultDecl = mkAnn ("default (" <> child <> ")") . UDefaultDecl . mkAnnList (listSep ", ")

mkTypeSigDecl :: TypeSignature dom -> Decl dom
mkTypeSigDecl = mkAnn child . UTypeSigDecl

mkValueBinding :: ValueBind dom -> Decl dom
mkValueBinding = mkAnn child . UValueBinding

mkSpliceDecl :: Splice dom -> Decl dom
mkSpliceDecl = mkAnn child . USpliceDecl

-- * Type roles

mkRoleDecl :: QualifiedName dom -> [Role dom] -> Decl dom
mkRoleDecl name roles = mkAnn ("type role " <> child <> child) $ URoleDecl name $ mkAnnList (listSepBefore " " " ") roles

mkNominalRole :: Role dom
mkNominalRole = mkAnn "nominal" UNominal 

mkRepresentationalRole :: Role dom
mkRepresentationalRole = mkAnn "representational" URepresentational

mkPhantomRole :: Role dom
mkPhantomRole = mkAnn "phantom" UPhantom

-- * Foreign imports and exports

mkForeignImport :: CallConv dom -> Maybe (Safety dom) -> Name dom -> Type dom -> Decl dom
mkForeignImport cc safety name typ = mkAnn (child <> child <> " " <> child <> " :: " <> child) 
                                       $ UForeignImport cc (mkAnnMaybe (optBefore " ") safety) name typ

mkForeignExport :: CallConv dom -> Name dom -> Type dom -> Decl dom
mkForeignExport cc name typ = mkAnn (child <> " " <> child <> " :: " <> child) $ UForeignExport cc name typ

mkStdCall :: CallConv dom
mkStdCall = mkAnn "stdcall" UStdCall

mkCCall :: CallConv dom
mkCCall = mkAnn "ccall" UCCall

mkCApi :: CallConv dom
mkCApi = mkAnn "capi" UCApi

mkUnsafe :: Safety dom
mkUnsafe = mkAnn "unsafe" UUnsafe 

-- * Pattern synonyms

mkPatternSynonym :: PatSynLhs dom -> PatSynRhs dom -> Decl dom
mkPatternSynonym lhs rhs = mkAnn child $ UPatternSynonymDecl $ mkAnn ("pattern " <> child <> " " <> child) $ UPatternSynonym lhs rhs

mkConPatSyn :: Name dom -> [Name dom] -> PatSynLhs dom
mkConPatSyn con args = mkAnn (child <> child) $ UNormalPatSyn con $ mkAnnList (listSepBefore " " " ") args

mkInfixPatSyn :: Name dom -> Operator dom -> Name dom -> PatSynLhs dom
mkInfixPatSyn lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ UInfixPatSyn lhs op rhs

mkRecordPatSyn :: Name dom -> [Name dom] -> PatSynLhs dom
mkRecordPatSyn con args = mkAnn (child <> child) $ URecordPatSyn con $ mkAnnList (listSepBeforeAfter ", " "{ " " }") args

mkSymmetricPatSyn :: Pattern dom -> PatSynRhs dom
mkSymmetricPatSyn = mkAnn ("= " <> child) . flip UBidirectionalPatSyn (mkAnnMaybe opt Nothing)

mkOneWayPatSyn :: Pattern dom -> PatSynRhs dom
mkOneWayPatSyn = mkAnn ("<- " <> child) . UOneDirectionalPatSyn

mkTwoWayPatSyn :: Pattern dom -> [Match dom] -> PatSynRhs dom
mkTwoWayPatSyn pat match = mkAnn ("<- " <> child <> child) $ UBidirectionalPatSyn pat $ mkAnnMaybe (optBefore " where ") 
                             $ Just $ mkAnn child $ UPatSynWhere $ mkAnnList indentedList match

mkPatternSignatureDecl :: PatternSignature dom -> Decl dom
mkPatternSignatureDecl = mkAnn child . UPatTypeSigDecl

mkPatternSignature :: Name dom -> Type dom -> PatternSignature dom
mkPatternSignature name typ = mkAnn (child <> " :: " <> child) $ UPatternTypeSignature name typ

-- * Type families

mkTypeFamilyKindSpec :: KindConstraint dom -> TypeFamilySpec dom
mkTypeFamilyKindSpec = mkAnn child . UTypeFamilyKind

mkTypeFamilyInjectivitySpec :: Name dom -> [Name dom] -> TypeFamilySpec dom
mkTypeFamilyInjectivitySpec res dependent 
  = mkAnn child (UTypeFamilyInjectivity $ mkAnn (child <> " -> " <> child) $ UInjectivityAnn res (mkAnnList (listSep " ") dependent))

-- * Class declarations

mkClassDecl :: Maybe (Context dom) -> DeclHead dom -> Maybe (ClassBody dom) -> Decl dom
mkClassDecl ctx dh body = mkAnn ("class " <> child <> child <> child <> child) 
                            $ UClassDecl (mkAnnMaybe (optAfter " ") ctx) dh (mkAnnMaybe (optBefore " | ") Nothing) (mkAnnMaybe opt body) 

mkClassBody :: [ClassElement dom] -> ClassBody dom
mkClassBody = mkAnn (" where " <> child) . UClassBody . mkAnnList indentedList

mkClassElemSig :: TypeSignature dom -> ClassElement dom
mkClassElemSig = mkAnn child . UClsSig

mkClassElemDef :: ValueBind dom -> ClassElement dom
mkClassElemDef = mkAnn child . UClsDef

mkClassElemTypeFam :: DeclHead dom -> Maybe (TypeFamilySpec dom) -> ClassElement dom
mkClassElemTypeFam dh tfSpec = mkAnn ("type " <> child) $ UClsTypeFam (mkAnn (child <> child) $ UTypeFamily dh (mkAnnMaybe opt tfSpec))

mkClassElemDataFam :: DeclHead dom -> Maybe (KindConstraint dom) -> ClassElement dom
mkClassElemDataFam dh kind = mkAnn ("data " <> child) $ UClsTypeFam (mkAnn (child <> child) $ UDataFamily dh (mkAnnMaybe opt kind))

-- * Declaration heads

mkNameDeclHead :: Name dom -> DeclHead dom
mkNameDeclHead = mkAnn child . UDeclHead

mkParenDeclHead :: DeclHead dom -> DeclHead dom
mkParenDeclHead = mkAnn child . UDHParen

mkDeclHeadApp :: DeclHead dom -> TyVar dom -> DeclHead dom
mkDeclHeadApp dh tv = mkAnn (child <> " " <> child) $ UDHApp dh tv

mkInfixDeclHead :: TyVar dom -> Operator dom -> TyVar dom -> DeclHead dom
mkInfixDeclHead lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ UDHInfix lhs op rhs

-- * Type class instance declarations

mkInstanceDecl :: InstanceRule dom -> Maybe (InstBody dom) -> Decl dom
mkInstanceDecl instRule body = mkAnn ("instance " <> child <> child <> child) 
                                 $ UInstDecl (mkAnnMaybe (optBefore " ") Nothing) instRule (mkAnnMaybe opt body)

mkInstanceBody :: [InstBodyDecl dom] -> InstBody dom
mkInstanceBody = mkAnn (" where " <> child) . UInstBody . mkAnnList indentedList

mkInstanceElemDef :: ValueBind dom -> InstBodyDecl dom
mkInstanceElemDef = mkAnn child . UInstBodyNormalDecl

mkInstanceElemTypeDef :: TypeEqn dom -> InstBodyDecl dom
mkInstanceElemTypeDef = mkAnn child . UInstBodyTypeDecl

mkInstanceElemDataDef :: DataOrNewtypeKeyword dom -> InstanceRule dom -> [ConDecl dom] -> Maybe (Deriving dom) -> InstBodyDecl dom
mkInstanceElemDataDef keyw instRule cons derivs 
  = mkAnn (child <> " " <> child <> child <> child) 
      $ UInstBodyDataDecl keyw instRule (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkInstanceElemGadtDataDef :: DataOrNewtypeKeyword dom -> InstanceRule dom -> Maybe (KindConstraint dom) -> [GadtConDecl dom] 
                               -> Maybe (Deriving dom) -> InstBodyDecl dom
mkInstanceElemGadtDataDef keyw instRule kind cons derivs 
  = mkAnn (child <> " " <> child <> child <> child) 
      $ UInstBodyGadtDataDecl mkDataKeyword instRule (mkAnnMaybe opt kind) (mkAnnList (listSepBefore " | " " = ") cons) 
                             (mkAnnMaybe (optBefore " deriving ") derivs)

-- * Data type definitions

mkGadtConDecl :: [Name dom] -> Type dom -> GadtConDecl dom
mkGadtConDecl names typ = mkAnn (child <> " :: " <> child) $ UGadtConDecl (mkAnnList (listSep ", ") names) (mkAnn child $ UGadtNormalType typ)

mkConDecl :: Name dom -> [Type dom] -> ConDecl dom
mkConDecl name args = mkAnn (child <> child) $ UConDecl name (mkAnnList (listSepBefore " " " ") args)

mkRecordConDecl :: Name dom -> [FieldDecl dom] -> ConDecl dom
mkRecordConDecl name fields = mkAnn (child <> " { " <> child <> " }") $ URecordDecl name (mkAnnList (listSep ", ") fields)

mkInfixConDecl :: Type dom -> Operator dom -> Type dom -> ConDecl dom
mkInfixConDecl lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ UInfixConDecl lhs op rhs

mkFieldDecl :: [Name dom] -> Type dom -> FieldDecl dom
mkFieldDecl names typ = mkAnn (child <> " :: " <> child) $ UFieldDecl (mkAnnList (listSep ", ") names) typ

mkDeriving :: [InstanceHead dom] -> Deriving dom
mkDeriving [deriv] = mkAnn child $ UDerivingOne deriv
mkDeriving derivs = mkAnn ("(" <> child <> ")") $ UDerivings (mkAnnList (listSep ", ") derivs)

mkInstanceRule :: Maybe (Context dom) -> InstanceHead dom -> InstanceRule dom
mkInstanceRule ctx ih 
  = mkAnn (child <> child <> child) $ UInstanceRule (mkAnnMaybe (optBefore " ") Nothing) (mkAnnMaybe (optBefore " ") ctx) ih

mkInstanceHead :: Name dom -> InstanceHead dom
mkInstanceHead = mkAnn child . UInstanceHeadCon

mkInfixInstanceHead :: Type dom -> Name dom -> InstanceHead dom
mkInfixInstanceHead typ n = mkAnn (child <> child) $ UInstanceHeadInfix typ n

mkParenInstanceHead :: InstanceHead dom -> InstanceHead dom
mkParenInstanceHead = mkAnn ("(" <> child <> ")") . UInstanceHeadParen

mkAppInstanceHead :: InstanceHead dom -> Type dom -> InstanceHead dom
mkAppInstanceHead fun arg = mkAnn (child <> " " <> child) $ UInstanceHeadApp fun arg

mkTypeEqn :: Type dom -> Type dom -> TypeEqn dom
mkTypeEqn lhs rhs = mkAnn (child <> " = " <> child) $ UTypeEqn lhs rhs

mkDataKeyword :: DataOrNewtypeKeyword dom
mkDataKeyword = mkAnn "data" UDataKeyword

mkNewtypeKeyword :: DataOrNewtypeKeyword dom
mkNewtypeKeyword = mkAnn "newtype" UNewtypeKeyword

-- * Top level pragmas

mkPragmaDecl :: TopLevelPragma dom -> Decl dom
mkPragmaDecl = mkAnn child . UPragmaDecl

mkRulePragma :: [Rule dom] -> TopLevelPragma dom
mkRulePragma = mkAnn ("{-# RULES " <> child <> " #-}") . URulePragma . mkAnnList (listSep ", ")

mkDeprPragma :: [Name dom] -> String -> TopLevelPragma dom
mkDeprPragma defs msg = mkAnn ("{-# DEPRECATED " <> child <> " " <> child <> " #-}") 
                          $ UDeprPragma (mkAnnList (listSep ", ") defs) $ mkAnn ("\"" <> child <> "\"") $ UStringNode msg

mkWarningPragma :: [Name dom] -> String -> TopLevelPragma dom
mkWarningPragma defs msg = mkAnn ("{-# WARNING " <> child <> " " <> child <> " #-}") 
                             $ UWarningPragma (mkAnnList (listSep ", ") defs) $ mkAnn ("\"" <> child <> "\"") $ UStringNode msg

mkAnnPragma :: AnnotationSubject dom -> Expr dom -> TopLevelPragma dom
mkAnnPragma subj ann = mkAnn ("{-# ANN " <> child <> " " <> child <> " #-}") $ UAnnPragma subj ann

mkInlinePragma :: Maybe (ConlikeAnnot dom) -> Maybe (PhaseControl dom) -> Name dom -> TopLevelPragma dom
mkInlinePragma conlike phase name 
  = mkAnn ("{-# INLINE " <> child <> child <> child <> " #-}") 
      $ UInlinePragma (mkAnnMaybe (optAfter " ") conlike) (mkAnnMaybe (optAfter " ") phase) name

mkNoInlinePragma :: Maybe (ConlikeAnnot dom) -> Maybe (PhaseControl dom) -> Name dom -> TopLevelPragma dom
mkNoInlinePragma conlike phase name 
  = mkAnn ("{-# NOINLINE " <> child <> child <> child <> " #-}") 
     $ UNoInlinePragma (mkAnnMaybe (optAfter " ") conlike) (mkAnnMaybe (optAfter " ") phase) name

mkInlinablePragma :: Maybe (PhaseControl dom) -> Name dom -> TopLevelPragma dom
mkInlinablePragma phase name
  = mkAnn ("{-# INLINEABLE " <> child <> child <> " #-}") 
     $ UInlinablePragma (mkAnnMaybe (optAfter " ") phase) name

mkLinePragma :: Int -> Maybe (StringNode dom) -> TopLevelPragma dom
mkLinePragma line filename 
  = mkAnn ("{-# LINE " <> child <> child <> " #-}") 
     $ ULinePragma (mkAnn child $ LineNumber line) (mkAnnMaybe (optBefore " ") filename)

mkSpecializePragma :: Maybe (PhaseControl dom) -> Name dom -> [Type dom] -> TopLevelPragma dom
mkSpecializePragma phase def specTypes 
  = mkAnn ("{-# SPECIALIZE " <> child <> child <> " " <> child <> " #-}") 
     $ USpecializePragma (mkAnnMaybe (optBefore " ") phase) def $ mkAnnList (listSep ", ") specTypes