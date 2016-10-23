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

mkDataDecl :: Maybe (Context dom) -> DeclHead dom -> [ConDecl dom] -> Maybe (Deriving dom) -> Decl dom
mkDataDecl ctx dh cons derivs 
  = mkAnn (child <> " " <> child <> child <> child <> child) 
      $ UDataDecl mkDataKeyword (mkAnnMaybe (optBefore " ") ctx) dh 
                 (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkNewtypeDecl :: Maybe (Context dom) -> DeclHead dom -> [ConDecl dom] -> Maybe (Deriving dom) -> Decl dom
mkNewtypeDecl ctx dh cons derivs 
  = mkAnn (child <> " " <> child <> child <> child <> child <> child) 
      $ UDataDecl mkNewtypeKeyword (mkAnnMaybe (optBefore " ") ctx) dh 
                 (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkGADTDataDecl :: Maybe (Context dom) -> DeclHead dom -> Maybe (KindConstraint dom)
                    -> [GadtConDecl dom] -> Maybe (Deriving dom) -> Decl dom
mkGADTDataDecl ctx dh kind cons derivs 
  = mkAnn (child <> " " <> child <> child <> child <> child <> child) 
      $ UGDataDecl mkDataKeyword (mkAnnMaybe (optBefore " ") ctx) dh 
                  (mkAnnMaybe (optBefore " ") kind) (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkTypeInstance :: InstanceRule dom -> Type dom -> Decl dom
mkTypeInstance instRule typ = mkAnn ("type instance " <> child <> " = " <> child) $ UTypeInstDecl instRule typ

mkDataInstance :: InstanceRule dom -> [ConDecl dom] -> Maybe (Deriving dom) -> Decl dom
mkDataInstance instRule cons derivs 
  = mkAnn (child <> " instance " <> child <> " = " <> child <> child) 
      $ UDataInstDecl mkDataKeyword instRule (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkNewtypeInstance :: InstanceRule dom -> [ConDecl dom] -> Maybe (Deriving dom) -> Decl dom
mkNewtypeInstance instRule cons derivs 
  = mkAnn (child <> " instance " <> child <> " = " <> child <> child) 
      $ UDataInstDecl mkNewtypeKeyword instRule (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkGadtDataInstance :: InstanceRule dom -> Maybe (KindConstraint dom) -> [GadtConDecl dom] -> Decl dom
mkGadtDataInstance instRule kind cons 
  = mkAnn (child <> " instance " <> child <> child <> " where " <> child) 
      $ UGDataInstDecl mkDataKeyword instRule (mkAnnMaybe (optBefore " ") kind) (mkAnnList indentedList cons)

mkClassDecl :: Maybe (Context dom) -> DeclHead dom -> Maybe (ClassBody dom) -> Decl dom
mkClassDecl ctx dh body = mkAnn ("class " <> child <> child <> child <> child) 
                            $ UClassDecl (mkAnnMaybe (optAfter " ") ctx) dh (mkAnnMaybe (optBefore " | ") Nothing) (mkAnnMaybe opt body) 

mkInstanceDecl :: InstanceRule dom -> Maybe (InstBody dom) -> Decl dom
mkInstanceDecl instRule body = mkAnn ("instance " <> child <> child <> child) 
                                 $ UInstDecl (mkAnnMaybe (optBefore " ") Nothing) instRule (mkAnnMaybe opt body)

mkStandaloneDeriving :: InstanceRule dom -> Decl dom
mkStandaloneDeriving instRule = mkAnn ("deriving instance" <> child <> child) $ UDerivDecl (mkAnnMaybe (optBefore " ") Nothing) instRule

mkFixityDecl :: FixitySignature dom -> Decl dom
mkFixityDecl = mkAnn child . UFixityDecl

mkTypeSigDecl :: TypeSignature dom -> Decl dom
mkTypeSigDecl = mkAnn child . UTypeSigDecl

mkValueBinding :: ValueBind dom -> Decl dom
mkValueBinding = mkAnn child . UValueBinding

mkTypeFamilyKindSpec :: KindConstraint dom -> TypeFamilySpec dom
mkTypeFamilyKindSpec = mkAnn child . UTypeFamilyKind

mkTypeFamilyInjectivitySpec :: Name dom -> [Name dom] -> TypeFamilySpec dom
mkTypeFamilyInjectivitySpec res dependent 
  = mkAnn child (UTypeFamilyInjectivity $ mkAnn (child <> " -> " <> child) $ UInjectivityAnn res (mkAnnList (listSep " ") dependent))

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

mkNameDeclHead :: Name dom -> DeclHead dom
mkNameDeclHead = mkAnn child . UDeclHead

mkParenDeclHead :: DeclHead dom -> DeclHead dom
mkParenDeclHead = mkAnn child . UDHParen

mkDeclHeadApp :: DeclHead dom -> TyVar dom -> DeclHead dom
mkDeclHeadApp dh tv = mkAnn (child <> " " <> child) $ UDHApp dh tv

mkInfixDeclHead :: TyVar dom -> Operator dom -> TyVar dom -> DeclHead dom
mkInfixDeclHead lhs op rhs = mkAnn (child <> " " <> child <> " " <> child) $ UDHInfix lhs op rhs

mkInstanceBody :: [InstBodyDecl dom] -> InstBody dom
mkInstanceBody = mkAnn (" where " <> child) . UInstBody . mkAnnList indentedList

mkInstanceElemDef :: ValueBind dom -> InstBodyDecl dom
mkInstanceElemDef = mkAnn child . UInstBodyNormalDecl

mkInstanceElemTypeDef :: TypeEqn dom -> InstBodyDecl dom
mkInstanceElemTypeDef = mkAnn child . UInstBodyTypeDecl

mkInstanceElemDataDef :: InstanceRule dom -> [ConDecl dom] -> Maybe (Deriving dom) -> InstBodyDecl dom
mkInstanceElemDataDef instRule cons derivs 
  = mkAnn (child <> " " <> child <> child <> child) 
      $ UInstBodyDataDecl mkDataKeyword instRule (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkInstanceElemNewtypeDef :: InstanceRule dom -> [ConDecl dom] -> Maybe (Deriving dom) -> InstBodyDecl dom
mkInstanceElemNewtypeDef instRule cons derivs 
  = mkAnn (child <> " " <> child <> child <> child) 
      $ UInstBodyDataDecl mkNewtypeKeyword instRule (mkAnnList (listSepBefore " | " " = ") cons) (mkAnnMaybe (optBefore " deriving ") derivs)

mkInstanceElemGadtDataDef :: InstanceRule dom -> Maybe (KindConstraint dom) -> [GadtConDecl dom] 
                               -> Maybe (Deriving dom) -> InstBodyDecl dom
mkInstanceElemGadtDataDef instRule kind cons derivs 
  = mkAnn (child <> " " <> child <> child <> child) 
      $ UInstBodyGadtDataDecl mkDataKeyword instRule (mkAnnMaybe opt kind) (mkAnnList (listSepBefore " | " " = ") cons) 
                             (mkAnnMaybe (optBefore " deriving ") derivs)

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
