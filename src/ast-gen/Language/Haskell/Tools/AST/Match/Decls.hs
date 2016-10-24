-- | UPattern matching on declaration-level AST fragments for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Decls where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.AST.Match.Names

-- WORKAROUND: nested pattern synonyms don't work in GHC 8.0, so I replaced them with longer but working pattern

-- * Declarations

pattern TypeDecl :: DeclHead dom -> Type dom -> Decl dom 
pattern TypeDecl dh typ <- Ann _ (UTypeDecl dh typ)

pattern TypeFamily :: DeclHead dom -> MaybeTypeFamilySpec dom -> Decl dom
pattern TypeFamily dh famSpec <- Ann _ (UTypeFamilyDecl (Ann _ (UTypeFamily dh famSpec)))

pattern DataFamily :: DeclHead dom -> MaybeKindConstraint dom -> Decl dom
pattern DataFamily dh kind <- Ann _ (UTypeFamilyDecl (Ann _ (UDataFamily dh kind)))

pattern ClosedTypeFamily :: DeclHead dom -> MaybeKindConstraint dom -> TypeEqnList dom -> Decl dom
pattern ClosedTypeFamily dh kind typeqs <- Ann _ (UClosedTypeFamilyDecl dh kind typeqs)

pattern DataDecl :: DataOrNewtypeKeyword dom -> MaybeContext dom -> DeclHead dom -> ConDeclList dom -> MaybeDeriving dom -> Decl dom
pattern DataDecl keyw ctx dh cons derivs <- Ann _ (UDataDecl keyw ctx dh cons derivs)

pattern GADTDataDecl :: DataOrNewtypeKeyword dom -> MaybeContext dom -> DeclHead dom -> MaybeKindConstraint dome -> AnnListG UGadtConDecl dom stage -> MaybeDeriving dom -> Decl dom
pattern GADTDataDecl keyw ctx dh kind cons derivs  <- Ann _ (UGDataDecl keyw ctx dh kind cons derivs )

pattern TypeInstance :: InstanceRule dom -> Type dom -> Decl dom
pattern TypeInstance instRule typ <- Ann _ (UTypeInstDecl instRule typ)

pattern DataInstance :: DataOrNewtypeKeyword dom -> InstanceRule dom -> ConDeclList dom -> MaybeDeriving dom -> Decl dom
pattern DataInstance keyw instRule cons derivs  <- Ann _ (UDataInstDecl keyw instRule cons derivs )

pattern GadtDataInstance :: DataOrNewtypeKeyword dom -> InstanceRule dom -> MaybeKindConstraint dome -> GadtConDeclList dom -> Decl dom
pattern GadtDataInstance keyw instRule kind cons  <- Ann _ (UGDataInstDecl keyw instRule kind cons )

pattern StandaloneDeriving :: InstanceRule dom -> Decl dom
pattern StandaloneDeriving instRule <- Ann _ (UDerivDecl _ instRule)

pattern FixityDecl :: FixitySignature dom -> Decl dom
pattern FixityDecl fixity <- Ann _ (UFixityDecl fixity)

pattern DefaultDecl :: TypeList dom -> Decl dom
pattern DefaultDecl types <- Ann _ (UDefaultDecl types)

pattern TypeSigDecl :: TypeSignature dom -> Decl dom
pattern TypeSigDecl typeSig <- Ann _ (UTypeSigDecl typeSig)

pattern ValueBinding :: ValueBind dom -> Decl dom
pattern ValueBinding bind <- Ann _ (UValueBinding bind)

pattern SpliceDecl :: Splice dom -> Decl dom
pattern SpliceDecl sp <- Ann _ (USpliceDecl sp)

-- * Type roles

pattern RoleDecl :: QualifiedName dom -> RoleList dom -> Decl dom
pattern RoleDecl name roles <- Ann _ (URoleDecl name roles)

pattern Nominal :: Role dom
pattern Nominal <- Ann _ UNominal

pattern Representational :: Role dom
pattern Representational <- Ann _ URepresentational

pattern Phantom :: Role dom
pattern Phantom <- Ann _ UPhantom

-- * Foreign imports and exports

pattern ForeignImport :: CallConv dom -> MaybeSafety dom -> Name dom -> Type dom -> Decl dom
pattern ForeignImport cc safety name typ <- Ann _ (UForeignImport cc safety name typ)

pattern ForeignExport :: CallConv dom -> Name dom -> Type dom -> Decl dom
pattern ForeignExport cc name typ <- Ann _ (UForeignExport cc name typ)

-- * Pattern synonyms

pattern PatternSynonym :: PatSynLhs dom -> PatSynRhs dom -> Decl dom
pattern PatternSynonym lhs rhs <- Ann _ (UPatternSynonymDecl (Ann _ (UPatternSynonym lhs rhs)))

pattern ConPatSyn :: Name dom -> NameList dom -> PatSynLhs dom
pattern ConPatSyn con args <- Ann _ (UNormalPatSyn con args)

pattern InfixPatSyn :: Name dom -> Operator dom -> Name dom -> PatSynLhs dom
pattern InfixPatSyn lhs op rhs <- Ann _ (UInfixPatSyn lhs op rhs)

pattern RecordPatSyn :: Name dom -> NameList dom -> PatSynLhs dom
pattern RecordPatSyn con args <- Ann _ (URecordPatSyn con args)

pattern SymmetricPatSyn :: Pattern dom -> PatSynRhs dom
pattern SymmetricPatSyn pat <- Ann _ (UBidirectionalPatSyn pat AnnNothing)

pattern OneWayPatSyn :: Pattern dom -> PatSynRhs dom
pattern OneWayPatSyn pat <- Ann _ (UOneDirectionalPatSyn pat)

pattern TwoWayPatSyn :: Pattern dom -> MatchList dom -> PatSynRhs dom
pattern TwoWayPatSyn pat match <- Ann _ (UBidirectionalPatSyn pat (AnnJust (Ann _ (UPatSynWhere match))))

pattern PatternSignatureDecl :: PatternSignature dom -> Decl dom
pattern PatternSignatureDecl patsig <- Ann _ (UPatTypeSigDecl patsig)

pattern PatternSignature :: Name dom -> Type dom -> PatternSignature dom
pattern PatternSignature name typ <- Ann _ (UPatternTypeSignature name typ)


-- * Type families

pattern TypeFamilyKindSpec :: KindConstraint dom -> TypeFamilySpec dom
pattern TypeFamilyKindSpec kind <- Ann _ (UTypeFamilyKind kind)

pattern TypeFamilyInjectivitySpec :: Name dom -> NameList dom -> TypeFamilySpec dom
pattern TypeFamilyInjectivitySpec res dependent <- Ann _ (UTypeFamilyInjectivity (Ann _ (UInjectivityAnn res dependent)))

-- * Type class declarations

pattern ClassDecl :: MaybeContext dom -> DeclHead dom -> MaybeClassBody dom -> Decl dom
pattern ClassDecl ctx dh body <- Ann _ (UClassDecl ctx dh _ body)

pattern ClassBody :: ClassElementList dom -> ClassBody dom
pattern ClassBody body <- Ann _ (UClassBody body)

pattern ClassElemSig :: TypeSignature dom -> ClassElement dom
pattern ClassElemSig typeSig <- Ann _ (UClsSig typeSig)

pattern ClassElemDef :: ValueBind dom -> ClassElement dom
pattern ClassElemDef def <- Ann _ (UClsDef def)

pattern ClassElemTypeFam :: DeclHead dom -> MaybeTypeFamilySpec dom -> ClassElement dom
pattern ClassElemTypeFam dh tfSpec <- Ann _ (UClsTypeFam (Ann _ (UTypeFamily dh tfSpec)))

pattern ClassElemDataFam :: DeclHead dom -> MaybeKindConstraint dome -> ClassElement dom
pattern ClassElemDataFam dh kind <- Ann _ (UClsTypeFam (Ann _ (UDataFamily dh kind)))

-- * Declaration heads

pattern NameDeclHead :: Name dom -> DeclHead dom
pattern NameDeclHead name <- Ann _ (UDeclHead name)

pattern ParenDeclHead :: DeclHead dom -> DeclHead dom
pattern ParenDeclHead dh <- Ann _ (UDHParen dh)

pattern DeclHeadApp :: DeclHead dom -> TyVar dom -> DeclHead dom
pattern DeclHeadApp dh tv <- Ann _ (UDHApp dh tv)

pattern InfixDeclHead :: TyVar dom -> Operator dom -> TyVar dom -> DeclHead dom
pattern InfixDeclHead lhs op rhs <- Ann _ (UDHInfix lhs op rhs)

-- * Type class instance declarations

pattern InstanceDecl :: InstanceRule dom -> MaybeInstBody dom -> Decl dom
pattern InstanceDecl instRule body <- Ann _ (UInstDecl _ instRule body)

pattern InstanceBody :: InstBodyDeclList dom -> InstBody dom
pattern InstanceBody defs <- Ann _ (UInstBody defs)

pattern InstanceElemDef :: ValueBind dom -> InstBodyDecl dom
pattern InstanceElemDef bind <- Ann _ (UInstBodyNormalDecl bind)

pattern InstanceElemTypeDef :: TypeEqn dom -> InstBodyDecl dom
pattern InstanceElemTypeDef typeEq <- Ann _ (UInstBodyTypeDecl typeEq)

pattern InstanceElemDataDef :: DataOrNewtypeKeyword dom -> InstanceRule dom -> ConDeclList dom -> MaybeDeriving dom -> InstBodyDecl dom
pattern InstanceElemDataDef keyw instRule cons derivs  <- Ann _ (UInstBodyDataDecl keyw instRule cons derivs )

pattern InstanceElemGadtDataDef :: DataOrNewtypeKeyword dom -> InstanceRule dom -> MaybeKindConstraint dome -> AnnListG UGadtConDecl dom stage 
                                     -> MaybeDeriving dom -> InstBodyDecl dom
pattern InstanceElemGadtDataDef keyw instRule kind cons derivs  <- Ann _ (UInstBodyGadtDataDecl keyw instRule kind cons derivs )

-- * Data type definitions

pattern GadtConDecl :: NameList dom -> Type dom -> GadtConDecl dom
pattern GadtConDecl names typ <- Ann _ (UGadtConDecl names (Ann _ (UGadtNormalType typ)))

pattern ConDecl :: Name dom -> TypeList dom -> ConDecl dom
pattern ConDecl name args <- Ann _ (UConDecl name args)

pattern RecordConDecl :: Name dom -> FieldDeclList dom -> ConDecl dom
pattern RecordConDecl name fields <- Ann _ (URecordDecl name fields)

pattern InfixConDecl :: Type dom -> Operator dom -> Type dom -> ConDecl dom
pattern InfixConDecl lhs op rhs <- Ann _ (UInfixConDecl lhs op rhs)

pattern FieldDecl :: NameList dom -> Type dom -> FieldDecl dom
pattern FieldDecl names typ <- Ann _ (UFieldDecl names typ)

pattern DerivingOne :: InstanceHead dom -> Deriving dom
pattern DerivingOne deriv <- Ann _ (UDerivingOne deriv)

pattern DerivingMulti :: InstanceHeadList dom -> Deriving dom
pattern DerivingMulti derivs <- Ann _ (UDerivings derivs)

pattern InstanceRule :: AnnMaybeG (AnnListG UTyVar) dom stage -> MaybeContext dom -> InstanceHead dom -> InstanceRule dom
pattern InstanceRule tvs ctx ih <- Ann _ (UInstanceRule tvs ctx ih)

pattern InstanceHead :: Name dom -> InstanceHead dom
pattern InstanceHead name <- Ann _ (UInstanceHeadCon name)

pattern InfixInstanceHead :: Type dom -> Name dom -> InstanceHead dom
pattern InfixInstanceHead typ n <- Ann _ (UInstanceHeadInfix typ n)

pattern ParenInstanceHead :: InstanceHead dom -> InstanceHead dom
pattern ParenInstanceHead ih <- Ann _ (UInstanceHeadParen ih)

pattern AppInstanceHead :: InstanceHead dom -> Type dom -> InstanceHead dom
pattern AppInstanceHead fun arg <- Ann _ (UInstanceHeadApp fun arg)

pattern TypeEqn :: Type dom -> Type dom -> TypeEqn dom
pattern TypeEqn lhs rhs <- Ann _ (UTypeEqn lhs rhs)

pattern DataKeyword :: DataOrNewtypeKeyword dom
pattern DataKeyword <- Ann _ UDataKeyword

pattern NewtypeKeyword :: DataOrNewtypeKeyword dom
pattern NewtypeKeyword <- Ann _ UNewtypeKeyword

-- * Top level pragmas

pattern PragmaDecl :: TopLevelPragma dom -> Decl dom
pattern PragmaDecl pragma <- Ann _ (UPragmaDecl pragma)

pattern RulePragma :: RuleList dom -> TopLevelPragma dom
pattern RulePragma rules <- Ann _ (URulePragma rules)

pattern DeprPragma :: NameList dom -> String -> TopLevelPragma dom
pattern DeprPragma defs msg <- Ann _ (UDeprPragma defs (Ann _ (UStringNode msg)))

pattern WarningPragma :: NameList dom -> String -> TopLevelPragma dom
pattern WarningPragma defs msg <- Ann _ (UWarningPragma defs (Ann _ (UStringNode msg)))

pattern AnnPragma :: AnnotationSubject dom -> Expr dom -> TopLevelPragma dom
pattern AnnPragma subj ann <- Ann _ (UAnnPragma subj ann)

pattern InlinePragma :: MaybeConlikeAnnot dom -> MaybePhaseControl dom -> Name dom -> TopLevelPragma dom
pattern InlinePragma conlike phase name <- Ann _ (UInlinePragma conlike phase name)

pattern NoInlinePragma :: MaybeConlikeAnnot dom -> MaybePhaseControl dom -> Name dom -> TopLevelPragma dom
pattern NoInlinePragma conlike phase name <- Ann _ (UNoInlinePragma conlike phase name)

pattern InlinablePragma :: MaybePhaseControl dom -> Name dom -> TopLevelPragma dom
pattern InlinablePragma phase name <- Ann _ (UInlinablePragma phase name)

pattern LinePragma :: Int -> MaybeStringNode dom -> TopLevelPragma dom
pattern LinePragma line filename <- Ann _ (ULinePragma (Ann _ (LineNumber line)) filename)

pattern SpecializePragma :: MaybePhaseControl dom -> Name dom -> TypeList dom -> TopLevelPragma dom
pattern SpecializePragma phase def specTypes <- Ann _ (USpecializePragma phase def specTypes)

