module Language.Haskell.Tools.AST.ElementTypes where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AnnTrf.SourceTemplate

type AnnList node dom = AnnListG node dom SrcTemplateStage 
type AnnMaybe node dom = AnnMaybeG node dom SrcTemplateStage 

-- * Modules

type Module dom = Ann UModule dom SrcTemplateStage
type ModuleHead dom = Ann UModuleHead dom SrcTemplateStage
type ExportSpecs dom = Ann UExportSpecs dom SrcTemplateStage
type ExportSpec dom = Ann UExportSpec dom SrcTemplateStage
type IESpec dom = Ann UIESpec dom SrcTemplateStage
type SubSpec dom = Ann USubSpec dom SrcTemplateStage
type ModulePragma dom = Ann UModulePragma dom SrcTemplateStage
type FilePragma dom = Ann UFilePragma dom SrcTemplateStage
type ImportDecl dom = Ann UImportDecl dom SrcTemplateStage
type ImportSpec dom = Ann UImportSpec dom SrcTemplateStage
type ImportQualified dom = Ann UImportQualified dom SrcTemplateStage
type ImportSource dom = Ann UImportSource dom SrcTemplateStage
type ImportSafe dom = Ann UImportSafe dom SrcTemplateStage
type TypeNamespace dom = Ann UTypeNamespace dom SrcTemplateStage
type ImportRenaming dom = Ann UImportRenaming dom SrcTemplateStage

-- * Declarations

-- | Haskell declaration
type Decl dom = Ann UDecl dom SrcTemplateStage

-- | The list of declarations that can appear in a typeclass
type ClassBody dom = Ann UClassBody dom SrcTemplateStage

-- | Members of a class declaration 
type ClassElement dom = Ann UClassElement dom SrcTemplateStage

-- The declared (possibly parameterized) type (@ A x :+: B y @).
type DeclHead dom = Ann UDeclHead dom SrcTemplateStage

-- | Instance body is the implementation of the class functions (@ where a x = 1; b x = 2 @)
type InstBody dom = Ann UInstBody dom SrcTemplateStage

-- | Declarations inside an instance declaration.
type InstBodyDecl dom = Ann UInstBodyDecl dom SrcTemplateStage


type GadtConDecl dom = Ann UGadtConDecl dom SrcTemplateStage
type GadtConType dom = Ann UGadtConType dom SrcTemplateStage
type FieldWildcard dom = Ann UFieldWildcard dom SrcTemplateStage
type FunDeps dom = Ann UFunDeps dom SrcTemplateStage
type FunDep dom = Ann UFunDep dom SrcTemplateStage
type ConDecl dom = Ann UConDecl dom SrcTemplateStage
type FieldDecl dom = Ann UFieldDecl dom SrcTemplateStage
type Deriving dom = Ann UDeriving dom SrcTemplateStage

-- | The instance declaration rule, which is, roughly, the part of the instance declaration before the where keyword.
type InstanceRule dom = Ann UInstanceRule dom SrcTemplateStage

-- | The specification of the class instance declaration
type InstanceHead dom = Ann UInstanceHead dom SrcTemplateStage

type OverlapPragma dom = Ann UOverlapPragma dom SrcTemplateStage

type TypeEqn dom = Ann UTypeEqn dom SrcTemplateStage
type KindConstraint dom = Ann UKindConstraint dom SrcTemplateStage
type TyVar dom = Ann UTyVar dom SrcTemplateStage
type Type dom = Ann UType dom SrcTemplateStage
type Kind dom = Ann UKind dom SrcTemplateStage
type Context dom = Ann UContext dom SrcTemplateStage
type Assertion dom = Ann UAssertion dom SrcTemplateStage
type Expr dom = Ann UExpr dom SrcTemplateStage
type CompStmt dom = Ann UCompStmt dom SrcTemplateStage
type ValueBind dom = Ann UValueBind dom SrcTemplateStage
type Pattern dom = Ann UPattern dom SrcTemplateStage
type PatternField dom = Ann UPatternField dom SrcTemplateStage
type Splice dom = Ann USplice dom SrcTemplateStage
type QString dom = Ann QQString dom SrcTemplateStage
type Match dom = Ann UMatch dom SrcTemplateStage
type Rhs dom = Ann URhs dom SrcTemplateStage
type GuardedRhs dom = Ann UGuardedRhs dom SrcTemplateStage
type FieldUpdate dom = Ann UFieldUpdate dom SrcTemplateStage
-- type racket dom = Ann Bracket dom SrcTemplateStage
type TopLevelPragma dom = Ann UTopLevelPragma dom SrcTemplateStage
type Rule dom = Ann URule dom SrcTemplateStage
type AnnotationSubject dom = Ann UAnnotationSubject dom SrcTemplateStage
type MinimalFormula dom = Ann UMinimalFormula dom SrcTemplateStage
-- type xprPragma dom = Ann ExprPragma dom SrcTemplateStage
-- type ourceRange dom = Ann SourceRange dom SrcTemplateStage
-- type umber dom = Ann Number dom SrcTemplateStage
-- type uasiQuote dom = Ann QuasiQuote dom SrcTemplateStage
type RhsGuard dom = Ann URhsGuard dom SrcTemplateStage
type LocalBind dom = Ann ULocalBind dom SrcTemplateStage
type LocalBinds dom = Ann ULocalBinds dom SrcTemplateStage
type FixitySignature dom = Ann UFixitySignature dom SrcTemplateStage
type TypeSignature dom = Ann UTypeSignature dom SrcTemplateStage
type ListCompBody dom = Ann UListCompBody dom SrcTemplateStage
type TupSecElem dom = Ann UTupSecElem dom SrcTemplateStage
type TypeFamily dom = Ann UTypeFamily dom SrcTemplateStage
type TypeFamilySpec dom = Ann UTypeFamilySpec dom SrcTemplateStage
type InjectivityAnn dom = Ann UInjectivityAnn dom SrcTemplateStage
type PatternSynonym dom = Ann UPatternSynonym dom SrcTemplateStage
type PatSynRhs dom = Ann UPatSynRhs dom SrcTemplateStage
type PatSynLhs dom = Ann UPatSynLhs dom SrcTemplateStage
type PatSynWhere dom = Ann UPatSynWhere dom SrcTemplateStage
type PatternSignature dom = Ann UPatternTypeSignature dom SrcTemplateStage
type Role dom = Ann URole dom SrcTemplateStage
-- type md dom = Ann Cmd dom SrcTemplateStage
type LanguageExtension dom = Ann ULanguageExtension dom SrcTemplateStage
type MatchLhs dom = Ann UMatchLhs dom SrcTemplateStage
type Stmt dom = Ann UStmt dom SrcTemplateStage
type Alt dom = Ann UAlt dom SrcTemplateStage
type CaseRhs dom = Ann UCaseRhs dom SrcTemplateStage
type GuardedCaseRhs dom = Ann UGuardedCaseRhs dom SrcTemplateStage

-- * Literals

type Literal dom = Ann ULiteral dom SrcTemplateStage
type PromotedKind dom = Ann (UPromoted UKind) dom SrcTemplateStage

-- * Names

type Operator dom = Ann UOperator dom SrcTemplateStage
type Name dom = Ann UName dom SrcTemplateStage
type QualifiedName dom = Ann UQualifiedName dom SrcTemplateStage
type ModuleName dom = Ann UModuleName dom SrcTemplateStage
type NamePart dom = Ann UNamePart dom SrcTemplateStage
type StringNode dom = Ann UStringNode dom SrcTemplateStage
type DataOrNewtypeKeyword dom = Ann UDataOrNewtypeKeyword dom SrcTemplateStage
type DoKind dom = Ann UDoKind dom SrcTemplateStage
-- type ypeKeyword dom = Ann TypeKeyword dom SrcTemplateStage
type CallConv dom = Ann UCallConv dom SrcTemplateStage
-- type rrowAppl dom = Ann ArrowAppl dom SrcTemplateStage
type Safety dom = Ann USafety dom SrcTemplateStage
type ConlikeAnnot dom = Ann UConlikeAnnot dom SrcTemplateStage
type PhaseControl dom = Ann UPhaseControl dom SrcTemplateStage

-- * Optional AST elements

type MaybeContext dom = AnnMaybe UContext dom
type MaybeDeriving dom = AnnMaybe UDeriving dom
type MaybeLocalBinds dom = AnnMaybe ULocalBinds dom
type MaybeTypeFamilySpec dom = AnnMaybe UTypeFamilySpec dom
type MaybeKindConstraint dom = AnnMaybe UKindConstraint dom
type MaybeClassBody dom = AnnMaybe UClassBody dom
type MaybeInstBody dom = AnnMaybe UInstBody dom
type MaybeExpr dom = AnnMaybe UExpr dom
type MaybeExportSpecs dom = AnnMaybe UExportSpecs dom
type MaybeImportQualified dom = AnnMaybe UImportQualified dom
type MaybeImportSource dom = AnnMaybe UImportSource dom
type MaybeImportSafe dom = AnnMaybe UImportSafe dom
type MaybeImportSpec dom = AnnMaybe UImportSpec dom
type MaybeModuleHead dom = AnnMaybe UModuleHead dom
type MaybeModulePragma dom = AnnMaybe UModulePragma dom
type MaybeSubSpec dom = AnnMaybe USubSpec dom
type MaybeStringNode dom = AnnMaybe UStringNode dom
type MaybeImportRenaming dom = AnnMaybe UImportRenaming dom
type MaybeSafety dom = AnnMaybe USafety dom
type MaybePhaseControl dom = AnnMaybe UPhaseControl dom
type MaybeConlikeAnnot dom = AnnMaybe UConlikeAnnot dom
type MaybeFunDeps dom = AnnMaybe UFunDeps dom


-- * AST elements with multiplicity

type MatchList dom = AnnList UMatch dom
type DeclList dom = AnnList UDecl dom
type PatternList dom = AnnList UPattern dom
type OperatorList dom = AnnList UOperator dom
type NameList dom = AnnList UName dom
type LocalBindList dom = AnnList ULocalBind dom
type IESpecList dom = AnnList UIESpec dom
type RhsGuardList dom = AnnList URhsGuard dom
type GuardedRhsList dom = AnnList UGuardedRhs dom
type GuardedCaseRhsList dom = AnnList UGuardedCaseRhs dom
type ConDeclList dom = AnnList UConDecl dom
type TypeEqnList dom = AnnList UTypeEqn dom
type TypeList dom = AnnList UType dom
type FieldDeclList dom = AnnList UFieldDecl dom
type ExprList dom = AnnList UExpr dom
type FieldUpdateList dom = AnnList UFieldUpdate dom
type GadtConDeclList dom = AnnList UGadtConDecl dom
type ClassElementList dom = AnnList UClassElement dom
type InstBodyDeclList dom = AnnList UInstBodyDecl dom
type InstanceHeadList dom = AnnList UInstanceHead dom
type AltList dom = AnnList UAlt dom
type StmtList dom = AnnList UStmt dom
type KindList dom = AnnList UKind dom
type ListCompBodyList dom = AnnList UListCompBody dom
type ExportSpecList dom = AnnList UExportSpec dom
type FilePragmaList dom = AnnList UFilePragma dom
type ImportDeclList dom = AnnList UImportDecl dom
type PatternFieldList dom = AnnList UPatternField dom
type TyVarList dom = AnnList UTyVar dom
type AssertionList dom = AnnList UAssertion dom
type CompStmtList dom = AnnList UCompStmt dom
type RuleList dom = AnnList URule dom
type RoleList dom = AnnList URole dom
type MinimalFormulaList dom = AnnList UMinimalFormula dom
type FunDepList dom = AnnList UFunDep dom
