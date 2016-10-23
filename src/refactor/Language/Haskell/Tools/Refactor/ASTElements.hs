module Language.Haskell.Tools.Refactor.ASTElements where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AnnTrf.SourceTemplate

type AnnList node dom = AnnListG node dom SrcTemplateStage 
type AnnMaybe node dom = AnnMaybeG node dom SrcTemplateStage 

-- * Modules

type Module dom = Ann UModule dom SrcTemplateStage
type ModuleHead dom = Ann UModuleHead dom SrcTemplateStage
type ExportSpecList dom = Ann UExportSpecList dom SrcTemplateStage
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

type Decl dom = Ann UDecl dom SrcTemplateStage
type ClassBody dom = Ann UClassBody dom SrcTemplateStage
type ClassElement dom = Ann UClassElement dom SrcTemplateStage
type DeclHead dom = Ann UDeclHead dom SrcTemplateStage
type InstBody dom = Ann UInstBody dom SrcTemplateStage
type InstBodyDecl dom = Ann UInstBodyDecl dom SrcTemplateStage
type GadtConDecl dom = Ann UGadtConDecl dom SrcTemplateStage
type GadtConType dom = Ann UGadtConType dom SrcTemplateStage
type FieldWildcard dom = Ann UFieldWildcard dom SrcTemplateStage
type FunDeps dom = Ann UFunDeps dom SrcTemplateStage
type FunDep dom = Ann UFunDep dom SrcTemplateStage
type ConDecl dom = Ann UConDecl dom SrcTemplateStage
type FieldDecl dom = Ann UFieldDecl dom SrcTemplateStage
type Deriving dom = Ann UDeriving dom SrcTemplateStage
type InstanceRule dom = Ann UInstanceRule dom SrcTemplateStage
type InstanceHead dom = Ann UInstanceHead dom SrcTemplateStage
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
-- type plice dom = Ann Splice dom SrcTemplateStage
type QString dom = Ann QQString dom SrcTemplateStage
type Match dom = Ann UMatch dom SrcTemplateStage
type Rhs dom = Ann URhs dom SrcTemplateStage
type GuardedRhs dom = Ann UGuardedRhs dom SrcTemplateStage
type FieldUpdate dom = Ann UFieldUpdate dom SrcTemplateStage
-- type racket dom = Ann Bracket dom SrcTemplateStage
-- type opLevelPragma dom = Ann TopLevelPragma dom SrcTemplateStage
-- type ule dom = Ann Rule dom SrcTemplateStage
-- type nnotationSubject dom = Ann AnnotationSubject dom SrcTemplateStage
-- type inimalFormula dom = Ann MinimalFormula dom SrcTemplateStage
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
type PatternTypeSignature dom = Ann UPatternTypeSignature dom SrcTemplateStage
-- type ole dom = Ann Role dom SrcTemplateStage
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
-- type verlapPragma dom = Ann OverlapPragma dom SrcTemplateStage
-- type allConv dom = Ann CallConv dom SrcTemplateStage
-- type rrowAppl dom = Ann ArrowAppl dom SrcTemplateStage
-- type afety dom = Ann Safety dom SrcTemplateStage
-- type onlikeAnnot dom = Ann ConlikeAnnot dom SrcTemplateStage
-- type ssoc dom = Ann Assoc dom SrcTemplateStage
-- type recedence dom = Ann Precedence dom SrcTemplateStage
-- type ineNumber dom = Ann LineNumber dom SrcTemplateStage
-- type haseControl dom = Ann PhaseControl dom SrcTemplateStage
-- type haseNumber dom = Ann PhaseNumber dom SrcTemplateStage
-- type haseInvert dom = Ann PhaseInvert dom SrcTemplateStage


type MaybeLocalBinds dom = AnnMaybeG ULocalBinds dom SrcTemplateStage
type DeclList dom = AnnListG UDecl dom SrcTemplateStage
type LocalBindList dom = AnnListG ULocalBind dom SrcTemplateStage
type IESpecList dom = AnnListG UIESpec dom SrcTemplateStage
