-- | Representation of Haskell AST definitions. These include definition of data types, classes, instances and so on. 
-- The definition of value bindings are in the Binds module.
module Language.Haskell.Tools.AST.Decls where

import Language.Haskell.Tools.AST.Binds
import Language.Haskell.Tools.AST.Types
import Language.Haskell.Tools.AST.Patterns
import Language.Haskell.Tools.AST.Kinds
import Language.Haskell.Tools.AST.Exprs
import Language.Haskell.Tools.AST.Names
import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Ann
import {-# SOURCE #-} Language.Haskell.Tools.AST.TH

-- * Declarations

-- | Haskell declaration
data UDecl dom stage
  = UTypeDecl             { _declHead :: Ann UDeclHead dom stage
                          , _declType :: Ann UType dom stage
                          } -- ^ A type synonym ( @type String = [Char]@ )
  | UTypeFamilyDecl       { _declTypeFamily :: Ann UTypeFamily dom stage
                          }
  | UClosedTypeFamilyDecl { _declHead :: Ann UDeclHead dom stage
                          , _declKind :: AnnMaybe UKindConstraint dom stage
                          , _declDecl :: AnnList UTypeEqn dom stage -- ^ cannot be empty
                          } -- ^ A closed type family declaration
  | UDataDecl             { _declNewtype :: Ann UDataOrNewtypeKeyword dom stage
                          , _declCtx  :: AnnMaybe UContext dom stage
                          , _declHead :: Ann UDeclHead dom stage
                          , _declCons :: AnnList UConDecl dom stage
                          , _declDeriving :: AnnMaybe UDeriving dom stage
                          } -- ^ A data or newtype declaration. Empty data type declarations without 
                            -- where keyword are always belong to DataDecl.
  | UGDataDecl            { _declNewtype :: Ann UDataOrNewtypeKeyword dom stage
                          , _declCtx  :: AnnMaybe UContext dom stage
                          , _declHead :: Ann UDeclHead dom stage
                          , _declKind :: AnnMaybe UKindConstraint dom stage
                          , _declGadt :: AnnList UGadtConDecl dom stage
                          , _declDeriving :: AnnMaybe UDeriving dom stage
                          } -- ^ A data or newtype declaration.
  | UTypeInstDecl         { _declInstance :: Ann UInstanceRule dom stage
                          , _declAssignedType :: Ann UType dom stage
                          } -- ^ UType instance declaration (@ type instance Fam T = AssignedT @)
  | UDataInstDecl         { _declNewtype :: Ann UDataOrNewtypeKeyword dom stage
                          , _declInstance :: Ann UInstanceRule dom stage
                          , _declCons :: AnnList UConDecl dom stage
                          , _declDeriving :: AnnMaybe UDeriving dom stage
                          } -- ^ Data instance declaration (@ data instance Fam T = Con1 | Con2 @)
  | UGDataInstDecl        { _declNewtype :: Ann UDataOrNewtypeKeyword dom stage
                          , _declInstance :: Ann UInstanceRule dom stage
                          , _declKind :: AnnMaybe UKindConstraint dom stage
                          , _declGadt :: AnnList UGadtConDecl dom stage
                          } -- ^ Gadt style data instance declaration (@ data instance Fam T where ... @)
  | UClassDecl            { _declCtx :: AnnMaybe UContext dom stage
                          , _declHead :: Ann UDeclHead dom stage
                          , _declFunDeps :: AnnMaybe UFunDeps dom stage
                          , _declBody :: AnnMaybe UClassBody dom stage
                          } -- ^ UType class declaration (@ class X a [where f = ...] @)
  | UInstDecl             { _declOverlap :: AnnMaybe OverlapPragma dom stage
                          , _declInstRule :: Ann UInstanceRule dom stage
                          , _declInstDecl :: AnnMaybe UInstBody dom stage
                          } -- ^ Instance declaration (@ instance X T [where f = ...] @)
  | UPatternSynonymDecl   { _declPatSyn :: Ann UPatternSynonym dom stage
                          } -- ^ UPattern synonyms (@ pattern Arrow t1 t2 = App "->" [t1, t2] @)
  | UDerivDecl            { _declOverlap :: AnnMaybe OverlapPragma dom stage
                          , _declInstRule :: Ann UInstanceRule dom stage
                          } -- ^ Standalone deriving declaration (@ deriving instance X T @)
  | UFixityDecl           { _declFixity :: Ann UFixitySignature dom stage
                          } -- ^ Fixity declaration (@ infixl 5 +, - @)
  | UDefaultDecl          { _declTypes :: AnnList UType dom stage
                          } -- ^ Default types (@ default (T1, T2) @)
  | UTypeSigDecl          { _declTypeSig :: Ann UTypeSignature dom stage
                          } -- ^ UType signature declaration (@ _f :: Int -> Int @)
  | UPatTypeSigDecl       { _declPatTypeSig :: Ann UPatternTypeSignature dom stage
                          } -- ^ UType signature declaration (@ _f :: Int -> Int @)
  | UValueBinding         { _declValBind :: Ann UValueBind dom stage
                          } -- ^ Function binding (@ f x = 12 @)
  | UForeignImport        { _declCallConv :: Ann CallConv dom stage
                          , _declSafety :: AnnMaybe Safety dom stage
                          , _declName :: Ann UName dom stage
                          , _declType :: Ann UType dom stage
                          } -- ^ Foreign import (@ foreign import _foo :: Int -> IO Int @)
  | UForeignExport        { _declCallConv :: Ann CallConv dom stage
                          , _declName :: Ann UName dom stage
                          , _declType :: Ann UType dom stage
                          } -- ^ foreign export (@ foreign export ccall _foo :: Int -> IO Int @)
  | UPragmaDecl           { _declPragma :: Ann TopLevelPragma dom stage
                          } -- ^ top level pragmas
  | URoleDecl             { _declRoleType :: Ann UQualifiedName dom stage
                          , _declRoles :: AnnList Role dom stage
                          } -- ^ role annotations (@ type role Ptr representational @)
  | USpliceDecl           { _declSplice :: Ann Splice dom stage
                          } -- ^ A Template Haskell splice declaration (@ $(generateDecls) @)

-- The declared (possibly parameterized) type (@ A x :+: B y @).
data UDeclHead dom stage
  = UDeclHead { _dhName :: Ann UName dom stage
              } -- ^ UType or class name
  | UDHParen  { _dhBody :: Ann UDeclHead dom stage
              } -- ^ Parenthesized type
  | UDHApp    { _dhAppFun :: Ann UDeclHead dom stage
              , _dhAppOperand :: Ann UTyVar dom stage
              } -- ^ UType application
  | UDHInfix  { _dhLeft :: Ann UTyVar dom stage
              , _dhOperator :: Ann UOperator dom stage
              , _dhRight :: Ann UTyVar dom stage
              } -- ^ Infix application of the type/class name to the left operand

-- * Type class definitions

-- | The list of declarations that can appear in a typeclass
data UClassBody dom stage
  = UClassBody { _cbElements :: AnnList UClassElement dom stage
               }
                 
-- | Members of a class declaration       
data UClassElement dom stage
  = UClsSig     { _ceTypeSig :: Ann UTypeSignature dom stage
                } -- ^ Signature: @ _f :: A -> B @
  | UClsDef     { _ceBind :: Ann UValueBind dom stage
                } -- ^ Default binding: @ f x = "aaa" @
  | UClsTypeFam { _ceTypeFam :: Ann UTypeFamily dom stage
                } -- ^ Declaration of an associated type synonym: @ type T _x :: * @ 
  | UClsTypeDef { _ceHead :: Ann UDeclHead dom stage
                , _ceKind :: Ann UType dom stage
                } -- ^ Default choice for type synonym: @ type T x = TE @ or @ type instance T x = TE @ 
  | UClsDefSig  { _ceName :: Ann UName dom stage
                , _ceType :: Ann UType dom stage
                } -- ^ Default signature (by using @DefaultSignatures@): @ default _enum :: (Generic a, GEnum (Rep a)) => [a] @
  | UClsMinimal { _pragmaFormula :: Ann MinimalFormula dom stage
                } -- ^ Minimal pragma: @ {-# MINIMAL (==) | (/=) #-} @

   -- not supported yet (GHC 7.10.3)
  | UClsPatSig  { _cePatSig :: Ann UPatternTypeSignature dom stage
                } -- ^ UPattern signature in a class declaration (by using @PatternSynonyms@)

-- * Type class instances
  
-- | The instance declaration rule, which is, roughly, the part of the instance declaration before the where keyword.
data UInstanceRule dom stage
  = UInstanceRule  { _irVars :: AnnMaybe (AnnList UTyVar) dom stage
                   , _irCtx :: AnnMaybe UContext dom stage
                   , _irHead :: Ann UInstanceHead dom stage
                   }
  | UInstanceParen { _irRule :: Ann UInstanceRule dom stage
                   }

-- | The specification of the class instance declaration
data UInstanceHead dom stage
  = UInstanceHeadCon   { _ihConName :: Ann UName dom stage
                       } -- ^ UType or class name
  | UInstanceHeadInfix { _ihLeftOp :: Ann UType dom stage
                       , _ihOperator :: Ann UName dom stage
                       } -- ^ Infix application of the type/class name to the left operand
  | UInstanceHeadParen { _ihHead :: Ann UInstanceHead dom stage
                       } -- ^ Parenthesized instance head
  | UInstanceHeadApp   { _ihFun :: Ann UInstanceHead dom stage
                       , _ihType :: Ann UType dom stage
                       } -- ^ Application to one more type

-- | Instance body is the implementation of the class functions (@ where a x = 1; b x = 2 @)
data UInstBody dom stage
  = UInstBody { _instBodyDecls :: AnnList UInstBodyDecl dom stage
              }

-- | Declarations inside an instance declaration.
data UInstBodyDecl dom stage
  = UInstBodyNormalDecl   { _instBodyDeclFunbind :: Ann UValueBind dom stage
                          } -- ^ A normal declaration (@ f x = 12 @)
  | UInstBodyTypeSig      { _instBodyTypeSig :: Ann UTypeSignature dom stage
                          } -- ^ UType signature in instance definition with @InstanceSigs@
  | UInstBodyTypeDecl     { _instBodyTypeEqn :: Ann UTypeEqn dom stage
                          } -- ^ An associated type definition (@ type A X = B @)
  | UInstBodyDataDecl     { _instBodyDataNew :: Ann UDataOrNewtypeKeyword dom stage
                          , _instBodyLhsType :: Ann UInstanceRule dom stage
                          , _instBodyDataCons :: AnnList UConDecl dom stage
                          , _instBodyDerivings :: AnnMaybe UDeriving dom stage
                          } -- ^ An associated data type implementation (@ data A X = C1 | C2 @)
  | UInstBodyGadtDataDecl { _instBodyDataNew :: Ann UDataOrNewtypeKeyword dom stage
                          , _instBodyLhsType :: Ann UInstanceRule dom stage
                          , _instBodyDataKind :: AnnMaybe UKindConstraint dom stage
                          , _instBodyGadtCons :: AnnList UGadtConDecl dom stage
                          , _instBodyDerivings :: AnnMaybe UDeriving dom stage
                          } -- ^ An associated data type implemented using GADT style
  | USpecializeInstance   { _specializeInstanceType :: Ann UType dom stage
                          } -- ^ Specialize instance pragma (no phase selection is allowed)
  -- not supported yet
  | UInstBodyPatSyn       { _instBodyPatSyn :: Ann UPatternSynonym dom stage
                          } -- ^ A pattern synonym in a class instance

-- | Recognised overlaps for overlap pragmas. Can be applied to class declarations and class instance declarations.    
data OverlapPragma dom stage
  = EnableOverlap     -- ^ @OVERLAP@ pragma
  | DisableOverlap    -- ^ @NO_OVERLAP@ pragma
  | Overlappable      -- ^ @OVERLAPPABLE@ pragma
  | Overlapping       -- ^ @OVERLAPPING@ pragma
  | Overlaps          -- ^ @OVERLAPS@ pragma
  | IncoherentOverlap -- ^ @INCOHERENT@ pragma

-- * Type families

-- | Open type and data families
data UTypeFamily dom stage
  = UTypeFamily { _tfHead :: Ann UDeclHead dom stage
                , _tfSpec :: AnnMaybe UTypeFamilySpec dom stage
                } -- ^ A type family declaration (@ type family A _a :: * -> * @)    
  | UDataFamily { _tfHead :: Ann UDeclHead dom stage
                , _tfKind :: AnnMaybe UKindConstraint dom stage
                } -- ^ Data family declaration

-- | UType family specification with kinds specification and injectivity.
data UTypeFamilySpec dom stage
  = UTypeFamilyKind { _tfSpecKind :: Ann UKindConstraint dom stage
                    }
  | UTypeFamilyInjectivity { _tfInjectivity :: Ann UInjectivityAnn dom stage
                           }

-- | Injectivity annotation for type families (@ = r | r -> a @)
data UInjectivityAnn dom stage
  = UInjectivityAnn { _injAnnRes :: Ann UName dom stage
                    , _injAnnDeps :: AnnList UName dom stage
                    }

-- | UType equations as found in closed type families (@ T A = S @)
data UTypeEqn dom stage
  = UTypeEqn { _teLhs :: Ann UType dom stage
             , _teRhs :: Ann UType dom stage
             }

-- * Type definitions

-- | GADT constructor declaration (@ _D1 :: { _val :: Int } -> T String @)
data UGadtConDecl dom stage
  = UGadtConDecl { _gadtConNames :: AnnList UName dom stage
                 , _gadtConType :: Ann UGadtConType dom stage
                 }
                   
-- | The @data@ or the @newtype@ keyword to define ADTs.
data UDataOrNewtypeKeyword dom stage
  = UDataKeyword
  | UNewtypeKeyword

-- | UType of GADT constructors (can be record types: @{ _val :: Int }@)
data UGadtConType dom stage
  = UGadtNormalType { _gadtConNormalType :: Ann UType dom stage
                    }
  | UGadtRecordType { _gadtConRecordFields :: AnnList UFieldDecl dom stage
                    , _gadtConResultType :: Ann UType dom stage
                    }

-- | A list of functional dependencies: @ | a -> b, c -> d @ separated by commas  
data UFunDeps dom stage
  = UFunDeps { _funDeps :: AnnList UFunDep dom stage
             } 
         
-- | A functional dependency, given on the form @l1 ... ln -> r1 ... rn@         
data UFunDep dom stage
  = UFunDep { _funDepLhs :: AnnList UName dom stage
            , _funDepRhs :: AnnList UName dom stage
            }
  
-- | A constructor declaration for a datatype
data UConDecl dom stage
  = UConDecl      { _conDeclName :: Ann UName dom stage
                  , _conDeclArgs :: AnnList UType dom stage
                  } -- ^ ordinary data constructor (@ C t1 t2 @)
  | URecordDecl   { _conDeclName :: Ann UName dom stage
                  , _conDeclFields :: AnnList UFieldDecl dom stage
                  } -- ^ record data constructor (@ C { _n1 :: t1, _n2 :: t2 } @)
  | UInfixConDecl { _conDeclLhs :: Ann UType dom stage
                  , _conDeclOp :: Ann UOperator dom stage
                  , _conDeclRhs :: Ann UType dom stage
                  } -- ^ infix data constructor (@ t1 :+: t2 @)
  
-- | Field declaration (@ _fld :: Int @)
data UFieldDecl dom stage
  = UFieldDecl { _fieldNames :: AnnList UName dom stage
               , _fieldType :: Ann UType dom stage
               }
  
-- | A deriving clause following a data type declaration. (@ deriving Show @ or @ deriving (Show, Eq) @)
data UDeriving dom stage
  = UDerivingOne { _oneDerived :: Ann UInstanceHead dom stage }
  | UDerivings { _allDerived :: AnnList UInstanceHead dom stage }

-- * Pattern synonyms

-- | A pattern type signature (@ pattern p :: Int -> T @)
data UPatternTypeSignature dom stage
  = UPatternTypeSignature { _patSigName :: Ann UName dom stage
                          , _patSigType :: Ann UType dom stage
                          }   

-- | UPattern synonyms: @ pattern Arrow t1 t2 = App "->" [t1, t2] @
data UPatternSynonym dom stage
  = UPatternSynonym { _patLhs :: Ann UPatSynLhs dom stage
                    , _patRhs :: Ann UPatSynRhs dom stage
                    }

-- | Left hand side of a pattern synonym
data UPatSynLhs dom stage
  = UNormalPatSyn { _patName :: Ann UName dom stage
                  , _patArgs :: AnnList UName dom stage
                  }
  | UInfixPatSyn { _patSynLhs :: Ann UName dom stage
                 , _patSynOp :: Ann UOperator dom stage
                 , _patSynRhs :: Ann UName dom stage
                 }
  | URecordPatSyn { _patName :: Ann UName dom stage
                  , _patArgs :: AnnList UName dom stage
                  }

-- | Right-hand side of pattern synonym
data UPatSynRhs dom stage
  -- TODO: this feels bad, changing _patRhsOpposite may switch between <- and =
  = UBidirectionalPatSyn { _patRhsPat :: Ann UPattern dom stage
                         , _patRhsOpposite :: AnnMaybe UPatSynWhere dom stage
                         } -- ^ @ pattern Int = App "Int" [] @ or @ pattern Int <- App "Int" [] where Int = App "Int" [] @
  | UOneDirectionalPatSyn { _patRhsPat :: Ann UPattern dom stage
                          } -- ^ @ pattern Int <- App "Int" [] @

-- | Where clause of pattern synonym (explicit expression direction)
data UPatSynWhere dom stage
  = UPatSynWhere { _patOpposite :: AnnList UMatch dom stage }

-- * Foreign imports
  
-- | Call conventions of foreign functions
data CallConv dom stage
  = StdCall
  | CCall
  | CPlusPlus
  | DotNet
  | Jvm
  | Js
  | JavaScript
  | CApi

-- | Safety annotations for foreign calls
data Safety dom stage
  = Safe
  | ThreadSafe
  | Unsafe
  | Interruptible

-- * Role annotations

-- | Role annotations for types
data Role dom stage
  = Nominal
  | Representational
  | Phantom

-- * Rewrite rules

-- | Controls the activation of a rewrite rule (@ [1] @)
data PhaseControl dom stage
  = PhaseControl { _phaseUntil :: AnnMaybe PhaseInvert dom stage
                 , _phaseNumber :: Ann PhaseNumber dom stage
                 } 

-- | Phase number for rewrite rules
data PhaseNumber dom stage
  = PhaseNumber { _phaseNum :: Integer }

-- | A tilde that marks the inversion of the phase number
data PhaseInvert dom stage = PhaseInvert

-- * Pragmas

-- | Top level pragmas
data TopLevelPragma dom stage
  = URulePragma       { _pragmaRule :: AnnList Rule dom stage
                      }
  | UDeprPragma       { _pragmaObjects :: AnnList UName dom stage
                      , _pragmaMessage :: Ann UStringNode dom stage
                      }
  | UWarningPragma    { _pragmaObjects :: AnnList UName dom stage
                      , _pragmaMessage :: Ann UStringNode dom stage
                      }
  | UAnnPragma        { _annotationSubject :: Ann AnnotationSubject dom stage
                      , _annotateExpr :: Ann UExpr dom stage
                      }
  | UInlinePragma     { _pragmaConlike :: AnnMaybe ConlikeAnnot dom stage
                      , _pragmaPhase :: AnnMaybe PhaseControl dom stage
                      , _inlineDef :: Ann UName dom stage
                      }
  | UNoInlinePragma   { _pragmaConlike :: AnnMaybe ConlikeAnnot dom stage
                      , _pragmaPhase :: AnnMaybe PhaseControl dom stage
                      , _noInlineDef :: Ann UName dom stage
                      }
  | UInlinablePragma  { _pragmaPhase :: AnnMaybe PhaseControl dom stage
                      , _inlinableDef :: Ann UName dom stage
                      }
  | ULinePragma       { _pragmaLineNum :: Ann LineNumber dom stage
                      , _pragmaFileName :: AnnMaybe UStringNode dom stage
                      }
  | USpecializePragma { _pragmaPhase :: AnnMaybe PhaseControl dom stage
                      , _specializeDef :: Ann UName dom stage
                      , _specializeType :: AnnList UType dom stage
                      }

-- | A rewrite rule (@ "map/map" forall f g xs. map f (map g xs) = map (f.g) xs @)
data Rule dom stage
  = URule { _ruleName :: Ann UStringNode dom stage -- ^ User name of the rule
          , _rulePhase :: AnnMaybe PhaseControl dom stage -- ^ The compilation phases in which the rule can be applied
          , _ruleBounded :: AnnList UTyVar dom stage -- ^ Variables bound in the rule
          , _ruleLhs :: Ann UExpr dom stage -- ^ The transformed expression
          , _ruleRhs :: Ann UExpr dom stage -- ^ The resulting expression
          }
 
-- | Annotation allows you to connect an expression to any declaration. 
data AnnotationSubject dom stage
  = UNameAnnotation { _annotateName :: Ann UName dom stage
                    } -- ^ The definition with the given name is annotated
  | UTypeAnnotation { _annotateName :: Ann UName dom stage
                    } -- ^ A type with the given name is annotated
  | UModuleAnnotation -- ^ The whole module is annotated

-- | Formulas of minimal annotations declaring which functions should be defined.
data MinimalFormula dom stage
  = UMinimalName  { _minimalName :: Ann UName dom stage
                  }
  | UMinimalParen { _minimalInner :: Ann MinimalFormula dom stage
                  }
  | UMinimalOr    { _minimalOrs :: AnnList MinimalFormula dom stage
                  } -- ^ One of the minimal formulas are needed (@ min1 | min2 @)
  | UMinimalAnd   { _minimalAnds :: AnnList MinimalFormula dom stage
                  } -- ^ Both of the minimal formulas are needed (@ min1 , min2 @)

data ConlikeAnnot dom stage = ConlikeAnnot

data LineNumber dom stage
  = LineNumber { _lineNumber :: Int } 
