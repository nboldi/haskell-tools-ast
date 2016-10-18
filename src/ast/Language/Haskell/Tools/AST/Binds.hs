-- | Representation of Haskell AST value and function bindings (both local and top-level)
module Language.Haskell.Tools.AST.Binds where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Patterns
import Language.Haskell.Tools.AST.Exprs
import Language.Haskell.Tools.AST.Types
import Language.Haskell.Tools.AST.Literals
import {-# SOURCE #-} Language.Haskell.Tools.AST.TH

-- | Value binding for top-level and local bindings
data UValueBind dom stage
  = USimpleBind { _valBindPat :: Ann Pattern dom stage
                , _valBindRhs :: Ann URhs dom stage
                , _valBindLocals :: AnnMaybe ULocalBinds dom stage
                } -- ^ Non-function binding (@ v = "12" @)  
  -- TODO: use one name for a function instead of names in each match
  | UFunBind    { _funBindMatches :: AnnList UMatch dom stage
                } -- ^ Function binding (@ f 0 = 1; f x = x @). All matches must have the same name.

-- | Clause of function (or value) binding   
data UMatch dom stage
  = UMatch { _matchLhs :: Ann UMatchLhs dom stage
           , _matchRhs :: Ann URhs dom stage
           , _matchBinds :: AnnMaybe ULocalBinds dom stage
           } 

-- | Something on the left side of the match
data UMatchLhs dom stage
  = UNormalLhs { _matchLhsName :: Ann UName dom stage
               , _matchLhsArgs :: AnnList Pattern dom stage
               }
  | UInfixLhs { _matchLhsLhs :: Ann Pattern dom stage
              , _matchLhsOperator :: Ann UOperator dom stage
              , _matchLhsRhs :: Ann Pattern dom stage
              , _matchLhsArgs :: AnnList Pattern dom stage
              }
    
-- | Local bindings attached to a declaration (@ where x = 42 @)             
data ULocalBinds dom stage
  = ULocalBinds { _localBinds :: AnnList ULocalBind dom stage
                }
  
-- | Bindings that are enabled in local blocks (where or let).
data ULocalBind dom stage
  = ULocalValBind   { _localVal :: Ann UValueBind dom stage
                    }
  -- TODO: check that no other signature can be inside a local binding
  | ULocalSignature { _localSig :: Ann UTypeSignature dom stage
                    }
  | ULocalFixity    { _localFixity :: Ann UFixitySignature dom stage
                    }
                   
-- | A type signature (@ _f :: Int -> Int @)
data UTypeSignature dom stage
  = UTypeSignature { _tsName :: AnnList UName dom stage
                   , _tsType :: Ann Type dom stage
                   }     
                   
-- | A fixity signature (@ infixl 5 +, - @).
data UFixitySignature dom stage
  = UFixitySignature { _fixityAssoc :: Ann Assoc dom stage
                     , _fixityPrecedence :: Ann Precedence dom stage
                     , _fixityOperators :: AnnList UOperator dom stage
                     }
   
-- | Right hand side of a value binding (possible with guards): (@ = 3 @ or @ | x == 1 = 3; | otherwise = 4 @)
data URhs dom stage
  = UUnguardedRhs { _rhsExpr :: Ann UExpr dom stage
                  }
  | UGuardedRhss  { _rhsGuards :: AnnList UGuardedRhs dom stage
                  }
      
-- | A guarded right-hand side of a value binding (@ | x > 3 = 2 @)      
data UGuardedRhs dom stage
  = UGuardedRhs { _guardStmts :: AnnList URhsGuard dom stage -- ^ Cannot be empty.
                , _guardExpr :: Ann UExpr dom stage
                } 

-- | Guards for value bindings and pattern matches (@ Just v <- x, v > 1 @)
data URhsGuard dom stage
  = UGuardBind  { _guardPat :: Ann Pattern dom stage
                , _guardRhs :: Ann UExpr dom stage
                }
  | UGuardLet   { _guardBinds :: AnnList ULocalBind dom stage
                }
  | UGuardCheck { _guardCheck :: Ann UExpr dom stage
                }

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
                      , _specializeType :: AnnList Type dom stage
                      }

-- | A rewrite rule (@ "map/map" forall f g xs. map f (map g xs) = map (f.g) xs @)
data Rule dom stage
  = URule { _ruleName :: Ann UStringNode dom stage -- ^ User name of the rule
          , _rulePhase :: AnnMaybe PhaseControl dom stage -- ^ The compilation phases in which the rule can be applied
          , _ruleBounded :: AnnList TyVar dom stage -- ^ Variables bound in the rule
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
