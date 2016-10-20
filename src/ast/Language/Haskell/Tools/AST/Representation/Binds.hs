-- | Representation of Haskell AST value and function bindings (both local and top-level)
module Language.Haskell.Tools.AST.Representation.Binds where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Representation.Names
import Language.Haskell.Tools.AST.Representation.Patterns
import Language.Haskell.Tools.AST.Representation.Exprs
import Language.Haskell.Tools.AST.Representation.Types
import Language.Haskell.Tools.AST.Representation.Literals
import {-# SOURCE #-} Language.Haskell.Tools.AST.Representation.TH

-- | Value binding for top-level and local bindings
data UValueBind dom stage
  = USimpleBind { _valBindPat :: Ann UPattern dom stage
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
               , _matchLhsArgs :: AnnList UPattern dom stage
               }
  | UInfixLhs { _matchLhsLhs :: Ann UPattern dom stage
              , _matchLhsOperator :: Ann UOperator dom stage
              , _matchLhsRhs :: Ann UPattern dom stage
              , _matchLhsArgs :: AnnList UPattern dom stage
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
                   , _tsType :: Ann UType dom stage
                   }     
            
-- * Fixities

-- | A fixity signature (@ infixl 5 +, - @).
data UFixitySignature dom stage
  = UFixitySignature { _fixityAssoc :: Ann Assoc dom stage
                     , _fixityPrecedence :: Ann Precedence dom stage
                     , _fixityOperators :: AnnList UOperator dom stage
                     }

-- | Associativity of an operator.
data Assoc dom stage
  = AssocNone  -- ^ non-associative operator (declared with @infix@)
  | AssocLeft  -- ^ left-associative operator (declared with @infixl@)
  | AssocRight -- ^ right-associative operator (declared with @infixr@)
   
-- | Numeric precedence of an operator
data Precedence dom stage
  = Precedence { _precedenceValue :: Int } 

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
  = UGuardBind  { _guardPat :: Ann UPattern dom stage
                , _guardRhs :: Ann UExpr dom stage
                }
  | UGuardLet   { _guardBinds :: AnnList ULocalBind dom stage
                }
  | UGuardCheck { _guardCheck :: Ann UExpr dom stage
                }
