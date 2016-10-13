-- | Representation of Haskell expressions
module Language.Haskell.Tools.AST.Exprs where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Types
import Language.Haskell.Tools.AST.Patterns
import Language.Haskell.Tools.AST.Stmts
import {-# SOURCE #-} Language.Haskell.Tools.AST.TH
import {-# SOURCE #-} Language.Haskell.Tools.AST.Binds (LocalBind, LocalBinds, RhsGuard)

-- | Haskell expressions
data Expr dom stage
  = UVar            { _exprName :: Ann Name dom stage 
                    } -- ^ A variable or a data constructor (@ a @)
  | ULit            { _exprLit :: Ann Literal dom stage
                    } -- ^ Primitive literal
  | UInfixApp       { _exprLhs :: Ann Expr dom stage
                    , _exprOperator :: Ann Operator dom stage
                    , _exprRhs :: Ann Expr dom stage
                    } -- ^ Infix operator application (@ a + b @)
  | UPrefixApp      { _exprOperator :: Ann Operator dom stage
                    , _exprRhs :: Ann Expr dom stage
                    } -- ^ Prefix operator application (@ -x @)
  | UApp            { _exprFun :: Ann Expr dom stage
                    , _exprArg :: Ann Expr dom stage
                    } -- ^ Function application (@ f 4 @)
                    -- unary minus omitted
  | ULambda         { _exprBindings :: AnnList Pattern dom stage -- ^ at least one
                    , _exprInner :: Ann Expr dom stage
                    } -- ^ Lambda expression (@ \a b -> a + b @)
  | ULet            { _exprFunBind :: AnnList LocalBind dom stage -- ^ nonempty
                    , _exprInner :: Ann Expr dom stage
                    } -- ^ Local binding (@ let x = 2; y = 3 in e x y @)
  | UIf             { _exprCond :: Ann Expr dom stage
                    , _exprThen :: Ann Expr dom stage
                    , _exprElse :: Ann Expr dom stage
                    } -- ^ If expression (@ if a then b else c @)
  | UMultiIf        { _exprIfAlts :: AnnList GuardedCaseRhs dom stage
                    } -- ^ Multi way if expressions with @MultiWayIf@ extension (@ if | guard1 -> expr1; guard2 -> expr2 @)
  | UCase           { _exprCase :: Ann Expr dom stage
                    , _exprAlts :: AnnList Alt dom stage
                    } -- ^ Pattern matching expression (@ case expr of pat1 -> expr1; pat2 -> expr2 @)
  | UDo             { _doKind :: Ann DoKind dom stage
                    , _exprStmts :: AnnList Stmt dom stage
                    } -- ^ Do-notation expressions (@ do x <- act1; act2 @)
  | UTuple          { _tupleElems :: AnnList Expr dom stage
                    } -- ^ Tuple expression (@ (e1, e2, e3) @)
  | UUnboxedTuple   { _tupleElems :: AnnList Expr dom stage
                    } -- ^ Unboxed tuple expression (@ (# e1, e2, e3 #) @)
  | UTupleSection   { _tupleSectionElems :: AnnList TupSecElem dom stage
                    } -- ^ Tuple section, enabled with @TupleSections@ (@ (a,,b) @). One of the elements must be missing.
  | UUnboxedTupSec  { _tupleSectionElems :: AnnList TupSecElem dom stage
                    }
  | UList           { _listElems :: AnnList Expr dom stage
                    } -- ^ List expression: @[1,2,3]@
  | UParArray       { _listElems :: AnnList Expr dom stage
                    } -- ^ Parallel array expression: @[: 1,2,3 :]@
  | UParen          { _exprInner :: Ann Expr dom stage
                    }
  | ULeftSection    { _exprLhs :: Ann Expr dom stage
                    , _exprOperator :: Ann Operator dom stage
                    } -- ^ Left operator section: @(1+)@
  | URightSection   { _exprOperator :: Ann Operator dom stage
                    , _exprRhs :: Ann Expr dom stage
                    } -- ^ Right operator section: @(+1)@
  | URecCon         { _exprRecName :: Ann Name dom stage
                    , _exprRecFields :: AnnList FieldUpdate dom stage
                    } -- ^ Record value construction: @Point { x = 3, y = -2 }@
  | URecUpdate      { _exprInner :: Ann Expr dom stage
                    , _exprRecFields :: AnnList FieldUpdate dom stage
                    } -- ^ Record value  update: @p1 { x = 3, y = -2 }@
  | UEnum           { _enumFrom :: Ann Expr dom stage
                    , _enumThen :: AnnMaybe Expr dom stage
                    , _enumTo :: AnnMaybe Expr dom stage
                    } -- ^ Enumeration expression (@ [1,3..10] @)
  | UParArrayEnum   { _enumFrom :: Ann Expr dom stage
                    , _enumThen :: AnnMaybe Expr dom stage
                    , _enumToFix :: Ann Expr dom stage
                    } -- ^ Parallel array enumeration (@ [: 1,3 .. 10 :] @)
  | UListComp       { _compExpr :: Ann Expr dom stage
                    , _compBody :: AnnList ListCompBody dom stage -- ^ Can only have 1 element without @ParallelListComp@
                    } -- ^ List comprehension (@ [ (x, y) | x <- xs | y <- ys ] @)
  | UParArrayComp   { _compExpr :: Ann Expr dom stage
                    , _compBody :: AnnList ListCompBody dom stage
                    } -- ^ Parallel array comprehensions @ [: (x, y) | x <- xs , y <- ys :] @ enabled by @ParallelArrays@
  | UTypeSig        { _exprInner :: Ann Expr dom stage
                    , _exprSig :: Ann Type dom stage
                    } -- ^ Explicit type signature (@ _x :: Int @)
  | UExplTypeApp    { _exprInner :: Ann Expr dom stage
                    , _exprType :: Ann Type dom stage
                    } -- ^ Explicit type application (@ show \@Integer (read "5") @)
  | UVarQuote       { _quotedName :: Ann Name dom stage
                    } -- ^ @'x@ for template haskell reifying of expressions
  | UTypeQuote      { _quotedName :: Ann Name dom stage
                    } -- ^ @''T@ for template haskell reifying of types
  | UBracketExpr    { _bracket :: Ann Bracket dom stage
                    } -- ^ Template haskell bracket expression
  | USplice         { _innerExpr :: Ann Splice dom stage
                    } -- ^ Template haskell splice expression, for example: @$(gen a)@ or @$x@
  | UQuasiQuoteExpr { _exprQQ :: Ann QuasiQuote dom stage
                    } -- ^ Template haskell quasi-quotation: @[$quoter|str]@
  | UExprPragma     { _exprPragma :: Ann ExprPragma dom stage
                    }
  -- Arrows
  | UProc           { _procPattern :: Ann Pattern dom stage
                    , _procExpr :: Ann Cmd dom stage
                    } -- ^ Arrow definition: @proc a -> f -< a+1@
  | UArrowApp       { _exprLhs :: Ann Expr dom stage
                    , _arrowAppl :: Ann ArrowAppl dom stage
                    , _exprRhs :: Ann Expr dom stage
                    } -- ^ Arrow application: @f -< a+1@
  | ULamCase        { _exprAlts :: AnnList Alt dom stage
                    } -- ^ Lambda case ( @\case 0 -> 1; 1 -> 2@ )
  | UStaticPtr      { _exprInner :: Ann Expr dom stage
                    } -- ^ Static pointer expression (@ static e @). The inner expression must be closed (cannot have variables bound outside)
  -- XML expressions omitted
                   
-- | Field update expressions
data FieldUpdate dom stage
  = UNormalFieldUpdate { _fieldName :: Ann Name dom stage
                       , _fieldValue :: Ann Expr dom stage
                       } -- ^ Update of a field (@ x = 1 @)
  | UFieldPun          { _fieldUpdateName :: Ann Name dom stage
                       } -- ^ Update the field to the value of the same name (@ x @)
  | UFieldWildcard     { _fieldWildcard :: Ann FieldWildcard dom stage
                       } -- ^ Update the fields of the bounded names to their values (@ .. @). Must be the last initializer. Cannot be used in a record update expression.
      
-- | Marker for a field wildcard. Only needed to attach semantic information in a type-safe way.
data FieldWildcard dom stage = FldWildcard

-- | An element of a tuple section that can be an expression or missing (indicating a value from a parameter)
data TupSecElem dom stage
  = Present { _tupSecExpr :: Ann Expr dom stage
            } -- ^ An existing element in a tuple section
  | Missing -- ^ A missing element in a tuple section
  
-- | Clause of case expression          
data Alt' expr dom stage
  = UAlt { _altPattern :: Ann Pattern dom stage
         , _altRhs :: Ann (CaseRhs' expr) dom stage
         , _altBinds :: AnnMaybe LocalBinds dom stage
         }
type Alt = Alt' Expr
type CmdAlt = Alt' Cmd

  
-- | Right hand side of a match (possible with guards): (@ = 3 @ or @ | x == 1 = 3; | otherwise = 4 @)
data CaseRhs' expr dom stage
  = UUnguardedCaseRhs { _rhsCaseExpr :: Ann expr dom stage
                      }
  | UGuardedCaseRhss  { _rhsCaseGuards :: AnnList (GuardedCaseRhs' expr) dom stage
                      }
type CaseRhs = CaseRhs' Expr
type CmdCaseRhs = CaseRhs' Cmd
                     
-- | A guarded right-hand side of pattern matches binding (@ | x > 3 -> 2 @)      
data GuardedCaseRhs' expr dom stage
  = UGuardedCaseRhs { _caseGuardStmts :: AnnList RhsGuard dom stage -- ^ Cannot be empty.
                    , _caseGuardExpr :: Ann expr dom stage
                    } 
type GuardedCaseRhs = GuardedCaseRhs' Expr
type CmdGuardedCaseRhs = GuardedCaseRhs' Cmd
               
-- | Pragmas that can be applied to expressions
data ExprPragma dom stage
  = CorePragma      { _pragmaStr :: Ann StringNode dom stage
                    }
  | SccPragma       { _pragmaStr :: Ann StringNode dom stage
                    }
  | GeneratedPragma { _pragmaSrcRange :: Ann SourceRange dom stage
                    }

-- | In-AST source ranges (for generated pragmas)
data SourceRange dom stage
  = SourceRange { _srFileName :: Ann StringNode dom stage
                , _srFromLine :: Ann Number dom stage
                , _srFromCol :: Ann Number dom stage
                , _srToLine :: Ann Number dom stage
                , _srToCol :: Ann Number dom stage
                }  
                
data Number dom stage
  = Number { _numberInteger :: Integer 
           }
        
data Cmd dom stage
  = ArrowAppCmd   { _cmdLhs :: Ann Expr dom stage
                  , _cmdArrowOp :: Ann ArrowAppl dom stage
                  , _cmdRhs :: Ann Expr dom stage
                  }
  | ArrowFormCmd  { _cmdExpr :: Ann Expr dom stage
                  , _cmdInnerCmds :: AnnList Cmd dom stage
                  }
  | AppCmd        { _cmdInnerCmd :: Ann Cmd dom stage
                  , _cmdApplied :: Ann Expr dom stage
                  }
  | InfixCmd      { _cmdLeftCmd :: Ann Cmd dom stage
                  , _cmdOperator :: Ann Name dom stage
                  , _cmdRightCmd :: Ann Cmd dom stage
                  }
  | LambdaCmd     { _cmdBindings :: AnnList Pattern dom stage -- ^ at least one
                  , _cmdInner :: Ann Cmd dom stage
                  }
  | ParenCmd      { _cmdInner :: Ann Cmd dom stage
                  }
  | CaseCmd       { _cmdExpr :: Ann Expr dom stage
                  , _cmdAlts :: AnnList CmdAlt dom stage
                  }
  | IfCmd         { _cmdExpr :: Ann Expr dom stage
                  , _cmdThen :: Ann Cmd dom stage
                  , _cmdElse :: Ann Cmd dom stage
                  }
  | LetCmd        { _cmdBinds :: AnnList LocalBind dom stage -- ^ nonempty
                  , _cmdInner :: Ann Cmd dom stage
                  }
  | DoCmd         { _cmdStmts :: AnnList (Stmt' Cmd) dom stage
                  }
 
