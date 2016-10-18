-- | Representation of Haskell statements (both do-notation and comprehensions)
module Language.Haskell.Tools.AST.Stmts where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Patterns
import {-# SOURCE #-} Language.Haskell.Tools.AST.Exprs (UExpr, Cmd)
import {-# SOURCE #-} Language.Haskell.Tools.AST.Binds (ULocalBind)

-- | Normal monadic statements
data UStmt' expr dom stage
  = UBindStmt { _stmtPattern :: Ann UPattern dom stage
              , _stmtExpr :: Ann expr dom stage
              } -- ^ Binding statement (@ x <- action @)
  | UExprStmt { _stmtExpr :: Ann expr dom stage
              } -- ^ Non-binding statement (@ action @)
  | ULetStmt  { _stmtBinds :: AnnList ULocalBind dom stage
              } -- ^ Let statement (@ let x = 3; y = 4 @)
  | URecStmt  { _cmdStmtBinds :: AnnList (UStmt' expr) dom stage
              } -- ^ A recursive binding statement with (@ rec b <- f a c; c <- f b a @)
type UStmt = UStmt' UExpr

-- | Body of a list comprehension: (@ | x <- [1..10] @)
data UListCompBody dom stage
  = UListCompBody { _compStmts :: AnnList UCompStmt dom stage
                  } 
         
-- | List comprehension statement
data UCompStmt dom stage
  = UCompStmt  { _compStmt :: Ann UStmt dom stage
               } -- ^ Normal monadic statement of a list comprehension
  | UThenStmt  { _thenExpr :: Ann UExpr dom stage
               , _byExpr :: AnnMaybe UExpr dom stage
               } -- ^ Then statements by @TransformListComp@ (@ then sortWith by (x + y) @)
  | UGroupStmt { _byExpr :: AnnMaybe UExpr dom stage
               , _usingExpr :: AnnMaybe UExpr dom stage
               } -- ^ Grouping statements by @TransformListComp@ (@ then group by (x + y) using groupWith @) 
                 -- Note: either byExpr or usingExpr must have a value