-- | Representation of Template Haskell AST elements
module Language.Haskell.Tools.AST.TH where
              
import Language.Haskell.Tools.AST.Decls
import Language.Haskell.Tools.AST.Binds
import Language.Haskell.Tools.AST.Exprs
import Language.Haskell.Tools.AST.Patterns
import Language.Haskell.Tools.AST.Types
import Language.Haskell.Tools.AST.Base
import Language.Haskell.Tools.AST.Ann
              
-- | A template haskell splice          
data Splice dom stage
  = IdSplice    { _spliceId :: Ann UName dom stage
                } -- ^ A simple name splice: @$generateX@
  | ParenSplice { _spliceExpr :: Ann UExpr dom stage
                } -- ^ A splice with parentheses: @$(generate input)@
  
-- | Template haskell quasi-quotation: @[quoter|str]@  
data QuasiQuote dom stage
  = QuasiQuote { _qqExprName :: Ann UName dom stage
               , _qqExprBody :: Ann QQString dom stage
               } 
        
-- | Template Haskell Quasi-quotation content
data QQString dom stage
  = QQString { _qqString :: String 
             } 

          
-- | Template Haskell bracket expressions
data Bracket dom stage
  = ExprBracket    { _bracketExpr :: Ann UExpr dom stage
                   } -- ^ Expression bracket (@ [| x + y |] @)
  | PatternBracket { _bracketPattern :: Ann UPattern dom stage
                   } -- ^ UPattern bracket (@ [| Point x y |] @)
  | TypeBracket    { _bracketType :: Ann Type dom stage
                   } -- ^ UPattern bracket (@ [| (Int,Int) |] @)
  | DeclsBracket   { _bracketDecl :: AnnList UDecl dom stage
                   } -- ^ Declaration bracket (@ [| _f :: Int -> Int; f x = x*x |] @)
            