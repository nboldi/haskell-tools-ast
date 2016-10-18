-- | Representation of Haskell patterns
module Language.Haskell.Tools.AST.Patterns where
          
import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base  
import Language.Haskell.Tools.AST.Literals
import Language.Haskell.Tools.AST.Types
import {-# SOURCE #-} Language.Haskell.Tools.AST.Exprs (UExpr, UFieldWildcard)
import {-# SOURCE #-} Language.Haskell.Tools.AST.TH

        
-- | Representation of patterns for pattern bindings
data Pattern dom stage
  = UVarPat         { _patternName :: Ann UName dom stage
                    } -- ^ Pattern name binding
  | ULitPat         { _patternLiteral :: Ann ULiteral dom stage
                    } -- ^ ULiteral pattern
  | UInfixAppPat    { _patternLhs :: Ann Pattern dom stage
                    , _patternOperator :: Ann UOperator dom stage
                    , _patternRhs :: Ann Pattern dom stage
                    } -- ^ Infix constructor application pattern (@ a :+: b @)
  | UAppPat         { _patternName :: Ann UName dom stage
                    , _patternArgs :: AnnList Pattern dom stage
                    } -- ^ Constructor application pattern (@ Point x y @)
  | UTuplePat       { _patternElems :: AnnList Pattern dom stage
                    } -- ^ Tuple pattern (@ (x,y) @)
  | UUnboxTuplePat  { _patternElems :: AnnList Pattern dom stage
                    } -- ^ Unboxed tuple pattern (@ (# x, y #) @)
  | UListPat        { _patternElems :: AnnList Pattern dom stage
                    } -- ^ List pattern (@ [1,2,a,x] @)
  | UParArrPat      { _patternElems :: AnnList Pattern dom stage
                    } -- ^ Parallel array pattern (@ [:1,2,a,x:] @)
  | UParenPat       { _patternInner :: Ann Pattern dom stage
                    } -- ^ Parenthesised patterns
  | URecPat         { _patternName :: Ann UName dom stage
                    , _patternFields :: AnnList PatternField dom stage
                    } -- ^ Record pattern (@ Point { x = 3, y } @)
  | UAsPat          { _patternName :: Ann UName dom stage
                    , _patternInner :: Ann Pattern dom stage
                    } -- ^ As-pattern (explicit name binding) (@ ls\@(hd:_) @)
  | UWildPat       -- ^ Wildcard pattern: (@ _ @)
  | UIrrefutablePat { _patternInner :: Ann Pattern dom stage
                    } -- ^ Irrefutable pattern (@ ~(x:_) @)
  | UBangPat        { _patternInner :: Ann Pattern dom stage
                    } -- ^ Bang pattern (@ !x @)
  | UTypeSigPat     { _patternInner :: Ann Pattern dom stage
                    , _patternType :: Ann Type dom stage
                    } -- ^ Pattern with explicit type signature (@ __ :: Int @)
  | UViewPat        { _patternExpr :: Ann UExpr dom stage
                    , _patternInner :: Ann Pattern dom stage
                    } -- ^ View pattern (@ f -> Just 1 @)
  -- regular list pattern omitted
  -- xml patterns omitted
  | USplicePat     { _patternSplice :: Ann Splice dom stage
                   } -- ^ Splice patterns: @$(generateX inp)@
  | UQuasiQuotePat { _patQQ :: Ann QuasiQuote dom stage
                   } -- ^ Quasi-quoted patterns: @[| 1 + 2 |]@
  | UNPlusKPat     { _patternName :: Ann UName dom stage
                   , _patternLit :: Ann ULiteral dom stage
                   }
                  
-- Field specification of a record pattern
data PatternField dom stage
  = UNormalFieldPattern   { _fieldPatternName :: Ann UName dom stage
                          , _fieldPattern :: Ann Pattern dom stage
                          } -- ^ Named field pattern (@ p = Point 3 2 @)
  | UFieldPunPattern      { _fieldPatternName :: Ann UName dom stage
                          } -- ^ Named field pun (@ p @)
  | UFieldWildcardPattern { _fieldPatternWildcard :: Ann UFieldWildcard dom stage
                          } -- ^ Wildcard field pattern (@ .. @)
