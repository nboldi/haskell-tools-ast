
{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           #-}

-- | Simple AST elements of Haskell
module Language.Haskell.Tools.AST.Base where
  
import Language.Haskell.Tools.AST.Ann

data UOperator dom stage
  = UBacktickOp { _operatorName :: Ann UQualifiedName dom stage } -- ^ Backtick operator name: @ a `mod` b @
  | UNormalOp { _operatorName :: Ann UQualifiedName dom stage }

data UName dom stage
  = UParenName { _simpleName :: Ann UQualifiedName dom stage } -- ^ Parenthesized name: @ foldl (+) 0 @
  | UNormalName { _simpleName :: Ann UQualifiedName dom stage }
  | UImplicitName { _simpleName :: Ann UQualifiedName dom stage } -- ^ Implicit name: @ ?var @

-- | Possible qualified names. Contains also implicit names.
-- Linear implicit parameter: @%x@. Non-linear implicit parameter: @?x@.
data UQualifiedName dom stage
  = UQualifiedName { _qualifiers :: AnnList UNamePart dom stage
                  , _unqualifiedName :: Ann UNamePart dom stage
                  }

nameFromList :: AnnList UNamePart dom stage -> UQualifiedName dom stage
nameFromList (AnnListC a xs) | not (null xs) 
  = UQualifiedName (AnnListC a (init xs)) (last xs) 
nameFromList _ = error "nameFromList: empty list"
         
-- | Parts of a qualified name.         
data UNamePart dom stage
  = UNamePart { _simpleNameStr :: String } 
               
-- | Program elements formatted as string literals (import packages, pragma texts)
data UStringNode dom stage
  = UStringNode { _stringNodeStr :: String }

-- | The name of a module
data UModuleName dom stage = UModuleName { _moduleNameString :: String }
                   
-- | The @data@ or the @newtype@ keyword to define ADTs.
data UDataOrNewtypeKeyword dom stage
  = UDataKeyword
  | UNewtypeKeyword
    
-- | Keywords @do@ or @mdo@ to start a do-block
data UDoKind dom stage
  = UDoKeyword
  | UMDoKeyword
  
-- | The @type@ keyword used to qualify that the type and not the constructor of the same name is referred
data TypeKeyword dom stage = TypeKeyword
  
-- | Recognised overlaps for overlap pragmas. Can be applied to class declarations and class instance declarations.    
data OverlapPragma dom stage
  = EnableOverlap     -- ^ @OVERLAP@ pragma
  | DisableOverlap    -- ^ @NO_OVERLAP@ pragma
  | Overlappable      -- ^ @OVERLAPPABLE@ pragma
  | Overlapping       -- ^ @OVERLAPPING@ pragma
  | Overlaps          -- ^ @OVERLAPS@ pragma
  | IncoherentOverlap -- ^ @INCOHERENT@ pragma
  
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
  
data ArrowAppl dom stage
  = LeftAppl -- ^ Left arrow application: @-<@
  | RightAppl -- ^ Right arrow application: @>-@
  | LeftHighApp -- ^ Left arrow high application: @-<<@
  | RightHighApp -- ^ Right arrow high application: @>>-@
  
-- | Safety annotations for foreign calls
data Safety dom stage
  = Safe
  | ThreadSafe
  | Unsafe
  | Interruptible

-- | Associativity of an operator.
data Assoc dom stage
  = AssocNone  -- ^ non-associative operator (declared with @infix@)
  | AssocLeft  -- ^ left-associative operator (declared with @infixl@)
  | AssocRight -- ^ right-associative operator (declared with @infixr@)

data Role dom stage
  = Nominal
  | Representational
  | Phantom
  
data ConlikeAnnot dom stage = ConlikeAnnot

-- | Numeric precedence of an operator
data Precedence dom stage
  = Precedence { _precedenceValue :: Int } 

data LineNumber dom stage
  = LineNumber { _lineNumber :: Int } 
     
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