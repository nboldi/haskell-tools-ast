-- | Representation of Haskell Kinds
module Language.Haskell.Tools.AST.Representation.Kinds where

import Language.Haskell.Tools.AST.Ann (Ann, AnnListG)
import Language.Haskell.Tools.AST.Representation.Names (UName, UOperator)
import {-# SOURCE #-} Language.Haskell.Tools.AST.Representation.Types (UType)

-- | Kind constraint (@ :: * -> * @)
data UKindConstraint dom stage
  = UKindConstraint { _kindConstr :: Ann UKind dom stage
                    }

-- | Haskell kinds
data UKind dom stage
  = UStarKind  -- ^ @*@, the kind of types
  | UUnboxKind -- ^ @#@, the kind of unboxed types
  | UFunKind      { _kindLeft :: Ann UKind dom stage
                  , _kindRight :: Ann UKind dom stage
                  } -- ^ @->@, the kind of type constructor
  | UParenKind    { _kindParen :: Ann UKind dom stage
                  } -- ^ A parenthesised kind
  | UVarKind      { _kindVar :: Ann UName dom stage
                  } -- ^ Kind variable (using @PolyKinds@ extension)
  | UAppKind      { _kindAppFun :: Ann UKind dom stage
                  , _kindAppArg :: Ann UKind dom stage
                  } -- ^ Kind application (@ k1 k2 @)
  | UInfixAppKind { _kindLhs ::Ann UKind dom stage
                  , _kindAppOp :: Ann UOperator dom stage
                  , _kindRhs :: Ann UKind dom stage
                  } -- ^ Infix kind application (@ k1 ~> k2 @)
  | UListKind     { _kindElem :: Ann UKind dom stage
                  } -- ^ A list kind (@ [k] @)
  | UTupleKind    { _kindElems :: AnnListG UKind dom stage
                  } -- ^ A tuple kind (@ (Symbol, *) @)
  | UPromotedKind { _kindPromoted :: Ann (UPromoted UKind) dom stage
                  } -- ^ A promoted kind (@ '(k1,k2,k3) @)
  | UTypeKind     { _kindType :: Ann UType dom stage
                  } -- ^ A type on the kind level with @TypeInType@

data UPromoted t dom stage
  = UPromotedInt    { _promotedIntValue :: Integer
                    } -- ^ Numeric value promoted to the kind level.
  | UPromotedString { _promotedStringValue :: String
                    } -- ^ String value promoted to the kind level.
  | UPromotedCon    { _promotedConName :: Ann UName dom stage
                    } -- ^ A data constructor value promoted to the kind level.
  | UPromotedList   { _promotedElements :: AnnListG t dom stage
                    } -- ^ A list of elements as a kind.
  | UPromotedTuple  { _promotedElements :: AnnListG t dom stage
                    } -- ^ A tuple of elements as a kind.
  | UPromotedUnit -- ^ Kind of the unit value @()@.
