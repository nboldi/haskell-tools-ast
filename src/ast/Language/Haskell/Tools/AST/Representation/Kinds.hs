-- | Representation of Haskell Kinds
module Language.Haskell.Tools.AST.Representation.Kinds where

import Language.Haskell.Tools.AST.Representation.Literals
import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Representation.Names

-- | UKind constraint (@ :: * -> * @)
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
                  } -- ^ kind variable (using @PolyKinds@ extension)
  | UAppKind      { _kindAppFun :: Ann UKind dom stage
                  , _kindAppArg :: Ann UKind dom stage 
                  } -- ^ UKind application (@ k1 k2 @)
  | UListKind     { _kindElem :: Ann UKind dom stage
                  } -- ^ A list kind (@ [k] @)
  | UPromotedKind { _kindPromoted :: Ann (UPromoted UKind) dom stage
                  } -- ^ A promoted kind (@ '(k1,k2,k3) @)

data UPromoted t dom stage
  = UPromotedInt    { _promotedIntValue :: Integer }
  | UPromotedString { _promotedStringValue :: String }
  | UPromotedCon    { _promotedConName :: Ann UName dom stage }
  | UPromotedList   { _promotedElements :: AnnList t dom stage }
  | UPromotedTuple  { _promotedElements :: AnnList t dom stage }
  | UPromotedUnit