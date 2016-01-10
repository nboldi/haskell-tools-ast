-- | Haskell literals
module Language.Haskell.Tools.AST.Literals where

import Language.Haskell.Tools.AST.Ann
import Language.Haskell.Tools.AST.Base

-- | Haskell literals
data Literal wt a
  = CharLit       { charLitValue :: Char } -- ^ character literal: @'c'@
  | StringLit     { stringLitValue :: String }
  | IntLit        { intLitValue :: Integer }
  | FracLit       { fracLitValue :: Rational }
  | PrimIntLit    { intLitValue :: Integer }
  | PrimFloatLit  { floatLitValue :: Rational }
  | PrimDoubleLit { floatLitValue :: Rational }
  | PrimCharLit   { charLitValue :: Char }
  | PrimStringLit { stringLitValue :: String }
               
-- | Literals promoted to kinds
data Promoted wt a
  = PromotedInt    { promotedIntValue :: Integer }
  | PromotedString { promotedStringValue :: String }
  | PromotedCon    { promotedConName :: Name wt a }
  | PromotedList   { promotedElements :: ListType Promoted wt a }
  | PromotedTuple  { promotedElements :: ListType Promoted wt a }
  | PromotedUnit
