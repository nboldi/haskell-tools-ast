-- | Functions that convert the type-related elements of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.AST.FromGHC.Types where

import HsTypes as GHC
import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.AST.FromGHC.Monad
import Language.Haskell.Tools.AST.FromGHC.Names

trfType' :: TransformName n r => HsType n -> Trf (AST.UType (Dom r) RangeStage)
