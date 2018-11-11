{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}


module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.TraverseAST
  ( module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.TraverseAST
  , module Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
  ) where

import Data.Generics.ClassyPlate

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.ExtMonad
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Instances.Checkable()
import Language.Haskell.Tools.Refactor.Builtin.ExtensionOrganizer.Instances.AppSelector()


traverseModule :: CheckNode Module
traverseModule = topDownM @Checkable check
