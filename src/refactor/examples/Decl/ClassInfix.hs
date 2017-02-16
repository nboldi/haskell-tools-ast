module Decl.ClassInfix where

import Control.Monad

class (MonadPlus m) => MonadLogic m where
  (>>-)      :: m a -> (a -> m b) -> m b
  infixl 1 >>-
