module Definitions
  ( module Definitions
  , module X
  ) where

import Data.Data      as X
import Data.Typeable  as X
import Data.Generics  as X
import Data.Ix        as X

class C1 a where
  f1 :: a -> ()
  f1 _ = ()
