{-# LANGUAGE DeriveDataTypeable,
             DeriveTraversable,
             GeneralizedNewtypeDeriving,
             StandaloneDeriving
             #-}

module StandaloneNewtype where

import SynonymDefinitions
import StandaloneData


deriving instance Show    (T0' a)  {-* StandaloneDeriving *-}
deriving instance Read    (T0' a)  {-* StandaloneDeriving *-}

deriving instance Eq      (T0' a)  {-* StandaloneDeriving *-}
deriving instance Ord     (T0' a)  {-* StandaloneDeriving *-}
deriving instance Bounded (T0' a)  {-* StandaloneDeriving *-}
deriving instance Ix      (T0' a)  {-* StandaloneDeriving *-}

deriving instance Data a     => Data     (T0' a) {-* StandaloneDeriving, DeriveDataTypeable *-}
deriving instance Typeable a => Typeable (T0' a) {-* StandaloneDeriving, DeriveDataTypeable *-}

deriving instance Functor     T0'  {-* StandaloneDeriving, GeneralizedNewtypeDeriving *-}
deriving instance Foldable    T0'  {-* StandaloneDeriving, GeneralizedNewtypeDeriving *-}
deriving instance Traversable T0'  {-* StandaloneDeriving, DeriveTraversable *-}
