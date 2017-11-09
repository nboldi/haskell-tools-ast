{-# LANGUAGE DeriveDataTypeable,
             DeriveFunctor,
             DeriveFoldable,
             DeriveTraversable,
             GeneralizedNewtypeDeriving,
             StandaloneDeriving,
             TypeSynonymInstances
             #-}

module StandaloneNewtypeSynonyms where

import SynonymDefinitions
import StandaloneDataSynonyms


deriving instance Show    (T0 a)  {-* TypeSynonymInstances, StandaloneDeriving *-}
deriving instance Read    (T0 a)  {-* TypeSynonymInstances, StandaloneDeriving *-}

deriving instance Eq      (T0 a)  {-* TypeSynonymInstances, StandaloneDeriving, GeneralizedNewtypeDeriving *-}
deriving instance Ord     (T0 a)  {-* TypeSynonymInstances, StandaloneDeriving, GeneralizedNewtypeDeriving *-}
deriving instance Bounded (T0 a)  {-* TypeSynonymInstances, StandaloneDeriving, GeneralizedNewtypeDeriving *-}
deriving instance Ix      (T0 a)  {-* TypeSynonymInstances, StandaloneDeriving, GeneralizedNewtypeDeriving *-}

deriving instance Data a     => Data     (T0 a) {-* TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable *-}
deriving instance Typeable a => Typeable (T0 a) {-* TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable *-}

deriving instance Functor     T0  {-* TypeSynonymInstances, StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveFunctor *-}
deriving instance Foldable    T0  {-* TypeSynonymInstances, StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveFoldable *-}
deriving instance Traversable T0  {-* TypeSynonymInstances, StandaloneDeriving, GeneralizedNewtypeDeriving, DeriveTraversable *-}
