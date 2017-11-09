{-# LANGUAGE DeriveDataTypeable,
             DeriveFunctor,
             DeriveFoldable,
             DeriveTraversable,
             DeriveAnyClass,
             GeneralizedNewtypeDeriving,
             StandaloneDeriving,
             TypeSynonymInstances
             #-}

module StandaloneDataSynonyms where

import SynonymDefinitions

deriving instance Show    (D0 a)  {-* TypeSynonymInstances, StandaloneDeriving *-}
deriving instance Read    (D0 a)  {-* TypeSynonymInstances, StandaloneDeriving *-}
deriving instance Eq      (D0 a)  {-* TypeSynonymInstances, StandaloneDeriving *-}
deriving instance Enum    (D0 a)  {-* TypeSynonymInstances, StandaloneDeriving *-}
deriving instance Ord     (D0 a)  {-* TypeSynonymInstances, StandaloneDeriving *-}
deriving instance Bounded (D0 a)  {-* TypeSynonymInstances, StandaloneDeriving *-}
deriving instance Ix      (D0 a)  {-* TypeSynonymInstances, StandaloneDeriving *-}

deriving instance Data a     => Data     (D0 a) {-* TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable *-}
deriving instance Typeable a => Typeable (D0 a) {-* TypeSynonymInstances, StandaloneDeriving, DeriveDataTypeable *-}
deriving instance Functor     D0  {-* TypeSynonymInstances, StandaloneDeriving, DeriveFunctor *-}
deriving instance Foldable    D0  {-* TypeSynonymInstances, StandaloneDeriving, DeriveFoldable *-}
deriving instance Traversable D0  {-* TypeSynonymInstances, StandaloneDeriving, DeriveTraversable *-}

deriving instance C1 (D0 a) {-* TypeSynonymInstances, StandaloneDeriving, DeriveAnyClass *-}
