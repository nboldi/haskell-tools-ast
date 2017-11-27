{-# LANGUAGE KindSignatures,
             TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances
             #-}

module NoOccurenceON where

import Definitions

{-# ANN module "HLint: ignore Redundant bracket" #-}

-- NOTE: The FlexibleInstancesChecker should find some extensions,
--       because FlexibleInstances is turned on.
--       (However it shouldn't find FlexibleInstances)
--       Same test-cases as in NoOccurenceOFF.


instance (C1 (((T4) (a)) b c d)) where
    f1 _ = True

instance C1 (T2 a b) where
    f1 _ = True

instance C1 (T1 a) where
    f1 _ = True

-- (because T0 is a type ctor here)
instance C1 T0 where
    f1 _ = True

instance C1 (a :+: b) where   {-* TypeOperators *-}
  f1 _ = True

instance C1 ((:-:) a b) where
  f1 _ = True

instance (:?:) T0 T0 where    {-* MultiParamTypeClasses *-}
  h _ _ = True

instance T0 :!: T0 where      {-* TypeOperators, MultiParamTypeClasses *-}
  j _ _ = True

instance T0 :!: (T1 a) where  {-* TypeOperators, MultiParamTypeClasses *-}
  j _ _ = True

instance (T2 a b) :!: (T1 a) where  {-* TypeOperators, MultiParamTypeClasses *-}
  j _ _ = True

instance (a :+: b) :!: (T1 a) where  {-* TypeOperators, TypeOperators, MultiParamTypeClasses *-}
  j _ _ = True

instance C1 [(a :: *)] where         {-* KindSignatures *-}
  f1 _ = True

instance C2 (T1 a) (T1 a) where      {-* MultiParamTypeClasses *-}
  f2 _ _ = True
