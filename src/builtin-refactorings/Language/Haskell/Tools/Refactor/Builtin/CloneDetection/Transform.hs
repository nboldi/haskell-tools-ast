{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}

module Language.Haskell.Tools.Refactor.Builtin.CloneDetection.Transform where

import Control.Reference
import GHC.Generics
import Language.Haskell.Tools.AST
import Data.Data
import SrcLoc
import Text.Read (readMaybe)
import GHC (Name)
import Data.Maybe (fromMaybe)
import Outputable

instance Show Name where
  show = showSDocUnsafe . ppr

data CCTree
    = CCNode { _height :: Int
             , _info :: CCInfo
             , _subtrees :: [CCTree]
             } deriving (Show, Eq)

data CCInfo
    = Info { _src :: SrcSpan
           , _cons :: Constr
           , _name :: Maybe (Either GHC.Name Int)
           }
    | Attr { a :: String}
    | NoInfo deriving (Show, Eq)

makeReferences ''CCInfo
makeReferences ''CCTree

type ShowSrcInfo st = (Show (SpanInfo st), Show (ListInfo st), Show (OptionalInfo st))

class (SourceInfo st,  Domain dom, Data (e dom st))
        => CCSerialize e dom st where
    put :: e dom st -> CCTree
    default put :: (GCCSerialize (Rep (e dom st)), Generic (e dom st), Domain dom) => e dom st -> CCTree
    put = gput . from

class GCCSerialize f where
  gput :: f p -> CCTree

instance GCCSerialize V1 where
  gput = error "GCCSerialize V1"

instance GCCSerialize U1 where
  gput U1 = CCNode 0 NoInfo []

instance (GCCSerialize f, GCCSerialize g) => GCCSerialize (f :+: g) where
  gput (L1 x) = gput x
  gput (R1 x) = gput x

instance (GCCSerialize f, GCCSerialize g) => GCCSerialize (f :*: g) where
  gput (x :*: y) = mkNode NoInfo [gput x, gput y]

instance {-# OVERLAPPING #-} CCSerialize e dom st => GCCSerialize (K1 i (e dom st)) where
  gput (K1 x) = put x

instance {-# OVERLAPPABLE #-} Show c => GCCSerialize (K1 i c) where
  gput (K1 x) = mkNode (Attr (mkString x)) []
    where
        mkString x = let x' = show x in fromMaybe x' (readMaybe x' :: Maybe String)

instance GCCSerialize f => GCCSerialize (M1 i t f) where
  gput (M1 x) = gput x

mkNode :: CCInfo -> [CCTree] -> CCTree
mkNode i ts = CCNode (1 + maxHeight ts) i ts
  where maxHeight ts = if null ts then 0 else (maximum . map (^. height)) ts

mkNode' :: SrcSpan -> Constr -> [CCTree] -> CCTree
mkNode' s c = mkNode (Info s c Nothing)

