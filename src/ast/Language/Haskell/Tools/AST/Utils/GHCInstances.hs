{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, OverloadedStrings #-}

module Language.Haskell.Tools.AST.Utils.GHCInstances where

import Data.Data
import Data.Aeson as A
import SrcLoc
import FastString (unpackFS)

deriving instance Data SrcLoc

instance Data RealSrcLoc where
    gfoldl k z rsl = z mkRealSrcLoc `k` srcLocFile rsl `k` srcLocLine rsl `k` srcLocCol rsl

    gunfold k z c = case constrIndex c of
                        1 -> k (k (k (z mkRealSrcLoc)))
                        _ -> error "gunfold: has only 1 constructor"

    toConstr _ = con_RSL
    dataTypeOf _ = ty_RSL

con_RSL :: Constr
con_RSL = mkConstr ty_RSL "RSL" [] Prefix

ty_RSL :: DataType
ty_RSL   = mkDataType "SrcLoc.RealSrcLoc" [con_RSL]


instance ToJSON SrcSpan where
  toJSON (RealSrcSpan sp) = object [ "file" A..= unpackFS (srcSpanFile sp)
                                   , "startRow" A..= srcLocLine (realSrcSpanStart sp)
                                   , "startCol" A..= srcLocCol (realSrcSpanStart sp)
                                   , "endRow" A..= srcLocLine (realSrcSpanEnd sp)
                                   , "endCol" A..= srcLocCol (realSrcSpanEnd sp)
                                   ]
  toJSON _ = Null

