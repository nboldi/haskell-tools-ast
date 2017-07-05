{-# LANGUAGE ImplicitParams #-}

module Expr.ImplicitParamsOrder where

x = do let ?a = ()
           ?b = ()
       return ()
