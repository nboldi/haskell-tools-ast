{-# LANGUAGE TemplateHaskell #-}
module TH.Splice.UseImported where

$( [d| f :: Show a => a -> String; f = show |] )
