{-# LANGUAGE RoleAnnotations #-}
module Language.Haskell.Tools.AST.Representation.TH where

type role USplice nominal nominal
data USplice dom stage

type role QuasiQuote nominal nominal
data QuasiQuote dom stage

type role Bracket nominal nominal
data Bracket dom stage