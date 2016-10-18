{-# LANGUAGE RoleAnnotations #-}
module Language.Haskell.Tools.AST.Exprs where

type role UExpr nominal nominal
data UExpr dom stage

type role Cmd nominal nominal
data Cmd dom stage

type role UFieldWildcard phantom phantom
data UFieldWildcard dom stage