{-# LANGUAGE RoleAnnotations #-}
module Language.Haskell.Tools.AST.Names where

type role UQualifiedName nominal nominal
data UQualifiedName dom stage