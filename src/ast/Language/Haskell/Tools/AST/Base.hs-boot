{-# LANGUAGE RoleAnnotations #-}
module Language.Haskell.Tools.AST.Base where

type role UQualifiedName nominal nominal
data UQualifiedName dom stage