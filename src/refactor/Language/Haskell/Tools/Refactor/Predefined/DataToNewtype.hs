module Language.Haskell.Tools.Refactor.Predefined.DataToNewtype (dataToNewtype) where

import Control.Reference

import Language.Haskell.Tools.Refactor

tryItOut moduleName = tryRefactor (localRefactoring $ dataToNewtype) moduleName

dataToNewtype :: Domain dom => LocalRefactoring dom
dataToNewtype = return . (modDecl & annList .- changeDeclaration)

changeDeclaration :: Decl dom -> Decl dom
changeDeclaration dd@(DataDecl ctx declHead (AnnList [ConDecl name (AnnList [arg])]) derivs)
  = declNewtype .= mkNewtypeKeyword $ dd
changeDeclaration decl = decl