module Language.Haskell.Tools.Refactor.Predefined.DataToNewtype (dataToNewtype) where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Rewrite
import Language.Haskell.Tools.Refactor.ASTElements
import Language.Haskell.Tools.Refactor.RefactorBase

import Language.Haskell.Tools.Refactor (tryRefactor)

tryItOut moduleName = tryRefactor (localRefactoring $ dataToNewtype) moduleName

dataToNewtype :: Domain dom => LocalRefactoring dom
dataToNewtype mod
  = let modElem = _element mod
        modDecls = _modDecl modElem
        changedDecls = map changeDeclaration $ _annListElems modDecls
     in return mod { _element = modElem { _modDecl = modDecls { _annListElems = changedDecls } } }

changeDeclaration :: Decl dom -> Decl dom
changeDeclaration dd@(DataDecl _ _ cons _) 
  | annLength cons == 1 
      && annLength (_conDeclArgs (_element (_annListElems cons !! 0))) == 1
  = dd { _element = (_element dd) { _declNewtype = mkNewtypeKeyword }}
changeDeclaration decl = decl