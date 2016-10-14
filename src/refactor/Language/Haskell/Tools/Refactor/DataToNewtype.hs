module Language.Haskell.Tools.Refactor.DataToNewtype (dataToNewtype) where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Rewrite
import Language.Haskell.Tools.Refactor.RefactorBase

import Language.Haskell.Tools.Refactor (tryRefactor)

tryItOut moduleName = tryRefactor (localRefactoring $ dataToNewtype) moduleName

dataToNewtype :: Domain dom => LocalRefactoring dom
dataToNewtype (Ann mi mod@(Module { _modDecl = AnnListC li ls })) 
  = return (Ann mi mod { _modDecl = AnnListC li (map changeDeclaration ls) })

changeDeclaration :: Ann Decl dom SrcTemplateStage -> Ann Decl dom SrcTemplateStage
changeDeclaration dd@(DataDecl _ _ cons _) 
  | annLength cons == 1 
      && annLength (_conDeclArgs (_element (_annListElems cons !! 0))) == 1
  = dd { _element = (_element dd) { _declNewtype = mkNewtypeKeyword }}
changeDeclaration decl = decl