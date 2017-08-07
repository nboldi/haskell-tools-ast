module Language.Haskell.Tools.Refactor.Builtin.DataToNewtype (dataToNewtype, tryItOut) where

import Control.Reference ((.=), (.-), (&))
import Language.Haskell.Tools.Refactor

tryItOut :: String -> IO ()
tryItOut moduleName = tryRefactor (\_ -> localRefactoring dataToNewtype) moduleName ""

dataToNewtype :: LocalRefactoring dom
dataToNewtype = return . (modDecl & annList .- changeDeclaration)

changeDeclaration :: Decl dom -> Decl dom
changeDeclaration dd@(DataDecl DataKeyword _ _ (AnnList [ConDecl _ (AnnList [_])]) _)
  = declNewtype .= mkNewtypeKeyword $ dd
changeDeclaration decl = decl