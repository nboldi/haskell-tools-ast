module Language.Haskell.Tools.Refactor.Builtin.HelloRefactor where

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.AST.SemaInfoClasses


import Control.Reference
import Debug.Trace (trace)
import SrcLoc (RealSrcSpan)

import Unique (getUnique)
import Id (idName)
import PrelNames (dollarIdKey)
import PrelInfo (wiredInIds)


[dollarName] = map idName $ filter ((dollarIdKey==) . getUnique) wiredInIds
tryItOut :: String -> String -> IO ()
tryItOut = tryRefactor (localRefactoring . helloRefactor)

helloRefactor :: Domain dom => RealSrcSpan -> LocalRefactoring dom
helloRefactor sp = return . (nodesContained sp !~ myRefactorFunc)

helloExpr :: Expr dom -> Expr dom
helloExpr e = trace ("\n### Hello: " ++ prettyPrint e) $ e
{-
myRefactorFunc :: (HasImportInfo dom,HasModuleInfo dom)=>Expr dom -> LocalRefactor Expr
myRefactorFunc (App a (Paren b)) = LocalRefactor(mkInfixApp a (referenceOperator (mkName "$")) b)
myRefactorFunc dom = LocalRefactor dom
-}

myRefactorFunc :: Expr dom -> Expr
myRefactorFunc (App a (Paren b)) = mkInfixApp a (mkUnqualOp (mkName "$")) b
myRefactorFunc dom = LocalRefactor dom
