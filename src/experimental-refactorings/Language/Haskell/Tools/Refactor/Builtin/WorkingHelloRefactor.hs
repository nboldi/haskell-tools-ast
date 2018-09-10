{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Language.Haskell.Tools.Refactor.Builtin.DollarApp1 where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Debug.Trace (trace)
import Control.Reference ((.-))
import SrcLoc (RealSrcSpan)

tryItOut :: String -> String -> IO ()
tryItOut = tryRefactor (localRefactoring . dollarApp)

dollarApp :: Domain dom => RealSrcSpan -> LocalRefactoring dom
dollarApp sp = return . (nodesContained sp .- helloExpr)

replaceExpr :: Expr dom -> Expr dom
replaceExpr (App fun (Paren arg)) = mkInfixApp fun (mkUnqualOp "$") arg
replaceExpr e = e

helloExpr :: Expr dom -> Expr dom
helloExpr e = trace ("\n### Hello: " ++ prettyPrint e) $ e