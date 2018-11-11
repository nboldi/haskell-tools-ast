{-# LANGUAGE MonoLocalBinds #-}

module Language.Haskell.Tools.Refactor.Builtin.HelloRefactor where

import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Language.Haskell.Tools.Refactor

import Control.Reference ((.-))
import Debug.Trace (trace)
import SrcLoc (RealSrcSpan)

tryItOut :: String -> String -> IO ()
tryItOut = tryRefactor (localRefactoring . helloRefactor)

helloRefactor :: RealSrcSpan -> LocalRefactoring
helloRefactor sp = return . (nodesContained sp .- helloExpr)

helloExpr :: Expr -> Expr
helloExpr e = trace ("\n### Hello: " ++ prettyPrint e) $ e
