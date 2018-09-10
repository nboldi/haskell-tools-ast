{-{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
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
helloExpr e
 |prettyPrint e == "f (f (g 2))"=trace("True") $ e
 |otherwise = trace("false") $ e
-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Language.Haskell.Tools.Refactor.Builtin.DollarApp1 where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.PrettyPrint (prettyPrint)
import Debug.Trace (trace)
import Control.Reference
import Control.Monad.State
import Control.Monad.Identity
import SrcLoc (RealSrcSpan)
import Unique (getUnique)
import Id (idName)
import PrelNames (dollarIdKey)
import PrelInfo (wiredInIds)

tryItOut :: String -> String -> IO ()
tryItOut = tryRefactor (localRefactoring . dollarApp)

type DollarMonad = StateT Int Identity
myfunciton x = Just (x + 2)
dollarApp :: (HasImportInfo dom, HasModuleInfo dom) => RealSrcSpan -> LocalRefactoring dom
dollarApp sp = nodesContained sp !~ replaceExpr

replaceExpr :: (HasImportInfo dom, HasModuleInfo dom) => Expr dom -> LocalRefactor dom (Expr dom)
replaceExpr (App fun (Paren arg)) = ((mkInfixApp fun) <$> (referenceOperator dollarName)) <*> pure arg
replaceExpr e = pure e

helloExpr :: Expr dom -> Expr dom
helloExpr e = trace ("\n### Hello: " ++ prettyPrint e) $ e

[dollarName] = map idName $ filter ((dollarIdKey==) . getUnique) wiredInIds

{-m = put 2 :: StateT Int Identity ()
m = put 2 :: DollarMonad ()
m = get :: DollarMonad Int
-}

