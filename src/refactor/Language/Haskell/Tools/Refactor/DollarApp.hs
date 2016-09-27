{-# LANGUAGE RankNTypes, FlexibleContexts, ViewPatterns, TypeApplications, ScopedTypeVariables #-}
module Language.Haskell.Tools.Refactor.DollarApp (dollarApp) where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen
import Language.Haskell.Tools.Refactor.RefactorBase

import Control.Reference hiding (element)
import SrcLoc
import PrelNames
import Data.Generics.Uniplate.Data

import Language.Haskell.Tools.Refactor
--
import Control.Monad.State

--
import Debug.Trace

tryItOut moduleName sp = tryRefactor (localRefactoring $ dollarApp (readSrcSpan (toFileName "." moduleName) sp)) moduleName

dollarApp :: Domain dom => RealSrcSpan -> LocalRefactoring dom
dollarApp sp = return . (nodesContained sp .- replaceExpr)

replaceExpr :: Ann Expr dom SrcTemplateStage -> Ann Expr dom SrcTemplateStage
replaceExpr (e -> App fun (e -> Paren arg)) = mkInfixApp fun (mkUnqualOp "$") arg
replaceExpr e = e

----------------------------------------------------------------------------------

tryItOut2 moduleName sp = tryRefactor (localRefactoring $ dollarApp2 (readSrcSpan (toFileName "." moduleName) sp)) moduleName

dollarApp2 :: forall dom . Domain dom => RealSrcSpan -> LocalRefactoring dom
dollarApp2 sp = return . flip evalState [] . ((nodesContained sp !~ replaceExpr2) >=> (biplateRef !~ parenExpr @dom))

replaceExpr2 :: Ann Expr dom SrcTemplateStage -> State [SrcSpan] (Ann Expr dom SrcTemplateStage)
replaceExpr2 expr@(e -> App fun (e -> Paren arg)) = do modify (getRange arg :)
                                                       return $ mkInfixApp fun (mkUnqualOp "$") arg
replaceExpr2 expr = return expr


parenExpr :: Ann Expr dom SrcTemplateStage -> State [SrcSpan] (Ann Expr dom SrcTemplateStage)
parenExpr e = (element&exprLhs !~ parenDollar) =<< (element&exprRhs !~ parenDollar $ e)

parenDollar :: Ann Expr dom SrcTemplateStage -> State [SrcSpan] (Ann Expr dom SrcTemplateStage)
parenDollar expr@(e -> InfixApp _ _ arg) 
  = do replacedRanges <- get
       if getRange arg `elem` replacedRanges 
         then return $ mkParen expr
         else return expr
parenDollar e = return e

----------------------------------------------------------------------------------

-- tryItOut3 moduleName sp = tryRefactor (localRefactoring $ dollarApp3 (readSrcSpan (toFileName "." moduleName) sp)) moduleName

-- dollarApp3 :: forall dom . (Domain dom, HasNameInfo (SemanticInfo dom QualifiedName)) => RealSrcSpan -> LocalRefactoring dom
-- dollarApp3 sp = return . flip evalState [] . ((nodesContained sp !~ replaceExpr3) >=> (biplateRef !~ parenExpr3 @dom))

-- replaceExpr3 :: (Domain dom, HasNameInfo (SemanticInfo dom QualifiedName)) => Ann Expr dom SrcTemplateStage -> State [SrcSpan] (Ann Expr dom SrcTemplateStage)
-- replaceExpr3 expr@(e -> App fun (e -> Paren (e -> InfixApp _ op _))) 
--   | Just sema <- semanticsName (op ^. element&operatorName&semantics)
--   , getUnique sema /= dollarIdKey && sema
--   = return expr
-- replaceExpr3 expr@(e -> App fun (e -> Paren arg)) 
--   = do modify (getRange arg :)
--        return $ mkInfixApp fun (mkUnqualOp "$") arg
-- replaceExpr3 expr = return expr


-- parenExpr3 :: Ann Expr dom SrcTemplateStage -> State [SrcSpan] (Ann Expr dom SrcTemplateStage)
-- parenExpr3 e = (element&exprLhs !~ parenDollar3) =<< (element&exprRhs !~ parenDollar3 $ e)

-- parenDollar3 :: Ann Expr dom SrcTemplateStage -> State [SrcSpan] (Ann Expr dom SrcTemplateStage)
-- parenDollar3 expr@(e -> InfixApp _ _ arg) 
--   = do replacedRanges <- get
--        if getRange arg `elem` replacedRanges 
--          then return $ mkParen expr
--          else return expr
-- parenDollar3 e = return e

--------------------------------------------------------------------------------

e = (^. element)

