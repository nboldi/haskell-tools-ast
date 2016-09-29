{-# LANGUAGE RankNTypes, FlexibleContexts, ViewPatterns, TypeApplications, ScopedTypeVariables, ConstraintKinds, TypeFamilies #-}
module Language.Haskell.Tools.Refactor.DollarApp (dollarApp) where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.Gen
import Language.Haskell.Tools.Refactor.RefactorBase

import GHC
import Control.Reference hiding (element)
import SrcLoc
import PrelNames
import Outputable
import Data.Generics.Uniplate.Data
import BasicTypes as GHC
import MkId as GHC

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.RefactorBase
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

tryItOut3 moduleName sp = tryRefactor (localRefactoring $ dollarApp3 (readSrcSpan (toFileName "." moduleName) sp)) moduleName

type DollarRefactor dom = (Domain dom, HasNameInfo (SemanticInfo dom QualifiedName), HasFixityInfo (SemanticInfo dom QualifiedName))

dollarApp3 :: forall dom . DollarRefactor dom => RealSrcSpan -> LocalRefactoring dom
dollarApp3 sp = return . flip evalState [] . ((nodesContained sp !~ replaceExpr3) >=> (biplateRef !~ parenExpr3 @dom))

replaceExpr3 :: DollarRefactor dom => Ann Expr dom SrcTemplateStage -> State [SrcSpan] (Ann Expr dom SrcTemplateStage)
replaceExpr3 expr@(e -> App fun (e -> Paren (e -> InfixApp _ op _))) 
  | sema <- (op ^. element&operatorName&semantics) 
  , fmap getUnique (semanticsName sema) /= Just dollarIdKey 
      && (case semanticsFixity sema of Just (GHC.Fixity _ p _) | p > 0 -> False; _ -> True)
  = return expr
replaceExpr3 expr@(e -> App fun (e -> Paren arg)) 
  = do modify (getRange arg :)
       return $ mkInfixApp fun (mkUnqualOp "$") arg
replaceExpr3 expr = return expr


parenExpr3 :: Ann Expr dom SrcTemplateStage -> State [SrcSpan] (Ann Expr dom SrcTemplateStage)
parenExpr3 e = (element&exprLhs !~ parenDollar3) =<< (element&exprRhs !~ parenDollar3 $ e)

parenDollar3 :: Ann Expr dom SrcTemplateStage -> State [SrcSpan] (Ann Expr dom SrcTemplateStage)
parenDollar3 expr@(e -> InfixApp _ _ arg) 
  = do replacedRanges <- get
       if getRange arg `elem` replacedRanges 
         then return $ mkParen expr
         else return expr
parenDollar3 e = return e

----------------------------------------------------------------------------------

tryItOut4 moduleName sp = tryRefactor (localRefactoring $ dollarApp4 (readSrcSpan (toFileName "." moduleName) sp)) moduleName

type DollarRefactor2 dom = ( Domain dom, HasNameInfo (SemanticInfo dom QualifiedName), HasFixityInfo (SemanticInfo dom QualifiedName)
                           , SemanticInfo' dom SameInfoImportCls ~ ImportInfo Id)

dollarApp4 :: forall dom . DollarRefactor2 dom => RealSrcSpan -> LocalRefactoring dom
dollarApp4 sp = flip evalStateT [] . ((nodesContained sp !~ replaceExpr4) >=> (biplateRef !~ parenExpr4 @_ @dom))

replaceExpr4 :: DollarRefactor2 dom => Ann Expr dom SrcTemplateStage -> StateT [SrcSpan] (LocalRefactor dom) (Ann Expr dom SrcTemplateStage)
replaceExpr4 expr@(e -> App fun (e -> Paren (e -> InfixApp _ op _))) 
  | sema <- (op ^. element&operatorName&semantics) 
  , fmap getUnique (semanticsName sema) /= Just dollarIdKey 
      && (case semanticsFixity sema of Just (GHC.Fixity _ p _) | p > 0 -> False; _ -> True)
  = return expr
replaceExpr4 expr@(e -> App fun (e -> Paren arg)) 
  = do modify (getRange arg :)
       lift $ mkInfixApp fun <$> referenceOperator dollarId <*> pure arg
replaceExpr4 expr = return expr


parenExpr4 :: Monad m => Ann Expr dom SrcTemplateStage -> StateT [SrcSpan] m (Ann Expr dom SrcTemplateStage)
parenExpr4 e = (element&exprLhs !~ parenDollar4) =<< (element&exprRhs !~ parenDollar4 $ e)

parenDollar4 :: Monad m => Ann Expr dom SrcTemplateStage -> StateT [SrcSpan] m (Ann Expr dom SrcTemplateStage)
parenDollar4 expr@(e -> InfixApp _ _ arg) 
  = do replacedRanges <- get
       if getRange arg `elem` replacedRanges 
         then return $ mkParen expr
         else return expr
parenDollar4 e = return e

[dollarId] = filter ((dollarIdKey==) . getUnique) wiredInIds

--------------------------------------------------------------------------------

e = (^. element)

