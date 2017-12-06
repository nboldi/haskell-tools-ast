{-# LANGUAGE ConstraintKinds
           , FlexibleContexts
           , RankNTypes
           , ScopedTypeVariables
           , TupleSections
           , TypeApplications
           , TypeFamilies
           , ViewPatterns
           #-}
module Language.Haskell.Tools.Refactor.Utils.Type (typeExpr, appTypeMatches, literalType, splitType) where

import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Monad.IO.Class
import Control.Reference
import GHC
import Unify as GHC
import SrcLoc as GHC
import Type as GHC
import Name as GHC
import Outputable as GHC
import Id as GHC
import Var as GHC
import UniqSupply as GHC
import Unique as GHC
import Unify as GHC
import TysWiredIn as GHC
import TysPrim as GHC
import PrelNames as GHC
import BasicTypes as GHC
import Language.Haskell.Tools.Rewrite.ElementTypes as AST
import Language.Haskell.Tools.Rewrite.Match as AST
import Language.Haskell.Tools.AST as AST
import Language.Haskell.Tools.Refactor.Monad

typeExpr :: Expr -> Ghc GHC.Type
typeExpr e = do usupp <- liftIO $ mkSplitUniqSupply 'z'
                evalStateT (typeExpr' e) (uniqsFromSupply usupp)

typeExpr' :: Expr -> StateT [Unique] Ghc GHC.Type
typeExpr' Hole = resultType
typeExpr' (Var n) = return $ idType $ semanticsId (n ^. simpleName)
typeExpr' (Lambda args e) = do
  (resType, recomp) <- splitType <$> typeExpr' e
  (argTypes, recomps) <- unzip <$> mapM (\_ -> splitType <$> resultType) (args ^? annList)
  return $ foldr (.) recomp recomps $ mkFunTys argTypes resType
typeExpr' (Paren inner) = typeExpr' inner
typeExpr' (InfixApp lhs op rhs) = do
  lhsType <- typeExpr' lhs
  rhsType <- typeExpr' rhs
  let opType = idType $ semanticsId (op ^. operatorName)
  let (opt, repack) = splitType opType
      (args, resTyp) = splitFunTys opt
      lhsT:rhsT:rest = case args of _:_:_ -> args
                                    _ -> error (showSDocUnsafe (ppr (fst $ splitType opType)))
      subst = tcUnifyTys (\_ -> BindMe) [lhsType, rhsType] [lhsT, rhsT]
  return $ maybe id substTy subst $ repack (mkFunTys rest resTyp)
typeExpr' (PrefixApp op rhs) = do
  let opType = idType $ semanticsId (op ^. operatorName)
  argType <- typeExpr' rhs
  let (ft, repack) = splitType opType
      Just (argTyp, resTyp) = splitFunTy_maybe ft
      subst = tcUnifyTy argType argTyp
  return $ maybe id substTy subst $ repack resTyp
typeExpr' (Let _ e) = typeExpr' e
typeExpr' (If cond then' else') = typeExpr' then'
typeExpr' (MultiIf alts) =
  case alts ^? annList & caseGuardExpr of e:_ -> typeExpr' e
                                          _   -> resultType
typeExpr' (Case _ alts) =
  case alts ^? annList & altRhs & rhsCaseExpr of e:_ -> typeExpr' e
                                                 _   -> resultType
typeExpr' (Do stmts) =
  case stmts ^? annList & stmtExpr of e:_ -> typeExpr' e
                                      _   -> resultType
typeExpr' (App f arg) = do
  argType <- typeExpr' arg
  funType <- typeExpr' f
  let (ft, repack) = splitType funType
  case splitFunTy_maybe ft of
    Just (argTyp, resTyp) -> do
      let subst = tcUnifyTy argType argTyp
      return $ maybe id substTy subst $ repack resTyp
    Nothing -> return funType
typeExpr' (Tuple elems) = do
  (elemTypes, pack) <- unzip . map splitType <$> mapM typeExpr' (elems ^? annList)
  return $ foldr (.) id pack $ mkTupleTy Boxed elemTypes
typeExpr' (AST.UnboxedTuple elems) = do
  (elemTypes, pack) <- unzip . map splitType <$> mapM typeExpr' (elems ^? annList)
  return $ foldr (.) id pack $ mkTupleTy Unboxed elemTypes
-- typeExpr' (TupleSection elems) = 
-- typeExpr' (UnboxedTupSec elems) = 
typeExpr' (List elems) = do
  case elems ^? annList of []  -> do (typ, pack) <- splitType <$> resultType
                                     return $ pack $ mkListTy typ
                           e:_ -> do (typ, pack) <- splitType <$> typeExpr' e
                                     return $ pack $ mkListTy typ
typeExpr' (ParArray elems) = do
  case elems ^? annList of []  -> do (typ, pack) <- splitType <$> resultType
                                     return $ pack $ mkPArrTy typ
                           e:_ -> do (typ, pack) <- splitType <$> typeExpr' e
                                     return $ pack $ mkPArrTy typ
typeExpr' (Lit l) = literalType' l

literalType :: Literal -> Ghc GHC.Type
literalType e = do usupp <- liftIO $ mkSplitUniqSupply 'z'
                   evalStateT (literalType' e) (uniqsFromSupply usupp)

literalType' :: Literal -> StateT [Unique] Ghc GHC.Type
literalType' (StringLit {}) = return stringTy
literalType' (CharLit {}) = return charTy
literalType' (IntLit {}) = litType numClassName
literalType' (FracLit {}) = litType fractionalClassName
literalType' (PrimIntLit {}) = return $ intPrimTy
literalType' (PrimWordLit {}) = return $ word32PrimTy
literalType' (PrimFloatLit {}) = return $ floatX4PrimTy
literalType' (PrimDoubleLit {}) = return $ doubleX2PrimTy
literalType' (PrimCharLit {}) = return $ charPrimTy
literalType' (PrimStringLit {}) = return $ addrPrimTy

appTypeMatches :: [ClsInst] -> GHC.Type -> [GHC.Type] -> Bool
appTypeMatches insts funT argTs -- TODO: check instances
  = let (args, _) = splitFunTys $ fst $ splitType funT
        argTypes = fst $ unzip $ map splitType argTs
     in length args >= length argTypes
          && and (zipWith (\t -> isJust . tcUnifyTy t) args argTypes)

splitType :: GHC.Type -> (GHC.Type, GHC.Type -> GHC.Type)
splitType typ =
  let (foralls, typ') = splitForAllTys typ
      (args, coreType) = splitFunTys typ'
      (implicitArgs, realArgs) = partition isPredTy args
      repack t = mkInvForAllTys (filter (`elem` tvs) foralls) . mkFunTys (filter hasCommonTv implicitArgs) $ t
        where tvs = tyCoVarsOfTypeWellScoped t
              hasCommonTv = not . null . intersect tvs . tyCoVarsOfTypeWellScoped
   in (mkFunTys realArgs coreType, repack)

resultType :: StateT [Unique] Ghc GHC.Type
resultType = do 
  name <- newName
  let tv = mkTyVar name (mkTyConTy starKindTyCon)
      occn  = mkOccName tvName "a"
  return $ mkInvForAllTys [tv] (mkTyVarTy tv)

litType :: GHC.Name -> StateT [Unique] Ghc GHC.Type
litType constraint = do 
  Just (ATyCon numTyCon) <- lookupName numClassName
  name <- newName
  let tv = mkTyVar name (mkTyConTy starKindTyCon)
  return $ mkInvForAllTys [tv] $ mkFunTy (mkTyConApp numTyCon [mkTyVarTy tv]) (mkTyVarTy tv)

newName :: Monad m => StateT [Unique] m GHC.Name
newName = do uniq <- gets head
             modify tail
             let occn  = mkOccName tvName "a"
             return (mkInternalName uniq occn noSrcSpan)
