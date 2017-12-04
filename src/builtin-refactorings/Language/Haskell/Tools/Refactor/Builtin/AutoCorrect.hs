{-# LANGUAGE ConstraintKinds
           , TypeFamilies
           , FlexibleContexts
           , ScopedTypeVariables
           , ViewPatterns
           , TupleSections
           #-}
module Language.Haskell.Tools.Refactor.Builtin.AutoCorrect (autoCorrect, tryItOut, autoCorrectRefactoring) where

import SrcLoc
import GHC
import Unify
import Type
import TysWiredIn
import TysPrim
import PrelNames
import qualified FastString as FS
-- 
import Control.Monad.State
import Control.Reference
import Data.List
import Data.Maybe
import Data.Either
-- 
import Language.Haskell.Tools.Refactor as HT
-- 
import Language.Haskell.Tools.PrettyPrint
import Debug.Trace
import Outputable

autoCorrectRefactoring :: RefactoringChoice
autoCorrectRefactoring = SelectionRefactoring "AutoCorrect" (localRefactoring . autoCorrect)

tryItOut :: String -> String -> IO ()
tryItOut mod sp = tryRefactor (localRefactoring . autoCorrect) mod sp

autoCorrect :: RealSrcSpan -> LocalRefactoring
autoCorrect sp mod
  = do res <- mapM (\f -> f sp mod) [reParen, reOrder]
       case catMaybes res of mod':_ -> return mod'
                             []     -> refactError "Cannot auto-correct the selection."

--------------------------------------------------------------------------------------------

reOrder :: RealSrcSpan -> HT.Module -> LocalRefactor (Maybe HT.Module)
reOrder sp mod = do let rng:_ = map getRange (mod ^? nodesContained sp :: [Expr])
                    (res,done) <- liftGhc $ flip runStateT False ((nodesContained sp & filtered ((==rng) . getRange) !~ reOrderExpr) mod)
                    return (if done then Just res else Nothing)

reOrderExpr :: Expr -> StateT Bool Ghc Expr
reOrderExpr e@(App (App f a1) a2)
  = do funTy <- lift $ typeExpr f
       arg1Ty <- lift $ typeExpr a1
       arg2Ty <- lift $ typeExpr a2
       liftIO $ putStrLn $ showSDocUnsafe (ppr funTy) ++ ", " 
                             ++ showSDocUnsafe (ppr arg1Ty) ++ ", " 
                             ++ showSDocUnsafe (ppr arg2Ty)
       liftIO $ putStrLn $ show (appTypeMatches funTy [arg2Ty, arg1Ty])
       liftIO $ putStrLn $ show (not (appTypeMatches funTy [arg1Ty, arg2Ty]))
       if not (appTypeMatches funTy [arg1Ty, arg2Ty]) && appTypeMatches funTy [arg2Ty, arg1Ty]
          then put True >> return (exprArg .= a1 $ exprFun&exprArg .= a2 $ e)
          else return e
reOrderExpr e@(InfixApp lhs op rhs) 
  = do let funTy = idType $ semanticsId (op ^. operatorName)
       lhsTy <- lift $ typeExpr lhs
       rhsTy <- lift $ typeExpr rhs
       if not (appTypeMatches funTy [lhsTy, rhsTy]) && appTypeMatches funTy [rhsTy, lhsTy]
          then put True >> return (exprLhs .= rhs $ exprRhs .= lhs $ e)
          else return e
reOrderExpr e = return e

reParen :: RealSrcSpan -> HT.Module -> LocalRefactor (Maybe HT.Module)
reParen sp mod = do let rng:_ = map getRange (mod ^? nodesContained sp :: [Expr])
                        (res,done) = flip runState False ((nodesContained sp & filtered ((==rng) . getRange) !~ reParenExpr) mod)
                    return (if done then Just res else Nothing)

reParenExpr :: Expr -> State Bool Expr
reParenExpr e = case correctParening $ map (_2 .- Left) $ extractAtoms e of
                  [e'] -> put True >> return (wrapAtom e')
                  [] -> return e
                  ls -> -- TODO: choose the best one
                        error $ "multiple correct parentheses were found: " ++ intercalate ", " (map (either prettyPrintAtom prettyPrint) ls)

data Atom = NameA { aName :: HT.Name }
          | OperatorA { aOperator :: Operator }
          | LiteralA { aLiteral :: Literal }

prettyPrintAtom :: Atom -> String
prettyPrintAtom (NameA n) = prettyPrint n
prettyPrintAtom (OperatorA o) = prettyPrint o
prettyPrintAtom (LiteralA l) = prettyPrint l

type Build = Either Atom Expr

extractAtoms :: Expr -> [(GHC.Type, Atom)]
extractAtoms e = sortOn (srcSpanStart . atomRange . snd) 
                   $ map (\n -> (idType $ semanticsId (n ^. simpleName), NameA n)) (e ^? biplateRef) 
                       ++ map (\o -> (idType $ semanticsId (o ^. operatorName), OperatorA o)) (e ^? biplateRef) 
                       ++ map (\l -> (literalType l, LiteralA l)) (e ^? biplateRef)

literalType :: Literal -> GHC.Type
literalType (StringLit {}) = stringTy
literalType (CharLit {}) = charTy
literalType (IntLit {}) = intTy -- TODO: polymorph type
literalType (FracLit {}) = intTy -- TODO: polymorph type
literalType (PrimIntLit {}) = intPrimTy
literalType (PrimWordLit {}) = word32PrimTy
literalType (PrimFloatLit {}) = floatX4PrimTy
literalType (PrimDoubleLit {}) = doubleX2PrimTy
literalType (PrimCharLit {}) = charPrimTy
literalType (PrimStringLit {}) = addrPrimTy

atomRange :: Atom -> SrcSpan
atomRange (NameA n) = getRange n
atomRange (OperatorA n) = getRange n
atomRange (LiteralA n) = getRange n

wrapAtom :: Build -> Expr
wrapAtom (Right e) = e
wrapAtom (Left (NameA n)) = mkVar n
wrapAtom (Left (OperatorA (NormalOp o))) = mkVar (mkParenName o)
wrapAtom (Left (OperatorA (BacktickOp n))) = mkVar (mkNormalName n)
wrapAtom (Left (LiteralA l)) = mkLit l

correctParening :: [(GHC.Type, Build)] -> [Build]
correctParening [(_,e)] = [e]
correctParening ls = concatMap correctParening (reduceAtoms ls)

reduceAtoms :: [(GHC.Type, Build)] -> [[(GHC.Type, Build)]]
reduceAtoms [(t,e)] = [[(t,e)]]
reduceAtoms ls = concatMap (reduceBy ls) [0 .. length ls - 1]

reduceBy :: [(GHC.Type, Build)] -> Int -> [[(GHC.Type, Build)]]
reduceBy (zip [0..] -> ls) i = maybeToList (reduceFunctionApp ls i) ++ maybeToList (reduceOperatorApp ls i)
  where reduceFunctionApp ls i | Just (funT, fun) <- lookup i ls
                               , Just (argT, arg) <- lookup (i+1) ls
                               , (filter (not . isPredTy) -> inpT:inpsT, resT) <- splitFunTys $ snd $ splitForAllTys funT
                               , Just subst <- tcUnifyTy argT inpT
          = Just $ map ((_1 .- substTy subst) . snd) (take i ls) 
                     ++ [(substTy subst (mkFunTys inpsT resT), mkParen' (mkApp' fun arg))] 
                     ++ map ((_1 .- substTy subst) . snd) (drop (i + 2) ls)
        reduceFunctionApp ls i = Nothing
        
        reduceOperatorApp ls i | Just (opT, Left (OperatorA op)) <- lookup i ls
                               , Just (lArgT, lArg) <- lookup (i-1) ls
                               , Just (rArgT, rArg) <- lookup (i+1) ls
                               , (filter (not . isPredTy) -> inp1T:inp2T:inpsT, resT) <- splitFunTys $ snd $ splitForAllTys opT
                               , Just subst <- tcUnifyTys (\_ -> BindMe) [lArgT,rArgT] [inp1T,inp2T]
          = Just $ map ((_1 .- substTy subst) . snd) (take (i - 1) ls) 
                     ++ [(substTy subst (mkFunTys inpsT resT), mkParen' (mkInfixApp' lArg op rArg))] 
                     ++ map ((_1 .- substTy subst) . snd) (drop (i + 2) ls)
        reduceOperatorApp ls i = Nothing

mkApp' :: Build -> Build -> Build
mkApp' (wrapAtom -> f) (wrapAtom -> a) = Right $ mkApp f a

mkInfixApp' :: Build -> Operator -> Build -> Build
mkInfixApp' (wrapAtom -> lhs) op (wrapAtom -> rhs) = Right $ mkInfixApp lhs op rhs

mkParen' :: Build -> Build
mkParen' (wrapAtom -> e) = Right $ mkParen e
