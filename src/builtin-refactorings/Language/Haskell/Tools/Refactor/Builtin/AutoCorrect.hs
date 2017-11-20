{-# LANGUAGE ConstraintKinds
           , TypeFamilies
           , FlexibleContexts
           , ScopedTypeVariables
           , ViewPatterns
           , TupleSections
           #-}
module Language.Haskell.Tools.Refactor.Builtin.AutoCorrect
  (autoCorrect, AutoCorrectDomain, tryItOut, autoCorrectRefactoring) where

import SrcLoc
import GHC
import Unify
import Type
import TysWiredIn
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

autoCorrectRefactoring :: (AutoCorrectDomain dom, HasModuleInfo dom) => RefactoringChoice dom
autoCorrectRefactoring = SelectionRefactoring "AutoCorrect" (localRefactoring . autoCorrect)

type AutoCorrectDomain dom = (HasIdInfo dom)

tryItOut :: String -> String -> IO ()
tryItOut mod sp = tryRefactor (localRefactoring . autoCorrect) mod sp

autoCorrect :: AutoCorrectDomain dom => RealSrcSpan -> LocalRefactoring dom
autoCorrect sp mod
  = do res <- mapM (\f -> f sp mod) [reParen]
       case catMaybes res of mod':_ -> return mod'
                             []     -> refactError "Cannot auto-correct the selection."

reParen :: forall dom . AutoCorrectDomain dom => RealSrcSpan -> HT.Module dom -> LocalRefactor dom (Maybe (HT.Module dom))
reParen sp mod = do let rng:_ = map getRange (mod ^? nodesContained sp :: [Expr dom])
                        (res,done) = flip runState False ((nodesContained sp & filtered ((==rng) . getRange) !~ reParenExpr) mod)
                    return (if done then Just res else Nothing)

reParenExpr :: AutoCorrectDomain dom => Expr dom -> State Bool (Expr dom)
reParenExpr e = trace ("### atoms: " ++ show (map (prettyPrintAtom . snd) (extractAtoms e)))
                 $ case correctParening $ map (_2 .- Left) $ extractAtoms e of
                     [e'] -> put True >> return (wrapAtom e')
                     [] -> return e
                     ls -> -- TODO: choose the best one
                           error $ "multiple correct parentheses were found: " ++ intercalate ", " (map (either prettyPrintAtom prettyPrint) ls)

data Atom dom = NameA { aName :: HT.Name dom }
              | OperatorA { aOperator :: Operator dom }
              | LiteralA { aLiteral :: Literal dom }

prettyPrintAtom :: Atom dom -> String
prettyPrintAtom (NameA n) = prettyPrint n
prettyPrintAtom (OperatorA o) = prettyPrint o
prettyPrintAtom (LiteralA l) = prettyPrint l

type Build dom = Either (Atom dom) (Expr dom)

extractAtoms :: AutoCorrectDomain dom => Expr dom -> [(GHC.Type, Atom dom)]
-- TODO: make it more efficient
extractAtoms e = trace ("### " ++ shortShowSpan (getRange e))
                   $ sortOn (srcSpanStart . atomRange . snd) 
                   $ map (\n -> (idType $ semanticsId (n ^. simpleName), NameA n)) (e ^? biplateRef) 
                       ++ map (\o -> (idType $ semanticsId (o ^. operatorName), OperatorA o)) (e ^? biplateRef) 
                       ++ map (\l -> (literalType l, LiteralA l)) (e ^? biplateRef)

literalType :: Literal dom -> GHC.Type
literalType (StringLit {}) = stringTy

atomRange :: Atom dom -> SrcSpan
atomRange (NameA n) = getRange n
atomRange (OperatorA n) = getRange n
atomRange (LiteralA n) = getRange n

wrapAtom :: Build dom -> Expr dom
wrapAtom (Right e) = e
wrapAtom (Left (NameA n)) = mkVar n
wrapAtom (Left (OperatorA (NormalOp o))) = mkVar (mkParenName o)
wrapAtom (Left (OperatorA (BacktickOp n))) = mkVar (mkNormalName n)
wrapAtom (Left (LiteralA l)) = mkLit l

correctParening :: [(GHC.Type, Build dom)] -> [Build dom]
correctParening [(_,e)] = [e]
correctParening ls = concatMap correctParening (reduceAtoms ls)

reduceAtoms :: [(GHC.Type, Build dom)] -> [[(GHC.Type, Build dom)]]
reduceAtoms [(t,e)] = [[(t,e)]]
reduceAtoms ls = concatMap (reduceBy ls) [0 .. length ls - 1]

reduceBy :: [(GHC.Type, Build dom)] -> Int -> [[(GHC.Type, Build dom)]]
reduceBy (zip [0..] -> ls) i = maybeToList (reduceFunctionApp ls i) ++ maybeToList (reduceOperatorApp ls i)
  where reduceFunctionApp ls i | Just (funT, fun) <- lookup i ls
                               , Just (argT, arg) <- lookup (i+1) ls
                               , (filter (not . isPredTy) -> inpT:inpsT, resT) <- splitFunTys $ snd $ splitForAllTys funT
                               , Just subst <- tcUnifyTy argT inpT
          = Just $ map ((_1 .- substTy subst) . snd) (take i ls) 
                     ++ [(substTy subst (mkFunTys inpsT resT), mkParen' (mkApp' fun arg))] 
                     ++ map ((_1 .- substTy subst) . snd) (drop (i + 2) ls)
        reduceFunctionApp ls i = 
          trace ("### reduceFunctionApp: " ++ show (map (_2 .- ((_1 .- (showSDocUnsafe . ppr)) . (_2 .- (either prettyPrintAtom prettyPrint)))) ls) ++ " " ++ show i 
                    ++ "\n" ++ case (lookup i ls, lookup (i+1) ls) of (Just (funT, fun), Just (argT, arg)) -> case splitFunTy_maybe $ snd (splitForAllTys funT) of
                                                                                                                Just (inpT, resT) -> showSDocUnsafe $ ppr $ tcUnifyTy argT inpT
                                                                                                                _ -> "bzz"
                                                                      _ -> "mo match" 
                ) Nothing
        
        reduceOperatorApp ls i | Just (opT, Left (OperatorA op)) <- lookup i ls
                               , Just (lArgT, lArg) <- lookup (i-1) ls
                               , Just (rArgT, rArg) <- lookup (i+1) ls
                               , (filter (not . isPredTy) -> inp1T:inp2T:inpsT, resT) <- splitFunTys $ snd $ splitForAllTys opT
                               , Just subst <- tcUnifyTys (\_ -> BindMe) [lArgT,rArgT] [inp1T,inp2T]
          = Just $ map ((_1 .- substTy subst) . snd) (take (i - 1) ls) 
                     ++ [(substTy subst (mkFunTys inpsT resT), mkParen' (mkInfixApp' lArg op rArg))] 
                     ++ map ((_1 .- substTy subst) . snd) (drop (i + 2) ls)
        reduceOperatorApp ls i = 
          trace ("### reduceOperatorApp: " ++ show (map (_2 .- ((_1 .- (showSDocUnsafe . ppr)) . (_2 .- (either prettyPrintAtom prettyPrint)))) ls) ++ " " ++ show i 
                    ++ "\n" ++ case (lookup (i-1) ls, lookup i ls, lookup (i+1) ls) of (Just (lArgT, lArg), Just (funT, fun), Just (rArgT, rArg)) -> case splitFunTys $ snd (splitForAllTys funT) of
                                                                                                                   (inp1T:inp2T:inpsT, resT) -> showSDocUnsafe $ ppr $ tcUnifyTys (\_ -> BindMe) [lArgT,rArgT] [inp1T,inp2T]
                                                                                                                   _ -> "bzz"
                                                                                       _ -> "mo match" 
                ) Nothing

mkApp' :: Build dom -> Build dom -> Build dom
mkApp' (wrapAtom -> f) (wrapAtom -> a) = Right $ mkApp f a

mkInfixApp' :: Build dom -> Operator dom -> Build dom -> Build dom
mkInfixApp' (wrapAtom -> lhs) op (wrapAtom -> rhs) = Right $ mkInfixApp lhs op rhs

mkParen' :: Build dom -> Build dom
mkParen' (wrapAtom -> e) = Right $ mkParen e
