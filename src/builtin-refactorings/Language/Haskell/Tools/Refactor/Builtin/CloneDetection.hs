module Language.Haskell.Tools.Refactor.Builtin.CloneDetection where

import Language.Haskell.Tools.Refactor
import Language.Haskell.Tools.Refactor.Builtin.CloneDetection.Transform
import Language.Haskell.Tools.Refactor.Builtin.CloneDetection.TransformInstances ()
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.Refactor.Querying (QueryChoice(..))

import Control.Reference hiding (contains)
import Data.Data
import Data.Hashable
import Debug.Trace
import qualified Data.Map.Strict as SMap
import Data.Matrix hiding (trace)
import qualified Data.Vector as V
import Data.List hiding ((!!), head)
import GHC (Name)
import Name (mkInternalName, mkVarOcc)
import Unique (mkVarOccUnique)
import FastString (fsLit)
import qualified Control.Monad.State as S
import Data.Ratio
import Control.Monad (when)
import Control.Arrow
import Data.Ord
import Data.List.Safe ((!!), head)
import Prelude hiding ((!!), head)
import Data.Maybe
import Data.Aeson
import SrcLoc

cloneDetectionQuery :: QueryChoice
cloneDetectionQuery = ProjectQuery "DetectClones" detectClones

cloneDetectionQueryReadable :: QueryChoice
cloneDetectionQueryReadable = ProjectQueryReadable "DetectClonesReadable" detectClonesReadable

detectClones :: [ModuleDom] -> QueryMonad Value
detectClones mods = return $ (toJSON . dropTrees . mkSortedClonePairs) projectRoot
    where
        projectRoot = mkNode NoInfo (map (mkCloneTree . snd) mods)
        dropTrees = map ((_2 .- fromMaybe noSrcSpan . (^? info & src)) . (_1 .- fromMaybe noSrcSpan . (^? info & src)))

detectClonesReadable :: [ModuleDom] -> QueryMonad Value
detectClonesReadable mods = return $ (toJSON . showClonePairs) projectRoot
    where
        projectRoot = mkNode NoInfo (map (mkCloneTree . snd) mods)

globMinHeight = 10
globMaxDepth = 5
globMaxDiff = 1%5
globMaxNubLength = 1000000

showClones :: CCSerialize e dom st => e dom st -> String
showClones = showClonePairs . mkCloneTree

showClonePairs :: CCTree -> String
showClonePairs = (\ps -> concatMap showPairs ps ++ "Found " ++ show (length ps) ++ " clones\n")
                             . mkSortedClonePairs
     where showPairs (t1, t2, r) =  "(" ++ showInfo t1 ++ ", \n " ++ showInfo t2 ++ ") -- "
                                           ++ (show . round) ((1 - r) * 100)
                                           ++ "% match\n\n"
                where showInfo t = case t ^? info & src of
                                         Just (RealSrcSpan rs) -> show (srcSpanFile  rs) ++ ": ["
                                                                  ++ show (srcSpanStartLine rs, srcSpanStartCol rs) ++ ", "
                                                                  ++ show (srcSpanEndLine rs, srcSpanEndCol rs) ++ "]"
                                         Nothing -> "?"


mkSortedClonePairs :: CCTree -> [(CCTree, CCTree, Rational)]
mkSortedClonePairs = sortBy (comparing (Down . relevance))
                 . (\cs -> if length cs <= globMaxNubLength then removePairs cs else cs) -- TODO: consider diff
                 . concatMap (mkClonePairs globMaxDiff)
                 . mkCloneClasses (globMinHeight, globMaxDepth)
    where
        relevance (t1, _, d) = case t1 ^? info & src of
                           Just (RealSrcSpan rs) -> let start = srcSpanStartLine rs
                                                        end = srcSpanEndLine rs
                                                        size = toInteger (if start == end then 1 else end - start)
                                                    in (size%1) * (((1%1) - d) ^^ 3)
                           Nothing -> d ^^ 3

removePairs :: [(CCTree, CCTree, Rational)] -> [(CCTree, CCTree, Rational)]
removePairs = nubBy2 (comparing f) contains
    where f (t1,t2, _) = (getRealSpan t1, getRealSpan t2)

nubBy2 :: (a -> a -> Ordering) -> (a -> a -> Bool) -> [a] -> [a]
nubBy2 ord eq = map (fromJust . head) . groupBy eq . sortBy ord

contains :: (CCTree, CCTree, Rational) -> (CCTree, CCTree, Rational) -> Bool
contains (t11,t12,d1) (t21,t22,d2) = (t11 `containsNode` t21) && (t12 `containsNode` t22) && (d1 >= d2)
    where containsNode t1 t2 = fromMaybe False $ do
            s1 <- getRealSpan t1
            s2 <- getRealSpan t2
            return $ s1 `containsSpan` s2

mkClonePairs :: Rational -> [CCTree] -> [(CCTree, CCTree, Rational)]
mkClonePairs maxDiff = mkClonePairs'
    where
        mkClonePairs' [] = []
        mkClonePairs' (t:ts) =  S.execState (mapM (f t) ts) (mkClonePairs' ts)
        f :: CCTree -> CCTree -> S.State [(CCTree, CCTree, Rational)] ()
        f t1 t2 = do
            let diff = difference t1 t2
            when (diff <= maxDiff) $ S.modify (orderPair (t1, t2, diff):)
                where orderPair p@(t1,t2,d) = if t1 `before` t2 then p else (t2,t1,d)
                      before t1 t2 = fromMaybe False $ do
                        s1 <- getRealSpan t1
                        s2 <- getRealSpan t2
                        return $ s1 <= s2 --srcSpanStartLine s1 <= srcSpanStartLine s2

mkCloneClasses :: (Int, Int) -> CCTree -> [[CCTree]]
mkCloneClasses (minHeight, maxDepth) = SMap.elems
                                        . SMap.filter ((>1) . length)
                                        . SMap.fromListWith (++)
                                        . map (((hash . cutAt maxDepth) &&& (:[])) . deBruijn) -- TODO use hashmap, merge cut/hash
                                        . mkSubTrees minHeight
                                        . simplify
                                        . stripTree

difference :: CCTree -> CCTree -> Rational
difference (CCNode _ i xs) (CCNode _ j ys)
    | (l /= length ys) ||  not (i `equals` j) = 1
    | l == 0 = 0
    | otherwise = sum (map ( (/ toRational l) . uncurry difference) (zip  xs ys))
        where
            l = length xs
            equals (Info _ c1 i1) (Info _ c2 i2) = c1 == c2 && i1 == i2 -- every (i1,i2) should be valid indices or Nothing
            equals (Attr s1) (Attr s2) = s1 == s2
            equals NoInfo NoInfo = True
            equals _ _ = False

deBruijn :: CCTree -> CCTree
deBruijn t = S.evalState (traverseNode rename t) []
    where
        rename :: CCTree -> S.State [GHC.Name] CCTree
        rename (CCNode h (Info s c (Just (Left name))) _) =
            do
               st <- S.get
               let index = elemIndex name st
               case index of
                    Just i -> return (CCNode h (Info s c (Just (Right i))) [])
                    Nothing -> do
                        S.modify (++ [name])
                        S.return (CCNode h (Info s c (Just (Right (length st)))) [])
        rename t = return t

traverseNode :: Monad f => (CCTree -> f CCTree) -> CCTree -> f CCTree
traverseNode f (CCNode h i ts) = do
    ts' <- mapM (traverseNode f) ts
    f (CCNode h i ts')

simplify :: CCTree -> CCTree
simplify t = fromMaybe ((subtrees .- map simplify) t) $ do
    c <- t ^? info & cons
    simplify' c
    where
        simplify' c
            | c == toConstr (UParen u :: UExpr IdDom SrcTemplateStage) = head $ t ^. subtrees

            | c == toConstr (UInfixApp u u u :: UExpr IdDom SrcTemplateStage) = do
                let ts = t ^. subtrees

                lhs <- head ts
                op <- ts !! 1
                rhs <- ts !! 2

                opCons <- op^? info & cons
                opSrc <- op ^? info & src -- TODO: different spans for each new node?

                let opSubTrees = op ^. subtrees
                let opName = mkNode' opSrc (toConstr ((if opCons == toConstr (UNormalOp u :: UOperator IdDom SrcTemplateStage)
                                                  then UParenName u -- UNormalOp
                                                  else UNormalName u) :: UName IdDom SrcTemplateStage)) -- UBacktickOp
                                                  opSubTrees


                let opVar =  mkNode' opSrc varC [opName]
                let innerApp = mkNode' opSrc appC [opVar, simplify lhs]
                let outerApp = mkNode' opSrc appC [innerApp, simplify rhs]

                return outerApp
            | c == toConstr (ULamCase u :: (UExpr IdDom SrcTemplateStage)) = do
                    src <- t ^? info & src
                    let alts = t ^. subtrees

                    let lambdaVarName = mkInternalName  (mkVarOccUnique (fsLit "x")) (mkVarOcc "x") src

                    let qualName = mkNode (Info src (toConstr (UQualifiedName u u :: UQualifiedName IdDom SrcTemplateStage)) (Just (Left lambdaVarName))) []
                    let normalName = mkNode' src (toConstr (UNormalName u :: UName IdDom SrcTemplateStage)) [qualName]

                    let varPat = mkNode' src varPatC [normalName]
                    let var = mkNode' src varC [normalName]

                    let case_ = mkNode' src (toConstr (UCase u u  :: UExpr IdDom SrcTemplateStage)) (var:alts)
                    let lambda = mkNode' src (toConstr (ULambda u u :: UExpr IdDom SrcTemplateStage)) [varPat, case_]

                    return lambda
            | otherwise = Nothing
        u = undefined
        appC = toConstr (UApp u u :: UExpr IdDom SrcTemplateStage)
        varC = toConstr (UVar u :: UExpr IdDom SrcTemplateStage)
        varPatC = toConstr (UVarPat u :: UPattern IdDom SrcTemplateStage)

mkCloneTree :: CCSerialize e dom st => e dom st -> CCTree
mkCloneTree = put

instance Hashable CCInfo where
    hashWithSalt s (Info _ con (Just (Right index))) = s `hashWithSalt` show con `hashWithSalt` index
    hashWithSalt s (Info _ con _) = s `hashWithSalt` show con
    hashWithSalt s i = s `hashWithSalt` show i -- TODO exclude literals?

instance Hashable CCTree where
    hashWithSalt s (CCNode _ info ts) = s `hashWithSalt` info `hashWithSalt` ts

cutAt :: Int -> CCTree -> CCTree
cutAt depth (CCNode h i ts)
    | depth <= 0 = CCNode h i []
    | otherwise  = CCNode h i (map (cutAt (depth-1)) ts)

mkSubTrees :: Int -> CCTree -> [CCTree]
mkSubTrees minHeight = subTrees'
    where
        subTrees' t@(CCNode h _ ts)
            | h < minHeight = []
            | otherwise     = t : concatMap subTrees' ts

stripTree :: CCTree -> CCTree
stripTree (CCNode _ info ts) = mkNode info (concatMap strip' ts)
    where
        strip' (CCNode _ NoInfo ts) = concatMap strip' ts
        strip' (CCNode _ i ts) = [mkNode i (concatMap strip' ts)]

getRealSpan :: CCTree -> Maybe RealSrcSpan
getRealSpan t = case t ^? info & src of
                 Just (RealSrcSpan s) -> Just s
                 _ -> Nothing

-- DEBUG FUNCTIONS

showCloneMatrix :: CCSerialize e dom st => e dom st -> String -- only for debugging
showCloneMatrix = concatMap (\ts -> showWithIndex ts ++ "\n" ++ (show . mkDiffMtx) ts ++ "\n")
                            . mkCloneClasses (globMinHeight, globMaxDepth)
                            . mkCloneTree
    where
        showWithIndex ts = concatMap (\(i, t) -> show i ++ ":" ++ showIndented 0 t ++ "\n\n") (zip [1..length ts] ts)

showIndented i (CCNode _ info xs) = (replicate (i*2) ' ' ++ show info ++ "\n") ++ concatMap (showIndented (i+1)) xs

mkDiffMtx :: [CCTree] -> Matrix Rational
mkDiffMtx ts = m2
    where
        m2 = matrix n n f2
        m1 = matrix n n f1
        n = length ts
        v = V.fromList ts
        f1 (i, j)
            | i == j    = toRational 0
            | i > j     = difference (v V.! (i-1)) (v V.! (j-1))
            | otherwise = toRational 0
        f2 (i, j)
            | i >= j    = m1 ! (i, j)
            | otherwise = m1 ! (j, i)

trace' f a = trace (f a) a

