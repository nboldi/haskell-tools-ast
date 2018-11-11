{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeFamilies, LambdaCase #-}

module Language.Haskell.Tools.Refactor.Builtin.MoveForward
  (moveForwardRefactoring, moveForward, tryItOut) where

import Language.Haskell.Tools.Refactor
import Control.Reference
import SrcLoc
import Data.Maybe (mapMaybe)
import Data.List (findIndex, last)
import Name as GHC
import Data.Data (Data)

data Direction = Forward | Backward
    deriving (Eq)

moveForwardRefactoring :: RefactoringChoice
moveForwardRefactoring = SelectionRefactoring "MoveForward" (localRefactoring . moveForward)

tryItOut :: String -> String -> IO ()
tryItOut = tryRefactor (localRefactoring . moveForward)

moveForward :: RealSrcSpan -> LocalRefactoring
moveForward sp mod = do
  maybeIndex <- searchModule argIndexInBinding sp mod
  index <- maybe (refactError errMsg) return maybeIndex
  let funNames = mod ^? nodesContaining sp & matchLhsName & simpleName
  funName <- if null funNames then refactError errMsg else maybe (refactError errMsg) return (semanticsName $ last funNames)
  mod' <- (biplateRef !~ transformSignature funName index Forward) mod
  nodesContaining sp !~ maybe (refactError errMsg) return . moveArgument index Forward $ mod'


transformSignature :: GHC.Name -> Int -> Direction -> TypeSignature -> LocalRefactor TypeSignature
transformSignature name index direction ts
  | Just name `notElem` map semanticsName tsNames = return ts
  | length tsNames > 1 = refactError "too many names in the type signature"
  | otherwise = return (tsType .- (listToType . swapAt index (moveIndex direction index)) . typeToList $ ts)
    where
      tsNames = ts ^? tsName & annList & simpleName

typeToList :: Type -> [Type]
typeToList (FunctionType arg res) = arg : typeToList res
typeToList (ParenType t) = typeToList t -- parentheses will be removed
typeToList ty = [ty]

listToType :: [Type] -> Type
listToType [t] = t
listToType (t:ts) = mkFunctionType t (listToType ts)

searchModule :: (HasRange (inner dom stage), Data (node dom stage), Data (inner dom stage), RefactorMonad m) => (RealSrcSpan -> inner dom stage -> a) -> RealSrcSpan -> node dom stage -> m a
searchModule f sp m = case m ^? nodesContaining sp of
   []  -> refactError errMsg
   e:_ -> return $ f sp e

argIndexInBinding :: RealSrcSpan -> MatchLhs -> Maybe Int
argIndexInBinding sp m = i
  where
    i = findIndex (`containsSpan` sp)
        (mapMaybe
            ((\case
                RealSrcSpan s -> Just s
                _ -> Nothing) . getRange)
            (m ^. matchLhsArgs & annListElems))

moveIndex :: Direction -> Int -> Int
moveIndex Forward  i = i + 1
moveIndex Backward i = i - 1

moveArgument :: Int -> Direction -> ValueBind -> Maybe ValueBind
moveArgument indexFrom direction funBind@(FunctionBind _)
  | indexFrom < 0 || indexTo >= lstLen = Nothing
  | otherwise = Just $ (funBindMatches & annList & matchLhs & matchLhsArgs & annListElems .- swapAt indexFrom indexTo) funBind
    where
      lstLen = length $ head (funBind ^? funBindMatches & annList & matchLhs & matchLhsArgs & annListElems :: [[Pattern]])
      indexTo = moveIndex direction indexFrom

swapAt :: Int -> Int -> [a] -> [a]
swapAt i j = swapAt' (min i j) (max i j)
    where
        swapAt' i j xs
            | i == j = xs
            | i < 0 || j >= length xs = error $ "invalid indices: " ++ show (i, j)
            | otherwise = beg ++ (jth:med) ++ (ith:end)
                where
                    (beg, ith:rest) = splitAt i xs
                    (med, jth:end) = splitAt (j - i - 1) rest

errMsg = "illegal selecton"