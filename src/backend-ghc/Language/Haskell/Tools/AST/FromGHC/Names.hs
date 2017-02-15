{-# LANGUAGE LambdaCase
           , TupleSections
           , TypeFamilies
           , FlexibleInstances
           , FlexibleContexts
           , TypeSynonymInstances
           , ScopedTypeVariables
           , MultiParamTypeClasses
           , UndecidableInstances
           , AllowAmbiguousTypes
           , TypeApplications
           #-}
-- | Functions that convert the basic elements of the GHC AST to corresponding elements in the Haskell-tools AST representation
module Language.Haskell.Tools.AST.FromGHC.Names where

import Control.Monad.Reader ((=<<), asks)
import Data.Char (isDigit, isLetter, isAlphaNum)

import FastString as GHC (FastString, unpackFS)
import HsSyn as GHC
import Name as GHC (isSymOcc)
import qualified Name as GHC (Name)
import RdrName as GHC (RdrName)
import SrcLoc as GHC

import Language.Haskell.Tools.AST (Ann(..), AnnListG, RangeStage, Dom)
import qualified Language.Haskell.Tools.AST as AST

import Language.Haskell.Tools.AST.FromGHC.GHCUtils
import Language.Haskell.Tools.AST.FromGHC.Monad (TrfInput(..), Trf, getOriginalName)
import Language.Haskell.Tools.AST.FromGHC.Utils

trfOperator :: TransformName n r => Located n -> Trf (Ann AST.UOperator (Dom r) RangeStage)
trfOperator = trfLocNoSema trfOperator'

trfOperator' :: TransformName n r => n -> Trf (AST.UOperator (Dom r) RangeStage)
trfOperator' n
  | isSymOcc (occName n) = AST.UNormalOp <$> trfQualifiedNameFocus True n
  | otherwise = AST.UBacktickOp <$> trfQualifiedNameFocus True n

trfName :: TransformName n r => Located n -> Trf (Ann AST.UName (Dom r) RangeStage)
trfName = trfLocNoSema trfName'

trfName' :: TransformName n r => n -> Trf (AST.UName (Dom r) RangeStage)
trfName' n
  | isSymOcc (occName n) = AST.UParenName <$> trfQualifiedNameFocus False n
  | otherwise = AST.UNormalName <$> trfQualifiedNameFocus False n

trfAmbiguousFieldName :: TransformName n r => Located (AmbiguousFieldOcc n) -> Trf (Ann AST.UName (Dom r) RangeStage)
trfAmbiguousFieldName (L l af) = trfAmbiguousFieldName' l af

trfAmbiguousFieldName' :: forall n r . TransformName n r => SrcSpan -> AmbiguousFieldOcc n -> Trf (Ann AST.UName (Dom r) RangeStage)
trfAmbiguousFieldName' l (Unambiguous (L _ rdr) pr) = annLocNoSema (pure l) $ trfName' (unpackPostRn @n rdr pr)
-- no Id transformation is done, so we can basically ignore the postTC value
trfAmbiguousFieldName' _ (Ambiguous (L l rdr) _)
  = annLocNoSema (pure l)
      $ (if (isSymOcc (occName rdr)) then AST.UParenName else AST.UNormalName)
          <$> (annLoc (createAmbigousNameInfo rdr l) (pure l) $ AST.nameFromList <$> trfNameStr (isSymOcc (occName rdr)) (rdrNameStr rdr))

trfAmbiguousOperator' :: forall n r . TransformName n r => SrcSpan -> AmbiguousFieldOcc n -> Trf (Ann AST.UOperator (Dom r) RangeStage)
trfAmbiguousOperator' l (Unambiguous (L _ rdr) pr) = annLocNoSema (pure l) $ trfOperator' (unpackPostRn @n rdr pr)
-- no Id transformation is done, so we can basically ignore the postTC value
trfAmbiguousFieldOperator' _ (Ambiguous (L l rdr) _)
  = annLocNoSema (pure l)
      $ (if (isSymOcc (occName rdr)) then AST.UNormalOp else AST.UBacktickOp)
          <$> (annLoc (createAmbigousNameInfo rdr l) (pure l) $ AST.nameFromList <$> trfNameStr (not $ isSymOcc (occName rdr)) (rdrNameStr rdr))


class (DataId n, Eq n, GHCName n) => TransformableName n where
  correctNameString :: n -> Trf String
  getDeclSplices :: Trf [Located (HsSplice n)]
  fromGHCName :: GHC.Name -> n

instance TransformableName RdrName where
  correctNameString = pure . rdrNameStr
  getDeclSplices = pure []
  fromGHCName = rdrName

instance TransformableName GHC.Name where
  correctNameString n = getOriginalName (rdrName n)
  getDeclSplices = asks declSplices
  fromGHCName = id

-- | This class allows us to use the same transformation code for multiple variants of the GHC AST.
-- GHC UName annotated with 'name' can be transformed to our representation with semantic annotations of 'res'.
class (TransformableName name, HsHasName name, TransformableName res, HsHasName res, GHCName res)
        => TransformName name res where
  -- | Demote a given name
  transformName :: name -> res

instance {-# OVERLAPPABLE #-} (n ~ r, TransformableName n, HsHasName n) => TransformName n r where
  transformName = id

instance {-# OVERLAPS #-} (TransformableName res, GHCName res, HsHasName res) => TransformName GHC.Name res where
  transformName = fromGHCName

trfNameText :: String -> Trf (Ann AST.UName (Dom r) RangeStage)
trfNameText str
  = annContNoSema (AST.UNormalName <$> annLoc (createImplicitNameInfo str) (asks contRange) (AST.nameFromList <$> trfNameStr (isOperatorStr str) str))

trfImplicitName :: HsIPName -> Trf (Ann AST.UName (Dom r) RangeStage)
trfImplicitName (HsIPName fs)
  = let nstr = unpackFS fs
     in do rng <- asks contRange
           let rng' = mkSrcSpan (updateCol (+1) (srcSpanStart rng)) (srcSpanEnd rng)
           annContNoSema (AST.UImplicitName <$> annLoc (createImplicitNameInfo nstr) (pure rng')
                                                  (AST.nameFromList <$> trfNameStr (isOperatorStr nstr) nstr))

isOperatorStr :: String -> Bool
isOperatorStr = any (not . isAlphaNum)

trfQualifiedName :: TransformName n r => Bool -> Located n -> Trf (Ann AST.UQualifiedName (Dom r) RangeStage)
trfQualifiedName isOperator (L l n) = focusOn l $ trfQualifiedNameFocus isOperator n

trfQualifiedNameFocus :: TransformName n r => Bool -> n -> Trf (Ann AST.UQualifiedName (Dom r) RangeStage)
trfQualifiedNameFocus isOperator n
  = do rng <- asks contRange
       let rng' = if isOperator == isSymOcc (occName n) then rng
                    else mkSrcSpan (updateCol (+1) (srcSpanStart rng)) (updateCol (subtract 1) (srcSpanEnd rng))
       annLoc (createNameInfo (transformName n)) (pure rng') (trfQualifiedName' n)

trfQualifiedName' :: TransformName n r => n -> Trf (AST.UQualifiedName (Dom r) RangeStage)
trfQualifiedName' n = AST.nameFromList <$> (trfNameStr False =<< correctNameString n)

-- | Creates a qualified name from a name string
trfNameStr :: Bool -> String -> Trf (AnnListG AST.UNamePart (Dom r) RangeStage)
trfNameStr isInParenOrBackticks str = makeList "." atTheStart (trfNameStr' str . correct <$> atTheStart)
  where correct = if isInParenOrBackticks then updateCol (+1) else id

trfNameStr' :: String -> SrcLoc -> [Ann AST.UNamePart (Dom r) RangeStage]
trfNameStr' str startLoc = fst $
  foldl (\(r,loc) np -> let nextLoc = advanceAllSrcLoc loc np
                         in ( r ++ [Ann (noSemaInfo $ AST.NodeSpan (mkSrcSpan loc nextLoc)) (AST.UNamePart np)], advanceAllSrcLoc nextLoc "." ) )
  ([], startLoc) (nameParts str)
  where -- | Move the source location according to a string
        advanceAllSrcLoc :: SrcLoc -> String -> SrcLoc
        advanceAllSrcLoc (RealSrcLoc rl) str = RealSrcLoc $ foldl advanceSrcLoc rl str
        advanceAllSrcLoc oth _ = oth

        -- | Break up a name into parts, but take care for operators
        nameParts :: String -> [String]
        nameParts = nameParts' ""

        nameParts' :: String -> String -> [String]
        nameParts' carry (c : rest) | isLetter c || isDigit c || c == '\'' || c == '_' || c == '#'
                                    = nameParts' (c:carry) rest
        nameParts' carry@(_:_) ('.' : rest) = reverse carry : nameParts rest
        nameParts' "" rest = [rest]
        nameParts' carry [] = [reverse carry]
        nameParts' carry str = error $ "nameParts': " ++ show carry ++ " " ++ show str

trfFastString :: Located FastString -> Trf (Ann AST.UStringNode (Dom r) RangeStage)
trfFastString = trfLocNoSema $ pure . AST.UStringNode . unpackFS
