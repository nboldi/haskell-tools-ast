-- | UPattern matching on literals for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Literals where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes

pattern CharLit :: Char -> Literal dom
pattern CharLit c <- Ann _ (UCharLit c)

pattern StringLit :: String -> Literal dom
pattern StringLit s <- Ann _ (UStringLit s)

pattern IntLit :: Integer -> Literal dom
pattern IntLit i <- Ann _ (UIntLit i)

pattern FracLit :: Rational -> Literal dom
pattern FracLit f <- Ann _ (UFracLit f)

pattern PrimIntLit :: Integer -> Literal dom
pattern PrimIntLit i <- Ann _ (UPrimIntLit i)

pattern PrimWordLit :: Integer -> Literal dom
pattern PrimWordLit i <- Ann _ (UPrimWordLit i)

pattern PrimFloatLit :: Rational -> Literal dom
pattern PrimFloatLit i <- Ann _ (UPrimFloatLit i)

pattern PrimDoubleLit :: Rational -> Literal dom
pattern PrimDoubleLit i <- Ann _ (UPrimDoubleLit i)

pattern PrimCharLit :: Char -> Literal dom
pattern PrimCharLit i <- Ann _ (UPrimCharLit i)

pattern PrimStringLit :: String -> Literal dom
pattern PrimStringLit s <- Ann _ (UPrimStringLit s)
