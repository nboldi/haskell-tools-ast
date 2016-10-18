-- | Pattern matching on literals for refactorings.
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Literals where

import Language.Haskell.Tools.AST

pattern CharLit :: Char -> Ann ULiteral dom stage
pattern CharLit c <- Ann _ (UCharLit c)

pattern StringLit :: String -> Ann ULiteral dom stage
pattern StringLit s <- Ann _ (UStringLit s)

pattern IntLit :: Integer -> Ann ULiteral dom stage
pattern IntLit i <- Ann _ (UIntLit i)

pattern FracLit :: Rational -> Ann ULiteral dom stage
pattern FracLit f <- Ann _ (UFracLit f)

pattern PrimIntLit :: Integer -> Ann ULiteral dom stage
pattern PrimIntLit i <- Ann _ (UPrimIntLit i)

pattern PrimWordLit :: Integer -> Ann ULiteral dom stage
pattern PrimWordLit i <- Ann _ (UPrimWordLit i)

pattern PrimFloatLit :: Rational -> Ann ULiteral dom stage
pattern PrimFloatLit i <- Ann _ (UPrimFloatLit i)

pattern PrimDoubleLit :: Rational -> Ann ULiteral dom stage
pattern PrimDoubleLit i <- Ann _ (UPrimDoubleLit i)

pattern PrimCharLit :: Char -> Ann ULiteral dom stage
pattern PrimCharLit i <- Ann _ (UPrimCharLit i)

pattern PrimStringLit :: String -> Ann ULiteral dom stage
pattern PrimStringLit s <- Ann _ (UPrimStringLit s)
