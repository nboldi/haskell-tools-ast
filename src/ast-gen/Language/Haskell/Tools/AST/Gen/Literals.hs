-- | Generation of literals for refactorings.
-- The bindings defined here are the AST constructor names with an "mk" prefix.
{-# LANGUAGE OverloadedStrings 
           , TypeFamilies 
           #-}
module Language.Haskell.Tools.AST.Gen.Literals where

import qualified Name as GHC
import Data.List
import Data.String
import Data.Function (on)
import Control.Reference
import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes
import Language.Haskell.Tools.AST.Gen.Utils
import Language.Haskell.Tools.AST.Gen.Names
import Language.Haskell.Tools.AnnTrf.SourceTemplate
import Language.Haskell.Tools.AnnTrf.SourceTemplateHelpers

mkCharLit :: Char -> Literal dom
mkCharLit c = mkAnn (fromString $ show c) $ UCharLit c

mkStringLit :: String -> Literal dom
mkStringLit s = mkAnn (fromString $ show s) $ UStringLit s

mkIntLit :: Integer -> Literal dom
mkIntLit i = mkAnn (fromString $ show i) $ UIntLit i

mkFracLit :: Rational -> Literal dom
mkFracLit f = mkAnn (fromString $ show f) $ UFracLit f

mkPrimIntLit :: Integer -> Literal dom
mkPrimIntLit i = mkAnn (fromString $ show i ++ "#") $ UPrimIntLit i

mkPrimWordLit :: Integer -> Literal dom
mkPrimWordLit i = mkAnn (fromString $ show i ++ "##") $ UPrimWordLit i

mkPrimFloatLit :: Rational -> Literal dom
mkPrimFloatLit f = mkAnn (fromString $ show f ++ "#") $ UPrimFloatLit f

mkPrimDoubleLit :: Rational -> Literal dom
mkPrimDoubleLit f = mkAnn (fromString $ show f ++ "##") $ UPrimDoubleLit f

mkPrimCharLit :: Char -> Literal dom
mkPrimCharLit c = mkAnn (fromString $ show c ++ "#") $ UPrimCharLit c

mkPrimStringLit :: String -> Literal dom
mkPrimStringLit s = mkAnn (fromString $ show s ++ "#") $ UPrimStringLit s