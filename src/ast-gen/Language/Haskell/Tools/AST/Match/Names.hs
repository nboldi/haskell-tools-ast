-- | Generation of basic AST fragments (names for example) for refactorings
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Names where

import Language.Haskell.Tools.AST
import Language.Haskell.Tools.AST.ElementTypes

pattern NormalOp :: QualifiedName dom -> Operator dom
pattern NormalOp n <- Ann _ (UNormalOp n)

pattern BacktickOp :: QualifiedName dom -> Operator dom
pattern BacktickOp n <- Ann _ (UBacktickOp n)

pattern NormalName :: QualifiedName dom -> Name dom
pattern NormalName n <- Ann _ (UNormalName n)

pattern ParenName :: QualifiedName dom -> Name dom
pattern ParenName n <- Ann _ (UParenName n)

pattern NamePart :: String -> NamePart dom
pattern NamePart s <- Ann _ (UNamePart s)

pattern StringNode :: String -> StringNode dom
pattern StringNode s <- Ann _ (UStringNode s )