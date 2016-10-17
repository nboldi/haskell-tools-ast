-- | Generation of basic AST fragments (names for example) for refactorings
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Base where

import Language.Haskell.Tools.AST

pattern NormalOp :: Ann QualifiedName dom stage -> Ann Operator dom stage
pattern NormalOp n <- Ann _ (UNormalOp n)

pattern BacktickOp :: Ann QualifiedName dom stage -> Ann Operator dom stage
pattern BacktickOp n <- Ann _ (UBacktickOp n)

pattern NormalName :: Ann QualifiedName dom stage -> Ann Name dom stage
pattern NormalName n <- Ann _ (UNormalName n)

pattern ParenName :: Ann QualifiedName dom stage -> Ann Name dom stage
pattern ParenName n <- Ann _ (UParenName n)

pattern NamePart :: String -> Ann NamePart dom stage
pattern NamePart s <- Ann _ (UNamePart s)

pattern StringNode :: String -> Ann StringNode dom stage
pattern StringNode s <- Ann _ (UStringNode s )

pattern ModuleName :: String -> Ann ModuleName dom stage
pattern ModuleName s <- Ann _ (UModuleName s)

pattern DataKeyword :: Ann DataOrNewtypeKeyword dom stage
pattern DataKeyword <- Ann _ UDataKeyword

pattern NewtypeKeyword :: Ann DataOrNewtypeKeyword dom stage
pattern NewtypeKeyword <- Ann _ UNewtypeKeyword

pattern DoKeyword :: Ann DoKind dom stage
pattern DoKeyword <- Ann _ UDoKeyword

pattern MDoKeyword :: Ann DoKind dom stage
pattern MDoKeyword <- Ann _ UMDoKeyword