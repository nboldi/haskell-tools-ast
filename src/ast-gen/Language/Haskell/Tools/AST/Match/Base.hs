-- | Generation of basic AST fragments (names for example) for refactorings
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.Tools.AST.Match.Base where

import Language.Haskell.Tools.AST

pattern NormalOp :: Ann UQualifiedName dom stage -> Ann UOperator dom stage
pattern NormalOp n <- Ann _ (UNormalOp n)

pattern BacktickOp :: Ann UQualifiedName dom stage -> Ann UOperator dom stage
pattern BacktickOp n <- Ann _ (UBacktickOp n)

pattern NormalName :: Ann UQualifiedName dom stage -> Ann UName dom stage
pattern NormalName n <- Ann _ (UNormalName n)

pattern ParenName :: Ann UQualifiedName dom stage -> Ann UName dom stage
pattern ParenName n <- Ann _ (UParenName n)

pattern NamePart :: String -> Ann UNamePart dom stage
pattern NamePart s <- Ann _ (UNamePart s)

pattern StringNode :: String -> Ann UStringNode dom stage
pattern StringNode s <- Ann _ (UStringNode s )

pattern ModuleName :: String -> Ann UModuleName dom stage
pattern ModuleName s <- Ann _ (UModuleName s)

pattern DataKeyword :: Ann UDataOrNewtypeKeyword dom stage
pattern DataKeyword <- Ann _ UDataKeyword

pattern NewtypeKeyword :: Ann UDataOrNewtypeKeyword dom stage
pattern NewtypeKeyword <- Ann _ UNewtypeKeyword

pattern DoKeyword :: Ann UDoKind dom stage
pattern DoKeyword <- Ann _ UDoKeyword

pattern MDoKeyword :: Ann UDoKind dom stage
pattern MDoKeyword <- Ann _ UMDoKeyword