#!/bin/bash
set -e # Exit with nonzero exit code if anything fails

if [ "$TRAVIS_EVENT_TYPE" = "cron" ]; then
  # run the self-tests
  stack exec ht-refact -- --no-watch --no-history --execute="Exit" demo src/ast src/backend-ghc src/builtin-refactorings src/cli src/daemon src/debug src/experimental-refactorings src/prettyprint src/refactor src/rewrite
  stack exec ht-refact -- --no-watch --no-history --execute="ChangeFile src/ast/Language/Haskell/Tools/AST/Ann.hs" demo src/ast src/backend-ghc src/builtin-refactorings src/cli src/daemon src/debug src/experimental-refactorings src/prettyprint src/refactor src/rewrite
  stack exec ht-refact -- --no-watch --no-history --execute="RenameDefinition src/ast/Language/Haskell/Tools/AST/Ann.hs 250:6 Ann'" demo src/ast src/backend-ghc src/builtin-refactorings src/cli src/daemon src/debug src/experimental-refactorings src/prettyprint src/refactor src/rewrite

  # test the completeness of distribution packages
  for PKG in ast backend-ghc cli daemon debug prettyprint refactor rewrite
  do
    echo "Extracting the distribution of ${PKG}"
    cp `find src/${PKG}/.stack-work -name haskell-tools-${PKG}-*.tar.gz` .
    rm -r src/${PKG}/*
    tar -zx -f haskell-tools-${PKG}-*.tar.gz
    mv haskell-tools-${PKG}-*/* src/${PKG}
  done
  echo "Running tests on the extracted folders"
  stack --no-terminal --coverage test haskell-tools-rewrite
  stack --no-terminal --coverage test haskell-tools-refactor
  stack --no-terminal --coverage test haskell-tools-cli
  stack --no-terminal --coverage test haskell-tools-daemon
  stack --no-terminal --coverage test haskell-tools-demo
fi
