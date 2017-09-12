#!/bin/bash
set -e # Exit with nonzero exit code if anything fails

if [ "$TRAVIS_EVENT_TYPE" = "cron" ]; then

  # test that the cabal packages contain all the required additional files
  for PKG in ast backend-ghc cli daemon debug prettyprint refactor rewrite
  do
    echo "Extracting the distribution of ${PKG}"
    cp `find src/${PKG}/.stack-work -name haskell-tools-${PKG}-*.tar.gz` .
    rm -r src/${PKG}/*
    tar -zx -f haskell-tools-${PKG}-*.tar.gz
    mv haskell-tools-${PKG}-*/* src/${PKG}
  done
  echo "Running tests on the extracted folders"
  # here all tests run, not just the unit tests
  stack --no-terminal test --coverage haskell-tools-rewrite haskell-tools-builtin-refactorings haskell-tools-experimental-refactorings haskell-tools-daemon haskell-tools-cli haskell-tools-demo

fi
