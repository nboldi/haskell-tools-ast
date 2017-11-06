#!/bin/bash

# Pull requests or nightly builds shouldn't try to deploy, but pushes to any branch should.
if [ "$TRAVIS_EVENT_TYPE" != "push" ]; then
    echo "Skipping deploy"
    exit 0
fi

# Run benchmarks. Need to uninstall first to run this without coverage reporting, so the result will be more accurate.
echo "Running benchmark"
travis_wait stack --no-terminal bench haskell-tools-cli > benchmark.txt 2>&1

echo "Starting deploy"

# Decrypt the private key using travis encrypted file service
openssl aes-256-cbc -K $encrypted_961cdb62d58f_key -iv $encrypted_961cdb62d58f_iv -in deploykey.enc -out deploykey -d
chmod 600 deploykey

# Start an ssh session and add the private key
eval `ssh-agent -s`
ssh-add deploykey

# Clone the existing repo into out/. Download only the files for the given folder
mkdir out
cd out
git init
git remote add -f origin git@github.com:haskell-tools/haskell-tools.github.io
git config core.sparseCheckout true
echo "$TRAVIS_BRANCH" >> .git/info/sparse-checkout
git pull origin master
cd ..

# Clean out existing contents
rm -rf out/$TRAVIS_BRANCH/api/**
rm -rf out/$TRAVIS_BRANCH/coverage/**

# Move generated haddock documentation

mkdir -p out/$TRAVIS_BRANCH/api
mv .stack-work/install/x86_64-linux/*/*/doc/* out/$TRAVIS_BRANCH/api

# Move the test coverage report

mkdir -p out/$TRAVIS_BRANCH/coverage
mv .stack-work/install/x86_64-linux/*/*/hpc/combined/all/* out/$TRAVIS_BRANCH/coverage

if [ "$TRAVIS_EVENT_TYPE" = "cron" ]; then
  # Copy the benchmark report
  mv benchmark.txt out/$TRAVIS_BRANCH/benchmark.txt
fi

# Create an index page
mv branch-info-index.html out/$TRAVIS_BRANCH/index.html

cd out

git config user.name "Travis CI"
git config user.email "nboldi@caesar.elte.hu"
git config push.default simple

git add -A .
git commit -m "Updating API documentation for: https://github.com/haskell-tools/haskell-tools/commit/$TRAVIS_COMMIT"
git pull --allow-unrelated-histories origin master
git push --set-upstream origin master
