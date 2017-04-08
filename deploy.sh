#!/bin/bash

set -e

echo "updating repository"
git fetch
git reset --hard origin/master

echo "building code generator"
cabal install --only-dependencies
cabal build
