#!/bin/bash

ssh tortoise@$1 bash <<'EOF'
set -e

source .bash_profile # so we get PRISM in our PATH
cd tortoise

echo "updating repository"
git fetch
git reset --hard origin/master

echo "building code generator"
cabal install --only-dependencies
cabal build
EOF
