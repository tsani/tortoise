#!/bin/bash

ssh tortoise@$1 bash <<'EOF'
set -e

die() {
    echo "$@" >&2
    exit 1
}

source .bash_profile # so we get PRISM in our PATH
cd tortoise

echo "updating repository"
git fetch
git reset --hard origin/master

echo "building code generator"
cabal install --only-dependencies
cabal build >/dev/null 2>&1 || die 'cabal build failed'
EOF
