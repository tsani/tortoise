#!/bin/bash

REMOTE_MACHINE="$1"
PROPERTIES="$2"

if test -z "$REMOTE_MACHINE" ; then
    echo "no remote machine specified" >&2
    exit 1
fi

ssh tortoise@$REMOTE_MACHINE 'bash' <<EOF
#!/bin/bash

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

OUTNAME="model-${NUM_BOTS}-${ENEMIES}-${EFFICIENCY}-${EXPONENT}"

prism_() {
    prism -dtmc -javamaxmem "$JAVA_MEM" -cuddmaxmem "$CUDD_MEM" "\$@"
}

if ! test -e "\${OUTNAME}.sta" ; then
    echo "Constructing model because it doesn't already exist."
    prism_ -exportmodel "\${OUTNAME}.all" \
        <(./run.sh -n "$NUM_BOTS" -e "$EFFICIENCY" -a "$EXPONENT" -m "$ENEMIES")
fi

if test -n "$PROPERTIES" ; then
    echo "running PRISM"
    prism_ -importmodel "\${OUTNAME}.all" "$PROPERTIES"
fi

EOF
