#!/bin/bash

# Run an experiment. Model construction is skipped if model files are detected.

OUTNAME="$(
    echo "model-${NUM_BOTS}-${ENEMIES}-${EFFICIENCY}-${EXPONENT}" |
    sed 's/,/-/'
)"

prism_() {
    prism -v -dtmc -javamaxmem "$JAVA_MEM" -cuddmaxmem "$CUDD_MEM" "$@"
}

run() {
    ./run.sh -n "$NUM_BOTS" -e "$EFFICIENCY" -a "$EXPONENT" -m "$ENEMIES" \
        -l "$LETHALITY"
}

mkdir -p results
echo "Hello"
prism_ <(run) "$PROPERTIES" -exportresults "results/${OUTNAME}.csv:csv"
