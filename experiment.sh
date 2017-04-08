#!/bin/bash

# Run an experiment. Model construction is skipped if model files are detected.

OUTNAME="$(
    echo "model_${NUM_BOTS}_${ENEMIES}_${EFFICIENCY}_${EXPONENT}_${LETHALITY}" |
    sed 's/,/-/'
)"

prism_() {
    prism -v -dtmc -javamaxmem "$JAVA_MEM" -cuddmaxmem "$CUDD_MEM" "$@"
}

run() {
    ./run.sh -n "$NUM_BOTS" -e "$EFFICIENCY" -a "$EXPONENT" -m "$ENEMIES" \
        -l "$LETHALITY"
}

mkdir -p results dump
echo "Hello"
prism_ <(run | tee dump/${OUTNAME}.pm) "$PROPERTIES" -exportresults "results/${OUTNAME}.csv:csv"
