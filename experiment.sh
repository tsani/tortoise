#!/bin/bash

# Run an experiment. Model construction is skipped if model files are detected.

OUTNAME="model-${NUM_BOTS}-${ENEMIES}-${EFFICIENCY}-${EXPONENT}"

prism_() {
    prism -dtmc -javamaxmem "$JAVA_MEM" -cuddmaxmem "$CUDD_MEM" "$@"
}

run() {
    ./run.sh -n "$NUM_BOTS" -e "$EFFICIENCY" -a "$EXPONENT" -m "$ENEMIES" \
        -l "$LETHALITY"
}

prism_ <(run) "$PROPERTIES" | tee "${OUTNAME}.result"
