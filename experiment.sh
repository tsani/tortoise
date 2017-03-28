#!/bin/bash

# Run an experiment. Model construction is skipped if model files are detected.

OUTNAME="model-${NUM_BOTS}-${ENEMIES}-${EFFICIENCY}-${EXPONENT}"

prism_() {
    prism -dtmc -javamaxmem "$JAVA_MEM" -cuddmaxmem "$CUDD_MEM" "$@"
}

if [ "$FORCE" = '1' -o ! -e "${OUTNAME}.sta" ] ; then
    prism_ -exportmodel "${OUTNAME}.all" \
        <(
            ./run.sh \
                -n "$NUM_BOTS" \
                -e "$EFFICIENCY" \
                -a "$EXPONENT" \
                -m "$ENEMIES" \
                -l "$LETHALITY"
        )
fi

if test -n "$PROPERTIES" ; then
    echo "running PRISM"
    prism_ -importmodel "${OUTNAME}.all" "$PROPERTIES" | tee "${OUTNAME}.results"
fi
