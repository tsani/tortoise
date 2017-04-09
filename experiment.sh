#!/bin/bash

ACTION="$1"

OUTNAME="$(
    echo "model_${NUM_BOTS}_${ENEMIES}_${EFFICIENCY}_${EXPONENT}_${LETHALITY}" |
    sed 's/,/-/'
)"

RESULTFILE="results/${OUTNAME}.csv"
DUMPFILE="dump/${OUTNAME}.pm"
LOGFILE="log/${OUTNAME}.log"

prism_() {
    prism -v -dtmc -javamaxmem "$JAVA_MEM" -cuddmaxmem "$CUDD_MEM" "$@"
}

run() {
    ./run.sh -n "$NUM_BOTS" -e "$EFFICIENCY" -a "$EXPONENT" -m "$ENEMIES" \
        -l "$LETHALITY"
}

check() {
    test -e "$RESULTFILE"
}

fullrun() {
    prism_ \
        <(run | tee $DUMPFILE) \
        "$PROPERTIES" \
        -exportresults "${RESULTFILE}:csv" > "$LOGFILE" 2>&1
}

mkdir -p results dump logs

case "$ACTION" in
    "check")
        if ! check ; then
            echo "missing: $RESULTFILE"
        fi
        ;;
    "check-run")
        if check ; then
            echo "skipping: $RESULTFILE"
        else
            echo "generating: $RESULTFILE"
            fullrun
        fi
    "run")
        echo "generating: $RESULTFILE"
        ;;
    *)
        echo "Unknown action $ACTION" >&2
        exit 1
        ;;
esac
