#!/bin/bash

ACTION="$1"

OUTNAME="$(
    echo "model_${NUM_BOTS}_${ENEMIES}_${EFFICIENCY}_${EXPONENT}_${LETHALITY}" |
    sed 's/,/-/'
)"

RESULTFILE="results/${OUTNAME}.csv"
DUMPFILE="dump/${OUTNAME}.pm"
LOGFILE=/dev/null #"logs/${OUTNAME}.log"
GUARDFILE="results/locks/${OUTNAME}"

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

check_lock() {
    set -o noclobber
    { > "$GUARDFILE" } &> /dev/null
}

clear_lock() {
    rm -f "$GUARDFILE" >/dev/null
}

fullrun() {
    prism_ \
        <(run | tee $DUMPFILE) \
        "$PROPERTIES" \
        -exportresults "${RESULTFILE}:csv" > "$LOGFILE" 2>&1
}

mkdir -p results/locks dump logs

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
            if check_lock ; then
                trap clear_lock SIGINT
                # this will make sure we clear the lock if we get interrupted

                echo "generating: $RESULTFILE"
                fullrun
                clear_lock
                trap '' SIGINT
            fi
        fi
        ;;
    "run")
        echo "generating: $RESULTFILE"
        ;;
    *)
        echo "Unknown action $ACTION" >&2
        exit 1
        ;;
esac
