#!/bin/bash

# This script generates a script to be run by a computer to collect data.
# "collect" is used loosely here because the action type percolates down to the
# "experiment.sh" script, which can either actually perform data collection, or
# just check whether the data was collected for a particular experiment
# parameters.

JOBS="$1"
ACTION="$2"

cat <<EOF
exec parallel \\
    -j $JOBS \\
    -a <(echo '10g') \\
    -a <(echo '10g') \\
    -a <(echo 20) \\
    -a <(echo 2.0) \\
    -a <(seq 0.1 0.25 5.0) \\
    -a <(
        ./format-monster.sh 10 20 30 40 ;
    ) \\
    -a <(echo 'properties.pf') \\
    -a <(echo 0.1) \\
    -a <(echo $ACTION) \\
    ./wrapper.sh
EOF
