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
    -a <(echo '3g') \\
    -a <(echo '3g') \\
    -a <(seq 15 5 150) \\
    -a <(seq 0.05 0.1 1.95) \\
    -a <(seq 0.1 0.25 5.0) \\
    -a <(
        ./format-monster-2.sh 25 200 ;
        ./format-monster-2.sh 50 150 ;
        ./format-monster-2.sh 50 50 ;
    ) \\
    -a <(echo 'properties.pf') \\
    -a <(seq 0.05 0.1 0.95) \\
    -a <(echo $ACTION) \\
    ./wrapper.sh
EOF
