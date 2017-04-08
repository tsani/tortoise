#!/bin/bash

exec parallel \
    -j 7 \
    -a <(echo '3g') \
    -a <(echo '3g') \
    -a <(seq 15 5 150) \
    -a <(seq 0.05 0.1 1.95) \
    -a <(seq 0.1 0.25 5.0) \
    -a <(
        ./format-monster-2.sh 100 100 ;
        ./format-monster-2.sh 150 150 ;
        ./format-monster-2.sh 200 200 ;
    ) \
    -a <(echo 'properties.pf') \
    -a <(seq 0.05 0.1 0.95) \
    ./wrapper.sh
