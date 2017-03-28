#!/bin/bash

exec parallel \
    -j 7 \
    -a <(echo '3g') \
    -a <(echo '3g') \
    -a <(seq 10 10 70) \
    -a <(seq 0.25 0.25 2.0) \
    -a <(seq -2.00 0.50 2.0) \
    -a <( \
        parallel -j 1 \
            -a <(seq 5 15 150) \
            -a <(seq 5 15 150) \
            ./format-monster-2.sh
    ) \
    -a <(echo 'properties.pf') \
    -a <(seq 0.1 0.2 1.0) \
    ./wrapper.sh
