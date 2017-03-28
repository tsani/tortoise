#!/bin/bash

exec parallel \
    -j 7 \
    -a <(echo '3g') \
    -a <(echo '3g') \
    -a <(echo 50) \
    -a <(seq 0.50 0.50 2.0) \
    -a <(seq 0.1 0.25 5.0) \
    -a <( \
        parallel -j 1 \
            -a <(echo 75 ; echo 100 ; echo 125) \
            -a <(echo 75 ; echo 100 ; echo 125) \
            ./format-monster-2.sh
    ) \
    -a <(echo 'properties.pf') \
    -a <(echo '0.25') \
    ./wrapper.sh
