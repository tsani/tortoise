#!/bin/bash

exec parallel \
    -j 7 \
    -a <(echo '3g') \
    -a <(echo '3g') \
    -a <(echo '10') \
    -a <(echo '1') \
    -a <(echo '1') \
    -a <( \
        parallel -j 1 \
            -a <(seq 3 3 50) \
            -a <(seq 3 3 50) \
            ./format-monster-2.sh
    ) \
    -a <(echo 'properties.pf') \
    -a <(echo '0.1') \
    ./wrapper.sh
