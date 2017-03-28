#!/bin/bash

# parallel -j 6 \
#     -a <(echo '3g') \
#     -a <(seq 5 5 100) \
#     echo

parallel \
    -j 7 \
    -a <(echo '3g') \
    -a <(echo '3g') \
    -a <(echo '200') \
    -a <(echo '1') \
    -a <(echo '1') \
    -a <( \
        parallel -j 1 \
            -a <(seq 5 5 100) \
            -a <(seq 5 5 100) \
            ./format-monster-2.sh
    ) \
    -a <(echo 'ghost-1') \
    ./wrapper.sh
