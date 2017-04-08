#!/bin/bash

exec parallel \
    -j 7 \
    -a <(echo '3g') \
    -a <(echo '3g') \
    -a <(seq 15 5 150) \
    -a <(echo 1.0) \
    -a <(seq 0.1 0.25 5.0) \
    -a <(./format-monster-2.sh 100 100 ; ./format-monster-2.sh 150 150) \
    -a <(echo 'properties.pf') \
    -a <(echo '0.25') \
    ./wrapper.sh
