#!/bin/bash

exec parallel \
    -j 7 \
    -a <(echo '3g') \
    -a <(echo '3g') \
    -a <(echo 50) \
    -a <(echo 1.0) \
    -a <(seq 0.1 0.25 5.0) \
    -a <(./format-monster.sh 100 100 ; ./format-monster.sh 150 150) \
    -a <(echo 'properties.pf') \
    -a <(echo '0.25') \
    ./wrapper.sh
