#!/bin/bash

MACHINE="$1" # the machine to build the models on

parallel \
    -a <(echo '3g') \
    -a <(echo '3g') \
    -a <(echo '200') \
    -a <(echo '1') \
    -a <(echo '1') \
    -a <(
        parallel -j 1 \
            -a <(seq 5 5 100) \
            -a <(seq 5 5 100) \
            format-monster-2.sh
    ) \
    -a <(echo 'ghost-1') \
    wrapper.sh
