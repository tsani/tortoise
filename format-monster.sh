#!/bin/bash

echo -n '['

i=1

while (( $# - 1 > 0 )) ; do
    echo -n "($i,$1),"
    i=$((i + 1))
    shift
done

echo -n "($i,$1)]"
