#!/bin/bash

# Wrapper script for the deploy script.

JAVA_MEM=$1 CUDD_MEM=$2 NUM_BOTS=$3 EFFICIENCY=$4 EXPONENT=$5 ENEMIES="$6" ./cnc/deploy.sh "$7" properties.pf
