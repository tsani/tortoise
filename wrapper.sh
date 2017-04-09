#!/bin/bash

# Wrapper script for running an experiment script.

JAVA_MEM=$1 CUDD_MEM=$2 NUM_BOTS=$3 EFFICIENCY=$4 EXPONENT=$5 ENEMIES="$6" PROPERTIES="$7" LETHALITY="$8" ./experiment.sh "$9"
