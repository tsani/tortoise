#!/bin/bash

./remotely.sh "$1" <(cat prep.sh deploy.sh)
