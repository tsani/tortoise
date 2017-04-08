#!/bin/bash

REMOTE="$1"
shift

ssh tortoise@$REMOTE bash < <(cat "$@")
