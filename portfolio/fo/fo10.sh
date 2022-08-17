#!/bin/bash

# $1: File name
# $2: Extra options

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  --timeout "$2" --avatar=off --mode=fo-complete-basic \
  -i tptp \
  -o tptp \
  "${@:3}"
