#!/bin/bash

# $1: File name
# $2: Extra options

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp \
  -o tptp \
  --timeout "$2" \
  --mode=fo-complete-basic --kbo-weight-fun=invfreqrank --simultaneous-sup=true --prec-gen-fun=invfreq \
  -q "10|const|conjecture-relative-e(0.1,1.0,100,100,100,100,1.5,1.5,1.5)" \
  -q "1|const|fifo" \
  --select=e-selection14 --kbo-const-weight=1 \
  "${@:3}"
