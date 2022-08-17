#!/bin/bash

# $1: File name
# $2: Extra options

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp \
  -o tptp \
  --timeout "$2" \
  --mode=fo-complete-basic --kbo-weight-fun-from-precedence=true --simultaneous-sup=true\
  -q "2|prefer-goals|conjecture-relative-e(0.5,0.5,100,100,100,100,1.5,1.5,1.0)"\
  -q "2|prefer-processed|conjecture-relative-e(0.1,0.5,100,100,100,100,1.5,1.5,1.5)"\
  -q "1|prefer-processed|fifo"\
  -q "1|prefer-non-goals|conjecture-relative-e(0.5,0.5,100,100,100,100,1.5,1.5,1.0)"\
  -q "4|prefer-goals|pnrefined(3,2,3,2,2,1.5,2)" --select=NoSelection --ord=lambdafree_kbo\
  --prec-gen-fun=invfreqhack --kbo-weight-fun-from-precedence=true --kbo-weight-fun-from-precedence-rank=20\
  --kbo-const-weight=1 "${@:3}"
