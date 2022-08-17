#!/bin/bash

# $1: File name
# $2: Extra options

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp \
  -o tptp \
  --timeout "$2" \
  --mode=fo-complete-basic --kbo-weight-fun=invfreqrank --simultaneous-sup=true\
  -q "10|prefer-sos|rel_lvl_weight(1,2,1,2,100,100,100,400,1.5,1.5,1)"\
  -q "3|prefer-non-goals|conjecture-relative-e(0.1,1,200,100,200,100,1.5,1.5,1.5)"\
  -q "1|prefer-processed|clauseweight(1,1,1)"\
  -q "1|prefer-processed|fifo"\
  --select=e-selection --ord=lambdafree_kbo --prec-gen-fun=invfreq --kbo-const-weight=1 "${@:3}"
