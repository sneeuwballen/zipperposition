#!/bin/bash

# $1: File name
# $2: Extra options

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp \
  -o tptp \
  --timeout "$2" \
  --mode=fo-complete-basic --kbo-weight-fun-from-precedence=true --simultaneous-sup=true --prec-gen-fun=invfreq_conj \
  -q "10|prefer-sos|pnrefined(1,1,1,2,2,2,0.5)" \
  -q "10|prefer-non-goals|pnrefined(2,1,1,1,2,2,2)" \
  -q "5|const|orient-lmax(2,1,2,1,1)" \
  -q "1|const|fifo" \
  --select=e-selection11 \
  --sine=100 --sine-tolerance=3 --sine-depth-max=3 --sine-depth-min=1 \
  --ord=lambdafree_rpo --local-rw=green-context \
  "${@:3}"

