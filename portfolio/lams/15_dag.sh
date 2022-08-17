#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --tptp-def-as-rewrite --rewrite-before-cnf=true\
  --mode=ho-pragmatic --boolean-reasoning=simpl-only\
  --ho-unif-level=full-framework\
  --ext-rules=ext-family --ext-rules-max-depth=1 --recognize-injectivity=true\
  --ho-prim-enum=none --ho-choice-inst=true\
  -q "5|prefer-goals|dagweight(2,1,1,1,t,f,f,t,f,f,f)" -q "5|prefer-goals|dagweight(1,2,1,1,t,f,f,t,f,f,f)" -q "1|const|fifo"\
  --select=e-selection12\
  --sine=60 --sine-tolerance=2.5 --sine-depth-max=3 --sine-depth-min=1\
  --kbo-weight-fun=invfreqrank --prec-gen-fun=invfreq_constmin --lazy-cnf=true --lazy-cnf-kind=simp \
  --trigger-bool-inst=1 --trigger-bool-include-quants=false --kbo-const-weight=1\ 
 "${@:4}"
