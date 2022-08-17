#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --tptp-def-as-rewrite --rewrite-before-cnf=true\
  --mode=lambda-free-intensional --check-lambda-free=false --boolean-reasoning=simpl-only --post-cnf-lambda-lifting=true\
  --ext-rules=none --local-rw=false\
  --ho-prim-enum=none\
  -q "5|prefer-goals|dagweight(2,1,1,1,t,f,f,t,f,f,f)" -q "5|prefer-goals|dagweight(1,2,1,1,t,f,f,t,f,f,f)" -q "1|const|fifo"\
  --select=e-selection12\
  --sine=60 --sine-tolerance=2.5 --sine-depth-max=3 --sine-depth-min=1\
  --kbo-weight-fun=invfreqrank --prec-gen-fun=invfreq_constmin --lazy-cnf=true --lazy-cnf-kind=simp \
  --kbo-const-weight=1\ 
 "${@:4}"
