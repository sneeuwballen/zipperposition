#!/bin/bash

# 1955 solved under 30 seconds

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout $2\
  --tptp-def-as-rewrite --rewrite-before-cnf=true\
  --mode=lambda-free-intensional --check-lambda-free=false --boolean-reasoning=simpl-only --post-cnf-lambda-lifting=true\
  --ext-rules=off\
  --ho-prim-enum=none  --recognize-injectivity=true\
  --no-max-vars\
  --select=e-selection12  --prec-gen-fun=invfreqhack --kbo-weight-fun=invfreqrank --kbo-const-weight=1  --ord=lambdafree_kbo\
  --ignore-orphans=true\
  -q "1|prefer-sos|conjecture-relative-e(0.5,0.75,100,100,100,100,1.5,1.5,1)" \
  -q "4|const|conjecture-relative-e(0.1,0.05,100,100,100,100,1.5,1.5,1.5)" \
  -q "1|prefer-processed|fifo" \
  -q "1|prefer-non-goals|conjecture-relative-e(0.3,0.25,100,100,100,100,1.5,1.5,1)" \
  -q "4|prefer-sos|pnrefined(3,2,3,2,2,1.5,2)" "${@:4}"
