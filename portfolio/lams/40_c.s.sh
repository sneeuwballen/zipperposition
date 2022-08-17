#!/bin/bash

# 1955 solved under 30 seconds

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" -nc --tptp-def-as-rewrite --rewrite-before-cnf=true \
  --mode=ho-competitive --boolean-reasoning=bool-hoist --bool-hoist-simpl=true \
  --ext-rules=ext-family --ext-rules-max-depth=2 \
  --ho-prim-enum=full --ho-prim-max=2 --bool-select=LI  \
  --ho-max-elims=1  --avatar=off \
  --recognize-injectivity=true  --ho-elim-leibniz=1 \
  --ho-unif-level=full-framework --no-max-vars  \
  -q "6|prefer-sos|pnrefined(1,1,1,2,2,2,0.5)" \
  -q "6|const|conjecture-relative-var(1.02,l,f)" \
  -q "1|prefer-processed|fifo" \
  -q "1|prefer-non-goals|conjecture-relative-var(1,l,f)" \
  -q "4|prefer-easy-ho|conjecture-relative-var(1.01,s,f)" \
  --select=e-selection7 --ho-choice-inst=true --try-e="$DIR/eprover-ho" --tmp-dir="$3" --e-timeout=30 \
  --sine=50 --sine-tolerance=2 --sine-depth-max=4 --sine-depth-min=1 --e-max-derived=48 \
  --e-encode-lambdas=keep --scan-clause-ac=false --lambdasup=0 --kbo-weight-fun=invfreqrank "${@:4}"
