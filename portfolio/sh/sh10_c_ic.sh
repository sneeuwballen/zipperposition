#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --mode=ho-pragmatic\
  -nc --tptp-def-as-rewrite --rewrite-before-cnf=true \
  --mode=ho-competitive --boolean-reasoning=simpl-only \
  --ext-rules=ext-family --ext-rules-max-depth=1 \
  --ho-prim-enum=none \
  --avatar=off \
  --recognize-injectivity=true  --ho-elim-leibniz=1 \
  --ho-unif-level=pragmatic-framework --no-max-vars  \
  --max-inferences=4 --ho-max-app-projections=1\
  --ho-max-elims=0 --ho-max-rigid-imitations=2 --ho-max-identifications=0 \
  --ho-unif-max-depth=3 \
  -q "6|prefer-sos|pnrefined(1,1,1,2,2,2,0.5)" \
  -q "6|const|conjecture-relative-var(1.02,l,f)" \
  -q "1|prefer-processed|fifo" \
  -q "1|prefer-non-goals|conjecture-relative-var(1,l,f)" \
  -q "4|prefer-easy-ho|conjecture-relative-var(1.01,s,f)" \
  --select=e-selection7 --ho-choice-inst=true --try-e="$DIR/eprover-ho" --tmp-dir="$3" --e-timeout=7 \
  --sine=50 --sine-tolerance=1 --sine-depth-max=2 --sine-depth-min=1 --e-max-derived=64 --sine-ignore-k-most-common-syms=2 --sine-trim-implications=true \
  --e-encode-lambdas=keep --scan-clause-ac=false --lambdasup=0 \
  --kbo-weight-fun=lambda-def-invfreqrank --demod-in-var-args=true --bool-demod=true --lambda-demod=true --e-call-point=0.1 "${@:4}"
