#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --mode=ho-pragmatic\
  --tptp-def-as-rewrite --rewrite-before-cnf=true \
  --mode=ho-competitive --boolean-reasoning=simpl-only \
  --ext-rules=ext-family --ext-rules-max-depth=1 \
  --ho-prim-enum=none \
  --avatar=off \
  --recognize-injectivity=true  --ho-elim-leibniz=1 \
  --ho-unif-level=pragmatic-framework --no-max-vars  \
  --max-inferences=2 \
  --ho-unif-max-depth=1 \
  -q "6|prefer-sos|pnrefined(1,1,1,2,2,2,0.5)" \
  -q "6|const|conjecture-relative-var(1.02,l,f)" \
  -q "1|prefer-processed|fifo" \
  -q "1|prefer-non-goals|conjecture-relative-var(1,l,f)" \
  -q "4|prefer-easy-ho|conjecture-relative-var(1.01,s,f)" \
  --select=e-selection16 --ho-choice-inst=true --try-e="$DIR/eprover-ho" --tmp-dir="$3" --e-timeout=3 --e-auto=true \
  --sine=50 --sine-tolerance=1.0 --sine-depth-max=3 --sine-depth-min=1\
  --sine-trim-implications=true --ho-unif-level=pragmatic-framework\
  --e-encode-lambdas=keep --scan-clause-ac=false \
  --kbo-weight-fun=lambda-def-invfreqrank --e-call-point=0.1 "${@:4}"
