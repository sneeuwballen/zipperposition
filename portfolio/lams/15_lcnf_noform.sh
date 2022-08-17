#!/bin/bash

# 1955 solved under 30 seconds

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" -nc --tptp-def-as-rewrite --rewrite-before-cnf=true --tptp-rewrite-formulas-only=true \
  --mode=ho-pragmatic --boolean-reasoning=simpl-only \
  --ext-rules=ext-family --ext-rules-max-depth=1 \
  --ho-prim-enum=neg --ho-prim-max=1\
  --recognize-injectivity=true  --ho-elim-leibniz=1 \
  --ho-unif-level=full-framework --no-max-vars  \
  -q "1|prefer-sos|conjecture-relative-var(1.02,l,f)" \
  -q "4|const|conjecture-relative-var(1,s,f)" \
  -q "1|prefer-processed|fifo" \
  -q "1|prefer-non-goals|conjecture-relative-var(1,l,f)" \
  -q "4|prefer-sos|pnrefined(2,1,1,1,2,2,2)" \
  --select=ho-selection4 --ho-choice-inst=true --lazy-cnf=true --avatar=eager \
  --sine=50 --sine-tolerance=10 --sine-depth-max=5 --sine-depth-min=1 --e-max-derived=64 \
  --e-encode-lambdas=keep --scan-clause-ac=false --prec-gen-fun=invfreq_conj --ord=derived_ho_kbo \
  --solid-subsumption=true --ignore-orphans=true "${@:4}"
