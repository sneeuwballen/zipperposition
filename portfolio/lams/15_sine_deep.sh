#!/bin/bash

# 1955 solved under 30 seconds

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" -nc --tptp-def-as-rewrite --rewrite-before-cnf=true \
  --mode=ho-competitive --boolean-reasoning=simpl-only \
  --ext-rules=off \
  --ho-prim-enum=none  \
  --avatar=off \
  --recognize-injectivity=true  --ho-elim-leibniz=4 \
  --ho-unif-level=pragmatic-framework --ho-unif-max-depth=0 --no-max-vars  \
  -q "2|prefer-goals|conjecture-relative-e(0.5,1,100,100,100,100,1.5,1.5,1)" \
  -q "2|prefer-fo|conjecture-relative-e(0.1,1,100,100,100,100,1.5,1.5,1.5)" \
  -q "1|prefer-processed|fifo" \
  -q "2|prefer-non-goals|conjecture-relative-e(0.5,1,100,100,100,100,1.5,1.5,1.5)" \
  -q "1|prefer-ho-steps|pnrefined(1,1,1,1,2,1.5,2)" \
  -q "1|prefer-ground|conj_pref_weight(0.5,100,0.2,0.2,4)" \
  --select=bb+ho-selection5 --ho-choice-inst=true \
  --scan-clause-ac=false --kbo-weight-fun=lambda-def-arity0 --prec-gen-fun=invfreq_conj\
  --arg-cong-simpl=true --ho-neg-ext-simpl=true --sine=50 --sine-depth-max=3 --sine-depth-min=1 --sine-tolerance=9\
  --tptp-rewrite-formulas-only=true --post-cnf-lambda-lifting=true\
  "${@:4}"
