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
  --ext-rules=off \
  --ho-prim-enum=or --ho-prim-enum-early-bird=true \
  --avatar=off \
  --recognize-injectivity=true \
  --ho-unif-level=pragmatic-framework --no-max-vars  \
  -q "3|prefer-sos|pnrefined(1,1,1,2,2,2,0.5)" \
  -q "2|prefer-easy-ho|conjecture-relative-var(1.02,l,f)" \
  -q "1|prefer-processed|fifo" \
  -q "1|prefer-lambdas|conjecture-relative-var(1,l,f)" \
  -q "3|prefer-formulas|conjecture-relative-var(1.01,s,f)" \
  --lazy-cnf=true --lazy-cnf-kind=simp\
  --select=bb+ho-selection5 --ho-choice-inst=true  \
  --sine=50 --sine-tolerance=1.0 --sine-depth-max=3 --sine-depth-min=1\
  --sine-trim-implications=true\
  --scan-clause-ac=false  --ho-fixpoint-decider=true --avatar=lazy  --split-only-ground=true \
  --kbo-weight-fun=lambda-def-arity0  "${@:4}"
