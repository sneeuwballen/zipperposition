#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --mode=ho-pragmatic\
  --boolean-reasoning=bool-hoist --bool-hoist-simpl=true --bool-select=LI\
  --tptp-def-as-rewrite --rewrite-before-cnf=true\
  --ho-unif-max-depth=3 --ho-max-app-projections=1 --ho-max-rigid-imitations=2 --ho-max-elims=0 --ho-max-identifications=0\
  --ext-rules=ext-family --ext-rules-max-depth=1\
  --ho-prim-enum=none --ho-choice-inst=true\
  -q "2|prefer-long-trail|conjecture-relative-var(1.02,l,f)"\
  -q "3|prefer-ho-steps|pnrefined(1,1,1,2,2,2,0.5)"\
  -q "1|prefer-neg-unit|orient-lmax(2,1,2,1,1)"\
  -q "1|prefer-short-trail|default"\
  -q "1|prefer-processed|fifo"\
  --select=bb+e-selection14\
  --sine=60 --sine-tolerance=1.8 --sine-depth-max=4 --sine-depth-min=1 --sine-trim-implications=true\
  --prec-gen-fun=arity --kbo-weight-fun=arity0 --kbo-const-weight=1\
  --lambda-demod=true --quant-demod=true --avatar=lazy --ho-choice-inst=true "${@:4}"
