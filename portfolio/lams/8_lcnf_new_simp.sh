#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --mode=ho-pragmatic --boolean-reasoning=simpl-only \
  --ho-unif-max-depth=1 --ho-max-app-projections=1 --ho-max-rigid-imitations=1 --ho-max-elims=0 --ho-max-identifications=0 \
  --ext-rules=ext-family --ext-rules-max-depth=1 \
  --ho-prim-enum=none --ho-choice-inst=true\
  -q "3|prefer-fo|conjecture-relative-var(1.02,l,f)"\
  -q "3|prefer-sos|pnrefined(1,1,1,2,2,2,0.5)"\
  -q "7|prefer-ground|orient-lmax(2,1,2,1,1)"\
  -q "1|prefer-processed|fifo"\
  --select=e-selection6  \
  --sine=60 --sine-tolerance=3 --sine-depth-max=5 --sine-depth-min=1 \
  --prec-gen-fun=arity --lazy-cnf=true --lazy-cnf-kind=simp --lazy-cnf-inf-quant=true --lazy-cnf-renaming-threshold=3 \
  --kbo-weight-fun-from-precedence=true --trigger-bool-inst=1 \
  --tptp-def-as-rewrite --rewrite-before-cnf=true --lambda-demod=true --bool-demod=true --trigger-bool-ind=2 "${@:4}"
