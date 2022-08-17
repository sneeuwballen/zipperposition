#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --mode=ho-pragmatic --boolean-reasoning=simpl-only\
  --ho-unif-max-depth=2 --ho-max-app-projections=0 --ho-max-rigid-imitations=1\
  --ho-max-elims=0 --ho-max-identifications=1\
  --max-inferences=3\
  --ext-rules=off --recognize-injectivity=true\
  --ho-prim-enum=none --ho-choice-inst=true\
  -q "3|prefer-fo|conjecture-relative-var(1.02,l,f)"\
  -q "3|prefer-sos|pnrefined(1,1,1,2,2,2,0.5)"\
  -q "2|prefer-ground|orient-lmax(2,1,2,1,1)"\
  -q "1|prefer-processed|fifo"\
  --select=MaxGoalNS\
  --sine=60 --sine-tolerance=1.5 --sine-depth-max=3 --sine-depth-min=1\
  --prec-gen-fun=invfreqhack --lazy-cnf=true --lazy-cnf-kind=simp --lazy-cnf-renaming-threshold=3\
  --kbo-weight-fun-from-precedence=true --kbo-weight-fun-from-precedence-rank=5 --trigger-bool-inst=1\
  --avatar=lazy --tptp-def-as-rewrite --rewrite-before-cnf=true\
  --sup-from-var-headed=false --sup-at-vars=false "${@:4}"
