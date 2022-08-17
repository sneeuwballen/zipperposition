#!/bin/bash

# 1955 solved under 30 seconds

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" --mode=ho-pragmatic --kbo-weight-fun-from-precedence=true\
  --prec-gen-fun=invfreqhack\
   -q "6|prefer-sos|pnrefined(1,1,1,2,2,2,0.5)"\
  -q "6|defer-sos|pnrefined(2,1,1,1,2,2,2)"\
  -q "2|const|orient-lmax(2,1,2,1,1)" -q "4|const|conjecture-relative-e(0.1,0.5,100,100,100,100,1.5,1.5,1.5)"\
  -q "1|prefer-processed|fifo"\
  --ho-neg-ext-simpl=true --arg-cong-simpl=true\
  --ord=derived_ho_kbo --tptp-def-as-rewrite --rewrite-before-cnf=true\
  --sine=40 --sine-take-only-defs=true --sine-depth-min=1\
  --sine-depth-max=5 --sine-tolerance=10 --trigger-bool-inst=1\
  --lazy-cnf=true --lazy-cnf-kind=inf --lazy-cnf-simplify-quant=true --lazy-cnf-renaming-threshold=3\
  --lambda-demod=true --sup-from-var-headed=false --sup-at-var-headed=false --select=ho-selection5\
  --ho-selection-restriction=none --ho-unif-max-depth=1 --ho-max-app-projections=0\
  --ho-max-rigid-imitations=1 --ho-max-elims=0 --ho-max-identifications=0\
  --presaturate=true --boolean-reasoning=simpl-only "${@:4}"
