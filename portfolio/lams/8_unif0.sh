#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --mode=ho-pragmatic --boolean-reasoning=simpl-only\
  --ho-unif-level=pragmatic-framework --ho-unif-max-depth=0\
  -q "1|prefer-neg-unit|conjecture-relative-var(1.02,l,f)"\
  -q "1|prefer-formulas|pnrefined(1,1,1,2,2,2,0.75)"\
  -q "1|prefer-shallow|conjecture-relative-var(1.02,l,f)"\
  -q "2|prefer-ground|orient-lmax(2,1,2,1,1)"\
  -q "1|prefer-processed|fifo"\
  -q "2|defer-sos|pnrefined(1,1,1,2,2,2,0.75)"\
  -q "1|const|staggered(1.5)"\
  --select=e-selection9\
  --prec-gen-fun=invfreq_conj\
  --kbo-weight-fun-from-precedence=true\
  --tptp-def-as-rewrite --rewrite-before-cnf=true\
  --ho-selection-restriction=none --sup-at-var-headed=false\
  --sup-from-var-headed=false --sup-at-vars=false --lazy-cnf=true\
  --lazy-cnf-kind=simp --lazy-cnf-renaming-threshold=8\
  --max-inferences=1 --quant-demod=true --trigger-bool-inst=1 --trigger-bool-include-quants=false  "${@:4}"
