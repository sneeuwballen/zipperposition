#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --tptp-def-as-rewrite --rewrite-before-cnf=true\
  --mode=ho-pragmatic --boolean-reasoning=simpl-only\
  --ho-unif-level=full-framework --ho-prim-enum=eq --ho-prim-enum-early-bird=true\
  --ext-rules=ext-family --ext-rules-max-depth=1 --recognize-injectivity=true\
  -q "1|defer-sos|conj_pref_weight(0.5,100,0.2,0.2,4)"\
  -q "2|prefer-formulas|conj_pref_weight(0.5,100,0.2,0.2,4)"\
  -q "1|const|pnrefined(4,300,4,300,4,4,0.7)"\
  -q "2|prefer-processed|rel_lvl_weight(0,1,2,1,1,1,200,200,2.5,999,999)"\
  -q "1|prefer-sos|staggered(1)"\
  -q "1|const|clauseweight(20,300,4)"\
  -q "2|prefer-goals|conjecture-relative-e(0.01,1,999,20,50,10,3,3,0.5)"\
  --select=e-selection\
  --sine=60 --sine-tolerance=2.5 --sine-depth-max=3 --sine-depth-min=1\
  --kbo-weight-fun=invfreqrank --prec-gen-fun=invfreq_constmin --lazy-cnf=true --lazy-cnf-kind=simp \
  --trigger-bool-inst=1 --trigger-bool-include-quants=false --trigger-bool-ind=1 --kbo-const-weight=1\ 
 "${@:4}"
