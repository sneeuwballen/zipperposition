#!/bin/bash

# 30s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --mode=ho-pragmatic --boolean-reasoning=simpl-only\
  --ho-unif-max-depth=0 --ho-prim-enum=none\
  -q "2|prefer-ho-steps|conjecture-relative-e(0.1,0.5,100,100,100,100,1.5,1.5,1.5)"\
  -q "1|prefer-sos|pnrefined(1,1,1,2,2,2,0.5)"\
  -q "2|prefer-ground|default"\
  -q "2|prefer-empty-trail|conjecture-relative-e(0.1,0.5,100,100,100,100,1.5,1.5,1.5)"\
   -q "1|prefer-processed|fifo"\
  --select=bb+e-selection7\
  --ho-pattern-decider=false --ho-fixpoint-decider=true --ho-solid-decider=false\
  --sine=150 --sine-tolerance=2 --sine-depth-max=3 --sine-depth-min=1\
  --prec-gen-fun=invfreqhack --lazy-cnf=true --lazy-cnf-kind=simp --lazy-cnf-renaming-threshold=2 --fluid-log-hoist=false\
  --tptp-def-as-rewrite --rewrite-before-cnf=true\
  --ho-prim-enum=eq --ho-prim-enum-add-var=true --ho-prim-max=1 --ho-prim-enum-early-bird=true -o tptp --avatar=eager --split-only-ground=true "${@:4}"
