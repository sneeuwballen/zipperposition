#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --mode=ho-pragmatic --boolean-reasoning=bool-hoist --tptp-rewrite-formulas-only=true\
  --ext-rules=off\
  --ho-prim-enum=combs --ho-prim-enum-early-bird=true --ho-prim-max=1\
  --select=bb+ho-selection5 --bool-select="sel3(shallow_log_ctx)"\
  --lazy-cnf=true --lazy-cnf-kind=simp --lazy-cnf-renaming-threshold=4\
  --tptp-def-as-rewrite --rewrite-before-cnf=true --ho-unif-level=pragmatic-framework\
  --ho-unif-max-depth=1\
  -q "3|prefer-fo|conjecture-relative-var(1.02,l,f)"\
  -q "3|prefer-sos|pnrefined(1,1,1,2,2,2,0.5)"\
  -q "4|prefer-ground|default"\
  -q "2|defer-formulas|conjecture-relative-e(0.1,0.5,100,100,100,100,1.5,1.5,1.5)" \
  -q "1|prefer-processed|fifo" \
  --ho-pattern-decider=true --ho-fixpoint-decider=true --ho-solid-decider=false\
  --avatar=eager --split-only-ground=true\
   "${@:4}"
