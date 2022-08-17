#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --mode=ho-pragmatic --boolean-reasoning=bool-hoist --tptp-rewrite-formulas-only=true\
  --ext-rules=ext-family --ext-rules-max-depth=1 --ho-prim-enum=neg\
  --ho-choice-inst=true\
  --select=bb+ho-selection5 --ho-selection-restriction=none --bool-select="sel1(consequent_ctx)"\
  --lazy-cnf=true --lazy-cnf-kind=simp --lazy-cnf-renaming-threshold=4\
  --trigger-bool-inst=1 --trigger-bool-ind=1 --trigger-bool-include-quants=false \
  --tptp-def-as-rewrite --rewrite-before-cnf=true --ho-unif-level=full-framework\
  --stream-queue-ratio=50 --stream-queue-guard=10 --stream-clause-num=30 --ho-choice-inst=true\
  -q "3|prefer-fo|conjecture-relative-var(1.02,l,f)"\
  -q "3|prefer-sos|pnrefined(1,1,1,2,2,2,0.5)"\
  -q "4|prefer-ground|default"\
  -q "2|defer-formulas|conjecture-relative-e(0.1,0.5,100,100,100,100,1.5,1.5,1.5)" \
  -q "1|prefer-processed|fifo" \
  -o tptp --ho-fixpoint-decider=true\
  --ho-pattern-decider=true --ho-fixpoint-decider=true "${@:4}"
