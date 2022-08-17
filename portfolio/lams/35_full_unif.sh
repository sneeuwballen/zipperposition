#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --mode=ho-pragmatic --boolean-reasoning=simpl-only\
  --ext-rules=off\
  --ho-prim-enum=neg --ho-prim-enum-early-bird=true --ho-prim-max=1 --ho-choice-inst=true\
  --select=ho-selection4 --ho-selection-restriction=none\
  --lazy-cnf=true --lazy-cnf-kind=inf --lazy-cnf-renaming-threshold=2\
  --tptp-def-as-rewrite --rewrite-before-cnf=true --ho-unif-level=full-framework --stream-queue-ratio=20 --stream-queue-guard=300\
  -q "3|prefer-goals|pnrefined(1,1,1,2,2,2,0.5)"\
  -q "1|prefer-ho-steps|clauseweight(1,1,1)"\
  -q "1|prefer-processed|fifo"\
  -q "1|prefer-non-goals|conjecture-relative-var(1,l,f)"\
  -q "3|prefer-easy-ho|conjecture-relative-var(1.01,s,f)"\
  --ho-solid-decider=false -o tptp --ho-fixpoint-decider=true\
  --trigger-bool-inst=1 --ho-pattern-decider=true --trigger-bool-include-quants=false "${@:4}"
