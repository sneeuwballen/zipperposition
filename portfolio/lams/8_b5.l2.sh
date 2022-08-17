#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --mode=ho-pragmatic --boolean-reasoning=simpl-only\
  --ho-unif-max-depth=4 --ho-max-app-projections=2 --ho-max-rigid-imitations=2 --ho-max-elims=0 --ho-max-identifications=1\
  --ext-rules=off\
  --ho-prim-enum=neg --ho-prim-enum-early-bird=true --ho-choice-inst=true  --select=ho-selection5 --ho-selection-restriction=none\
  --prec-gen-fun=const_first --lazy-cnf=true --lazy-cnf-kind=simp --lazy-cnf-renaming-threshold=2\
  --tptp-def-as-rewrite --rewrite-before-cnf=true --ho-unif-level=full-framework --stream-queue-ratio=25 --stream-queue-guard=150\
  -q "6|prefer-sos|pnrefined(1,1,1,2,2,2,0.5)"\
  -q "6|const|conjecture-relative-var(1.02,l,f)"\
  -q "1|prefer-processed|fifo"\
  -q "1|prefer-non-goals|conjecture-relative-var(1,l,f)"\
  -q "4|prefer-easy-ho|conjecture-relative-var(1.01,s,f)"  --ho-solid-decider=true "${@:4}"
