#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --mode=ho-pragmatic\
  --boolean-reasoning=simpl-only\
  --tptp-def-as-rewrite --rewrite-before-cnf=true\
  --ho-unif-level=full-framework\
  --kbo-weight-fun=invfreqrank\
  --stream-queue-guard=50 --stream-queue-ratio=15\
  -q "1|prefer-neg-unit|conjecture-relative-var(1.03,s,f)"\
  -q "3|prefer-short-trail|conjecture-relative-var(1.05,l,f)"\
  -q "1|prefer-processed|fifo"\
  -q "2|prefer-sos|conjecture-relative-var(1.05,l,f)"\
  -q "1|defer-sos|conjecture-relative-var(1.1,s,t)"\
  --select=pos-e-selection2 --ho-choice-inst=false --ho-selection-restriction=none\
  --sine=50 --sine-tolerance=2.5 --sine-depth-max=3 --sine-depth-min=1 --eq-encode=true --avatar=lazy --sine-trim-implications=true "${@:4}"
