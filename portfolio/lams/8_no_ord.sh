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
  --tptp-def-as-rewrite --rewrite-before-cnf=true --ho-unif-level=full-framework --stream-queue-ratio=10 --stream-queue-guard=100\
  -q "6|prefer-sos|pnrefined(1,1,1,2,2,2,0.5)"\
  -q "6|const|conjecture-relative-var(1.02,l,f)"\
  -q "1|prefer-processed|fifo"\
  -q "1|prefer-non-goals|conjecture-relative-var(1,l,f)"\
  -q "4|prefer-ho-steps|conjecture-relative-var(1.01,s,f)"\
  --lazy-cnf=false --ho-unif-level=full-framework --ho-solid-decider=false --max-inferences=2\
  --ord=subterm --select=NoSelection -o tptp "${@:4}"
