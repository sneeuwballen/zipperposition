#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --mode=ho-pragmatic\
  -nc --tptp-def-as-rewrite --rewrite-before-cnf=true\
  --mode=ho-competitive --boolean-reasoning=simpl-only\
  --ext-rules=off\
  --recognize-injectivity=true\
  --ho-unif-level=full-framework -q "5|prefer-ho-steps|pnrefined(1,1,1,2,2,2,0.5)"\
  -q "1|prefer-processed|fifo"\
  -q "5|prefer-easy-ho|conjecture-relative-var(1,l,f)"\
  --select=bb+ho-selection5 --ho-choice-inst=true\
  --sine=50 --sine-tolerance=1 --sine-depth-max=2 --sine-depth-min=1 --scan-clause-ac=false\
  --kbo-weight-fun=arity0 --lazy-cnf=true --lazy-cnf-kind=inf --lazy-cnf-renaming-threshold=8\
  --ho-prim-enum-early-bird=true --ho-unif-level=full-framework --stream-queue-ratio=5 --stream-queue-guard=50 --stream-force-limit=5\
  --bool-select="sel3(consequent_ctx)" --kbo-const-weight=1\
 "${@:4}"
