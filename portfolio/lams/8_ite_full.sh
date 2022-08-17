#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
 --tptp-def-as-rewrite --rewrite-before-cnf=true\
 --mode=ho-competitive --boolean-reasoning=bool-hoist\
 --ext-rules=ext-family --ext-rules-max-depth=1\
 --ho-max-elims=1  --avatar=off\
 --recognize-injectivity=true\
 --ho-unif-level=full-framework\
  -q "4|prefer-goals|pnrefined(1,1,1,2,2,2,0.5)"\
 -q "1|prefer-fo|conjecture-relative-var(1.02,l,f)"\
 -q "1|prefer-non-goals|conjecture-relative-var(1,l,f)"\
 -q "4|prefer-ho-steps|conjecture-relative-var(1.01,s,f)" -q "1|prefer-processed|fifo"\
 --select=bb+ho-selection\
 --scan-clause-ac=false --kbo-weight-fun=invfreqrank --fluidsup=true\
 --boolean-reasoning=bool-hoist --fluid-log-hoist=true --fluid-hoist=true\
 --ite-axioms=true --lazy-cnf=true --ho-solid-decider=true\
 --ho-fixpoint-decider=true --bool-select="sel1(pos_ctx)" "${@:4}"
