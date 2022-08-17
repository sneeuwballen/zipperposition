#!/bin/bash

# 1955 solved under 30 seconds

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" -nc --tptp-def-as-rewrite --rewrite-before-cnf=true\
  --mode=ho-competitive --boolean-reasoning=bool-hoist --ext-rules=off\
  --recognize-injectivity=true --ho-unif-level=full-framework\
  -q "4|prefer-goals|pnrefined(1,1,1,2,2,2,0.5)"\
  -q "1|prefer-fo|conjecture-relative-var(1.02,l,f)"\
  -q "1|prefer-non-goals|conjecture-relative-var(1,l,f)"\
  -q "4|prefer-ho-steps|conjecture-relative-var(1.01,s,f)"\
  -q "1|prefer-processed|fifo"\
  --select=bb+ho-selection\
  --scan-clause-ac=false\
  --kbo-weight-fun=invfreqrank --fluidsup=true --boolean-reasoning=bool-hoist --fluid-log-hoist=true\
  --fluid-hoist=true --ite-axioms=true --lazy-cnf=true --ho-solid-decider=true --ho-fixpoint-decider=true\
  --bool-select="sel1(pos_ctx)" --try-e="$DIR/eprover-ho" --tmp-dir="$3" --e-timeout=9 \
  --e-call-point=0.1 --avatar=off --e-max-derived=50 "${@:4}"
