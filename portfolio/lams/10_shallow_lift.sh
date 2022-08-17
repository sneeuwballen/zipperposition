#!/bin/bash

# 1955 solved under 30 seconds

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout $2\
  --mode=ho-pragmatic --tptp-def-as-rewrite --rewrite-before-cnf=true\
  --boolean-reasoning=simpl-only --ext-rules=off\
  --ho-prim-enum=none --avatar=off --recognize-injectivity=true\
  --ho-unif-level=pragmatic-framework --no-max-vars\
  --max-inferences=2 --ho-unif-max-depth=1 -q "6|prefer-sos|pnrefined(1,1,1,2,2,2,0.5)"\
  -q "1|const|conjecture-relative-var(1.02,l,f)"\
  -q "1|prefer-processed|fifo"\
  -q "1|prefer-non-goals|conjecture-relative-var(1.02,l,f)"\
  -q "1|prefer-easy-ho|conjecture-relative-var(1.02,s,f)" --select=e-selection2 --ho-choice-inst=true\
  --sine=50 --sine-tolerance=1.0 --sine-depth-max=3 --sine-depth-min=1\
  --sine-trim-implications=true --scan-clause-ac=false --kbo-weight-fun=lambda-def-invfreqrank\
  --post-cnf-lambda-lifting=true --ho-selection-restriction=none "${@:4}"
