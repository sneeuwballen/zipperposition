#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --tptp-def-as-rewrite --rewrite-before-cnf=true\
  --mode=ho-competitive --boolean-reasoning=bool-hoist --bool-hoist-simpl=true\
  --ext-rules=off  --avatar=off\
  --recognize-injectivity=true\
  --ho-unif-level=full-framework --no-max-vars\
  -q "4|prefer-sos|pnrefined(1,1,1,2,2,2,0.5)"\
  -q "2|prefer-goals|conjecture-relative-var(1.02,l,f)"\
  -q "1|prefer-processed|fifo"\
  -q "1|prefer-formulas|conjecture-relative-var(1,l,f)"\
  -q "2|prefer-easy-ho|conjecture-relative-var(1.01,s,f)"\
  --ho-choice-inst=true\
  --sine=50 --sine-tolerance=2 --sine-depth-max=4 --sine-depth-min=1 --e-max-derived=48\
  --e-encode-lambdas=keep --scan-clause-ac=false\
  --kbo-weight-fun=invfreqrank --lazy-cnf=true --lazy-cnf-kind=simp\
  --bool-select="sel1(antecedent_ctx)" --superposition-with-formulas=true\
  --select=NoSelection --lazy-cnf-clausify-implications=false --lazy-cnf-inf-quant=true "${@:4}"
