#!/bin/bash

# 1955 solved under 30 seconds

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout $2\
  --tptp-def-as-rewrite --rewrite-before-cnf=true\
  --mode=ho-competitive --boolean-reasoning=simpl-only\
  --ext-rules=off\
  --ho-prim-enum=full --ho-prim-max=1\
  --avatar=off\
  --recognize-injectivity=true  --ho-elim-leibniz=2\
  --ho-unif-level=full-framework --no-max-vars\
  -q "2|prefer-goals|conjecture-relative-e(0.5,1,100,100,100,100,1.5,1.5,1)"\
  -q "2|prefer-formulas|pnrefined(4,300,4,300,4,4,0.7)"\
  -q "3|prefer-fo|conjecture-relative-e(0.1,1,100,100,100,100,1.5,1.5,1.5)"\
  -q "1|prefer-processed|fifo"\
  -q "1|prefer-sos|conjecture-relative-e(0.5,1,100,100,100,100,1.5,1.5,1.5)"\
  -q "2|prefer-ground|pnrefined(1,1,1,1,2,1.5,2)" --ho-choice-inst=true\
  --sine=50 --sine-tolerance=2 --sine-depth-max=4 --sine-depth-min=1\
  --scan-clause-ac=false --kbo-weight-fun=invfreqrank --prec-gen-fun=invfreq_conj\
  --lazy-cnf=true --lazy-cnf-kind=simp --bool-select="sel1(antecedent_ctx)"\
  --superposition-with-formulas=true --select=bb+ho-selection4\
  --lazy-cnf-clausify-implications=false --lazy-cnf-inf-quant=true\
  --ho-fixpoint-decider=true\
  --stream-queue-guard=32 --stream-queue-ratio=256 --stream-clause-num=32 --ho-selection-restriction=none "${@:4}"
