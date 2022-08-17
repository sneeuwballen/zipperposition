#!/bin/bash

# Solves 3 under 18

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --tptp-def-as-rewrite --rewrite-before-cnf=true \
  --mode=ho-competitive --boolean-reasoning=simpl-only \
  --ext-rules=off   --ho-prim-enum=none   --recognize-injectivity=true  --ho-elim-leibniz=off \
  --ho-unif-level=full-framework --no-max-vars \
  -q "3|by-norm-factor|conjecture-relative-var(1.02,l,f)" \
  -q "1|prefer-ho-steps|conjecture-relative-var(1,s,f)" \
  -q "1|prefer-processed|fifo" \
  -q "3|by-app-var-num|pnrefined(2,1,1,1,2,2,2)" \
  --select=e-selection9 --ho-selection-restriction=none \
  --sine=50 --sine-tolerance=2 --sine-depth-max=3 --sine-depth-min=1 \
  --prec-gen-fun=const_first \
  --lazy-cnf=true --lazy-cnf-kind=simp --lazy-cnf-renaming-threshold=3 \
  --solid-subsumption=false --ignore-orphans=false \
  --ho-solid-decider=true  --ho-fixpoint-decider=true --ho-pattern-decider=true \
  --sup-at-vars=false --sup-at-var-headed=false --sup-from-var-headed=false --dupsup=true "${@:4}"
