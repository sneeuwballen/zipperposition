#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
   --mode=ho-pragmatic\
   --tptp-def-as-rewrite --rewrite-before-cnf=true\
   --boolean-reasoning=simpl-only\
   --ho-prim-enum=or --ho-prim-enum-early-bird=true\
   --select=ho-selection3 --prec-gen-fun=unary_first --kbo-weight-fun=modarity\
   --ord=derived_ho_rpo\
   --ho-unif-level=pragmatic-framework --ho-max-app-projections=0 --ho-max-rigid-imitations=1\
   --ho-max-identifications=0 --ho-max-elims=0 --lambda-demod=true --bool-demod=true\
   --ho-pattern-decider=true --ho-solid-decider=false --ho-fixpoint-decider=true\
   --lazy-cnf=true --lazy-cnf-kind=simp --lazy-cnf-renaming-threshold=8\
   --sine=65 --sine-depth-min=1 --sine-depth-max=5 --sine-tolerance=1.2 --sine-trim-implications=true\
   -q "1|prefer-ho-steps|default"\
   -q "4|prefer-easy-ho|conjecture-relative-var(1,l,f)"\
   -q "1|prefer-processed|fifo"\
   -q "1|defer-lambdas|explore"\
   -q "1|prefer-non-goals|orient-lmax(2,1,2,1,1)"\
   -q "1|prefer-sos|pnrefined(2,1,1,1,2,2,2)"\
   -q "1|prefer-processed|conjecture-relative-var(1,s,f)"\
   --ho-selection-restriction=none --max-inferences=3 --presaturate=true "${@:4}"
