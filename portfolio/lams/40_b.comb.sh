#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --mode=ho-comb-complete \
  --boolean-reasoning=simpl-only \
  --ext-rules=off --kbo-weight-fun=lambda-def-sqarity \
  --ho-prim-enum=none \
  --tptp-def-as-rewrite \
  -q "4|prefer-sos|orient-lmax(2,1,2,1,1)" \
  -q "4|defer-sos|conjecture-relative-var(1,s,f)" \
  -q "3|const|default" \
  -q "1|prefer-processed|fifo" \
  --ho-elim-leibniz=1 \
  --select=NoSelection --solve-formulas=true \
  --lazy-cnf=true --lazy-cnf-kind=simp --lazy-cnf-renaming-threshold=8 \
  --sine=60 --sine-tolerance=2 --sine-depth-max=5 --sine-depth-min=1 \
  --try-e="$DIR/eprover-ho" --tmp-dir="$3" --e-timeout=26 --e-max-derived=50 --e-encode-lambdas=drop \
  --scan-clause-ac=false --presaturate=true \
  --comb-b-penalty=3 --comb-c-penalty=3 --comb-k-penalty=1 --comb-s-penalty=5 --subvarsup=false --e-call-point=0.15 "${@:4}"
