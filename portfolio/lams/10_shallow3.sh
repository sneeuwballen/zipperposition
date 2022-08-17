#!/bin/bash

# 1955 solved under 30 seconds

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout $2\
  --mode=ho-pragmatic --tptp-def-as-rewrite --rewrite-before-cnf=true\
  --max-inferences=2 --ho-unif-max-depth=2 --ho-max-elims=0 --ho-max-app-projections=1 --ho-max-identifications=0\
  --ho-max-rigid-imitations=1 --ho-unif-level=pragmatic-framework\
  --boolean-reasoning=simpl-only\
  --ext-rules=off\
  --ho-prim-enum=none\
  -q "1|prefer-sos|default"\
  -q "8|const|conjecture-relative-var(1.02,l,f)"\
  -q "2|prefer-processed|fifo"\
  -q "1|prefer-ground|explore"\
  -q "1|prefer-non-goals|explore"\
  -q "1|prefer-processed|conjecture-relative-var(1.01,s,f)"\
  --recognize-injectivity=true\
  --ho-selection-restriction=none --select=e-selection9 --solve-formulas=true\
  --sine=50 --sine-tolerance=1 --sine-depth-max=3 --sine-depth-min=1 "${@:4}"
