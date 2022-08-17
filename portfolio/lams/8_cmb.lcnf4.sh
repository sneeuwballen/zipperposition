#!/bin/bash

# 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --mode=ho-comb-complete\
  --boolean-reasoning=simpl-only\
  --ext-rules=ext-family --ext-rules-max-depth=1 --kbo-weight-fun=docc\
  --ho-prim-enum=combs --ho-prim-enum-early-bird=true\
  --tptp-def-as-rewrite --rewrite-before-cnf=true\
  -q "1|const|conjecture-relative-var(1,s,f)"\
  -q "1|prefer-processed|pnrefined(1,1,1,2,2,2,0.5)"\
  -q "1|prefer-sos|staggered(1)"\
  -q "2|prefer-fo|default"\
  -q "1|prefer-neg-unit|orient-lmax(2,1,2,1,1)"\
  -q "2|prefer-easy-ho|conjecture-relative-struct(1.5,3.5,2,3)"\
  --ho-elim-leibniz=1\
  --select=e-selection2 --solve-formulas=true\
  --lazy-cnf=true --lazy-cnf-kind=simp --lazy-cnf-renaming-threshold=4\
  --sine=100 --sine-tolerance=3 --sine-depth-max=3 --sine-depth-min=1\
  --sine-trim-implications=true --avatar=eager "${@:4}"
