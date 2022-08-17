#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --mode=ho-pragmatic\
  --boolean-reasoning=simpl-only\
  --tptp-def-as-rewrite --rewrite-before-cnf=true\
  --kbo-weight-fun=freqrank\
  -q "1|prefer-sos|default"\
  -q "1|prefer-goals|conjecture-relative-var(1.03,s,f)"\
  -q "1|prefer-non-goals|default"\
  -q "5|const|conjecture-relative-var(1.01,l,f)"\
  -q "1|prefer-processed|fifo"\
  -q "1|const|conjecture-relative-var(1.05,l,f)"\
  -q "1|defer-sos|conjecture-relative-var(1.1,s,f)"\
  --select=e-selection9 --recognize-injectivity=true --ho-choice-inst=false --ho-selection-restriction=none\
  --sine=50 --sine-tolerance=3 --sine-depth-max=3 --sine-depth-min=1 --eq-encode=true --avatar=eager --sine-trim-implications=true "${@:4}"
