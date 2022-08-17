#!/bin/bash

# solves 4 under 15s

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" \
  --tptp-def-as-rewrite --rewrite-before-cnf=true\
  --mode=ho-competitive --boolean-reasoning=simpl-only\
  --ext-rules=off\
  --ho-prim-enum=none --kbo-weight-fun=invfreqrank --kbo-const-weight=1\
  --avatar=lazy --split-only-ground=true\
  --recognize-injectivity=true  --ho-elim-leibniz=1\
  --ho-unif-level=pragmatic-framework --no-max-vars\
  --max-inferences=1 --ho-unif-max-depth=0\
  --select=e-selection16  --prec-gen-fun=invfreq_conj --kbo-weight-fun-from-precedence=true  --bool-demod=true --lambda-demod=true\
  -q "1|prefer-sos|default"\
  -q "1|prefer-long-trail|conjecture-relative-var(1.01,s,f)"\
  -q "1|prefer-non-goals|default"\
  -q "3|const|conjecture-relative-var(1.01,l,f)"\
  -q "1|prefer-processed|fifo"\
  -q "1|prefer-short-trail|staggered(2)"\
  -q "1|prefer-easy-ho|staggered(1.5)"\
  -q "2|prefer-short-trail|conjecture-relative-var(1,s,f)" "${@:4}"
