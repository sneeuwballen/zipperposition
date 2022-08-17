#!/bin/bash

# 1955 solved under 30 seconds

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" -nc --tptp-def-as-rewrite --rewrite-before-cnf=true\
  --mode=ho-pragmatic --boolean-reasoning=simpl-only --post-cnf-lambda-lifting=true\
  --ext-rules=off\
  --ho-prim-enum=none\
  --recognize-injectivity=true\
  --no-max-vars\
  --select=bb+e-selection8\
  --prec-gen-fun=invfreq --kbo-weight-fun=invfreqrank --kbo-const-weight=2\
  --ord=derived_ho_kbo\
  --ignore-orphans=true -q "1|defer-sos|conj_pref_weight(0.1,5,0.1,1,4)"\
  -q "1|defer-sos|conj_pref_weight(0.5,100,0.2,0.2,4)"\
  -q "1|const|pnrefined(4,300,4,300,4,4,0.7)"\
  -q "2|prefer-processed|rel_lvl_weight(0,1,2,1,1,1,200,200,2.5,999,999)"\
  -q "1|prefer-sos|staggered(1)"\
  -q "2|const|clauseweight(20,300,4)"\
  -q "2|prefer-goals|conjecture-relative-e(0.01,1,999,20,50,10,3,3,0.5)"\
  --lazy-cnf=true --lazy-cnf=true --e-call-point=0.01 --e-timeout=6\
  --e-auto=true --presaturate=true --lazy-cnf-renaming-threshold=8 --try-e="$DIR/eprover-ho" --tmp-dir="$3" "${@:4}"
