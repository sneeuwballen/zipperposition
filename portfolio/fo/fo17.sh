#!/bin/bash

# $1: File name
# $2: Extra options

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp \
  -o tptp \
  --timeout "$2" \
  --mode=fo-complete-basic --kbo-weight-fun=invfreqrank --simultaneous-sup=true --prec-gen-fun=invfreq\
  -q "1|defer-sos|conj_pref_weight(0.1,5,0.1,1,4)"\
  -q "1|defer-sos|conj_pref_weight(0.5,100,0.2,0.2,4)"\
  -q "1|const|pnrefined(4,300,4,300,4,4,0.7)"\
  -q "1|prefer-processed|rel_lvl_weight(0,1,2,1,1,1,200,200,2.5,999,999)"\
  -q "1|prefer-sos|staggered(1)"\
  -q "2|const|clauseweight(20,300,4)"\
  -q "2|prefer-goals|conjecture-relative-e(0.01,1,999,20,50,10,3,3,0.5)"\
   --select=e-selection --kbo-const-weight=1 "${@:3}"
