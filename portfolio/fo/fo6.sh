#!/bin/bash

# $1: File name
# $2: Extra options

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp \
  -o tptp \
  --timeout "$2" \
  --mode=fo-complete-basic --kbo-weight-fun=invfreqrank --simultaneous-sup=true --prec-gen-fun=invfreq \
  -q "1|const|conjecture-relative-struct(1.5,3.5,2,3)" \
  -q "1|defer-sos|conjecture-relative-struct(1.5,3.5,5,10)" \
  -q "1|const|pnrefined(4,300,4,300,4,4,0.7)" \
  -q "1|prefer-sos|staggered(1)" \
  -q "4|const|conjecture-relative-e(0.1,0.5,100,100,100,100,1.5,1.5,1.5)" \
  -q "1|prefer-sos|staggered(2)" \
  --select=e-selection7 --kbo-const-weight=1 \
  "${@:3}"
