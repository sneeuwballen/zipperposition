#!/bin/bash

# $1: File name
# $2: Extra options

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/zipperposition ${1:+"$1"} \
  -i tptp \
  -o tptp \
  --timeout "$2" \
  --kbo-weight-fun=invdocc --mode=fo-complete-basic \
  -q "6|prefer-sos|conjecture-relative-var(1.02,l,f)" \
  -q "4|const|pnrefined(1,1,1,2,2,2,0.5)" \
  -q "1|prefer-processed|fifo" \
  -q "1|prefer-short-trail|conjecture-relative-var(1,l,f)" \
  -q "6|defer-sos|conjecture-relative-var(1.03,s,f)" \
  --select=e-selection4 --recognize-injectivity=true \
  "${@:3}"

