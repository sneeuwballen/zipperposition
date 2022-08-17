#!/bin/bash

# $1: File name
# $2: Extra options

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp \
  -o tptp \
  --timeout "$2" \
  --kbo-weight-fun=docc --mode=fo-complete-basic \
  -q "7|prefer-sos|pnrefined(2,1,1,1,2,2,2)" \
  -q "4|prefer-short-trail|orient-lmax(2,1,2,1,1)" \
  -q "1|prefer-processed|fifo" \
  -q "7|prefer-ground|conjecture-relative-var(1,l,f)" \
  -q "6|defer-sos|conjecture-relative-var(1.03,s,f)" \
  --select=e-selection7 --recognize-injectivity=true \
  --sine=50 --sine-tolerance=1.5 --sine-depth-max=2 --sine-depth-min=1 \
  "${@:3}"
