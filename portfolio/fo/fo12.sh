#!/bin/bash

# $1: File name
# $2: Extra options

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"


ulimit -t $2

$DIR/../zipperposition ${1:+"$1"} \
  -i tptp\
  -o tptp\
  --timeout "$2" --mode=fo-complete-basic \
  --recognize-injectivity=true --ho-unif-level=pragmatic-framework \
   -q "6|prefer-goals|default" \
   -q "6|prefer-non-goals|explore" \
   -q "2|prefer-sos|conjecture-relative-var(1.05,s,f)"\
   -q "1|const|fifo"   --select=e-selection3 --avatar=off \
  "${@:3}"
