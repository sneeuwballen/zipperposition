#!/bin/sh
 
export LD=./ld-2.25.so
export LD_LIBRARY_PATH=.

"$LD" ./zipperposition.native -o tptp "$1" \
  --timeout "$STAREXEC_WALLCLOCK_LIMIT" \
  --mem-limit "$STAREXEC_MAX_MEM" \
  --tptp-def-as-rewrite
