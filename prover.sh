#!/bin/sh

# helper to clausify then run prover (using eprover)

TARGET="$1"
PROVER="./main.native"

shift  # remove first option

. ./runtime.sh

echo "% run prover $PROVER on $TARGET"
eprover --cnf --tptp-in --tptp3-out -l0 "$TARGET" |
    sed -r 's/^#/%/g' |
    "$PROVER" stdin -calculus superposition $@
