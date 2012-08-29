#!/bin/sh

# helper to clausify then run prover (using eprover)

TARGET="$1"
PROVER="./src/main.native"

shift  # remove first option

echo "% run prover $PROVER on $TARGET"
eprover --cnf --tptp-in --tptp3-out -l0 "$TARGET" |
    sed -r 's/^#/%/g' |
    OCAMLRUNPARAM="l=5M,$OCAMLRUNPARAM" "$PROVER" stdin -calculus superposition $@
