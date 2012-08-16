#!/bin/sh

# helper to clausify then run prover (using eprover)

TMPFILE=$(mktemp /tmp/proverXXXXX)

echo "% clausify into $TMPFILE"
eprover --cnf --tptp-in --tptp3-out $1 | sed -r 's/^#/%/g' > "$TMPFILE"

PROVER="./src/main.native"

echo "% run prover $PROVER"
trap 'echo "% clean up $TMPFILE" && rm -f "$TMPFILE"' EXIT

shift
OCAMLRUNPARAM="l=5M,$OCAMLRUNPARAM" "$PROVER" "$TMPFILE" $@
