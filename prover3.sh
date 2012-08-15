#!/bin/sh

# helper to clausify then run prover (using eprover)

TMPFILE=$(mktemp /tmp/proverXXXXX)

echo "# clausify into $TMPFILE"
eprover --cnf --tptp3-in --tptp3-out $1 | sed -r 's/^#/%/g' > "$TMPFILE"

PROVER="./src/main.native"

echo "# run prover $PROVER"
shift
OCAMLRUNPARAM="b,$OCAMLRUNPARAM" "$PROVER" "$TMPFILE" $@

trap 'echo "# clean up $TMPFILE" && rm -f "$TMPFILE"' EXIT
