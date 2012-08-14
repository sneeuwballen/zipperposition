#!/bin/sh

# helper to clausify then run prover (using eprover)

TMPFILE=$(mktemp /tmp/proverXXXXX)

echo "# clausify into $TMPFILE"
eprover --cnf --tptp3-in --tptp3-out $1 | sed -r 's/^#/%/g' > "$TMPFILE"

echo "# run prover"
shift
OCAMLRUNPARAM="b,s=50M,$OCAMLRUNPARAM" ./main.native "$TMPFILE" $@

trap 'echo "# clean up $TMPFILE" && rm -f "$TMPFILE"' EXIT
