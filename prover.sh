#!/bin/sh

# helper to clausify then run prover (using eprover)

TMPFILE=$(mktemp proverXXXXX)

echo "clausify into $TMPFILE"
eprover --cnf --tptp-in --tptp3-out $1 | sed -r 's/^#/%/g' > "$TMPFILE"

echo "run prover"
./main.native "$TMPFILE"

echo "clean up $TMPFILE"
rm -f "$TMPFILE"
