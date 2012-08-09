#!/bin/sh

# helper to clausify then run prover (using eprover)

TMPFILE=$(mktemp proverXXXXX)

echo "# clausify into $TMPFILE"
if [ -z "$2" ]; then
    INPUT="--tptp3-in"
else
    INPUT="$2"
fi
eprover --cnf "$INPUT" --tptp3-out $1 | sed -r 's/^#/%/g' > "$TMPFILE"

echo "# run prover"
./main.native "$TMPFILE"

trap 'echo "# clean up $TMPFILE" && rm -f "$TMPFILE"' EXIT
