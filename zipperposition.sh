#!/bin/sh

# helper to run the Zipperposition prover

PROVER="./main.native"

. ./runtime.sh

echo "% run prover $PROVER on $TARGET"
exec "$PROVER" $@
