#!/bin/sh

# helper to run the Zipperposition prover

PROVER="./src/main.native"

. ./runtime.sh

echo "% run prover $PROVER on $TARGET"
exec "$PROVER" $@
