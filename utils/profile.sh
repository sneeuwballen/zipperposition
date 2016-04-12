#!/usr/bin/env bash

ZIPPER=./zipperposition.native

perf record --call-graph=dwarf "$ZIPPER" $@

perf script \
  | stackcollapse-perf --kernel \
  | sed 's/caml//g;
         s/Libzipperposition_prover//g; 
         s/Libzipperposition_core//g;
         s/Libzipperposition__//g' \
  | flamegraph > perf.svg

