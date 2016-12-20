#!/usr/bin/env bash

perf record --call-graph=dwarf $@

perf script \
  | stackcollapse-perf --kernel \
  | sed 's/caml//g;
         s/Libzipperposition_prover//g; 
         s/Libzipperposition_core//g;
         s/Libzipperposition__//g' \
  | flamegraph > perf.svg

