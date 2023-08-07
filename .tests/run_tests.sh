#!/usr/bin/env sh

exec dune exec --profile=release -- tests/run_tests.exe $@
