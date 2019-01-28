#!/bin/sh

exec dune exec --profile=release ./src/main/zipperposition.exe -- $@
