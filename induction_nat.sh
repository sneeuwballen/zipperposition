#!/usr/bin/env sh

./zipperposition.native -mem-limit 1300 -dot /tmp/truc.dot \
    -induction 'nat:s|z' \
    $@
