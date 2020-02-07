#!/bin/sh
ZIPPER=$1
cat "../examples/must_pass.txt" | while read i ; do
  echo "must pass: $i"; 
  (${ZIPPER} $i -t 5 -o none | grep "SZS status Theorem" > /dev/null) \
   || (echo "must-pass failed on $$i" && exit 1)
done
