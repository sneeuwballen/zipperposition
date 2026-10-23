#!/bin/sh
# Guard 258r: the rel_lvl_weight symbol table must be per-queue, so the clause
# weights computed for either problem cannot depend on the order of the files.
set -eu
z=$1
a=$2
b=$3
spec='2|prefer-processed|rel_lvl_weight(0,1,2,1,1,1,200,200,2.5,999,999)'
d=$(mktemp -d)
trap 'rm -rf "$d"' EXIT
"$z" -d 2 -q "$spec" "$a" "$b" >"$d/ab" 2>&1
"$z" -d 2 -q "$spec" "$b" "$a" >"$d/ba" 2>&1
for f in ab ba; do
  test "$(grep -c 'SZS status Theorem' "$d/$f")" -eq 2
  grep -o ')=[0-9]*$' "$d/$f" | sort >"$d/$f.weights"
done
diff "$d/ab.weights" "$d/ba.weights"
echo "rel_lvl_weight tables are per-queue"
