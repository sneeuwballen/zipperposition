#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;

let f x y = Comparison.of_total(x-y);;

assert (Multiset.compare_l f [1;2;3;4] [1;2;3] = Comparison.Gt);;
assert (Multiset.compare_l f [1;2;3;4] [1;2;5;3] = Comparison.Lt);;
assert (Multiset.compare_l f [1;2;2;1] [2;1;2;2] = Comparison.Lt);;
assert (Multiset.compare_l f [1;2;1] [2;1;3;2] = Comparison.Lt);;
assert (Multiset.compare_l f [1;2;10;1] [2;1;3;2;3] = Comparison.Gt);;
assert (Multiset.compare_l f [1;2;10;1] [2;10;1;1] = Comparison.Eq);;

ok ();;
