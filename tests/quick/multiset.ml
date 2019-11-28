#!/usr/bin/env ocaml
#use "tests/quick/.common.ml";;

open Logtk
let f x y = Comparison.of_total(x-y);;

module M = Multiset.Make(CCInt) ;;

let cmp_l f a b = M.compare_partial f (M.of_list a) (M.of_list b) ;;

assert (cmp_l f [1;2;3;4] [1;2;3] = Comparison.Gt);;
assert (cmp_l f [1;2;3;4] [1;2;5;3] = Comparison.Lt);;
assert (cmp_l f [1;2;2;1] [2;1;2;2] = Comparison.Lt);;
assert (cmp_l f [1;2;1] [2;1;3;2] = Comparison.Lt);;
assert (cmp_l f [1;2;10;1] [2;1;3;2;3] = Comparison.Gt);;
assert (cmp_l f [1;2;10;1] [2;10;1;1] = Comparison.Eq);;

ok ();;
