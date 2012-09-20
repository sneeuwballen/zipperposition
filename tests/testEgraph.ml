(* test the Egraph abilities to solve E-unification problems *)

open Types

module T = Terms
module S = FoSubst
module Utils = FoUtils
module Egraph = Egraph

let zero = T.mk_leaf "0" univ_sort
let succ n = T.mk_apply "s" univ_sort [n]
let plus a b = T.mk_apply "+" univ_sort [a; b]
let times a b = T.mk_apply "x" univ_sort [a; b]
let x = T.mk_var 1 univ_sort
let y = T.mk_var 2 univ_sort

let rec from_int n =
  assert (n >= 0);
  if n = 0 then zero else succ (from_int (n-1))

(** Simple theory of Peano arithmetic for + and x *)
let peano_theory =
  [ (plus (succ x) y, succ (plus x y));
    (plus zero y, y);
    (times (succ x) y, plus (times x y) y);
    (times y zero, zero);
  ]

let dot_file = ref "/tmp/egraph.dot"

let egraph = Egraph.empty ()

(** try to E-unify n+n and 2n *)
let test n =
  let a = plus (from_int n) (from_int n)
  and b = from_int (2 * n) in
  (* close by Peano theory *)
  let substs = Egraph.e_unify egraph peano_theory a b 5 in
  Format.printf "got %d answers for unification@." (List.length substs)

let () =
  Utils.set_debug 3;
  test 2;
  Format.printf "print to %s@." !dot_file;
  Egraph.to_dot_file ~name:"egraph" egraph !dot_file
