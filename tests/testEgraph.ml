(* test the Egraph abilities to solve E-unification problems *)

open Types

module T = Terms
module S = FoSubst
module Utils = FoUtils
module Egraph = Egraph

let a = T.mk_leaf "a" univ_sort
let b = T.mk_leaf "b" univ_sort
let f x y = T.mk_apply "f" univ_sort [x; y]
let g x = T.mk_apply "g" univ_sort [x]
let zero = T.mk_leaf "0" univ_sort
let succ n = T.mk_apply "s" univ_sort [n]
let plus a b = T.mk_apply "+" univ_sort [a; b]
let minus a = T.mk_apply "-" univ_sort [a]
let times a b = T.mk_apply "x" univ_sort [a; b]
let x = T.mk_var 1 univ_sort
let y = T.mk_var 2 univ_sort
let z = T.mk_var 3 univ_sort

let rec from_int n =
  assert (n >= 0);
  if n = 0 then zero else succ (from_int (n-1))

(** Simple theory of Peano arithmetic for + and x *)
let peano_theory =
  [ (plus (succ x) y, succ (plus x y));
    (plus zero x, x);
    (times (succ x) y, plus (times x y) y);
    (times x zero, zero);
  ]

(** theory f(a,b)=a, a=b *)
let simple_theory =
  [ (f a b, a);
    (a, b);
  ]

(** commutativity of f *)
let commutativity_theory =
  [ (f x y, f y x); ]

(** associative group theory: -y+y=0, x+0=x, (x+y)+z=x+(y+z) *)
let group_theory = 
  [ (plus zero x, x);
    (plus (minus x) x, zero);
    (plus (plus x y) z, plus x (plus y z));
  ]

let dot_file = ref "/tmp/egraph.dot"
let counter = ref 1

let egraph = Egraph.empty ()

let test ~depth theory a b =
  (* callback used for solutions *)
  let k subst =
    let file = Utils.sprintf "/tmp/egraph%d.dot" !counter in
    incr counter;
    Format.printf "unifying of %a and %a succeeds with %a. Print E-graph to file %s@."
      !T.pp_term#pp a !T.pp_term#pp b S.pp_substitution subst file;
    Egraph.push egraph;
    Egraph.apply_substitution egraph subst;
    assert (Egraph.are_equal (Egraph.node_of_term egraph a) (Egraph.node_of_term egraph b));
    Egraph.to_dot_file ~name:"egraph" egraph file;
    Egraph.pop egraph
  in
  (* E-unify with theory *)
  Egraph.e_unify egraph theory a b depth k

(** try to E-unify n+n and 2n in Peano theory*)
let test_peano n =
  let a = plus (from_int n) (from_int n)
  and b = from_int (2 * n) in
  test ~depth:4 peano_theory a b

(** try to E-unify f(x,x) and x in simple_theory *)
let test_simple () =
  let a = f x x
  and b = x in
  test ~depth:2 simple_theory a b

(** try to E-unify f(x,y) and f(a,b) in commutativity_theory *)
let test_commutativity () =
  let a = g (f x y)
  and b = g (f a b) in
  test ~depth:2 commutativity_theory a b

(** try to E-unify -0+x and x in group_theory *)
let test_group () =
  let a = plus (minus zero) x
  and b = x in
  test ~depth:2 group_theory a b

let () =
  Utils.set_debug 3;
  test_simple ();
  test_commutativity ();
  test_group ();
  test_peano 2;
  ()
