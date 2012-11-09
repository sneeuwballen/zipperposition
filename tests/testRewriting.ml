(** testing of rewriting *)

open Types

module T = Terms
module S = FoSubst
module Utils = FoUtils
module Rw = Rewriting

let a = T.mk_leaf "a" univ_sort
let b = T.mk_leaf "b" univ_sort
let c = T.mk_leaf "c" univ_sort
let d = T.mk_leaf "d" univ_sort
let f x y = T.mk_apply "f" univ_sort [x; y]
let g x = T.mk_apply "g" univ_sort [x]
let h x = T.mk_apply "h" univ_sort [x]
let zero = T.mk_leaf "0" univ_sort
let succ n = T.mk_apply "s" univ_sort [n]
let plus a b = T.mk_apply "+" univ_sort [a; b]
let minus a = T.mk_apply "-" univ_sort [a]
let times a b = T.mk_apply "x" univ_sort [a; b]
let x = T.mk_var 1 univ_sort
let y = T.mk_var 2 univ_sort
let z = T.mk_var 3 univ_sort
let u = T.mk_var 4 univ_sort

let rec from_int n =
  assert (n >= 0);
  if n = 0 then zero else succ (from_int (n-1))

(** convert Peano term t to int *)
let peano_to_int t =
  let s = T.mk_leaf "s" univ_sort in
  let rec count t n =
    match t.term with
    | _ when T.eq_term t zero -> n
    | Node [t1; t2] when T.eq_term t1 s -> count t2 (n+1)
    | _ -> failwith "not peano!"
  in count t 0

(** print a term with a nice representation for Peano numbers *)
let rec print_peano_nice formatter t =
  try
    Format.fprintf formatter "%d" (peano_to_int t)
  with Failure _ ->
    match t.term with
    | Var _ | Leaf _ -> !T.pp_term#pp formatter t
    | Node (h::l) ->
      Format.fprintf formatter "@[<h>%a(%a)@]" !T.pp_term#pp h
        (Utils.pp_list ~sep:", " print_peano_nice) l
    | Node [] -> assert false

(** Simple rewriting system for Peano arithmetic with + and x *)
let peano_trs =
  Rw.from_list
    [ (plus (succ x) y, succ (plus x y));
      (plus zero x, x);
      (times (succ x) y, plus (times x y) y);
      (times zero x, zero);
    ]

(** associative group theory: -y+y=0, x+0=x, (x+y)+z=x+(y+z) *)
let group_trs =
  Rw.from_list
    [ (plus zero x, x);
      (plus (minus x) x, zero);
      (plus (plus x y) z, plus x (plus y z));
    ]

(** check equality of normal forms *)
let test trs t1 t2 =
  Utils.debug 2 (lazy (Utils.sprintf "use TRS %a" Rw.pp_trs trs));
  let t1' = Rw.rewrite trs t1 in
  let t2' = Rw.rewrite trs t2 in
  Utils.debug 1 (lazy (Utils.sprintf "normal form of %a = normal form of %a (ie %a)"
                print_peano_nice t1 print_peano_nice t2 print_peano_nice t1'));
  assert (T.eq_term t1' t2');
  ()

(** compute normal form of (n+n) in peano TRS *)
let test_peano n () =
  let a = plus (from_int n) (from_int n)
  and b = from_int (2 * n) in
  test peano_trs a b

(** compute normal form of n+n and 2xn in Peano *)
let test_peano_bis n () =
  let a = plus (from_int n) (from_int n)
  and b = times (from_int 2) (from_int n) in
  test peano_trs a b
  

let tests:(unit -> unit) list =
  [ test_peano 2; test_peano 4; test_peano 100; test_peano 1000;
    test_peano_bis 2; test_peano_bis 4; test_peano_bis 100; test_peano_bis 1000 ]

let benchmark_count = 1  (* with caching, not accurate to do it several times *)

let benchmark ?(count=benchmark_count) trs a b =
  (* rewrite to normal form *)
  let one_step () =
    a.binding <- a; a.normal_form <- false;
    b.binding <- b; b.normal_form <- false;
    Gc.major ();
    let a' = Rw.rewrite trs a
    and b' = Rw.rewrite trs b in
    assert (T.eq_term a' b')
  in
  Gc.major ();
  let start = Unix.gettimeofday () in
  for i = 1 to count do one_step () done;
  let stop = Unix.gettimeofday () in
  Format.printf "@[<h>%f seconds to do %d joins of %a and %a (%f each)@]@."
    (stop -. start) count print_peano_nice a print_peano_nice b
    ((stop -. start) /. (float_of_int count))

let benchmark_peano n () =
  let a = plus (from_int n) (from_int n)
  and b = times (from_int 2) (from_int n) in
  benchmark peano_trs a b

let benchmark_peano_bis n () =
  let a = plus (plus (from_int n) (from_int n)) (plus (from_int n) (from_int n))
  and b = times (from_int 4) (from_int n) in
  benchmark peano_trs a b

let benchmarks = [ benchmark_peano 2; benchmark_peano 4;
                   benchmark_peano 100; benchmark_peano 1000;
                   benchmark_peano_bis 2; benchmark_peano_bis 4;
                   benchmark_peano_bis 50; benchmark_peano_bis 500 ]


let run () =
  Utils.set_debug 3;
  Format.printf "index : @.%a@." Rw.pp_trs_index peano_trs;
  Format.printf "run rewriting tests@.";
  List.iter (fun f -> f ()) tests;
  Utils.set_debug 1;
  List.iter (fun f -> f ()) benchmarks

let _ =
  HExtlib.profiling_enabled := true

