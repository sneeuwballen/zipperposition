(** test E-unification *)

open Types

module T = Terms
module C = Clauses
module S = FoSubst
module Utils = FoUtils
module EUnif = E_unif

let ord = Orderings.default_ordering ()

(* theory: f(a, b) = a;  a = b *)
let th1 =
  let a = T.mk_leaf "a" univ_sort
  and b = T.mk_leaf "b" univ_sort in
  let fab = T.mk_apply "f" univ_sort [a; b] in
  let theory = EUnif.empty_theory in
  let theory = EUnif.add_axiom theory 
    (C.mk_clause ~ord [C.mk_eq ~ord a b] (lazy (Axiom (".", "th1")))
        ~selected:(lazy []) (lazy [])) in
  let theory = EUnif.add_axiom theory
    (C.mk_clause ~ord [C.mk_eq ~ord fab a] (lazy (Axiom (".", "th2")))
        ~selected:(lazy []) (lazy [])) in
  theory

(* print list of substitutions *)
let print_solutions substs =
  Format.printf "answers: @[<v>%a@]@." (Utils.pp_list ~sep:"" S.pp_substitution) substs

(* test E-unification of f(x,x)=x in theory th1 *)
let test_1 () =
  let x = T.mk_var 1 univ_sort in
  let fxx = T.mk_apply "f" univ_sort [x; x] in
  let unifiers = EUnif.e_unify ~ord th1 fxx x in
  match EUnif.e_compute ~steps:20000 unifiers with
  | EUnif.ESat substs, _ ->
    Format.printf "f(x,x) = x is sat@.";
    print_solutions substs
  | EUnif.EUnknown substs, _ ->
    Format.printf "f(x,x) = x is unknown@.";
    print_solutions substs
  | EUnif.EUnsat, [] ->
    Format.printf "f(x,x) = x is unsat@."
  | EUnif.EUnsat, _ -> assert false

let tests:(unit -> unit) list =
  [ test_1 ]

let run () =
  Format.printf "run E-unification tests@.";
  List.iter (fun f -> f ()) tests
