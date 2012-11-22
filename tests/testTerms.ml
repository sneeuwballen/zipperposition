(** test terms *)

open Types

module T = Terms
module H = Helpers
module S = FoSubst
module Unif = FoUnif
module Utils = FoUtils

let preds = ["p"; "q"; "r"; "s"]
let funs = ["f"; "g"; "h"]
let symbols = ["a"; "b"; "c"]

let arities =
  let tbl = Hashtbl.create 3 in
  Hashtbl.add tbl "f" 2;
  Hashtbl.add tbl "g" 1;
  Hashtbl.add tbl "h" 1;
  Hashtbl.add tbl "p" 1;
  Hashtbl.add tbl "q" 1;
  Hashtbl.add tbl "r" 2;
  Hashtbl.add tbl "s" 2;
  tbl

(** random term *)
let random_term ?(ground=false) () =
  let depth = H.random_in 0 4 in
  let rec aux depth = match depth with
  | 0 -> random_leaf ()
  | n ->
    let head = random_fun_symbol () in
    let arity = Hashtbl.find arities head in
    let subterms =
      Utils.times arity (fun _ -> aux (depth - (H.random_in 1 2)))
    in
    T.mk_node head univ_sort subterms
  and random_leaf () =
    if ground || H.R.bool ()
      then T.mk_const (H.choose symbols) univ_sort
      else T.mk_var (H.random_in 0 3) univ_sort
  and random_fun_symbol () = H.choose funs
  in aux depth

(** random bool-sorted term *)
let random_pred ?(ground=false) () =
  let p = H.choose preds in
  let arity = Hashtbl.find arities p in
  if arity = 0
    then T.mk_const p bool_sort
    else T.mk_node p bool_sort (Utils.times arity (random_term ~ground))

(** random pairs of terms *)
let random_pair () = (random_term (), random_term ())

(** check all variables are subterms *)
let check_subterm t = 
  match t.vars with
  | [||] -> H.TestPreconditionFalse
  | vars ->
    if Array.fold_left (fun acc v -> acc && T.member_term v t) true vars
      then H.TestOk else H.TestFail t

(** check unification *)
let check_unif (t1, t2) =
  try
    let subst = Unif.unification S.id_subst t1 t2 in
    if T.eq_term (S.apply_subst subst t1) (S.apply_subst subst t2)
      then H.TestOk else H.TestFail (t1, t2)
  with UnificationFailure -> H.TestPreconditionFalse

let run () =
  Format.printf "run terms test@.";
  let pp_pair formater (t1,t2) = Format.fprintf formater "(%a, %a)"
    !T.pp_term#pp t1 !T.pp_term#pp t2 in
  H.check_and_print ~name:"check_subterm" check_subterm random_term !T.pp_term#pp 5000;
  H.check_and_print ~name:"check_unif" check_unif random_pair pp_pair 5000
