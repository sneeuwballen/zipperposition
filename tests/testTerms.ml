(** test terms *)

open Types
open Symbols

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

let ord = Orderings.default_ordering base_signature

(** Choose a symbol among the given list *)
let choose among =
  let s = mk_symbol (H.choose among) in
  ignore (ord#precedence#add_symbols [s]);
  s

(** random term *)
let random_term ?(ground=false) () =
  let depth = H.random_in 0 4 in
  let rec aux depth = match depth with
  | 0 -> random_leaf ()
  | n ->
    let head = random_fun_symbol () in
    let arity = Hashtbl.find arities (name_symbol head) in
    let subterms =
      Utils.times arity (fun _ -> aux (depth - (H.random_in 1 2)))
    in
    T.mk_node head univ_sort subterms
  and random_leaf () =
    if ground || H.R.bool ()
      then T.mk_const (choose symbols) univ_sort
      else T.mk_var (H.random_in 0 3) univ_sort
  and random_fun_symbol () =
    let s = choose funs in
    s
  in aux depth

(** random bool-sorted term *)
let random_pred ?(ground=false) () =
  let p = choose preds in
  let arity = Hashtbl.find arities (name_symbol p) in
  if arity = 0
    then T.mk_const p bool_sort
    else T.mk_node p bool_sort (Utils.times arity (random_term ~ground))

(** random pairs of terms *)
let random_pair () = (random_term (), random_term ())

(** check all variables are subterms *)
let check_subterm t = 
  let vars = T.vars t in
  match vars with
  | [] -> H.TestPreconditionFalse
  | vars ->
      if List.for_all (fun v -> T.member_term v t) vars
        then H.TestOk else H.TestFail t

(** check unification *)
let check_unif (t1, t2) =
  let offset = T.max_var (T.vars t1) + 1 in
  try
    let subst = Unif.unification S.id_subst (t1,0) (t2,offset) in
    if S.apply_subst subst (t1,0) == S.apply_subst subst (t2,offset)
      then H.TestOk else H.TestFail (t1, t2)
  with UnificationFailure -> H.TestPreconditionFalse

(** special cases *)
let check_special () =
  let x = T.mk_var 1 univ_sort in
  let t1 = T.mk_bind lambda_symbol
    (T.mk_bind lambda_symbol
      (T.mk_node (mk_symbol "f") univ_sort [
        T.mk_node (mk_symbol "f") univ_sort [x; T.mk_bound_var 1 univ_sort]
        ; T.mk_bound_var 0 univ_sort]))
  in
  let t2 = T.mk_node (mk_symbol "g") univ_sort [T.mk_bound_var 0 univ_sort] in
  let subst = S.bind S.id_subst (x,0) (t2,0) in
  let t1' = S.apply_subst subst (t1,0) in
  Format.printf "apply @[<h>%a to %a[%d] yields %a @]@."
    S.pp_substitution subst !T.pp_term#pp t1 0 !T.pp_term#pp t1';
  ()

let run () =
  Format.printf "run terms test@.";
  let pp_pair formater (t1,t2) = Format.fprintf formater "(%a, %a)"
    !T.pp_term#pp t1 !T.pp_term#pp t2 in
  H.check_and_print ~name:"check_subterm" check_subterm random_term !T.pp_term#pp 5000;
  H.check_and_print ~name:"check_unif" check_unif random_pair pp_pair 5000;
  check_special ();
  ()
