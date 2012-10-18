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

(** random term *)
let random_term () =
  let depth = H.random_in 0 4 in
  let rec aux depth = match depth with
  | 0 -> random_leaf ()
  | n ->
    let arity = H.random_in 1 3
    and head = random_fun () in
    let subterms =
      Utils.times arity (fun _ -> aux (depth - (H.random_in 1 2)))
    in
    T.mk_node (head::subterms)
  and random_leaf () =
    if H.R.bool ()
      then T.mk_leaf (H.choose symbols) univ_sort
      else T.mk_var (H.random_in 0 3) univ_sort
  and random_fun () =
    T.mk_leaf (H.choose funs) univ_sort
  in aux depth

(** random bool-sorted term *)
let random_pred () =
  let p = T.mk_leaf (H.choose preds) bool_sort in
  let arity = H.random_in 0 3 in
  if arity = 0
    then p
    else T.mk_node (p::(Utils.times arity random_term))

(** random pairs of terms *)
let random_pair () = (random_term (), random_term ())

(** check all variables are subterms *)
let check_subterm t = 
  match T.vars_of_term t with
  | [] -> H.TestPreconditionFalse
  | vars ->
      if List.for_all (fun v -> T.member_term v t) vars
        then H.TestOk else H.TestFail t

(** check unification *)
let check_unif (t1, t2) =
  try
    let subst = Unif.unification t1 t2 in
    if T.eq_term (S.apply_subst subst t1) (S.apply_subst subst t2)
      then H.TestOk else H.TestFail (t1, t2)
  with UnificationFailure _ -> H.TestPreconditionFalse

let run () =
  Format.printf "run terms test@.";
  let pp_pair formater (t1,t2) = Format.fprintf formater "(%a, %a)"
    !T.pp_term#pp t1 !T.pp_term#pp t2 in
  H.check_and_print ~name:"check_subterm" check_subterm random_term !T.pp_term#pp 5000;
  H.check_and_print ~name:"check_unif" check_unif random_pair pp_pair 5000
