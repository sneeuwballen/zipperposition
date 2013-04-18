(** test terms *)

open Basic
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

let ctx =
  { ctx_ord=Orderings.default_ordering base_signature;
    ctx_select=no_select;
  }

(** Choose a symbol among the given list *)
let choose among =
  let s = mk_symbol (H.choose among) in
  let prec', _ = ctx.ctx_ord.ord_precedence.prec_add_symbols [s] in
  ctx.ctx_ord <- ctx.ctx_ord.ord_set_precedence prec';
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
    T.mk_node head univ_ subterms
  and random_leaf () =
    if ground || H.R.bool ()
      then T.mk_const (choose symbols) univ_
      else T.mk_var (H.random_in 0 3) univ_
  and random_fun_symbol () =
    let s = choose funs in
    s
  in aux depth

(** random bool-sorted term *)
let random_pred ?(ground=false) () =
  let p = choose preds in
  let arity = Hashtbl.find arities (name_symbol p) in
  if arity = 0
    then T.mk_const p bool_
    else T.mk_node p bool_ (Utils.times arity (random_term ~ground))

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

(** Check curry/uncurry *)
let check_curry t =
  let t' = T.curry t in
  Format.printf "@[<h>curry %a = %a@]@." !T.pp_term#pp t !T.pp_term#pp t';
  let t'' = T.uncurry t' in
  Format.printf "@[<h>uncurry %a = %a@]@." !T.pp_term#pp t' !T.pp_term#pp t'';
  if t'' == t then H.TestOk else H.TestFail (T.mk_eq t' t'')

(** Check AC-matching *)
let check_ac_matching () =
  let x = T.mk_var 1 bool_
  and y = T.mk_var 2 bool_
  and z = T.mk_var 3 bool_
  and x4 = T.mk_var 4 univ_ in
  let t1 = T.mk_node (mk_symbol "f1") univ_ [T.mk_const (mk_symbol "a1") univ_]
  and t2 = T.mk_node (mk_symbol "f1") univ_ [T.mk_const (mk_symbol "b1") univ_]
  and t3 = T.mk_node (mk_symbol "f1") univ_ [x4]
  and p1 = T.mk_const (mk_symbol "p1") bool_
  and p2 = T.mk_const (mk_symbol "p2") bool_
  and p3 = T.mk_const (mk_symbol "p3") bool_ in
  let t1 =
    T.mk_or
      (T.mk_or
        (T.mk_or x y)
        (T.mk_or z (T.mk_eq t1 t3)))
      p1
  and t2 =
    T.mk_or
      (T.mk_or p1 (T.mk_eq t2 t1))
      (T.mk_or
        (T.mk_or p1 p2)
        (T.mk_or y p3))
  in
  (* big *)
  Format.printf "AC-match @[<h>%a and %a@]@." !T.pp_term#pp t1 !T.pp_term#pp t2;
  Sequence.iter
    (fun subst -> 
      Format.printf "-----------------------------------------@.";
      Format.printf "  --> @[<h>%a@]@." S.pp_substitution subst;
      let t1' = S.apply_subst subst (t1,0)
      and t2' = S.apply_subst subst (t2,1) in
      let ok = T.ac_eq t1' t2' in
      Format.printf "@[<h>%a =_AC %a? %B@]@." !T.pp_term#pp t1' !T.pp_term#pp t2' ok)
    (Unif.matching_ac S.id_subst (t1,0) (t2,1));
  (* small *)
  let t1 =
    T.mk_eq
      (T.mk_at
        (T.mk_var 0 (univ_ <=. univ_))
        (T.mk_var 1 univ_))
      (T.mk_at
        (T.mk_var 2 (univ_ <=. univ_))
        (T.mk_var 1 univ_))
  and t2 = 
    T.mk_eq
      (T.mk_at
        (T.mk_const (mk_symbol "f1") (univ_ <=. univ_))
        (T.mk_var 3 univ_))
      (T.mk_at
        (T.mk_const (mk_symbol "f2") (univ_ <=. univ_))
        (T.mk_var 3 univ_))
  in
  Format.printf "AC-match @[<h>%a and %a@]@." !T.pp_term#pp t1 !T.pp_term#pp t2;
  Sequence.iter
    (fun subst -> 
      Format.printf "-----------------------------------------@.";
      Format.printf "  --> @[<h>%a@]@." S.pp_substitution subst;
      let t1' = S.apply_subst subst (t1,0)
      and t2' = S.apply_subst subst (t2,1) in
      let ok = T.ac_eq t1' t2' in
      Format.printf "@[<h>%a =_AC %a? %B@]@." !T.pp_term#pp t1' !T.pp_term#pp t2' ok)
    (Unif.matching_ac S.id_subst (t1,0) (t2,1));
  ()

let check_beta () =
  let t1 =
    T.mk_at
      (T.mk_at
        (T.mk_const (mk_symbol "f") ((univ_ <=. univ_) <=. univ_))
        (T.mk_const (mk_symbol "a") univ_))
      (T.mk_at
        (T.mk_bound_var 0 (univ_ <=. univ_))
        (T.mk_const (mk_symbol "b") univ_))
  and t2 =
    T.mk_lambda (univ_ <=. univ_) univ_
      (T.mk_at
        (T.mk_bound_var 0 (univ_ <=. univ_))
        (T.mk_at
          (T.mk_bound_var 0 (univ_ <=. univ_))
          (T.mk_const (mk_symbol "c") univ_)))
  in
  let t1 = T.mk_lambda (t1.sort <=. t2.sort) t2.sort t1 in
  Format.printf "beta reduce @[<h>%a@]@ @@ @[<h>%a@]@." !T.pp_term#pp t1 !T.pp_term#pp t2;
  let redex = T.mk_at t1 t2 in
  let reduced = T.beta_reduce redex in
  Format.printf "beta reduce @[<h>%a@]@ yields @[<h>%a@]@."
    !T.pp_term#pp redex !T.pp_term#pp reduced;
  ()

(** Constants *)
let check_consts (t1, t2) =
  let t1', t2' = Experts.ground_pair t1 t2 in
  assert (T.is_ground_term t1' && T.is_ground_term t2');
  Format.printf "grounded terms : @[<h>%a %s %a@]@."
    !T.pp_term#pp t1'
      (string_of_comparison (ctx.ctx_ord.ord_compare t1' t2')) !T.pp_term#pp t2';
  if ctx.ctx_ord.ord_compare t1' t2' = Incomparable
    then H.TestFail (t1, t2)
    else H.TestOk

(** special cases *)
let check_special () =
  let x = T.mk_var 1 univ_ in
  let t1 = T.mk_lambda univ_ univ_
    (T.mk_lambda univ_ univ_
      (T.mk_node (mk_symbol "f") univ_ [
        T.mk_node (mk_symbol "f") univ_ [x; T.mk_bound_var 1 univ_]
        ; T.mk_bound_var 0 univ_]))
  in
  let t2 = T.mk_node (mk_symbol "g") univ_ [T.mk_bound_var 0 univ_] in
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
  H.check_and_print ~name:"check_consts" check_consts random_pair pp_pair 20;
  H.check_and_print ~name:"check_curry" check_curry random_term !T.pp_term#pp 20;
  check_ac_matching ();
  check_beta ();
  check_special ();
  ()
