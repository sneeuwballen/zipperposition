(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** Term rewriting *)

open Types

module T = Terms
module S = FoSubst
module DT = Dtree
module Unif = FoUnif
module Utils = FoUtils

type rule = (term * term)

(* ----------------------------------------------------------------------
 * Definition of TRS
 * ---------------------------------------------------------------------- *)

(** Term Rewriting System *)
type trs = {
  mutable index : term DT.dtree;    (** index of rules *)
  mutable nbr_rules : int;          (** number of rules *)
}

let create () = {
  index = DT.empty T.eq_term;
  nbr_rules = 0;
}

let var_offset = 50000              (** negative offset for variables in rules *)

let add_rule trs (l, r) =
  (* check that the rule does not introduce variables *)
  assert (List.for_all
    (fun v -> T.member_term v l)
    r.vars);
  assert (not (T.is_var l));
  assert (l.sort = r.sort);
  (* use low, negative variables *)
  let vars = l.vars in
  List.iter (fun v ->
    match v.term with
    | Var i -> T.set_binding v (T.mk_var (-(i+var_offset)) v.sort)
    | _ -> assert false) vars;
  let l, r = T.expand_bindings l, T.expand_bindings r in
  (* add rule to the discrimination tree *)
  (* now add the rule to the index *)
  trs.index <- DT.add trs.index l r;
  trs.nbr_rules <- trs.nbr_rules + 1

let add_rules trs l = List.iter (add_rule trs) l

let from_list l =
  let trs = create () in
  add_rules trs l;
  trs

let size trs = trs.nbr_rules

let iter trs k =
  DT.iter trs.index (fun l r -> k (l, r))

let pp_rule formatter (l, r) =
  Format.fprintf formatter "@[<h>%a → %a@]" !T.pp_term#pp l !T.pp_term#pp r

let pp_trs formatter trs =
  let rules = ref [] in
  iter trs (fun rule -> rules := rule :: !rules);
  Format.fprintf formatter "{@[<hv>%a@]}"
    (Utils.pp_list ~sep:";" pp_rule) !rules

(* ----------------------------------------------------------------------
 * Computation of normal forms
 * ---------------------------------------------------------------------- *)

exception RewrittenIn of term

(** Compute normal form of the term, and set its binding to the normal form *)
let rec rewrite trs t = 
  match t.term with
  | Var _ -> t  (* always in normal form *)
  | _ when t.normal_form -> t.binding  (* normal form already computed *)
  | _ -> (* compute normal form, store it, and return it *)
    let normal_form = compute_nf trs t in
    T.set_binding t normal_form;
    t.normal_form <- true;
    normal_form
(* compute normal form of this term *)
and compute_nf trs t =
  match t.term with
  | Var _ -> t
  | Node (hd, l) ->
    (* rewrite subterms first *)
    let l' = List.map (rewrite trs) l in
    let t' = T.mk_node hd t.sort l' in
    (* rewrite at root *)
    reduce_at_root trs t'
(* assuming subterms are in normal form, reduce the term *)
and reduce_at_root trs t =
  try
    DT.iter_match trs.index t (rewrite_with t);
    t  (* not rewritten *)
  with (RewrittenIn t') ->
    rewrite trs t' (* rewritten in t', continue *)
(* attempt to use one of the rules to rewrite t *)
and rewrite_with t l r subst =
  try
    T.reset_vars r;
    S.apply_subst_bind subst;
    let t' = T.expand_bindings r in  (* variables in r that are bound, are by subst *)
    raise (RewrittenIn t')
  with UnificationFailure -> ()


let pp_trs_index formatter trs = DT.pp_term_tree formatter trs.index
