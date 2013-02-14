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
    (T.vars r));
  assert (not (T.is_var l));
  assert (l.sort = r.sort);
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
let rewrite trs t = 
  (* compute normal form of this term *)
  let rec compute_nf offset trs t =
    match t.term with
    | Bind (s, t') ->
      let t'' = compute_nf offset trs t' in
      let new_t = T.mk_bind s t.sort t'' in
      reduce_at_root offset trs new_t
    | Node (hd, l) ->
      (* rewrite subterms first *)
      let l' = List.map (compute_nf offset trs) l in
      let t' = T.mk_node hd t.sort l' in
      (* rewrite at root *)
      reduce_at_root offset trs t'
    | Var _ | BoundVar _ -> assert false
  (* assuming subterms are in normal form, reduce the term *)
  and reduce_at_root offset trs t =
    try
      DT.iter_match (trs.index,offset) (t,0) rewrite_handler;
      t  (* normal form *)
    with (RewrittenIn t') ->
      compute_nf offset trs t' (* rewritten in t', continue *)
  (* attempt to use one of the rules to rewrite t *)
  and rewrite_handler (l,o) r subst =
    let t' = S.apply_subst subst (r,o) in (* all vars in [r] are bound in [subst] *)
    raise (RewrittenIn t')
  in
  (* any offset will do, as long as it's <> 0, because no variable of the TRS
     should remain free during instantiation (vars(r) \subset vars(l) for all rules) *)
  compute_nf 1 trs t

let pp_trs_index formatter trs = DT.pp_term_tree formatter trs.index
