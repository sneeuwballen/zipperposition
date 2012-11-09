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
module Unif = FoUnif

(* ----------------------------------------------------------------------
 * Dedicated indexing structure (discrimination tree)
 * ---------------------------------------------------------------------- *)

type rule = (term * term)

module DT = Discrimination_tree.DiscriminationTree

(** associate a list of rules to a path *)
type rw_index = rule list DT.t

(** add rule to discrimination tree *)
let add_rule tree (l,r) =
  let path = Discrimination_tree.path_string_of l in
  let rule_list =
    try DT.find path tree
    with Not_found -> [] in
  let rule_list = (l,r) :: rule_list in
  DT.add path rule_list tree

(** iter on all rules *)
let iter_rules tree k =
  DT.iter (fun _ rules -> List.iter k rules) tree

(** iter on nodes that match the input term *)
let retrieve_rules tree t k =
  let path = Discrimination_tree.path_string_of t in
  (* recursive traversal of trie *)
  let rec retrieve path tree =
    match tree, path with
    | DT.Node (Some rules, _), [] ->
      List.iter k rules  (* call k on rules *)
    | DT.Node (None, _), [] -> ()
    | DT.Node (_, map), node::path' ->
        (* try to follow the branch of the trie that corresponds to the query symbol *)
        (try
          let next_node = Discrimination_tree.PSMap.find node map in
          retrieve path' next_node
        with Not_found -> ());
        (* try to follow a 'variable' branch of the trie *)
        (try
          let next_node = Discrimination_tree.PSMap.find Discrimination_tree.Variable map in
          let arity = Discrimination_tree.arity_of node in
          match next_node, Discrimination_tree.skip arity path with
          | DT.Node (Some rules, _), [] ->
            List.iter k rules
          | node', path -> retrieve path node'
        with Not_found -> ())
  in
  retrieve path tree

(* ----------------------------------------------------------------------
 * Definition of TRS
 * ---------------------------------------------------------------------- *)

(** Term Rewriting System *)
type trs = {
  mutable index : rw_index;         (** index of rules *)
  ground_index : term T.THashtbl.t; (** rules that are ground term -> term *)
  mutable nbr_rules : int;          (** number of rules *)
}

let create () = {
  index = DT.empty;
  ground_index = T.THashtbl.create 13;
  nbr_rules = 0;
}

let add_rule trs ((l, r) as rule) =
  (* check that the rule does not introduce variables *)
  assert (List.for_all
    (fun v -> T.member_term v l)
    l.vars);
  assert (not (T.is_var l));
  assert (l.sort = r.sort);
  (* now add the rule to the index (depending on whether it is ground or not) *)
  (if T.is_ground_term l
    then T.THashtbl.replace trs.ground_index l r
    else trs.index <- add_rule trs.index rule);
  trs.nbr_rules <- trs.nbr_rules + 1

let add_rules trs l = List.iter (add_rule trs) l

let size trs = trs.nbr_rules

let iter trs f =
  iter_rules trs.index f;
  T.THashtbl.iter (fun l r -> f (l,r)) trs.ground_index

(* ----------------------------------------------------------------------
 * Computation of normal forms
 * ---------------------------------------------------------------------- *)

exception RewrittenIn of term

(** Compute normal form of the term, and set its binding to the normal form *)
let rec rewrite trs t = 
  (* compute normal form of this term *)
  let rec reduce t =
    match t.term with
    | Var _ -> t
    | Leaf _ ->
      (try
        let t' = T.THashtbl.find trs.ground_index t in
        reduce t'
      with Not_found -> t)  (* irreducible *)
    | Node l ->
      let l' = List.map (rewrite trs) l in
      let t' = T.mk_node l' in
      if T.is_ground_term t'
        then reduce_ground t'
        else reduce_at_root t'
  (* assuming subterms are in normal form and t is groudn, reduce t *)
  and reduce_ground t =
    match t.term with
    | Node l when T.is_ground_term t ->
      let t' = try T.THashtbl.find trs.ground_index t
               with Not_found -> t in
      if T.eq_term t t'
        then reduce_at_root t (* try with discrimination tree *)
        else rewrite trs t    (* one step succeeded *)
    | _ -> assert false
  (* assuming subterms are in normal form, reduce the term *)
  and reduce_at_root t =
    match t.term with
    | Node _ ->
      assert (not (T.is_ground_term t));
      (try
        retrieve_rules trs.index t (rewrite_with t);
        t  (* not rewritten *)
      with (RewrittenIn t') ->
        reduce t') (* rewritten in t' *)
    | Leaf _ | Var _ -> assert false
  (* attempt to use one of the rules to rewrite t *)
  and rewrite_with t (l, r) =
    try
      T.reset_vars r;
      let _ = Unif.matching S.id_subst l t in
      let t' = T.expand_bindings r in  (* variables in r that are bound, are by subst *)
      raise (RewrittenIn t')
    with UnificationFailure -> ()
  in
  (* now find normal form of t *)
  match t.term with
  | Var _ -> t  (* always in normal form *)
  | _ when t.normal_form -> t.binding  (* normal form already computed *)
  | _ -> (* compute normal form, store it, and return it *)
    let normal_form = reduce t in
    T.set_binding t normal_form;
    t.normal_form <- true;
    normal_form
