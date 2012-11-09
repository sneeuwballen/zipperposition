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

(* TODO renaming... (use negative indices?)
   TODO optimize (looks like path_string_of is bottleneck)
   TODO use a better indexing structure? *)

open Types

module T = Terms
module S = FoSubst
module Unif = FoUnif
module Utils = FoUtils

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
      begin
        (* try to follow the branch of the trie that corresponds to the query symbol *)
        (try
          let next_node = Discrimination_tree.PSMap.find node map in
          retrieve path' next_node
        with Not_found -> ());
        (* try to follow a 'variable' branch of the trie *)
        (try
          let next_node = Discrimination_tree.PSMap.find Discrimination_tree.Variable map in
          let arity = Discrimination_tree.arity_of node in
          match next_node, Discrimination_tree.skip arity path' with
          | DT.Node (Some rules, _), [] ->
            List.iter k rules
          | node', path' -> retrieve path' node'
        with Not_found -> ())
      end
  in
  retrieve path tree


(* ----------------------------------------------------------------------
 * Definition of TRS
 * ---------------------------------------------------------------------- *)

(** Term Rewriting System *)
type trs = {
  mutable index : rw_index;         (** index of rules *)
  mutable nbr_rules : int;          (** number of rules *)
}

let create () = {
  index = DT.empty;
  nbr_rules = 0;
}

let var_offset = 10000              (** negative offset for variables in rules *)

let add_rule trs (l, r) =
  (* check that the rule does not introduce variables *)
  assert (List.for_all
    (fun v -> T.member_term v l)
    l.vars);
  assert (not (T.is_var l));
  assert (l.sort = r.sort);
  (* use low, negative variables *)
  let vars = l.vars in
  List.iter (fun v ->
    match v.term with
    | Var i -> T.set_binding v (T.mk_var (-(i+var_offset)) v.sort)
    | _ -> assert false) vars;
  let rule = T.expand_bindings l, T.expand_bindings r in
  (* add rule to the discrimination tree *)
  (* now add the rule to the index *)
  trs.index <- add_rule trs.index rule;
  trs.nbr_rules <- trs.nbr_rules + 1

let add_rules trs l = List.iter (add_rule trs) l

let from_list l =
  let trs = create () in
  add_rules trs l;
  trs

let size trs = trs.nbr_rules

let iter trs f = iter_rules trs.index f

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
  | Leaf _ -> reduce_at_root trs t
  | Node l ->
    (* rewrite subterms first *)
    let l' = List.map (rewrite trs) l in
    let t' = T.mk_node l' in
    (* rewrite at root *)
    reduce_at_root trs t'
(* assuming subterms are in normal form, reduce the term *)
and reduce_at_root trs t =
  try
    retrieve_rules trs.index t (rewrite_with t);
    t  (* not rewritten *)
  with (RewrittenIn t') ->
    rewrite trs t' (* rewritten in t', continue *)
(* attempt to use one of the rules to rewrite t *)
and rewrite_with t (l, r) =
  try
    T.reset_vars r;
    let _ = Unif.matching S.id_subst l t in
    let t' = T.expand_bindings r in  (* variables in r that are bound, are by subst *)
    raise (RewrittenIn t')
  with UnificationFailure -> ()

(* ----------------------------------------------------------------------
 * debug
 * ---------------------------------------------------------------------- *)

module PrintTree = Prtree.Make(
  struct
    type t = string * rule list DT.t

    (* get a list of (key, sub-node) *)
    let get_values map =
      let l : t list ref = ref [] in
      Discrimination_tree.PSMap.iter
        (fun key node -> 
          let key_repr = match key with
            | Discrimination_tree.Variable -> "[*]"
            | Discrimination_tree.Constant (s, _) -> "[" ^ s ^ "]"
            | _ -> "?" in
          l := (key_repr, node) :: !l) map;
      !l

    (* recurse in subterms *)
    let decomp (prefix, t) = match t with
      | DT.Node (None, map) -> prefix, get_values map
      | DT.Node (Some rules, map) ->
        let rules_repr = Utils.sprintf "%s {@[<h>%a@]}"
          prefix (Utils.pp_list ~sep:"; " pp_rule) rules in
        rules_repr, get_values map
  end)

let pp_trs_index formatter trs = PrintTree.print formatter ("", trs.index)
