(*
    ||M||  This file is part of HELM, an Hypertextual, Electronic
    ||A||  Library of Mathematics, developed at the Computer Science
    ||T||  Department, University of Bologna, Italy.
    ||I||
    ||T||  HELM is free software; you can redistribute it and/or
    ||A||  modify it under the terms of the GNU General Public License
    \   /  version 2 or (at your option) any later version.
     \ /   This software is distributed as is, NO WARRANTY.
      V_______________________________________________________________ *)

open Hashcons
open Terms

exception OccurCheck of (Terms.foterm * Terms.foterm)

let id_subst = []

let rec lookup var subst = match subst with
  | [] -> var
  | ((v,t) :: tail) ->
      if Terms.eq_foterm v var then t else lookup var tail

let is_in_subst var subst = lookup var subst != var

(* filter out from metasenv the variables in substs *)
let filter subst varlist =
  List.filter
    (fun var ->
       not (is_in_subst var subst))
    varlist

let rec reloc_subst subst t = match t.node.term with
  | Terms.Leaf _ -> t
  | Terms.Var _ ->
      let new_t = lookup t subst in
      assert (t != new_t);
      new_t
  | (Terms.Node l) ->
      Terms.mk_node (List.map (fun t -> reloc_subst subst t) l)

let rec apply_subst subst ?(recursive=true) t = match t.node.term with
  | Terms.Leaf _ -> t
  | Terms.Var _ ->
      let new_t = lookup t subst in
      if recursive then apply_subst subst ~recursive new_t else new_t
  | Terms.Node l ->
      Terms.mk_node (List.map (fun t -> apply_subst subst ~recursive t) l)

(* add v -> t to the substitution. If recursive is true,
 * then v -> subst(t) is considered instead.
 * If v occurs in t, OccurCheck (v,t) is raised.
 *)
let build_subst v t ?(recursive=false) tail =
  if recursive
    then (
      let new_t = apply_subst ~recursive tail t in
      (* v -> v, no need to add to subst *)
      if Terms.eq_foterm v new_t then tail
      (* v -> t[v], not well-formed substitution *)
      else if Terms.member_term v new_t then raise (OccurCheck (v, new_t))
      (* append to list *)
      else (v, new_t) :: tail )
    else if Terms.eq_foterm v t
      then tail
      else (v,t) :: tail

(* normalize the substitution, such that subst(subst(v)) = subst(v)
 * for all v. The result is idempotent. *)
let flat subst = List.map (fun (x, t) -> (x, apply_subst subst t)) subst

let concat x y = x @ y

