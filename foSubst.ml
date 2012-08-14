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
open Types

module T = Terms
module Utils = FoUtils

exception OccurCheck of (foterm * foterm)

let id_subst = []

let rec lookup var subst = match subst with
  | [] -> var
  | ((v,t) :: tail) ->
      if T.eq_foterm v var then t else lookup var tail

let is_in_subst var subst = lookup var subst != var

let filter subst varlist =
  List.filter
    (fun var ->
       not (is_in_subst var subst))
    varlist

let rec apply_subst ?(recursive=true) subst t = match t.node.term with
  | Leaf _ -> t
  | Var _ ->
      let new_t = lookup t subst in
      assert (new_t.node.sort = t.node.sort);
      if recursive && not (T.eq_foterm new_t t)
        then apply_subst subst ~recursive new_t
        else new_t
  | Node l ->
      T.mk_node (List.map (apply_subst subst ~recursive) l)

let build_subst ?(recursive=false) v t tail =
  assert (v.node.sort = t.node.sort);
  if recursive
    then (
      let new_t = apply_subst ~recursive tail t in
      (* v -> v, no need to add to subst *)
      if T.eq_foterm v new_t then tail
      (* v -> t[v], not well-formed substitution *)
      else if T.member_term v new_t then raise (OccurCheck (v, new_t))
      (* append to list *)
      else (v, new_t) :: tail )
    else if T.eq_foterm v t
      then tail
      else (v,t) :: tail

let flat subst = List.map (fun (x, t) -> (x, apply_subst subst t)) subst

let concat x y = x @ y

let relocate ?(recursive=true) maxvar varlist subst =
  List.fold_right
    (fun ({node={sort=sort}} as v) (maxvar, varlist, s) -> 
       let new_v = T.mk_var maxvar sort in
       maxvar+1, new_v::varlist, build_subst ~recursive v new_v s)
    varlist (maxvar+1, [], subst)

let fresh_foterm maxvar t =
  let _, subst = List.fold_left
    (fun (offset, subst) ({node={sort=sort}} as var) ->
      let new_var = T.mk_var offset sort in
      (offset+1, build_subst var new_var ~recursive:false subst))
    (maxvar+1, id_subst) (T.vars_of_term t)
  in
  apply_subst ~recursive:false subst t

let relocate_term varlist t =
  let idx = T.max_var varlist in
  let _, _, subst = relocate idx varlist id_subst in
  apply_subst ~recursive:false subst t

let pp_substitution formatter ?(sort=false) subst =
  let pp_pair formatter (v, t) =
    Format.fprintf formatter "%a -> %a" (T.pp_foterm ~sort) v (T.pp_foterm ~sort) t
  in
  Format.fprintf formatter "@[<h>{%a}@]" (Utils.pp_list ~sep:", " pp_pair) subst
