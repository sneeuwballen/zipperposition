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

let rec lookup var subst =
  match subst with
    | [] -> var
    | ((v,t) :: tail) -> if v == var then t else lookup var tail

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

let rec apply_subst subst ?(recursive=false) t = match t.node.term with
  | Terms.Leaf _ -> t
  | Terms.Var _ -> 
      let new_t = lookup t subst in
      if recursive then apply_subst subst ~recursive new_t else new_t
  | Terms.Node l ->
      Terms.mk_node (List.map (fun t -> apply_subst subst t) l)

let build_subst v t ?(recursive=true) tail =
  if recursive
    then (
      let new_t = apply_subst ~recursive tail t in
      if Terms.member_term v new_t then raise (OccurCheck (v, new_t));
      (v, new_t) :: tail )
    else
      (v,t) :: tail 

let flat subst = List.map (fun (x,t) -> (x, apply_subst subst t)) subst

let concat x y = x @ y

