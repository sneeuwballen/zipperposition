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

(* $Id: terms.ml 9836 2009-06-05 15:33:35Z denes $ *)

(* lexicographic order on lists l1,l2 which elements are ordered by f *)
let rec lexicograph f l1 l2 =
  match l1, l2 with
  | [], [] -> 0
  | x::xs, y::ys ->
     let c = f x y in
     if c <> 0 then c else lexicograph f xs ys
  | [],_ -> ~-1
  | _,[] -> 1

module Subst = FoSubst
module Order = Orderings

open Terms
open Hashcons

(* ----------------------------------------------------------------------
 * terms
 * ---------------------------------------------------------------------- *)

(* hashconsing! *)
let eq_foterm x y = x == y

(* lexicographic comparison *)
let rec compare_foterm x y =
  match x.node.term, y.node.term with
  | Terms.Leaf t1, Terms.Leaf t2 -> Signature.compare t1 t2
  | Terms.Var i, Terms.Var j -> i - j
  | Terms.Node l1, Terms.Node l2 -> lexicograph compare_foterm l1 l2
  | Terms.Leaf _, ( Terms.Node _ | Terms.Var _ ) -> ~-1
  | Terms.Node _, Terms.Leaf _ -> 1
  | Terms.Node _, Terms.Var _ -> ~-1
  | Terms.Var _, _ ->  1

(* ----------------------------------------------------------------------
 * literals
 * ---------------------------------------------------------------------- *)

(* equaliy of literals *)
let eq_literal l1 l2 =
  match l1, l2 with
  | Terms.Equation (l1,r1,sign1,o1), Terms.Equation (l2,r2,sign2,o2) ->
      o1 = o2 && eq_foterm l1 l2 && eq_foterm r1 r2 && sign1 = sign2

(* lexicographic comparison of literals *)
let compare_literal l1 l2 =
  match l1, l2 with
  | Terms.Equation (l1,r1,sign1,o1), Terms.Equation (l2,r2,sign2,o2) ->
      let c = Pervasives.compare o1 o2 in
      if c <> 0 then c else
        let c = compare_foterm l1 l2 in
        if c <> 0 then c else
          let c = compare_foterm r1 r2 in
          if c <> 0 then c else
            Pervasives.compare sign1 sign2

let check_type a b = if a.node.sort <> b.node.sort
  then raise (SortError "sides of equations of different sorts") else ()

let default_compare = Orderings.Default.compare_terms

let mk_eq ?(comp=default_compare) a b =
  check_type a b;
  Equation (a, b, true, comp a b)

let mk_neq ?(comp=default_compare) a b = 
  check_type a b;
  Equation (a, b, false, comp a b)

(* negate literal *)
let negate_lit (Equation (l,r,sign,ord)) = Equation (l,r,not sign,ord)

(* ----------------------------------------------------------------------
 * clauses
 * ---------------------------------------------------------------------- *)


let eq_clause (id1,_,_,_) (id2,_,_,_) = id1 = id2
let compare_clause (id1,_,_,_) (id2,_,_,_) = Pervasives.compare id1 id2

(* build a clause with a new ID *)
let mk_clause =
  let clause_id = ref 0 in
  fun lits proof ->
    let rec merge_vars vars = function
      | [] -> vars
      | (x::xs) -> if List.mem x vars
        then merge_vars vars xs else merge_vars (x::vars) xs
    and vars_of_lits (Equation (l, r, _, _)) =
      merge_vars (vars_of_term l) (vars_of_term r) in
    let all_vars =
      List.fold_left merge_vars [] (List.map vars_of_lits lits)
    and id = (let i = !clause_id in clause_id := i+1; i) in
    (id, lits, all_vars, proof)
  
(* perform renaming to get disjoint variables sets
   relocate [maxvar] [varlist] [subst] -> [newmaxvar] * [varlist] * [relocsubst] *)
let relocate maxvar varlist subst =
  List.fold_right
    (fun ({node={term=Var _; sort=sort}} as v) (maxvar, varlist, s) -> 
       let new_v = Terms.mk_var maxvar sort in
       maxvar+1, new_v::varlist, Subst.build_subst v new_v s)
    varlist (maxvar+1, [], subst)


let fresh_clause maxvar (id, lits, varlist, proof) =
  (* prerr_endline 
    ("varlist = " ^ (String.concat "," (List.map string_of_int varlist)));*)
  let maxvar, varlist, subst = relocate maxvar varlist Subst.id_subst in
  let lits = List.map
    (function
      | Terms.Equation (l,r,sign,o) ->
        let l = Subst.reloc_subst subst l in
        let r = Subst.reloc_subst subst r in
        Terms.Equation (l,r,sign,o))
    lits
  in
  (*  TODO modify proof?
  let proof =
    match proof with
    | Terms.Exact t -> Terms.Exact (Subst.reloc_subst subst t)
    | Terms.Step (rule,c1,c2,dir,pos,s) ->
        Terms.Step(rule,c1,c2,dir,pos,Subst.concat subst s)
  in
  *)
  (id, lits, varlist, proof), maxvar


(*
(* may be moved inside the bag *)
let mk_unit_clause maxvar ty proofterm =
  let varlist =
    let rec aux acc = function
      | Terms.Leaf _ -> acc
      | Terms.Var i -> if List.mem i acc then acc else i::acc
      | Terms.Node l -> List.fold_left aux acc l 
    in
     aux (aux [] ty) proofterm
  in
  let lit = 
    match B.is_eq ty with
    | Some(ty,l,r) ->
         let o = Order.compare_terms l r in
         Terms.Equation (l, r, ty, o)
    | None -> Terms.Predicate ty
  in
  let proof = Terms.Exact proofterm in
  fresh_unit_clause maxvar (0, lit, varlist, proof)


let mk_passive_clause cl =
  (Order.compute_unit_clause_weight cl, cl)


let mk_passive_goal g =
  (Order.compute_unit_clause_weight g, g)
*)

(*
let compare_passive_clauses_weight (w1,(id1,_,_,_)) (w2,(id2,_,_,_)) =
  if w1 = w2 then id1 - id2
  else w1 - w2

let compare_passive_clauses_age (_,(id1,_,_,_)) (_,(id2,_,_,_)) =
  id1 - id2
*)
