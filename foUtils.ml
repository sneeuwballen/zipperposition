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

module T = Terms
open Hashcons

(* ----------------------------------------------------------------------
 * terms
 * ---------------------------------------------------------------------- *)

(* hashconsing! *)
let eq_foterm x y = x == y

(* lexicographic comparison *)
let rec compare_foterm x y =
  match x.node.T.term, y.node.T.term with
  | T.Leaf t1, T.Leaf t2 -> Signature.compare t1 t2
  | T.Var i, T.Var j -> i - j
  | T.Node l1, T.Node l2 -> lexicograph compare_foterm l1 l2
  | T.Leaf _, ( T.Node _ | T.Var _ ) -> ~-1
  | T.Node _, T.Leaf _ -> 1
  | T.Node _, T.Var _ -> ~-1
  | T.Var _, _ ->  1

(* fresh term, which variables are all > maxvar *)
let fresh_foterm maxvar t =
  let _, subst = List.fold_left
    (fun (offset, subst) ({node={T.sort=sort}} as var) ->
      let new_var = T.mk_var offset sort in
      (offset+1, FoSubst.build_subst var new_var ~recursive:false subst))
    (maxvar+1, FoSubst.id_subst) (T.vars_of_term t)
  in
  FoSubst.apply_subst ~recursive:false subst t


(* ----------------------------------------------------------------------
 * literals
 * ---------------------------------------------------------------------- *)

(* equaliy of literals *)
let eq_literal l1 l2 =
  match l1, l2 with
  | T.Equation (l1,r1,sign1,o1), T.Equation (l2,r2,sign2,o2) ->
      o1 = o2 && eq_foterm l1 l2 && eq_foterm r1 r2 && sign1 = sign2

(* lexicographic comparison of literals *)
let compare_literal l1 l2 =
  match l1, l2 with
  | T.Equation (l1,r1,sign1,o1), T.Equation (l2,r2,sign2,o2) ->
      let c = Pervasives.compare o1 o2 in
      if c <> 0 then c else
        let c = compare_foterm l1 l2 in
        if c <> 0 then c else
          let c = compare_foterm r1 r2 in
          if c <> 0 then c else
            Pervasives.compare sign1 sign2

let check_type a b = if a.node.T.sort <> b.node.T.sort
  then raise (T.SortError "sides of equations of different sorts") else ()

let default_compare = Orderings.Default.compare_terms

let mk_eq ?(comp=default_compare) a b =
  check_type a b;
  T.Equation (a, b, true, comp a b)

let mk_neq ?(comp=default_compare) a b = 
  check_type a b;
  T.Equation (a, b, false, comp a b)

(* negate literal *)
let negate_lit (T.Equation (l,r,sign,ord)) = T.Equation (l,r,not sign,ord)

(* fmap in literal *)
let fmap_lit ?(comp=default_compare) f = function
  | T.Equation (left, right, sign, ord) ->
    let new_left = f left
    and new_right = f right in
    T.Equation (new_left, new_right, sign, comp new_left new_right)


(* ----------------------------------------------------------------------
 * clauses
 * ---------------------------------------------------------------------- *)


let eq_clause {T.cid=id1} {T.cid=id2} = id1 = id2
let compare_clause {T.cid=id1} {T.cid=id2} = Pervasives.compare id1 id2

(* build a clause with a new ID *)
let mk_clause =
  let clause_id = ref 0 in
  fun lits proof ->
    let rec merge_vars vars = function
      | [] -> vars
      | (x::xs) -> if List.mem x vars
        then merge_vars vars xs else merge_vars (x::vars) xs
    and vars_of_lits (T.Equation (l, r, _, _)) =
      merge_vars (T.vars_of_term l) (T.vars_of_term r) in
    let all_vars =
      List.fold_left merge_vars [] (List.map vars_of_lits lits)
    and id = (let i = !clause_id in clause_id := i+1; i) in
    {T.cid=id; T.clits=lits; T.cvars=all_vars; T.cproof=proof}

(* find the maximum variable index in the varlist *)
let max_var vars =
  let rec aux idx = function
  | [] -> idx
  | ({node={T.term=T.Var i}}::vars) -> aux (max i idx) vars
  | _::vars -> assert false
  in
  aux 0 vars
  
(* perform renaming to get disjoint variables sets
   relocate [maxvar] [varlist] [subst] -> [newmaxvar] * [varlist] * [relocsubst] *)
let relocate maxvar varlist subst =
  List.fold_right
    (fun ({node={T.term=T.Var _; T.sort=sort}} as v) (maxvar, varlist, s) -> 
       let new_v = T.mk_var maxvar sort in
       maxvar+1, new_v::varlist, Subst.build_subst v new_v s)
    varlist (maxvar+1, [], subst)

let fresh_clause maxvar c =
  (* prerr_endline 
    ("varlist = " ^ (String.concat "," (List.map string_of_int varlist)));*)
  let maxvar, varlist, subst = relocate maxvar c.T.cvars Subst.id_subst in
  let lits = List.map
    (function
      | T.Equation (l,r,sign,o) ->
        let l = Subst.reloc_subst subst l in
        let r = Subst.reloc_subst subst r in
        T.Equation (l,r,sign,o))
    c.T.clits
  in
  (*  TODO modify proof?
  let proof =
    match proof with
    | T.Exact t -> T.Exact (Subst.reloc_subst subst t)
    | T.Step (rule,c1,c2,dir,pos,s) ->
        T.Step(rule,c1,c2,dir,pos,Subst.concat subst s)
  in
  *)
  {c with T.clits=lits; T.cvars=varlist}, maxvar

(* rename clauses and terms so that they have no variable in varlist *)
let relocate_term varlist t =
  let idx = max_var varlist in
  let _, _, subst = relocate idx varlist FoSubst.id_subst in
  FoSubst.apply_subst subst t

let relocate_clause varlist c =
  let idx = max_var c.T.cvars in
  let _, newvars, subst = relocate idx c.T.cvars FoSubst.id_subst in
  let new_lits = List.map
    (fun lit -> fmap_lit (FoSubst.apply_subst subst) lit)
    c.T.clits
  in
  {c with T.clits=new_lits; T.cvars=newvars}
  (* TODO update proof *)


(*
(* may be moved inside the bag *)
let mk_unit_clause maxvar ty proofterm =
  let varlist =
    let rec aux acc = function
      | T.Leaf _ -> acc
      | T.Var i -> if List.mem i acc then acc else i::acc
      | T.Node l -> List.fold_left aux acc l 
    in
     aux (aux [] ty) proofterm
  in
  let lit = 
    match B.is_eq ty with
    | Some(ty,l,r) ->
         let o = Order.compare_terms l r in
         T.Equation (l, r, ty, o)
    | None -> T.Predicate ty
  in
  let proof = T.Exact proofterm in
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
