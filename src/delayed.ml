(*
zipperposition: a functional superposition prover for prototyping
copyright (c) 2012 simon cruanes

this is free software; you can redistribute it and/or
modify it under the terms of the gnu general public license
as published by the free software foundation; either version 2
of the license, or (at your option) any later version.

this is distributed in the hope that it will be useful,
but without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.  see the
gnu general public license for more details.

you should have received a copy of the gnu general public license
along with this program; if not, write to the free software
foundation, inc., 51 franklin street, fifth floor, boston, ma
02110-1301 usa.
*)

(** module for superposition with equivalence reasoning and delayed clausal form *)

open Types
open Hashcons
open Calculus

module T = Terms
module C = Clauses
module O = Orderings
module S = FoSubst
module Utils = FoUtils
module Sup = Superposition
module PS = ProofState

(* check whether s is a binding symbol *)
let is_binder_symbol s = s = lambda_symbol

(** check whether the term is (Leaf s) *)
let check_sym t s = match t.node.term with
  | Var _ -> false
  | Node _ -> false
  | Leaf s' -> s = s'

let rec pp_foterm formatter t = match t.node.term with
  | Node (({node={term=Leaf s}} as head)::args) ->
    (* general case for nodes *)
    if T.is_infix_symbol s
      then begin
        match args with
        | [l;r] -> Format.fprintf formatter "@[<h>%a %a %a@]" pp_foterm l
            pp_foterm head pp_foterm r
        | _ -> assert false (* infix and not binary? *)
      end else Format.fprintf formatter "@[<h>%a(%a)@]" pp_foterm head
        (Utils.pp_list ~sep:", " pp_foterm) args
  | Leaf s when s = eq_symbol -> Format.pp_print_string formatter "•="
  | Leaf s when s = lambda_symbol -> Format.pp_print_string formatter "•λ"
  | Leaf s when s = exists_symbol -> Format.pp_print_string formatter "•∃"
  | Leaf s when s = forall_symbol -> Format.pp_print_string formatter "•∀"
  | Leaf s when s = and_symbol -> Format.pp_print_string formatter "•&"
  | Leaf s when s = or_symbol -> Format.pp_print_string formatter "•|"
  | Leaf s when s = imply_symbol -> Format.pp_print_string formatter "•→"
  | Leaf s -> Format.pp_print_string formatter s
  | Var i -> Format.fprintf formatter "X%d" i
  | Node _ -> failwith "bad term"

let pp_clause formatter clause =
  let pp_lit formatter = function 
  | Equation (l,r,true,_) when T.eq_foterm r T.true_term ->
    pp_foterm formatter l
  | Equation (l,r,true,_) when T.eq_foterm l T.true_term ->
    pp_foterm formatter r
  | Equation (l,r,true,_)  ->
    Format.fprintf formatter "%a = %a" pp_foterm l pp_foterm r
  | Equation (l,r,false,_) when T.eq_foterm r T.true_term ->
    Format.fprintf formatter "~%a" pp_foterm l
  | Equation (l,r,false,_) when T.eq_foterm l T.true_term ->
    Format.fprintf formatter "~%a" pp_foterm r
  | Equation (l,r,false,_)  ->
    Format.fprintf formatter "%a != %a" pp_foterm l pp_foterm r
  in Utils.pp_list ~sep:" | " pp_lit formatter clause.clits


(* constraint on the ordering *)
let symbol_constraint =
  O.compose_constraints
    (O.max_constraint [succ_db_symbol; db_symbol])
    (O.min_constraint [eq_symbol; imply_symbol; forall_symbol; exists_symbol;
                       lambda_symbol; or_symbol; and_symbol;
                       not_symbol; false_symbol; true_symbol])

(* ----------------------------------------------------------------------
 * inference rules
 * ---------------------------------------------------------------------- *)

(** helper for alpha elimination (remove idx-th literal from
    clause and adds a and b to two new clauses *)
let alpha_eliminate ~ord clause idx a signa b signb =
  assert (a.node.sort = bool_sort && b.node.sort = bool_sort);
  let new_lita = C.mk_lit ~ord a T.true_term signa
  and new_litb = C.mk_lit ~ord b T.true_term signb in
  let other_lits = Utils.list_remove clause.clits idx in
  let proof = lazy (Proof ("alpha_eliminate", [clause, [idx], S.id_subst])) in
  [C.mk_clause ~ord (new_lita :: other_lits) proof;
   C.mk_clause ~ord (new_litb :: other_lits) proof]

(** helper for beta elimination (remove idx-th literal from
    clause and adds a and b *)
let beta_eliminate ~ord clause idx a signa b signb =
  assert (a.node.sort = bool_sort && b.node.sort = bool_sort);
  let new_lita = C.mk_lit ~ord a T.true_term signa
  and new_litb = C.mk_lit ~ord b T.true_term signb in
  let new_lits = new_lita :: new_litb :: (Utils.list_remove clause.clits idx) in
  let proof = lazy (Proof ("beta_eliminate", [clause, [idx], S.id_subst])) in
  C.mk_clause ~ord new_lits proof

exception FoundSort of sort

(** find the sort of the first De Bruijn term *)
let rec look_db_sort depth t = match t.node.term with
  | Node ({node={term=Leaf s}}::subterms) when is_binder_symbol s ->
    List.iter (look_db_sort (depth+1)) subterms  (* increment for binder *)
  | Node [{node={term=Leaf s}}; t] when s = succ_db_symbol ->
    look_db_sort (depth-1) t  (* decrement for lifted De Bruijn *)
  | Node l -> List.iter (look_db_sort depth) l
  | Leaf s when s = db_symbol && depth = 0 -> raise (FoundSort t.node.sort)
  | Leaf _ -> ()
  | Var _ -> ()

(** helper for gamma elimination (remove idx-th literal from clause
    and adds t where De Bruijn 0 is replaced by a fresh var) *)
let gamma_eliminate ~ord clause idx t sign =
  let maxvar = T.max_var (T.vars_of_term t) in
  assert (t.node.sort = bool_sort);
  let new_t =
    try
      look_db_sort 0 t; T.db_unlift t (* the variable is not present *)
    with FoundSort sort ->
      (* sort is the sort of the first DB symbol *)
      let new_var = T.mk_var (maxvar + 1) sort in
      T.db_unlift (T.db_replace t new_var)
  in
  let new_lit = C.mk_lit ~ord new_t T.true_term sign in
  let new_lits = new_lit :: (Utils.list_remove clause.clits idx) in
  let proof = lazy (Proof ("gamma_eliminate", [clause, [idx], S.id_subst])) in
  C.mk_clause ~ord new_lits proof

(** helper for delta elimination (remove idx-th literal from clause
    and adds t where De Bruijn 0 is replaced by a skolem
    of free variables of t) *)
let delta_eliminate ~ord clause idx t sign =
  let vars = T.vars_of_term t in
  assert (t.node.sort = bool_sort);
  let new_t =
    try
      look_db_sort 0 t; T.db_unlift t (* the DB variable is not present *)
    with FoundSort sort ->
      (* sort is the sort of the first DB symbol *)
      let new_skolem = skolem ord vars sort in
      T.db_unlift (T.db_replace t new_skolem)
  in
  let new_lit = C.mk_lit ~ord new_t T.true_term sign in
  let new_lits = new_lit :: (Utils.list_remove clause.clits idx) in
  let proof = lazy (Proof ("delta_eliminate", [clause, [idx], S.id_subst])) in
  C.mk_clause ~ord new_lits proof

(** elimination of unary/binary logic connectives *)
let connective_elimination ~ord clause =
  fold_lits ~both:false ~pos:true ~neg:true
    (fun acc l r sign l_pos ->
      (* if a literal is true_term, must be r because it is the smallest term *)
      if not (T.eq_foterm r T.true_term) then acc else
      let idx = List.hd l_pos in
      if not (C.check_maximal_lit ~ord clause idx S.id_subst) then acc else
      match l.node.term with
      | Node [{node={term=Leaf s}}; a; b] ->
        (* some alpha/beta eliminations *)
        if s = and_symbol && sign
        then (alpha_eliminate ~ord clause idx a true b true) @ acc
        else if s = and_symbol && (not sign)
        then (beta_eliminate ~ord clause idx a false b false) :: acc
        else if s = or_symbol && sign
        then (beta_eliminate ~ord clause idx a true b true) :: acc
        else if s = or_symbol && (not sign)
        then (alpha_eliminate ~ord clause idx a false b false) @ acc
        else if s = imply_symbol && sign
        then (beta_eliminate ~ord clause idx a false b true) :: acc
        else if s = imply_symbol && (not sign)
        then (alpha_eliminate ~ord clause idx a true b false) @ acc
        else acc
      | _ -> acc
    )
    [] (C.maxlits clause)

(** elimination of forall *)
let forall_elimination ~ord clause =
  fold_lits ~both:false ~pos:true ~neg:true
    (fun acc l r sign l_pos -> 
      if not (T.eq_foterm r T.true_term) then acc else
      let idx = List.hd l_pos in 
      if not (C.check_maximal_lit ~ord clause idx S.id_subst) then acc else
      match l.node.term with
      | Node [{node={term=Leaf s}};
          {node={term=Node [{node={term=Leaf s'}}; t]}}]
          when s = forall_symbol && s' = lambda_symbol ->
          (* we have a forall (lambda t) *)
          assert (t.node.sort = bool_sort);
          if sign
            then (gamma_eliminate ~ord clause idx t true) :: acc
            else (delta_eliminate ~ord clause idx t false) :: acc
      | _ -> acc
    )
    [] (C.maxlits clause)

(** elimination of exists *)
let exists_elimination ~ord clause =
  fold_lits ~both:false ~pos:true ~neg:true
    (fun acc l r sign l_pos -> 
      if not (T.eq_foterm r T.true_term) then acc else
      let idx = List.hd l_pos in 
      if not (C.check_maximal_lit ~ord clause idx S.id_subst) then acc else
      match l.node.term with
      | Node [{node={term=Leaf s}};
          {node={term=Node [{node={term=Leaf s'}}; t]}}]
          when s = exists_symbol && s' = lambda_symbol ->
          (* we have an exists (lambda t) *)
          assert (t.node.sort = bool_sort);
          if sign
            then (delta_eliminate ~ord clause idx t true) :: acc
            else (gamma_eliminate ~ord clause idx t false) :: acc
      | _ -> acc
    )
    [] (C.maxlits clause)

(** equivalence elimination *)
let equivalence_elimination ~ord clause =
  (* do the inference for positive equations *)
  let do_inferences_pos l r l_pos =
    if T.atomic l then [] else begin
    assert (r.node.sort = bool_sort);
    if ord#compare l r = Lt then [] else
    (* ok, do it *)
    match l_pos with
    | [idx; _] ->
      if not (C.check_maximal_lit ~ord clause idx S.id_subst) then [] else
      let new_lits = Utils.list_remove clause.clits idx in
      let new_lits1 = (C.mk_neq ~ord l T.true_term) ::
                      (C.mk_eq ~ord r T.true_term) :: new_lits
      and proof1 = lazy (Proof ("pos_equiv_elim1", [clause, l_pos, S.id_subst]))
      and new_lits2 = (C.mk_eq ~ord l T.true_term) ::
                      (C.mk_neq ~ord r T.true_term) :: new_lits
      and proof2 = lazy (Proof ("pos_equiv_elim2", [clause, l_pos, S.id_subst]))
      in
      [C.mk_clause ~ord new_lits1 proof1; C.mk_clause ~ord new_lits2 proof2]
    | _ -> assert false
    end
  (* do the inference for negative equations *)
  and do_inferences_neg l r l_pos =
    if not (l.node.sort = bool_sort) then [] else begin
    assert (r.node.sort = bool_sort);
    match l_pos with
    | [idx; _] ->
      if not (C.check_maximal_lit ~ord clause idx S.id_subst) then [] else
      let new_lits = Utils.list_remove clause.clits idx in
      let new_lits1 = (C.mk_eq ~ord l T.true_term) ::
                      (C.mk_eq ~ord r T.true_term) :: new_lits
      and proof1 = lazy (Proof ("neg_equiv_elim1", [clause, l_pos, S.id_subst]))
      and new_lits2 = (C.mk_neq ~ord l T.true_term) ::
                      (C.mk_neq ~ord r T.true_term) :: new_lits
      and proof2 = lazy (Proof ("neg_equiv_elim2", [clause, l_pos, S.id_subst]))
      in
      [C.mk_clause ~ord new_lits1 proof1; C.mk_clause ~ord new_lits2 proof2]
    | _ -> assert false
    end
  in
  fold_lits ~both:true ~pos:true ~neg:true
    (fun acc l r sign l_pos -> 
      if sign
        then (do_inferences_pos l r l_pos) @ acc
        else (do_inferences_neg l r l_pos) @ acc)
    [] (C.maxlits clause)

(* ----------------------------------------------------------------------
 * simplification
 * ---------------------------------------------------------------------- *)

(** Simplify the inner formula (double negation, trivial equalities...) *)
let simplify_inner ~ord c =
  let not_term = T.mk_leaf not_symbol bool_sort in
  (* simplify a term *)
  let rec simp_term t = match t.node.term with
  | Var _ | Leaf _ -> t
  | Node [{node={term=Leaf s}}; {node={term=Node [{node={term=Leaf s'}}; t']}}]
    when s = not_symbol && s' = not_symbol -> simp_term t'  (* double negation *)
  | Node [{node={term=Leaf s}}; t'] when s = not_symbol && T.eq_foterm t' T.true_term ->
    T.false_term  (* not true -> false *)
  | Node [{node={term=Leaf s}}; t'] when s = not_symbol && T.eq_foterm t' T.false_term ->
    T.true_term  (* not false -> true *)
  | Node [{node={term=Leaf s}}; {node={term=Node [{node={term=Leaf s'}}; t']}}]
    when (s = forall_symbol || s = exists_symbol) && s' = lambda_symbol
    && not (T.db_contains t' 0) -> simp_term (T.db_unlift t') (* eta-reduction *)
  | Node [{node={term=Node [{node={term=Leaf s}}; t']}}; v] when s = lambda_symbol ->
    let new_t' = T.db_unlift (T.db_replace t' v) in simp_term new_t' (* beta-reduction *)
  | Node [{node={term=Leaf s}}; a; b] when s = and_symbol &&
    (T.eq_foterm a T.false_term || T.eq_foterm b T.false_term) ->
    T.false_term  (* a and false -> false *)
  | Node [{node={term=Leaf s}}; a; b] when s = or_symbol &&
    (T.eq_foterm a T.true_term || T.eq_foterm b T.true_term) ->
    T.true_term  (* a or true -> true *)
  | Node [{node={term=Leaf s}}; a; b] when s = or_symbol && T.eq_foterm a T.false_term ->
    simp_term b (* b or false -> b *)
  | Node [{node={term=Leaf s}}; a; b] when s = or_symbol && T.eq_foterm b T.false_term ->
    simp_term a (* a or false -> a *)
  | Node [{node={term=Leaf s}}; a; b] when s = and_symbol && T.eq_foterm a T.true_term ->
    simp_term b (* b and true -> b *)
  | Node [{node={term=Leaf s}}; a; b] when s = and_symbol && T.eq_foterm b T.true_term ->
    simp_term a (* a and true -> a *)
  | Node [{node={term=Leaf s}}; a; b] when s = imply_symbol &&
    (T.eq_foterm a T.false_term || T.eq_foterm b T.true_term) ->
    T.true_term  (* (false => a) or (a => true) -> true *)
  | Node [{node={term=Leaf s}}; a; b] when s = imply_symbol && T.eq_foterm a T.true_term ->
    simp_term b  (* (true => a) -> a *)
  | Node [{node={term=Leaf s}}; a; b] when s = eq_symbol && T.eq_foterm a b ->
    T.true_term  (* a = a -> true *)
  | Node [{node={term=Leaf s}}; a; b] when s = eq_symbol && 
    ((T.eq_foterm a T.true_term && T.eq_foterm b T.false_term) ||
     (T.eq_foterm b T.true_term && T.eq_foterm a T.false_term)) ->
    T.false_term  (* true = false -> false *)
  | Node [{node={term=Leaf s}}; a; b] when s = eq_symbol && T.eq_foterm b T.true_term ->
    simp_term a  (* a = true -> a *)
  | Node [{node={term=Leaf s}}; a; b] when s = eq_symbol && T.eq_foterm a T.true_term ->
    simp_term b  (* b = true -> b *)
  | Node [{node={term=Leaf s}}; a; b] when s = eq_symbol && T.eq_foterm b T.false_term ->
    simp_term (T.mk_node [not_term; a])  (* a = false -> not a *)
  | Node [{node={term=Leaf s}}; a; b] when s = eq_symbol && T.eq_foterm a T.false_term ->
    simp_term (T.mk_node [not_term; b])  (* b = false -> not b *)
  | Node l ->
    let new_t = T.mk_node (List.map simp_term l) in
    if T.eq_foterm t new_t then t else simp_term new_t
  (* simplify a lit *)
  and simp_lit ((Equation (l,r,sign,_)) as lit) =
    let new_l = simp_term l
    and new_r = simp_term r in
    if T.eq_foterm l new_l && T.eq_foterm r new_r then lit
    else C.mk_lit ~ord new_l new_r sign
  in
  C.mk_clause ~ord (List.map simp_lit c.clits) c.cproof

(* ----------------------------------------------------------------------
 * the calculus object
 * ---------------------------------------------------------------------- *)

let delayed : calculus =
  object
    method binary_rules = ["superposition_active", Sup.infer_active;
                           "superposition_passive", Sup.infer_passive]

    method unary_rules = ["equality_resolution", Sup.infer_equality_resolution;
                          "equality_factoring", Sup.infer_equality_factoring;
                          "connective_elimination", connective_elimination;
                          "forall_elimination", forall_elimination;
                          "exists_elimination", exists_elimination;
                          "equivalence_elimination", equivalence_elimination]

    method basic_simplify ~ord c = Sup.basic_simplify ~ord (simplify_inner ~ord c)

    method simplify actives c = Sup.demodulate actives [] c

    method redundant actives c = Sup.subsumed_by_set actives c

    method redundant_set actives c = Sup.subsumed_in_set actives c

    method trivial c = Sup.is_tautology c

    method axioms = []

    method constr = symbol_constraint

    method preprocess ~ord l =
      List.map (fun c -> C.reord_clause ~ord (C.clause_of_fof ~ord c)) l
  end
