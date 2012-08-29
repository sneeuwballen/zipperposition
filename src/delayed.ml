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

(* replace 0 by s in t *)
let db_replace t s =
  (* lift the De Bruijn symbol *)
  let mk_succ db = T.mk_node [T.mk_leaf succ_db_symbol univ_sort; db] in
  (* replace db by s in t *)
  let rec replace db s t = match t.node.term with
  | _ when T.eq_foterm t db -> s
  | Leaf _ | Var _ -> t
  | Node (({node={term=Leaf symb}} as hd)::tl) when is_binder_symbol symb ->
    (* lift the De Bruijn to replace *)
    T.mk_node (hd :: (List.map (replace (mk_succ db) s) tl))
  | Node ({node={term=Leaf s}}::_) when s = succ_db_symbol || s = db_symbol ->
    t (* no the good De Bruijn symbol *)
  | Node l -> T.mk_node (List.map (replace db s) l)
  (* replace the 0 De Bruijn index by s in t *)
  in
  replace (T.mk_leaf db_symbol univ_sort) s t

(* replace v by a De Bruijn symbol in t *)
let db_make t v =
  assert (T.is_var v);
  (* go recursively and replace *)
  let rec replace_and_lift depth t = match t.node.term with
  | Var _ -> if T.eq_foterm t v then mk_db depth else t
  | Leaf _ -> t
  | Node [{node={term=Leaf s}} as hd; t'] when is_binder_symbol s ->
    T.mk_node [hd; replace_and_lift (depth+1) t']  (* increment depth *) 
  | Node l -> T.mk_node (List.map (replace_and_lift depth) l)
  (* make De Bruijn index of given index *)
  and mk_db n = match n with
  | 0 -> T.mk_leaf db_symbol v.node.sort
  | n when n > 0 ->
    let next = mk_db (n-1) in
    T.mk_apply succ_db_symbol v.node.sort [next]
  | _ -> assert false
  in
  replace_and_lift 0 t

(* constraint on the ordering *)
let symbol_constraint =
  O.compose_constraints
    (O.max_constraint [succ_db_symbol; db_symbol])
    (O.min_constraint [eq_symbol; imply_symbol; lambda_symbol; forall_symbol;
                       exists_symbol; or_symbol; and_symbol;
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
      look_db_sort 0 t; t (* the variable is not present *)
    with FoundSort sort ->
      (* sort is the sort of the first DB symbol *)
      let new_var = T.mk_var (maxvar + 1) sort in
      db_replace t new_var
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
      look_db_sort 0 t; t (* the DB variable is not present *)
    with FoundSort sort ->
      (* sort is the sort of the first DB symbol *)
      let new_skolem = skolem ord vars sort in
      db_replace t new_skolem
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
    [] (Utils.list_pos clause.clits)

(** elimination of forall *)
let forall_elimination ~ord clause =
  fold_lits ~both:false ~pos:true ~neg:true
    (fun acc l r sign l_pos -> 
      if not (T.eq_foterm r T.true_term) then acc else
      let idx = List.hd l_pos in 
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
    [] (Utils.list_pos clause.clits)

(** elimination of exists *)
let exists_elimination ~ord clause =
  fold_lits ~both:false ~pos:true ~neg:true
    (fun acc l r sign l_pos -> 
      if not (T.eq_foterm r T.true_term) then acc else
      let idx = List.hd l_pos in 
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
    [] (Utils.list_pos clause.clits)

(** equivalence elimination *)
let equivalence_elimination ~ord clause =
  (* check whether the term is not a non-equational proposition *)
  let rec is_not_nonequational t = match t.node.term with
  | Node ({node={term=Leaf s}}::_) ->
      t.node.sort = bool_sort &&
      (s = eq_symbol || s = exists_symbol || s = forall_symbol || s = not_symbol || 
       s = and_symbol || s = imply_symbol || s = or_symbol)
  | Var _ | Leaf _ -> false
  | Node _ -> assert false
  (* do the inference for positive equations *)
  and do_inferences_pos l r l_pos =
    if not (is_not_nonequational l) then [] else begin
    assert (r.node.sort = bool_sort);
    if ord#compare l r = Lt then [] else
    (* ok, do it *)
    match l_pos with
    | [idx; _] ->
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
    [] (Utils.list_pos clause.clits)

(* ----------------------------------------------------------------------
 * simplification
 * ---------------------------------------------------------------------- *)

(** Simplify the inner formula (double negation, trivial equalities...) TODO *)
let simplify_inner ~ord c = c

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

    method basic_simplify ~ord c = simplify_inner ~ord (Sup.basic_simplify ~ord c)

    method simplify actives c = Sup.demodulate actives [] c

    method redundant actives c = Sup.subsumed_by_set actives c

    method redundant_set actives c = Sup.subsumed_in_set actives c

    method trivial c = Sup.is_tautology c

    method axioms = []

    method constr = symbol_constraint

    method preprocess ~ord l =
      List.map (fun c -> C.reord_clause ~ord (C.clause_of_fof ~ord c)) l
  end
