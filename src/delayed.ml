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

(** check whether the term is (Leaf s) *)
let check_sym t s = match t.term with
  | Var _ -> false
  | Node _ -> false
  | Leaf s' -> s = s'

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
  assert (a.sort = bool_sort && b.sort = bool_sort);
  let new_lita = C.mk_lit ~ord a T.true_term signa
  and new_litb = C.mk_lit ~ord b T.true_term signb in
  let other_lits = Utils.list_remove clause.clits idx in
  let proof = lazy (Proof ("alpha_eliminate", [clause, [idx], S.id_subst])) in
  [C.mk_clause ~ord (new_lita :: other_lits) ~selected:(lazy []) proof (lazy [clause]);
   C.mk_clause ~ord (new_litb :: other_lits) ~selected:(lazy []) proof (lazy [clause])]

(** helper for beta elimination (remove idx-th literal from
    clause and adds a and b *)
let beta_eliminate ~ord clause idx a signa b signb =
  assert (a.sort = bool_sort && b.sort = bool_sort);
  let new_lita = C.mk_lit ~ord a T.true_term signa
  and new_litb = C.mk_lit ~ord b T.true_term signb in
  let new_lits = new_lita :: new_litb :: (Utils.list_remove clause.clits idx) in
  let proof = lazy (Proof ("beta_eliminate", [clause, [idx], S.id_subst])) in
  C.mk_clause ~ord new_lits ~selected:(lazy []) proof (lazy [clause])

(** helper for gamma elimination (remove idx-th literal from clause
    and adds t where De Bruijn 0 is replaced by a fresh var) *)
let gamma_eliminate ~ord clause idx t sign =
  let maxvar = max (T.max_var t.vars) 0 in
  assert (t.sort = bool_sort);
  let new_t =
    match T.look_db_sort 0 t with
    | None -> T.db_unlift t (* the variable is not present *)
    | Some sort ->
      (* sort is the sort of the first DB symbol *)
      let new_var = T.mk_var (maxvar + 1) sort in
      T.db_unlift (T.db_replace t new_var)
  in
  let new_lit = C.mk_lit ~ord new_t T.true_term sign in
  let new_lits = new_lit :: (Utils.list_remove clause.clits idx) in
  let proof = lazy (Proof ("gamma_eliminate", [clause, [idx], S.id_subst])) in
  C.mk_clause ~ord new_lits ~selected:(lazy []) proof (lazy [clause])

(** helper for delta elimination (remove idx-th literal from clause
    and adds t where De Bruijn 0 is replaced by a skolem
    of free variables of t) *)
let delta_eliminate ~ord clause idx t sign =
  assert (t.sort = bool_sort);
  let new_t =
    match T.look_db_sort 0 t with
    | None -> T.db_unlift t (* the variable is not present *)
    | Some sort ->
      (* sort is the sort of the first DB symbol *)
      skolem ~ord t sort
  in
  let new_lit = C.mk_lit ~ord new_t T.true_term sign in
  let new_lits = new_lit :: (Utils.list_remove clause.clits idx) in
  let proof = lazy (Proof ("delta_eliminate", [clause, [idx], S.id_subst])) in
  C.mk_clause ~ord new_lits ~selected:(lazy []) proof (lazy [clause])

(** elimination of unary/binary logic connectives *)
let connective_elimination ~ord clause =
  let lits = if C.selected clause = []
    then C.maxlits clause
    else C.selected_lits clause in
  fold_lits ~both:false ~pos:true ~neg:true
    (fun acc l r sign l_pos ->
      (* if a literal is true_term, must be r because it is the smallest term *)
      if not (T.eq_term r T.true_term) then acc else
      let idx = List.hd l_pos in
      if not (C.eligible_res ~ord clause idx S.id_subst) then acc else
      match l.term with
      | Node [{term=Leaf s}; a; b] ->
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
    [] lits

(** elimination of forall *)
let forall_elimination ~ord clause =
  let lits = if C.selected clause = []
    then C.maxlits clause
    else C.selected_lits clause in
  fold_lits ~both:false ~pos:true ~neg:true
    (fun acc l r sign l_pos -> 
      if not (T.eq_term r T.true_term) then acc else
      let idx = List.hd l_pos in 
      if not (C.eligible_res ~ord clause idx S.id_subst) then acc else
      match l.term with
      | Node [{term=Leaf s}; {term=Node [{term=Leaf s'}; t]}]
          when s = forall_symbol && s' = lambda_symbol ->
          (* we have a forall (lambda t) *)
          assert (t.sort = bool_sort);
          if sign
            then (gamma_eliminate ~ord clause idx t true) :: acc
            else (delta_eliminate ~ord clause idx t false) :: acc
      | _ -> acc
    )
    [] lits

(** elimination of exists *)
let exists_elimination ~ord clause =
  let lits = if C.selected clause = []
    then C.maxlits clause
    else C.selected_lits clause in
  fold_lits ~both:false ~pos:true ~neg:true
    (fun acc l r sign l_pos -> 
      if not (T.eq_term r T.true_term) then acc else
      let idx = List.hd l_pos in 
      if not (C.eligible_res ~ord clause idx S.id_subst) then acc else
      match l.term with
      | Node [{term=Leaf s}; {term=Node [{term=Leaf s'}; t]}]
          when s = exists_symbol && s' = lambda_symbol ->
          (* we have an exists (lambda t) *)
          assert (t.sort = bool_sort);
          if sign
            then (delta_eliminate ~ord clause idx t true) :: acc
            else (gamma_eliminate ~ord clause idx t false) :: acc
      | _ -> acc
    )
    [] lits

(** equivalence elimination *)
let equivalence_elimination ~ord clause =
  let lits = if C.selected clause = []
    then C.maxlits clause
    else C.selected_lits clause in
  (* do the inference for positive equations *)
  let do_inferences_pos l r l_pos =
    if T.atomic l then [] else begin
      assert (r.sort = bool_sort);
      assert (ord#compare l r <> Lt);
      (* ok, do it *)
      Format.printf "%%  equivalence_elim: @[<h>%a %s %a@]@." !T.pp_term#pp l
        (C.string_of_comparison (ord#compare l r)) !T.pp_term#pp r;
      let idx = List.hd l_pos in
      if not (C.eligible_res ~ord clause idx S.id_subst) then [] else
      let new_lits = Utils.list_remove clause.clits idx in
      let new_lits1 = (C.mk_neq ~ord l T.true_term) ::
                      (C.mk_eq ~ord r T.true_term) :: new_lits
      and proof1 = lazy (Proof ("pos_equiv_elim1", [clause, l_pos, S.id_subst]))
      and new_lits2 = (C.mk_eq ~ord l T.true_term) ::
                      (C.mk_neq ~ord r T.true_term) :: new_lits
      and proof2 = lazy (Proof ("pos_equiv_elim2", [clause, l_pos, S.id_subst]))
      in
      [C.mk_clause ~ord new_lits1 ~selected:(lazy []) proof1 (lazy [clause]);
       C.mk_clause ~ord new_lits2 ~selected:(lazy []) proof2 (lazy [clause])]
    end
  (* do the inference for negative equations *)
  and do_inferences_neg l r l_pos =
    if not (l.sort = bool_sort) then [] else begin
      assert (r.sort = bool_sort);
      let idx = List.hd l_pos in
      if not (C.eligible_res ~ord clause idx S.id_subst) then [] else
      let new_lits = Utils.list_remove clause.clits idx in
      let new_lits1 = (C.mk_eq ~ord l T.true_term) ::
                      (C.mk_eq ~ord r T.true_term) :: new_lits
      and proof1 = lazy (Proof ("neg_equiv_elim1", [clause, l_pos, S.id_subst]))
      and new_lits2 = (C.mk_neq ~ord l T.true_term) ::
                      (C.mk_neq ~ord r T.true_term) :: new_lits
      and proof2 = lazy (Proof ("neg_equiv_elim2", [clause, l_pos, S.id_subst]))
      in
      [C.mk_clause ~ord new_lits1 ~selected:(lazy []) proof1 (lazy [clause]);
       C.mk_clause ~ord new_lits2 ~selected:(lazy []) proof2 (lazy [clause])]
    end
  in
  fold_lits ~both:true ~pos:true ~neg:true
    (fun acc l r sign l_pos -> 
      if T.eq_term r T.true_term || T.eq_term r T.false_term
      then acc
      else if sign
        then (do_inferences_pos l r l_pos) @ acc
        else (do_inferences_neg l r l_pos) @ acc)
    [] lits

(* ----------------------------------------------------------------------
 * simplification
 * ---------------------------------------------------------------------- *)

(** Simplify the inner formula (double negation, trivial equalities...) *)
let simplify_inner ~ord c =
  let not_term = T.mk_leaf not_symbol bool_sort in
  (* simplify a term *)
  let rec simp_term t = match t.term with
  | Var _ | Leaf _ -> t
  | Node [{term=Leaf s}; {term=Node [{term=Leaf s'}; t']}]
    when s = not_symbol && s' = not_symbol -> simp_term t'  (* double negation *)
  | Node [{term=Leaf s}; t'] when s = not_symbol && T.eq_term t' T.true_term ->
    T.false_term  (* not true -> false *)
  | Node [{term=Leaf s}; t'] when s = not_symbol && T.eq_term t' T.false_term ->
    T.true_term  (* not false -> true *)
  | Node [{term=Leaf s}; {term=Node [{term=Leaf s'}; t']}]
    when (s = forall_symbol || s = exists_symbol) && s' = lambda_symbol
    && not (T.db_contains t' 0) -> simp_term (T.db_unlift t') (* eta-reduction *)
  | Node [{term=Node [{term=Leaf s}; t']}; v] when s = lambda_symbol ->
    let new_t' = T.db_unlift (T.db_replace t' v) in simp_term new_t' (* beta-reduction *)
  | Node [{term=Leaf s}; a; b] when s = and_symbol &&
    (T.eq_term a T.false_term || T.eq_term b T.false_term) ->
    T.false_term  (* a and false -> false *)
  | Node [{term=Leaf s}; a; b] when s = or_symbol &&
    (T.eq_term a T.true_term || T.eq_term b T.true_term) ->
    T.true_term  (* a or true -> true *)
  | Node [{term=Leaf s}; a; b] when s = or_symbol && T.eq_term a T.false_term ->
    simp_term b (* b or false -> b *)
  | Node [{term=Leaf s}; a; b] when s = or_symbol && T.eq_term b T.false_term ->
    simp_term a (* a or false -> a *)
  | Node [{term=Leaf s}; a; b] when s = and_symbol && T.eq_term a T.true_term ->
    simp_term b (* b and true -> b *)
  | Node [{term=Leaf s}; a; b] when s = and_symbol && T.eq_term b T.true_term ->
    simp_term a (* a and true -> a *)
  | Node [{term=Leaf s}; a; b] when s = imply_symbol &&
    (T.eq_term a T.false_term || T.eq_term b T.true_term) ->
    T.true_term  (* (false => a) or (a => true) -> true *)
  | Node [{term=Leaf s}; a; b] when s = imply_symbol && T.eq_term a T.true_term ->
    simp_term b  (* (true => a) -> a *)
  | Node [{term=Leaf s}; a; b] when s = eq_symbol && T.eq_term a b ->
    T.true_term  (* a = a -> true *)
  | Node [{term=Leaf s}; a; b] when s = eq_symbol && 
    ((T.eq_term a T.true_term && T.eq_term b T.false_term) ||
     (T.eq_term b T.true_term && T.eq_term a T.false_term)) ->
    T.false_term  (* true = false -> false *)
  | Node [{term=Leaf s}; a; b] when s = eq_symbol && T.eq_term b T.true_term ->
    simp_term a  (* a = true -> a *)
  | Node [{term=Leaf s}; a; b] when s = eq_symbol && T.eq_term a T.true_term ->
    simp_term b  (* b = true -> b *)
  | Node [{term=Leaf s}; a; b] when s = eq_symbol && T.eq_term b T.false_term ->
    simp_term (T.mk_node [not_term; a])  (* a = false -> not a *)
  | Node [{term=Leaf s}; a; b] when s = eq_symbol && T.eq_term a T.false_term ->
    simp_term (T.mk_node [not_term; b])  (* b = false -> not b *)
  | Node l ->
    let new_t = T.mk_node (List.map simp_term l) in
    if T.eq_term t new_t then t else simp_term new_t
  (* simplify a lit *)
  and simp_lit ((Equation (l,r,sign,_)) as lit) =
    let new_l = simp_term l
    and new_r = simp_term r in
    if T.eq_term l new_l && T.eq_term r new_r then lit
    else C.mk_lit ~ord new_l new_r sign
  in
  C.mk_clause ~ord (List.map simp_lit c.clits) ~selected:(lazy []) c.cproof (lazy [c])

(* ----------------------------------------------------------------------
 * the calculus object
 * ---------------------------------------------------------------------- *)

let delayed : calculus =
  object
    method binary_rules = ["superposition_active", Sup.infer_active;
                           "superposition_passive", Sup.infer_passive]

    method unary_rules = ["equality_resolution", Sup.infer_equality_resolution;
                          "equality_factoring", Sup.infer_equality_factoring; ]

    method basic_simplify ~ord c = Sup.basic_simplify ~ord (simplify_inner ~ord c)

    method simplify actives c =
      let ord = actives.PS.a_ord in
      let c = Sup.basic_simplify ~ord (simplify_inner ~ord c) in
      let c = Sup.basic_simplify ~ord (Sup.positive_simplify_reflect actives c) in
      let c = Sup.basic_simplify ~ord (Sup.negative_simplify_reflect actives c) in
      let c = Sup.basic_simplify ~ord (Sup.demodulate actives [] c) in
      simplify_inner ~ord c

    method redundant actives c = Sup.subsumed_by_set actives c

    method redundant_set actives c = Sup.subsumed_in_set actives c

    (* use elimination rules as simplifications rather than inferences, here *)
    method list_simplify ~ord c =
      let all_rules = [connective_elimination; forall_elimination;
                       exists_elimination; equivalence_elimination ] in
      (* try to use the rules to simplify c *)
      let rec try_simplify rules c =
        if Sup.is_tautology c then [] else
        match rules with 
        | [] -> [c]
        | rule::rules' ->
          (match rule ~ord c with
          | [] -> try_simplify rules' c  (* use next rules *)
          | clauses -> (* keep only non-tautologies, and simplify using all rules *)
            let clauses =  List.filter (fun c -> not (Sup.is_tautology c)) clauses in
            Utils.list_flatmap (try_simplify all_rules) clauses)
      in
      match try_simplify all_rules c with
      | [c'] when C.eq_clause c c' -> None
      | clauses -> Some clauses

    method axioms = []

    method constr = symbol_constraint

    method preprocess ~ord l =
      List.map (fun c -> C.reord_clause ~ord (C.clause_of_fof ~ord c)) l
  end
