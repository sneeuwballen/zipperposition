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

(** special predicate/connective symbols, in decreasing order *)
let special_preds =
  [eq_symbol; imply_symbol; forall_symbol; exists_symbol; lambda_symbol;
   or_symbol; and_symbol; not_symbol; false_symbol; true_symbol]

type symbol_kind = 
  | Predicate | DeBruijn | Function | Special

(** order on kinds of symbols *)
let order k1 k2 =
  match k1, k2 with
  | _, _ when k1 = k2 -> 0
  | Predicate, _ -> 1
  | _, Predicate -> -1
  | DeBruijn, _ -> 1
  | _, DeBruijn -> -1
  | Function, _ -> 1
  | Special, _ -> -1

(* classify symbol into categories *)
let classify =
  let special_set = Utils.SHashSet.from_list special_preds in 
  function s ->
    let sorts, _, _ = O.current_signature () in
    match s with
    | _ when s = succ_db_symbol || s = db_symbol -> DeBruijn
    | _ when Utils.SHashSet.member special_set s -> Special
    | _ when Hashtbl.find sorts s = bool_sort -> Predicate
    | _ -> Function

(** constraint on the ordering *)
let symbol_constraint _ =
  O.compose_constraints
    (fun x y -> order (classify x) (classify y))
    (O.min_constraint special_preds)

(* ----------------------------------------------------------------------
 * elimination rules
 * ---------------------------------------------------------------------- *)

(** to be raised when a simplification is found *)
exception Elimination of clause list

(** helper for alpha elimination (remove idx-th literal from
    clause and adds a and b to two new clauses *)
let alpha_eliminate ~ord clause idx a signa b signb =
  assert (a.sort = bool_sort && b.sort = bool_sort);
  let new_lita = C.mk_eqn a T.true_term signa
  and new_litb = C.mk_eqn b T.true_term signb in
  let other_lits = Utils.list_remove clause.clits idx in
  let proof = lazy (Proof ("α-elim", [clause, [idx], S.id_subst])) in
  let clauses = [C.mk_clause ~ord (new_lita :: other_lits) ~selected:(lazy []) proof (lazy [clause]);
                 C.mk_clause ~ord (new_litb :: other_lits) ~selected:(lazy []) proof (lazy [clause])]
  in raise (Elimination clauses)

(** helper for beta elimination (remove idx-th literal from
    clause and adds a and b *)
let beta_eliminate ~ord clause idx a signa b signb =
  assert (a.sort = bool_sort && b.sort = bool_sort);
  let new_lita = C.mk_lit ~ord a T.true_term signa
  and new_litb = C.mk_lit ~ord b T.true_term signb in
  let new_lits = new_lita :: new_litb :: (Utils.list_remove clause.clits idx) in
  let proof = lazy (Proof ("β-elim", [clause, [idx], S.id_subst])) in
  raise (Elimination [C.mk_clause ~ord new_lits ~selected:(lazy []) proof (lazy [clause])])

(** helper for gamma elimination (remove idx-th literal from clause
    and adds t where De Bruijn 0 is replaced by a fresh var) *)
let gamma_eliminate ~ord clause idx t sign =
  let maxvar = max (T.max_var clause.cvars) 0 in
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
  let proof = lazy (Proof ("γ-elim", [clause, [idx], S.id_subst])) in
  raise (Elimination [C.mk_clause ~ord new_lits ~selected:(lazy []) proof (lazy [clause])])

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
  let proof = lazy (Proof ("δ-elim", [clause, [idx], S.id_subst])) in
  raise (Elimination [C.mk_clause ~ord new_lits ~selected:(lazy []) proof (lazy [clause])])

(** An elimination rule transforms a clause into a list of new
    clauses in one step, or return None if it is not applicable *)
type elimination_rule = ord:ordering -> clause -> clause list option

(** elimination of unary/binary logic connectives *)
let connective_elimination ~ord clause =
  let lits = if C.selected clause = []
    then C.maxlits clause
    else C.selected_lits clause in
  try
    fold_lits ~both:false ~pos:true ~neg:true
    (fun () l r sign l_pos ->
      (* if a literal is true_term, must be r because it is the smallest term *)
      if not (T.eq_term r T.true_term) then () else
      let idx = List.hd l_pos in
      if not (C.eligible_res ~ord clause idx S.id_subst) then () else
      match l.term with
      | Node (s, [a; b]) ->
        (* some alpha/beta eliminations *)
        if s = and_symbol && sign
        then alpha_eliminate ~ord clause idx a true b true
        else if s = and_symbol && (not sign)
        then beta_eliminate ~ord clause idx a false b false
        else if s = or_symbol && sign
        then beta_eliminate ~ord clause idx a true b true
        else if s = or_symbol && (not sign)
        then alpha_eliminate ~ord clause idx a false b false
        else if s = imply_symbol && sign
        then beta_eliminate ~ord clause idx a false b true
        else if s = imply_symbol && (not sign)
        then alpha_eliminate ~ord clause idx a true b false
      | _ -> ()
    )
    () lits;
    None  (* no simplification *)
  with Elimination l -> Some l

(** elimination of forall *)
let forall_elimination ~ord clause =
  let lits = if C.selected clause = []
    then C.maxlits clause
    else C.selected_lits clause in
  try
    fold_lits ~both:false ~pos:true ~neg:true
    (fun () l r sign l_pos -> 
      if not (T.eq_term r T.true_term) then () else
      let idx = List.hd l_pos in 
      if not (C.eligible_res ~ord clause idx S.id_subst) then () else
      match l.term with
      | Node (s, [{term=Node (s', [t])}]) when s = forall_symbol && s' = lambda_symbol ->
        (* we have a forall (lambda t) *)
        assert (t.sort = bool_sort);
        if sign
          then gamma_eliminate ~ord clause idx t true
          else delta_eliminate ~ord clause idx t false
      | _ -> ()
    )
    () lits; None
  with Elimination l -> Some l

(** elimination of exists *)
let exists_elimination ~ord clause =
  let lits = if C.selected clause = []
    then C.maxlits clause
    else C.selected_lits clause in
  try
    fold_lits ~both:false ~pos:true ~neg:true
    (fun acc l r sign l_pos -> 
      if not (T.eq_term r T.true_term) then () else
      let idx = List.hd l_pos in 
      if not (C.eligible_res ~ord clause idx S.id_subst) then () else
      match l.term with
      | Node (s, [{term=Node (s', [t])}]) when s = exists_symbol && s' = lambda_symbol ->
        (* we have an exists (lambda t) *)
        assert (t.sort = bool_sort);
        if sign
          then delta_eliminate ~ord clause idx t true
          else gamma_eliminate ~ord clause idx t false
      | _ -> ()
    )
    () lits; None
  with Elimination l -> Some l

(** equivalence elimination *)
let equivalence_elimination ~ord clause =
  let lits = if C.selected clause = []
    then C.maxlits clause
    else C.selected_lits clause in
  (* do the inference for positive equations *)
  let do_inferences_pos l r l_pos =
    if T.atomic l then () else begin
      assert (r.sort = bool_sort);
      assert (ord#compare l r <> Lt);
      (* ok, do it *)
      let idx = List.hd l_pos in
      if not (C.eligible_res ~ord clause idx S.id_subst) then () else
      let new_lits = Utils.list_remove clause.clits idx in
      let new_lits1 = (C.mk_neq ~ord l T.true_term) ::
                      (C.mk_eq ~ord r T.true_term) :: new_lits
      and proof1 = lazy (Proof ("<=>-elim+", [clause, l_pos, S.id_subst]))
      and new_lits2 = (C.mk_eq ~ord l T.true_term) ::
                      (C.mk_neq ~ord r T.true_term) :: new_lits
      and proof2 = lazy (Proof ("<=>-elim+", [clause, l_pos, S.id_subst]))
      in
      let clauses = [C.mk_clause ~ord new_lits1 ~selected:(lazy []) proof1 (lazy [clause]);
                     C.mk_clause ~ord new_lits2 ~selected:(lazy []) proof2 (lazy [clause])]
      in raise (Elimination clauses)
    end
  (* do the inference for negative equations *)
  and do_inferences_neg l r l_pos =
    if not (l.sort = bool_sort) then () else begin
      assert (r.sort = bool_sort);
      let idx = List.hd l_pos in
      if not (C.eligible_res ~ord clause idx S.id_subst) then () else
      let new_lits = Utils.list_remove clause.clits idx in
      let new_lits1 = (C.mk_eq ~ord l T.true_term) ::
                      (C.mk_eq ~ord r T.true_term) :: new_lits
      and proof1 = lazy (Proof ("<=>-elim-", [clause, l_pos, S.id_subst]))
      and new_lits2 = (C.mk_neq ~ord l T.true_term) ::
                      (C.mk_neq ~ord r T.true_term) :: new_lits
      and proof2 = lazy (Proof ("<=>-elim-", [clause, l_pos, S.id_subst]))
      in
      let clauses = [C.mk_clause ~ord new_lits1 ~selected:(lazy []) proof1 (lazy [clause]);
                     C.mk_clause ~ord new_lits2 ~selected:(lazy []) proof2 (lazy [clause])]
      in raise (Elimination clauses)
    end
  in
  try
    fold_lits ~both:true ~pos:true ~neg:true
      (fun acc l r sign l_pos -> 
        if T.eq_term r T.true_term || T.eq_term r T.false_term
        then acc
        else if sign
          then do_inferences_pos l r l_pos
          else do_inferences_neg l r l_pos)
      () lits;
      None
  with Elimination l -> Some l

(* ----------------------------------------------------------------------
 * syntactic simplification
 * ---------------------------------------------------------------------- *)

(** Simplify the inner formula (double negation, trivial equalities...) *)
let simplify_inner ~ord c =
  let mark_simplified t = T.set_flag T.flag_simplified t true in
  (* simplify a term *)
  let rec simp_term t =
    if T.get_flag T.flag_simplified t then t else  (* maybe it's already simplified *)
    match t.term with
    | Var _ | Node (_, []) -> (mark_simplified t; t)
    | Node (s, [{term=Node (s', [t'])}]) when s = not_symbol && s' = not_symbol ->
      simp_term t'  (* double negation *)
    | Node (s, [t']) when s = not_symbol && T.eq_term t' T.true_term ->
      T.false_term  (* not true -> false *)
    | Node (s, [t']) when s = not_symbol && T.eq_term t' T.false_term ->
      T.true_term   (* not false -> true *)
    | Node (s, [{term=Node (s', [t'])}])
      when (s = forall_symbol || s = exists_symbol) && s' = lambda_symbol
        && not (T.db_contains t' 0) ->
        simp_term (T.db_unlift t') (* eta-reduction *)
    | Node (s, [a; b]) when s = and_symbol && (T.eq_term a T.false_term || T.eq_term b T.false_term) ->
      T.false_term  (* a and false -> false *)
    | Node (s, [a; b]) when s = or_symbol && (T.eq_term a T.true_term || T.eq_term b T.true_term) ->
      T.true_term  (* a or true -> true *)
    | Node (s, [a; b]) when s = or_symbol && T.eq_term a T.false_term ->
      simp_term b  (* b or false -> b *)
    | Node (s, [a; b]) when s = or_symbol && T.eq_term b T.false_term ->
      simp_term a  (* a or false -> a *)
    | Node (s, [a; b]) when s = and_symbol && T.eq_term a T.true_term ->
      simp_term b  (* b and true -> b *)
    | Node (s, [a; b]) when s = and_symbol && T.eq_term b T.true_term ->
      simp_term a  (* a and true -> a *)
    | Node (s, [a; b]) when s = imply_symbol && (T.eq_term a T.false_term || T.eq_term b T.true_term) ->
      T.true_term  (* (false => a) or (a => true) -> true *)
    | Node (s, [a; b]) when s = imply_symbol && T.eq_term a T.true_term ->
      simp_term b  (* (true => a) -> a *)
    | Node (s, [a; b]) when s = eq_symbol && T.eq_term a b ->
      T.true_term  (* a = a -> true *)
    | Node (s, [a; b]) when s = eq_symbol && 
      ((T.eq_term a T.true_term && T.eq_term b T.false_term) ||
       (T.eq_term b T.true_term && T.eq_term a T.false_term)) ->
      T.false_term (* true = false -> false *)
    | Node (s, [a; b]) when s = eq_symbol && T.eq_term b T.true_term ->
      simp_term a  (* a = true -> a *)
    | Node (s, [a; b]) when s = eq_symbol && T.eq_term a T.true_term ->
      simp_term b  (* b = true -> b *)
    | Node (s, [a; b]) when s = eq_symbol && T.eq_term b T.false_term ->
      simp_term (T.mk_not a)  (* a = false -> not a *)
    | Node (s, [a; b]) when s = eq_symbol && T.eq_term a T.false_term ->
      simp_term (T.mk_not b)  (* b = false -> not b *)
    | Node (s, l) ->
      let new_t = T.mk_node s t.sort (List.map simp_term l) in
      if T.eq_term t new_t
        then (mark_simplified t; t)  (* no more simplifications *)
        else simp_term new_t
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
  object (self)
    method binary_rules = ["superposition_active", Sup.infer_active;
                           "superposition_passive", Sup.infer_passive]

    method unary_rules = ["equality_resolution", Sup.infer_equality_resolution;
                          "equality_factoring", Sup.infer_equality_factoring; ]

    method basic_simplify ~ord c = Sup.basic_simplify ~ord (simplify_inner ~ord c)

    method simplify actives c =
      let ord = actives.PS.a_ord
      and old_c = c in
      let c = simplify_inner ~ord (Sup.basic_simplify ~ord c) in
      let c = Sup.basic_simplify ~ord (Sup.positive_simplify_reflect actives c) in
      let c = Sup.basic_simplify ~ord (Sup.negative_simplify_reflect actives c) in
      let c = Sup.basic_simplify ~ord (Sup.demodulate actives c) in
      let c = simplify_inner ~ord c in
      if C.eq_clause c old_c
        then c
        else self#simplify actives c

    method redundant actives c = Sup.subsumed_by_set actives c

    method redundant_set actives c = Sup.subsumed_in_set actives c

    (* use elimination rules as simplifications rather than inferences, here *)
    method list_simplify ~ord ~select c =
      let all_rules = [connective_elimination; forall_elimination;
                       exists_elimination; equivalence_elimination; ] in
      (* try to use the rules to simplify c *)
      let queue = Queue.create ()
      and clauses = ref [] in
      let c = C.select_clause ~select (self#basic_simplify ~ord c) in
      Queue.push (c, all_rules) queue;
      while not (Queue.is_empty queue) do
        (* process a clause *)
        let c, rules = Queue.pop queue in
        match rules with 
        | [] -> clauses := c :: !clauses  (* c is not simplifiable by any rule *)
        | rule::rules' ->
          (match rule ~ord c with
          | None -> Queue.push (c, rules') queue  (* use next rules *)
          | Some clauses -> 
            begin
              Utils.debug 3 (lazy (Utils.sprintf "@[<hov 4>@[<h>%a@]@ simplified into clauses @[<hv>%a@]@]"
                            !C.pp_clause#pp c (Utils.pp_list !C.pp_clause#pp) clauses));
              List.iter
                (fun c' ->  (* simplify the clause with all rules, if not trivial *)
                  let c' = self#basic_simplify ~ord (simplify_inner ~ord c') in
                  if not (Sup.is_tautology c')
                    then Queue.push (C.select_clause ~select c', all_rules) queue)
                clauses
            end)
      done;
      match !clauses with
      | [c'] when C.eq_clause c c' -> None
      | clauses -> Some clauses

    method axioms = []

    method constr clauses = symbol_constraint clauses

    method preprocess ~ord l =
      List.map (fun c -> C.reord_clause ~ord (C.clause_of_fof ~ord c)) l
  end
