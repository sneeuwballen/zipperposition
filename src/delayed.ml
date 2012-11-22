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

(** equation simplified into a disjunction of conjunctions of equations *)
type tableau_rule =
  | Alpha of tableau_rule * tableau_rule  (** alpha elimination *)
  | List of equation list                 (** list of literals, after transformation *)
  | Keep of equation                      (** keep this equation unchanged *)

(** helper for alpha elimination *)
let alpha_eliminate a signa b signb =
  Alpha (List [C.mk_eqn a T.true_term signa],
         List [C.mk_eqn b T.true_term signb])

(** helper for beta elimination *)
let beta_eliminate a signa b signb =
  List [C.mk_eqn a T.true_term signa; C.mk_eqn b T.true_term signb]

(** helper for gamma elimination *)
let gamma_eliminate offset t sign =
  assert (t.sort = bool_sort);
  let i = !offset in
  incr offset;
  let new_t =
    match T.look_db_sort 0 t with
    | None -> T.db_unlift t (* the variable is not present *)
    | Some sort ->
      (* sort is the sort of the first DB symbol *)
      let new_var = T.mk_var i sort in
      T.db_unlift (T.db_replace t new_var)
  in
  List [C.mk_eqn new_t T.true_term sign]

(** helper for delta elimination (remove idx-th literal from clause
    and adds t where De Bruijn 0 is replaced by a skolem
    of free variables of t) *)
let delta_eliminate ~ord t sign =
  assert (t.sort = bool_sort);
  let new_t =
    match T.look_db_sort 0 t with
    | None -> T.db_unlift t (* the variable is not present *)
    | Some sort ->
      (* sort is the sort of the first DB symbol *)
      skolem ~ord t sort
  in
  List [C.mk_eqn new_t T.true_term sign]

(** Just keep the equation as it is *)
let keep eqn = Keep eqn

(** perform at most one simplification on each literal. It
    returns an array of tableau_rule. *)
let eliminate_lits ~cs clause =
  let offset = ref (T.max_var clause.cvars + 1) in  (* offset to create variables *)
  (* is the literal eligible? *)
  let eligible i = C.eligible_res ~cs clause i S.id_subst in
  (* eliminate propositions (connective and quantifier eliminations) *)
  let prop eqn p sign =
    assert (p.sort = bool_sort);
    match p.term with
    | Node (s, [a; b]) when s = and_symbol && sign -> alpha_eliminate a true b true
    | Node (s, [a; b]) when s = and_symbol && not sign -> beta_eliminate a false b false
    | Node (s, [a; b]) when s = or_symbol && sign -> beta_eliminate a true b true
    | Node (s, [a; b]) when s = or_symbol && not sign -> alpha_eliminate a false b false
    | Node (s, [a; b]) when s = imply_symbol && sign -> beta_eliminate a false b true
    | Node (s, [a; b]) when s = imply_symbol && not sign -> alpha_eliminate a true b false
    | Node (s, [{term=Node (s', [t])}]) when s = forall_symbol && s' = lambda_symbol && sign ->
      gamma_eliminate offset t true
    | Node (s, [{term=Node (s', [t])}]) when s = forall_symbol && s' = lambda_symbol && not sign ->
      delta_eliminate ~ord:cs#ord t false
    | Node (s, [{term=Node (s', [t])}]) when s = exists_symbol && s' = lambda_symbol && sign ->
      delta_eliminate ~ord:cs#ord t true
    | Node (s, [{term=Node (s', [t])}]) when s = exists_symbol && s' = lambda_symbol && not sign ->
      gamma_eliminate offset t false
    | _ -> keep eqn
  (* eliminate equivalence *)
  and equiv eqn l r sign =
    match cs#ord#compare l r with
    | Gt when not (T.atomic l) && sign -> (* l <=> r -> (l => r) & (r => l)*)
      Alpha (List [C.mk_neq l T.true_term; C.mk_eq r T.true_term],
             List [C.mk_neq r T.true_term; C.mk_eq l T.true_term])
    | Lt when not (T.atomic r) && sign ->
      Alpha (List [C.mk_neq l T.true_term; C.mk_eq r T.true_term],
             List [C.mk_neq r T.true_term; C.mk_eq l T.true_term])
    | Gt when not (T.atomic l) && not sign -> (* not (l <=> r) -> (l | r) & (not l | not r) *)
      Alpha (List [C.mk_eq l T.true_term; C.mk_eq r T.true_term],
             List [C.mk_neq r T.true_term; C.mk_neq l T.true_term])
    | Lt when not (T.atomic r) && not sign ->
      Alpha (List [C.mk_eq l T.true_term; C.mk_eq r T.true_term],
             List [C.mk_neq r T.true_term; C.mk_neq l T.true_term])
    | _ -> keep eqn
  in
  (* try to eliminate each literal that is eligible for resolution *)
  let tableau_rules =
    Array.mapi
      (fun i lit -> 
        if eligible i
          then
            match lit.lit_eqn with
            | Equation (l, r, sign) when T.eq_term r T.true_term -> prop lit.lit_eqn l sign
            | Equation (l, r, sign) when T.eq_term l T.true_term -> prop lit.lit_eqn r sign
            | Equation (l, r, sign) when l.sort = bool_sort -> equiv lit.lit_eqn l r sign
            | _ -> keep lit.lit_eqn  (* equation between terms *)
          else keep lit.lit_eqn)
    clause.clits
  in
  tableau_rules

(** Produce a list of clauses from an array of tableau_rule, or None *)
let tableau_to_clauses ~cs clause a =
  if try Array.iter (function | Keep _ -> () | _ -> raise Exit) a; true
     with Exit -> false
  then None (* no change, the array contains only Keep *)
  else begin
    let clauses = ref []
    and eqns = Vector.create (Array.length a * 2) in
    let proof = lazy (Proof ("elim", [clause, [], S.id_subst]))
    and parents = [clause] in
    (* explore all combinations of tableau splits *)
    let rec explore_splits i =
      if i = Array.length a
        then  (* produce new clause *)
          let clause = C.mk_clause_vec ~cs eqns proof parents in
          clauses := clause :: !clauses
        else begin
          let len = Vector.size eqns in
          explore_branch i len a.(i)
        end
    (* recurse in sub-tableau *)
    and explore_branch i len rule =
      (match rule with
      | Keep eqn ->  (* push equation *)
        Vector.push eqns eqn; explore_splits (i+1)
      | List l ->    (* push equations *)
        List.iter (Vector.push eqns) l; explore_splits (i+1)
      | Alpha (left, right) ->  (* explore left, then right *)
        explore_branch i len left;
        explore_branch i len right);
      Vector.shrink eqns len  (* restore state *)
    in explore_splits 0;
    (* return the vector of clauses *)
    Some !clauses
  end

(** Perform eliminations recursively, until no elimination is possible *)
let recursive_eliminations ~cs c =
  let clauses = ref [] in
  (* process clauses until none of them is simplifiable *)
  let rec simplify c =
    let tableau_rules = eliminate_lits ~cs c in
    match tableau_to_clauses ~cs c tableau_rules with
    | None -> clauses := c :: !clauses (* done with this clause *)
    | Some clauses ->
      Utils.debug 3 (lazy (Utils.sprintf "@[<hov 4>@[<h>%a@]@ simplified into clauses @[<hv>%a@]@]"
                    !C.pp_clause#pp c (Utils.pp_list !C.pp_clause#pp) clauses));
      List.iter simplify clauses (* simplify recursively new clauses *)
  in
  simplify c;
  match !clauses with
  | [c'] when C.eq_clause c c' -> None (* no simplification *)
  | l -> Some l (* some simplifications *)

(* ----------------------------------------------------------------------
 * syntactic simplification
 * ---------------------------------------------------------------------- *)

(** Simplify the inner formula (double negation, trivial equalities...) *)
let simplify_inner ~cs c =
  let simplified = ref false in
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
  and simp_lit {lit_eqn=Equation (l,r,sign) as eqn} =
    let eqn' = C.mk_eqn (simp_term l) (simp_term r) sign in
    (if not (C.eq_eqn eqn eqn') then simplified := true);
    eqn'
  in
  let eqns = Array.map simp_lit c.clits in
  if !simplified
    then C.mk_clause ~cs eqns c.cproof c.cparents
    else c  (* no simplification *)

(* ----------------------------------------------------------------------
 * the calculus object
 * ---------------------------------------------------------------------- *)

let delayed : calculus =
  object (self)
    method binary_rules = ["superposition_active", Sup.infer_active;
                           "superposition_passive", Sup.infer_passive]

    method unary_rules = ["equality_resolution", Sup.infer_equality_resolution;
                          "equality_factoring", Sup.infer_equality_factoring; ]

    method basic_simplify ~cs c = Sup.basic_simplify ~cs (simplify_inner ~cs c)

    method simplify actives c =
      let cs = actives.PS.a_cs in
      let c = simplify_inner ~cs (Sup.basic_simplify ~cs c) in
      let c = Sup.basic_simplify ~cs (Sup.demodulate actives c) in
      let c = Sup.basic_simplify ~cs (Sup.simplify_reflect actives c) in
      let c = simplify_inner ~cs c in
      c

    method redundant actives c = Sup.subsumed_by_set actives c

    method redundant_set actives c = Sup.subsumed_in_set actives c

    (* use elimination rules as simplifications rather than inferences, here *)
    method list_simplify ~cs c =
      let c = Sup.basic_simplify ~cs c in
      recursive_eliminations ~cs c

    method axioms = []

    method constr clauses = symbol_constraint clauses

    method preprocess ~cs l =
      List.map (fun c -> Sup.basic_simplify ~cs (C.clause_of_fof ~cs c)) l
  end
