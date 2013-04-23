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

(** {1 Superposition with equivalence reasoning and delayed clausal form} *)

open Basic
open Symbols

module T = Terms
module C = Clauses
module O = Orderings
module S = FoSubst
module BV = Bitvector
module Lits = Literals
module Utils = FoUtils
module Sup = Superposition
module PS = ProofState

let prof_elim = Utils.mk_profiler "eliminate"

(** special predicate/connective symbols, in decreasing order *)
let special_preds =
  [at_symbol; num_symbol; split_symbol; const_symbol; eq_symbol; imply_symbol;
   forall_symbol; exists_symbol; lambda_symbol;
   or_symbol; and_symbol; not_symbol; false_symbol; true_symbol]

let special_set = List.fold_left (fun set s -> SSet.add s set) SSet.empty special_preds

type symbol_kind = 
  | Predicate | DeBruijn | Skolem | Function | Special

(** order on kinds of symbols *)
let order k1 k2 =
  match k1, k2 with
  | _, _ when k1 = k2 -> 0
  | Predicate, _ -> 1
  | _, Predicate -> -1
  | DeBruijn, _ -> 1
  | _, DeBruijn -> -1
  | Skolem, _ -> 1
  | _, Skolem -> -1
  | Function, _ -> 1
  | Special, _ -> -1

(* classify symbol into categories *)
let classify signature s =
  match s with
  | _ when s == db_symbol -> DeBruijn
  | _ when attrs_symbol s land attr_skolem <> 0 -> Skolem
  | _ when SSet.mem s special_set -> Special
  | _ -> (* classify between predicate and function by the sort *)
    let sort = try SMap.find s signature with Not_found -> univ_ in
    if sort == bool_ then Predicate else Function

(** constraint on the ordering *)
let symbol_constraint clauses =
  let signature = C.signature clauses in
  [Precedence.min_constraint special_preds;
   fun x y -> order (classify signature x) (classify signature y)]

(* ----------------------------------------------------------------------
 * elimination rules
 * ---------------------------------------------------------------------- *)

(** equation simplified into a disjunction of conjunctions of equations *)
type tableau_rule =
  | Alpha of tableau_rule * tableau_rule  (** alpha elimination *)
  | List of literal list                  (** list of literals, after transformation *)
  | Keep of literal                       (** keep this equation unchanged *)

(** helper for alpha elimination *)
let alpha_eliminate ~ord a signa b signb =
  Alpha (List [Lits.mk_lit ~ord a T.true_term signa],
         List [Lits.mk_lit ~ord b T.true_term signb])

(** helper for beta elimination *)
let beta_eliminate ~ord a signa b signb =
  List [Lits.mk_lit ~ord a T.true_term signa;
        Lits.mk_lit ~ord b T.true_term signb]

(** helper for gamma elimination *)
let gamma_eliminate ~ord offset t sign =
  assert (t.sort == bool_);
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
  List [Lits.mk_lit ~ord new_t T.true_term sign]

(** helper for delta elimination (remove idx-th literal from clause
    and adds t where De Bruijn 0 is replaced by a skolem
    of free variables of t) *)
let delta_eliminate ~ctx t sign =
  assert (t.sort == bool_);
  let new_t =
    match T.look_db_sort 0 t with
    | None -> T.db_unlift t (* the variable is not present *)
    | Some sort ->
      (* sort is the sort of the first DB symbol *)
      !T.skolem ~ctx t sort
  in
  List [Lits.mk_lit ~ord:ctx.ctx_ord new_t T.true_term sign]

(** Just keep the equation as it is *)
let keep eqn = Keep eqn

(** perform at most one simplification on each literal. It
    returns an array of tableau_rule. *)
let eliminate_lits hc =
  let ord = hc.hcctx.ctx_ord in
  let offset = ref ((max 0 (T.max_var hc.hcvars)) + 1) in  (* offset to create variables *)
  (* eliminate propositions (connective and quantifier eliminations) *)
  let prop eqn p sign =
    assert (p.sort == bool_);
    match p.term with
    | BoundVar _ | Var _ -> assert false
    | Node (s, [a; b]) when s == and_symbol && sign -> alpha_eliminate ~ord a true b true
    | Node (s, [a; b]) when s == and_symbol && not sign -> beta_eliminate ~ord a false b false
    | Node (s, [a; b]) when s == or_symbol && sign -> beta_eliminate ~ord a true b true
    | Node (s, [a; b]) when s == or_symbol && not sign -> alpha_eliminate ~ord a false b false
    | Node (s, [a; b]) when s == imply_symbol && sign -> beta_eliminate ~ord a false b true
    | Node (s, [a; b]) when s == imply_symbol && not sign -> alpha_eliminate ~ord a true b false
    | Bind (s, _, t) when s == forall_symbol && sign ->
      gamma_eliminate ~ord offset t true
    | Bind (s, _, t) when s == forall_symbol && not sign ->
      delta_eliminate ~ctx:hc.hcctx (T.mk_not t) true
    | Bind (s, _, t) when s == exists_symbol && sign ->
      delta_eliminate ~ctx:hc.hcctx t true
    | Bind (s, _, t) when s == exists_symbol && not sign ->
      gamma_eliminate ~ord offset t false
    | Bind _ | Node _ -> keep eqn
  (* eliminate equivalence *)
  and equiv eqn l r sign =
    match ord.ord_compare l r with
    | Gt when sign && not (T.atomic l) -> (* l <=> r -> (l => r) & (r => l)*)
      Alpha (List [Lits.mk_neq ~ord l T.true_term; Lits.mk_eq ~ord r T.true_term],
             List [Lits.mk_neq ~ord r T.true_term; Lits.mk_eq ~ord l T.true_term])
    | Lt when sign && not (T.atomic r) ->
      Alpha (List [Lits.mk_neq ~ord l T.true_term; Lits.mk_eq ~ord r T.true_term],
             List [Lits.mk_neq ~ord r T.true_term; Lits.mk_eq ~ord l T.true_term])
    | Incomparable when sign && (not (T.atomic l) || not (T.atomic r)) ->
      Alpha (List [Lits.mk_neq ~ord l T.true_term; Lits.mk_eq ~ord r T.true_term],
             List [Lits.mk_neq ~ord r T.true_term; Lits.mk_eq ~ord l T.true_term])
    | _ when not sign -> (* not (l <=> r) -> (l | r) & (not l | not r) *)
      Alpha (List [Lits.mk_eq ~ord l T.true_term; Lits.mk_eq ~ord r T.true_term],
             List [Lits.mk_neq ~ord r T.true_term; Lits.mk_neq ~ord l T.true_term])
    | _ -> keep eqn
  in
  (* try to eliminate each literal that is eligible for resolution *)
  let tableau_rules =
    Array.map
      (fun lit -> 
        match lit with
        | Equation (l, r, sign, _) when T.eq_term r T.true_term -> prop lit l sign
        | Equation (l, r, sign, _) when T.eq_term l T.true_term -> prop lit r sign
        | Equation (l, r, sign, _) when l.sort == bool_ -> equiv lit l r sign
        | _ -> keep lit)  (* equation between terms *)
      hc.hclits
  in
  tableau_rules

(** Produce a list of clauses from an array of tableau_rule, or None *)
let tableau_to_clauses hc a =
  let ctx = hc.hcctx in
  if Utils.array_forall (function | Keep _ -> true | _ -> false) a
  then None (* just keep all literals *)
  else begin
    let clauses = ref []
    and eqns = Vector.create (Array.length a * 2) in
    let proof c = Proof (c, "elim", [hc.hcproof]) in
    (* explore all combinations of tableau splits *)
    let rec explore_splits i =
      if i = Array.length a
        then  (* produce new clause *)
          let parents = hc :: hc.hcparents in
          let clause = C.mk_hclause_a ~parents ~ctx (Vector.to_array eqns) proof in
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
let recursive_eliminations hc =
  Utils.enter_prof prof_elim;
  let clauses = ref [] in
  (* process clauses until none of them is simplifiable *)
  let rec simplify hc =
    let hc' = C.clause_of_fof (Cnf.simplify hc) in
    (* miniscoping *)
    let hc' = Cnf.miniscope hc' in
    (* one step of reduction to clauses *)
    let tableau_rules = eliminate_lits hc' in
    match tableau_to_clauses hc' tableau_rules with
    | None -> clauses := hc' :: !clauses (* done with this clause *)
    | Some clauses ->
      Utils.debug 3 "@[<hov 4>@[<h>%a@]@ simplified into clauses @[<hv>%a@]@]"
                    !C.pp_clause#pp_h hc' (Utils.pp_list !C.pp_clause#pp_h) clauses;
      (* simplify recursively new clauses *)
      List.iter (fun hc -> simplify hc) clauses
  in
  simplify hc;
  Utils.exit_prof prof_elim;
  !clauses

(** Setup the environment for superposition with equivalence reasoning *)
let setup_env ~env =
  Sup.setup_env ~env;
  (* specific changes *)
  let basic_simplify' = env.Env.basic_simplify in
  env.Env.basic_simplify <-
    (fun hc -> basic_simplify' (Cnf.simplify hc));
  env.Env.list_simplify <-
    (fun hc ->
      let hc = env.Env.basic_simplify hc in
      let l = recursive_eliminations hc in
      let l = List.filter (fun hc -> not (Env.is_trivial ~env hc)) l in
      l);
  env.Env.constr <- [];
  Env.add_mk_constr env symbol_constraint;
  env.Env.preprocess <-
    (fun ~ctx l ->
      Utils.list_flatmap
        (fun hc ->
          let hc = C.update_ctx ~ctx hc in
          (* simplify the clause *)
          let hc = Sup.basic_simplify (C.clause_of_fof hc) in
          C.check_ord_hclause ~ord:env.Env.ctx.ctx_ord hc;
          let clauses = env.Env.list_simplify hc in
          List.fold_left
            (fun clauses hc ->
              let hc = C.clause_of_fof hc in
              C.check_ord_hclause ~ord:ctx.ctx_ord hc;
              (* keep only non-trivial clauses *)
              if not (Env.is_trivial ~env hc)
                then hc :: clauses
                else clauses)
            [] clauses)
        l);
  ()
