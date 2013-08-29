
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Superposition with equivalence reasoning and delayed clausal form} *)

open Logtk
open Comparison.Infix

module T = Term
module C = Clause
module O = Ordering
module S = Substs
module BV = Bitvector
module Lit = Literal
module Sup = Superposition
module PS = ProofState

let prof_elim = Util.mk_profiler "eliminate"

(** special predicate/connective symbols, in decreasing order *)
let special_preds =
  let open Symbol in
  [num_symbol; split_symbol; const_symbol; equiv_symbol; eq_symbol; imply_symbol;
   forall_symbol; exists_symbol; lambda_symbol;
   or_symbol; and_symbol; not_symbol; false_symbol; true_symbol]

let special_set =
  List.fold_left (fun set s -> Symbol.SSet.add s set) Symbol.SSet.empty special_preds

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
  | _ when Symbol.eq s Symbol.db_symbol -> DeBruijn
  | _ when Symbol.attrs_symbol s land Symbol.attr_skolem <> 0 -> Skolem
  | _ when Symbol.SSet.mem s special_set -> Special
  | _ -> (* classify between predicate and function by the sort *)
    let is_bool = try Signature.is_bool signature s with Not_found -> false in
    if is_bool then Predicate else Function

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
  | List of Literal.t list                (** list of literals, after transformation *)
  | Keep of Literal.t                     (** keep this equation unchanged *)

(** helper for alpha elimination *)
let alpha_eliminate ~ord a signa b signb =
  Alpha (List [Lit.mk_lit ~ord a T.true_term signa],
         List [Lit.mk_lit ~ord b T.true_term signb])

(** helper for beta elimination *)
let beta_eliminate ~ord a signa b signb =
  List [Lit.mk_lit ~ord a T.true_term signa;
        Lit.mk_lit ~ord b T.true_term signb]

(** helper for gamma elimination *)
let gamma_eliminate ~ctx offset t sign =
  assert (Ctx.check_term_type ~ctx t Type.o);
  let ord = Ctx.ord ctx in
  let i = !offset in
  incr offset;
  let new_t =
    if T.db_contains t 0
      then
        let ty = match T.db_type t 0 with
        | Some ty -> ty
        | None -> Type.i
        in
        let new_var = T.mk_var ~ty i in
        T.db_unlift (T.db_replace t new_var)
      else
        T.db_unlift t (* the variable is not present *)
  in
  List [Lit.mk_lit ~ord new_t T.true_term sign]

(** helper for delta elimination (remove idx-th literal from clause
    and adds t where De Bruijn 0 is replaced by a skolem
    of free variables of t) *)
let delta_eliminate ~ctx t sign =
  assert (Ctx.check_term_type ~ctx t Type.o);
  let ord = Ctx.ord ctx in
  let new_t =
    if T.db_contains t 0
      then begin
        let t' = Skolem.skolem_term ~ctx:(Ctx.skolem_ctx ~ctx) t in
        (* update types *)
        Ctx.constrain_term_term ~ctx t t';
        t'
      end else
        T.db_unlift t (* the variable is not present *)
  in
  List [Lit.mk_lit ~ord new_t T.true_term sign]

(** Just keep the equation as it is *)
let keep eqn = Keep eqn

(** perform at most one simplification on each literal. It
    returns an array of tableau_rule. *)
let eliminate_lits c =
  let ctx = c.C.hcctx in
  let ord = Ctx.ord ~ctx in
  let offset = ref ((max 0 (T.max_var c.C.hcvars)) + 1) in  (* offset to create variables *)
  (* eliminate propositions (connective and quantifier eliminations) *)
  let rec prop eqn p sign =
    assert (Ctx.check_term_type ~ctx p Type.o);
    match p.T.term with
    | T.BoundVar _ | T.Var _ -> keep eqn
    | T.Node (s, [a; b]) when Symbol.eq s Symbol.and_symbol && sign ->
      alpha_eliminate ~ord a true b true
    | T.Node (s, [a; b]) when Symbol.eq s Symbol.and_symbol && not sign ->
      beta_eliminate ~ord a false b false
    | T.Node (s, [a; b]) when Symbol.eq s Symbol.or_symbol && sign ->
      beta_eliminate ~ord a true b true
    | T.Node (s, [a; b]) when Symbol.eq s Symbol.or_symbol && not sign ->
      alpha_eliminate ~ord a false b false
    | T.Node (s, [a; b]) when Symbol.eq s Symbol.imply_symbol && sign ->
      beta_eliminate ~ord a false b true
    | T.Node (s, [a; b]) when Symbol.eq s Symbol.imply_symbol && not sign ->
      alpha_eliminate ~ord a true b false
    | T.Node (s, [a; b]) when Symbol.eq s Symbol.equiv_symbol && sign ->
      equiv eqn a b true
    | T.Node (s, [a; b]) when Symbol.eq s Symbol.equiv_symbol && not sign ->
      equiv eqn a b false
    | T.Bind (s, t) when Symbol.eq s Symbol.forall_symbol && sign ->
      gamma_eliminate ~ctx offset t true
    | T.Bind (s, t) when Symbol.eq s Symbol.forall_symbol && not sign ->
      delta_eliminate ~ctx (T.mk_not t) true
    | T.Bind (s, t) when Symbol.eq s Symbol.exists_symbol && sign ->
      delta_eliminate ~ctx t true
    | T.Bind (s, t) when Symbol.eq s Symbol.exists_symbol && not sign ->
      gamma_eliminate ~ctx offset t false
    | T.Bind _ | T.Node _ | T.At _ -> keep eqn
  (* eliminate equivalence *)
  and equiv eqn l r sign =
    match Ordering.compare ord l r with
    | Gt when sign && not (T.atomic l) -> (* l <=> r -> (l => r) & (r => l)*)
      Alpha (List [Lit.mk_neq ~ord l T.true_term; Lit.mk_eq ~ord r T.true_term],
             List [Lit.mk_neq ~ord r T.true_term; Lit.mk_eq ~ord l T.true_term])
    | Lt when sign && not (T.atomic r) ->
      Alpha (List [Lit.mk_neq ~ord l T.true_term; Lit.mk_eq ~ord r T.true_term],
             List [Lit.mk_neq ~ord r T.true_term; Lit.mk_eq ~ord l T.true_term])
    | Incomparable when sign && (not (T.atomic l) || not (T.atomic r)) ->
      Alpha (List [Lit.mk_neq ~ord l T.true_term; Lit.mk_eq ~ord r T.true_term],
             List [Lit.mk_neq ~ord r T.true_term; Lit.mk_eq ~ord l T.true_term])
    | _ when not sign -> (* not (l <=> r) -> (l | r) & (not l | not r) *)
      Alpha (List [Lit.mk_eq ~ord l T.true_term; Lit.mk_eq ~ord r T.true_term],
             List [Lit.mk_neq ~ord r T.true_term; Lit.mk_neq ~ord l T.true_term])
    | _ -> keep eqn
  in
  (* try to eliminate each literal that is eligible for resolution *)
  let tableau_rules =
    Array.map
      (fun lit -> 
        match lit with
        | Lit.Equation (l, r, sign, _) when T.eq r T.true_term -> prop lit l sign
        | Lit.Equation (l, r, sign, _) when T.eq l T.true_term -> prop lit r sign
        | Lit.Equation (l, r, sign, _) when Ctx.check_term_type ~ctx l Type.o ->
          equiv lit l r sign  (* bool *)
        | _ -> keep lit)  (* equation between terms *)
      c.C.hclits
  in
  tableau_rules

(** Produce a list of clauses from an array of tableau_rule, or None *)
let tableau_to_clauses c a =
  let ctx = c.C.hcctx in
  if Util.array_forall (function | Keep _ -> true | _ -> false) a
  then None (* just keep all literals *)
  else begin
    let clauses = ref []
    and eqns = Vector.create (Array.length a * 2) in
    let proof c' = Proof.mk_infer c' "elim" [c.C.hcproof] in
    (* explore all combinations of tableau splits *)
    let rec explore_splits i =
      if i = Array.length a
        then  (* produce new clause *)
          let parents = c :: c.C.hcparents in
          let clause = C.create_a ~parents ~ctx (Vector.to_array eqns) proof in
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

(* simplify a clause *)
let simplify_lit ~ord lit =
  match lit with
  | Lit.Equation (l, r, sign, _) ->
    let l' = Cnf.simplify l in
    let r' = Cnf.simplify r in
    Lit.mk_lit ~ord l' r' sign

(* simplify a clause *)
let simplify_clause ~ctx c =
  let ord = Ctx.ord ~ctx in
  let lits = Array.map (simplify_lit ~ord) c.C.hclits in
  if Util.array_forall2 Lit.eq c.C.hclits lits
    then c
    else begin
      let proof c' = Proof.mk_infer c' "simplify" [c.C.hcproof] in
      let new_c = C.create_a ~parents:[c] ~ctx lits proof in
      new_c
    end

(* miniscope a clause *)
let miniscope_lit ~ord lit =
  match lit with
  | Lit.Equation (l, r, sign, _) ->
    let l' = Cnf.miniscope l in
    let r' = Cnf.miniscope r in
    Lit.mk_lit ~ord l' r' sign

(* miniscope a clause *)
let miniscope_clause ~ctx c =
  let ord = Ctx.ord ~ctx in
  let lits = Array.map (miniscope_lit ~ord) c.C.hclits in
  if Util.array_forall2 Lit.eq c.C.hclits lits
    then c
    else begin
      let proof c' = Proof.mk_infer c' "miniscope" [c.C.hcproof] in
      let new_c = C.create_a ~parents:[c] ~ctx lits proof in
      new_c
    end

(** Perform eliminations recursively, until no elimination is possible *)
let recursive_eliminations c =
  Util.enter_prof prof_elim;
  let ctx = c.C.hcctx in
  let clauses = ref [] in
  (* process clauses until none of them is simplifiable *)
  let rec simplify c =
    let c' = C.clause_of_fof (simplify_clause ~ctx c) in
    (* miniscoping *)
    let c' = miniscope_clause ~ctx c' in
    (* one step of reduction to clauses *)
    let tableau_rules = eliminate_lits c' in
    match tableau_to_clauses c' tableau_rules with
    | None -> clauses := c' :: !clauses (* done with this clause *)
    | Some new_clauses ->
      Util.debug 3 "%a simplified into clauses\n\t%a" C.pp_debug c
        (Util.pp_list ~sep:"\n\t" C.pp_debug) new_clauses;
      (* simplify recursively new clauses *)
      List.iter (fun c -> simplify c) new_clauses
  in
  simplify c;
  Util.exit_prof prof_elim;
  !clauses

(** Setup the environment for superposition with equivalence reasoning *)
let setup_env ~env =
  Sup.setup_env ~env;
  (* specific changes *)
  let basic_simplify' = env.Env.basic_simplify in
  env.Env.basic_simplify <-
    (fun c -> basic_simplify' (simplify_clause ~ctx:c.C.hcctx c));
  env.Env.list_simplify <-
    (fun c ->
      let c = env.Env.basic_simplify c in
      let l = recursive_eliminations c in
      let l = List.filter (fun c -> not (Env.is_trivial ~env c)) l in
      l);
  env.Env.constr <- [];
  Env.add_mk_constr env symbol_constraint;
  env.Env.preprocess <-
    (fun ~ctx l ->
      Util.list_flatmap
        (fun c ->
          let c = C.update_ctx ~ctx c in
          (* simplify the clause *)
          let c = Sup.basic_simplify (C.clause_of_fof c) in
          C.check_ord ~ord:(Ctx.ord ctx) c;
          let clauses = env.Env.list_simplify c in
          List.fold_left
            (fun clauses c ->
              let c = C.clause_of_fof c in
              C.check_ord ~ord:(Ctx.ord ctx) c;
              (* keep only non-trivial clauses *)
              if not (Env.is_trivial ~env c)
                then c :: clauses
                else clauses)
            [] clauses)
        l);
  ()
