
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

open Logtk

module T = Term
module F = Formula
module PF = PFormula
module C = Clause
module O = Ordering
module S = Substs
module I = ProofState.TermIndex
module Lit = Literal
module Lits = Literal.Arr
module SubsumIdx = ProofState.SubsumptionIndex
module UnitIdx = ProofState.UnitIndex
module PS = ProofState
module Comp = Comparison

(* statistics *)
let stat_basic_simplify = Util.mk_stat "basic_simplify calls"
let stat_superposition_call = Util.mk_stat "superposition calls"
let stat_equality_resolution_call = Util.mk_stat "equality_resolution calls"
let stat_equality_factoring_call = Util.mk_stat "equality_factoring calls"
let stat_subsumption_call = Util.mk_stat "subsumption calls"
let stat_eq_subsumption_call = Util.mk_stat "equality subsumption calls"
let stat_subsumed_in_set_call = Util.mk_stat "subsumed_in_set calls"
let stat_subsumed_by_set_call = Util.mk_stat "subsumed_by_set calls"
let stat_demodulate_call = Util.mk_stat "demodulate calls"
let stat_demodulate_step = Util.mk_stat "demodulate steps"
let stat_splits = Util.mk_stat "splits"
let stat_semantic_tautology = Util.mk_stat "semantic_tautologies"

let prof_demodulate = Util.mk_profiler "demodulate"
let prof_back_demodulate = Util.mk_profiler "backward_demodulate"
let prof_pos_simplify_reflect = Util.mk_profiler "simplify_reflect+"
let prof_neg_simplify_reflect = Util.mk_profiler "simplify_reflect-"
let prof_clc = Util.mk_profiler "contextual_literal_cutting"
let prof_semantic_tautology = Util.mk_profiler "semantic_tautology"
let prof_condensation = Util.mk_profiler "condensation"
let prof_basic_simplify = Util.mk_profiler "basic_simplify"
let prof_subsumption = Util.mk_profiler "subsumption"
let prof_eq_subsumption = Util.mk_profiler "equality_subsumption"
let prof_subsumption_set = Util.mk_profiler "forward_subsumption"
let prof_subsumption_in_set = Util.mk_profiler "backward_subsumption"
let prof_infer_active = Util.mk_profiler "infer_active"
let prof_infer_passive = Util.mk_profiler "infer_passive"
let prof_infer_equality_resolution = Util.mk_profiler "infer_equality_resolution"
let prof_infer_equality_factoring = Util.mk_profiler "infer_equality_factoring"
let prof_split = Util.mk_profiler "infer_split"

(* ----------------------------------------------------------------------
 * inferences
 * ---------------------------------------------------------------------- *)

(* Helper that does one or zero superposition inference, with all
   the given parameters. Clauses have an scope. *)
let do_superposition ~ctx (active_clause, sc_a) active_pos
                          (passive_clause, sc_p) passive_pos subst acc =
  let ord = Ctx.ord ctx in
  assert (List.length active_pos = 2);
  Util.incr_stat stat_superposition_call;
  match passive_pos with
  | [] | _::[] -> assert false
  | passive_idx::passive_side::subterm_pos ->
  let active_idx = List.hd active_pos
  and u, v, sign_uv = Lits.get_eqn passive_clause.C.hclits [passive_idx; passive_side]
  and s, t, sign_st = Lits.get_eqn active_clause.C.hclits active_pos in
  Util.debug 3 ("sup %a s=%a t=%a \n%a u=%a v=%a p=%a subst=%a")
                C.pp active_clause T.pp s T.pp t
                C.pp passive_clause T.pp u T.pp v
                Position.pp passive_pos S.pp subst;
  if not (T.db_closed s)
  then (Util.debug 3 "... active term is not DB-closed"; acc)
  else if not sign_st 
  then (Util.debug 3 "... active literal is negative"; acc)
  else if not (T.atomic s) (* do not rewrite non-atomic formulas *)
  then (Util.debug 3 "... active term is not atomic or DB-closed"; acc)
  else if not (T.db_closed (T.at_pos u subterm_pos))
    && (List.exists (fun x -> S.is_in_subst subst x sc_p) (T.vars (T.at_pos u subterm_pos)))
  then (Util.debug 3 "... narrowing with De Bruijn indices"; acc)
  else if not (Ctx.check_term_term ~ctx s (T.at_pos u subterm_pos))
  then (Util.debug 3 "... incompatible types"; acc)
  else
  let renaming = Ctx.renaming_clear ~ctx in
  let t' = S.apply ~renaming subst t sc_a
  and v' = S.apply ~renaming subst v sc_p in
  if sign_uv && t' == v' && subterm_pos = []
  then (Util.debug 3 "... will yield a tautology"; acc)
  else begin
    if (O.compare ord (S.apply ~renaming subst s sc_a) t' = Comp.Lt ||
        O.compare ord (S.apply ~renaming subst u sc_p) v' = Comp.Lt ||
        not (BV.get (C.eligible_res passive_clause sc_p subst) passive_idx) ||
        not (BV.get (C.eligible_param active_clause sc_a subst) active_idx))
      then (Util.debug 3 "... has bad ordering conditions"; acc)
      else begin (* ordering constraints are ok *)
        let lits_a = Util.array_except_idx active_clause.C.hclits active_idx in
        let lits_p = Util.array_except_idx passive_clause.C.hclits passive_idx in
        (* replace s\sigma by t\sigma in u|_p\sigma *)
        let u' = S.apply ~renaming subst u sc_p in
        let new_u = T.replace_pos u' subterm_pos t' in
        (* apply substitution to other literals *)
        let new_lits =
          Lit.mk_lit ~ord new_u v' sign_uv :: 
          (Lit.apply_subst_list ~renaming ~ord subst lits_a sc_a) @
          (Lit.apply_subst_list ~renaming ~ord subst lits_p sc_p)
        in
        let rule = if sign_uv then "sup+" else "sup-" in
        let proof c = Proof.mk_c_step c rule
          [active_clause.C.hcproof; passive_clause.C.hcproof] in
        let parents = [active_clause; passive_clause] in
        let new_clause = C.create ~parents ~ctx new_lits proof in
        Util.debug 3 "... ok, conclusion %a" C.pp new_clause;
        new_clause :: acc
      end
  end

let infer_active (actives : ProofState.ActiveSet.t) clause =
  Util.enter_prof prof_infer_active;
  let ctx = actives#ctx in
  let scope = T.max_var clause.C.hcvars + 1 in
  (* no literal can be eligible for paramodulation if some are selected.
     This checks if inferences with i-th literal are needed? *)
  let eligible =
    let bv = C.eligible_param clause 0 S.empty in
    fun i lit -> BV.get bv i
  in
  (* do the inferences where clause is active; for this,
     we try to rewrite conditionally other clauses using
     non-minimal sides of every positive literal *)
  let new_clauses = Lits.fold_eqn ~both:true ~eligible clause.C.hclits []
    (fun acc s t _ s_pos ->
      (* rewrite clauses using s *)
      I.retrieve_unifiables actives#idx_sup_into scope s 0 acc
        (fun acc u_p with_pos subst ->
          (* rewrite u_p with s *)
          let passive = with_pos.C.WithPos.clause in
          let u_pos = with_pos.C.WithPos.pos in
          do_superposition ~ctx (clause, 0) s_pos (passive, scope) u_pos subst acc))
  in
  Util.exit_prof prof_infer_active;
  new_clauses

let infer_passive (actives:ProofState.ActiveSet.t) clause =
  Util.enter_prof prof_infer_passive;
  let ctx = actives#ctx in
  let scope = T.max_var clause.C.hcvars + 1 in
  (* perform inference on this lit? *)
  let eligible =
    let bv = C.eligible_res clause 0 S.empty in
    fun i lit -> BV.get bv i
  in
  (* do the inferences in which clause is passive (rewritten),
     so we consider both negative and positive literals *)
  let new_clauses = Lits.fold_eqn ~both:true ~eligible clause.C.hclits []
    (fun acc u v _ u_pos ->
      (* rewrite subterms of u *)
      T.all_positions ~pos:u_pos u acc
        (fun acc u_p p ->
          (* all terms that occur in an equation in the active_set
             and that are potentially unifiable with u_p (u at position p) *)
          I.retrieve_unifiables actives#idx_sup_from scope u_p 0 acc
            (fun acc s with_pos subst ->
              let active = with_pos.C.WithPos.clause in
              let s_pos = with_pos.C.WithPos.pos in
              do_superposition ~ctx (active, scope) s_pos (clause, 0) p subst acc)))
  in
  Util.exit_prof prof_infer_passive;
  new_clauses

let infer_equality_resolution clause =
  Util.enter_prof prof_infer_equality_resolution;
  let ctx = clause.C.hcctx in
  (* literals that can potentially be eligible for resolution *)
  let eligible =
    let bv = C.eligible_res clause 0 S.empty in
    fun i lit -> Lit.is_neg lit && BV.get bv i
  in
  (* iterate on those literals *)
  let new_clauses = Lits.fold_eqn ~both:false ~eligible clause.C.hclits []
    (fun acc l r sign l_pos ->
      assert (not sign);
      match l_pos with
      | [] -> assert false
      | pos::_ ->
      try
        let subst = Unif.unification l 0 r 0 in
        if BV.get (C.eligible_res clause 0 subst) pos
          (* subst(lit) is maximal, we can do the inference *)
          then begin
            Util.incr_stat stat_equality_resolution_call;
            let ord = Ctx.ord ~ctx in
            let renaming = Ctx.renaming_clear ~ctx in
            let proof c = Proof.mk_c_step c "eq_res" [clause.C.hcproof] in
            let new_lits = Util.array_except_idx clause.C.hclits pos in
            let new_lits = Lit.apply_subst_list ~ord ~renaming subst new_lits 0 in
            let new_clause = C.create ~parents:[clause] ~ctx new_lits proof in
            Util.debug 3 "equality resolution on %a yields %a"
              C.pp clause C.pp new_clause;
            new_clause::acc
          end else
            acc
      with Unif.Fail -> acc) (* l and r not unifiable, try next *)
  in
  Util.exit_prof prof_infer_equality_resolution;
  new_clauses

let infer_equality_factoring clause =
  Util.enter_prof prof_infer_equality_factoring;
  let ctx = clause.C.hcctx in
  let ord = Ctx.ord ctx in
  (* is the literal eligible for paramodulation? *)
  let eligible =
    let bv = C.eligible_param clause 0 S.empty in
    fun i lit -> BV.get bv i
  in
  (* find root terms that are unifiable with s and are not in the
     literal at s_pos. This returns a list of position and substitution *)
  let find_unifiable_lits s s_pos =
    Util.array_foldi
      (fun acc i lit ->
        match lit with
        | Lit.Equation (_, _, false, _)
        | Lit.Prop (_, false)
        | Lit.True
        | Lit.False -> acc
        | _ when List.hd s_pos = i -> acc (* same index *) 
        | Lit.Prop (p, true) ->
          begin try
            let subst = Unif.unification s 0 p 0 in
            ([i; Position.left_pos], subst) :: acc
          with Unif.Fail -> acc
          end
        | Lit.Equation (u, v, true, _) ->
          let try_u =  (* try inference between s and u *)
            try
              let subst = Unif.unification s 0 u 0 in
              [[i; Position.left_pos], subst]
            with Unif.Fail -> []
          and try_v =  (* try inference between s and v *)
            try
              let subst = Unif.unification s 0 v 0 in
              [[i; Position.right_pos], subst]
            with Unif.Fail -> []
          in try_u @ try_v @ acc)
      [] clause.C.hclits
  (* do the inference between given positions, if ordering
     conditions are respected *)
  and do_inference active_pos passive_pos subst =
    let s, t, sign_st = Lits.get_eqn clause.C.hclits active_pos
    and u, v, sign_uv = Lits.get_eqn clause.C.hclits passive_pos
    and active_idx = List.hd active_pos in
    assert (sign_st && sign_uv);
    (* check whether subst(lit) is maximal, and not (subst(s) < subst(t)) *)
    let renaming = Ctx.renaming_clear ~ctx in
    if O.compare ord  (S.apply ~renaming subst s 0)
                      (S.apply ~renaming subst t 0) <> Comp.Lt &&
       BV.get (C.eligible_param clause 0 subst) active_idx
      then begin
        Util.incr_stat stat_equality_factoring_call;
        let proof c = Proof.mk_c_step c "eq_fact" [clause.C.hcproof]
        (* new_lits: literals of the new clause. remove active literal
           and replace it by a t!=v one, and apply subst *)
        and new_lits = Util.array_except_idx clause.C.hclits active_idx in
        let new_lits = (Lit.mk_neq ~ord t v) :: new_lits in
        let renaming = Ctx.renaming_clear ~ctx in
        let new_lits = Lit.apply_subst_list ~renaming ~ord subst new_lits 0 in
        let new_clause = C.create ~parents:[clause] ~ctx new_lits proof in
        Util.debug 3 "equality factoring on %a yields %a"
          C.pp clause C.pp new_clause;
        [new_clause]
      end else
        []
  (* try to do inferences with each positive literal *)
  in
  let new_clauses = Lits.fold_eqn ~both:true ~eligible clause.C.hclits []
    (fun acc s t _ s_pos -> (* try with s=t *)
      let unifiables = find_unifiable_lits s s_pos in
      List.fold_left
        (fun acc (passive_pos, subst) ->
          (do_inference s_pos passive_pos subst) @ acc)
        acc unifiables)
  in
  Util.exit_prof prof_infer_equality_factoring;
  new_clauses

let split_count = ref 0
let split_limit = ref 100

(* union-find that maps terms to list of literals *)
module UF = UnionFind.Make(struct
  type key = Term.t
  type value = Lit.t list
  let equal = (==)
  let hash t = t.T.tag
  let zero = []
  let merge = List.rev_append
end)

(** Hyper-splitting *)
let infer_split c =
  let ctx = c.C.hcctx in
  (* only do splitting on large clauses *)
  if Array.length c.C.hclits < 4 || !split_count >= !split_limit then [] else begin
  Util.enter_prof prof_split;
  (* get a fresh split symbol *)
  let rec next_split_term () = 
    let s = "$$split_" ^ (string_of_int !split_count) in
    incr split_count;
    T.mk_const (Symbol.mk_const ~attrs:Symbol.attr_split s)
  in
  (* is the term made of a split symbol? *)
  let is_split_term t = match t.T.term with
  | T.Node (s, []) when Symbol.has_attr Symbol.attr_split s -> true
  | _ -> false
  in
  (* literals that are ground, or split symbols *)
  let branch = ref [] in
  (* maps variables to a list of literals *)
  let cluster = UF.create c.C.hcvars in
  (* for each literal, merge the list of all variables occurring in
     the literal *)
  Array.iter
    (fun lit ->
      match Lit.vars lit with
      | [] -> ()
      | x::vars' -> List.iter (fun y -> UF.union cluster x y) vars')
    c.C.hclits;
  (* Divide clause into components (that do not share variables and do not contain
     any split symbol), and a remaining (branch) part. Ground terms go in the "branch" part.
     [components] is a list of (vars, literal list ref), *)
  let rec find_components lits i =
    if i = Array.length lits then () else begin
      match lits.(i) with
      | Lit.Prop (p, _) when is_split_term p ->
        branch := lits.(i) :: !branch;
        find_components lits (i+1)  (* branch part *)
      | Lit.Equation (l, r, _, _) when T.is_ground l && T.is_ground r ->
        branch := lits.(i) :: !branch;
        find_components lits (i+1)  (* branch part *)
      | Lit.Equation (l, r, _, _) ->
        (* find which component this literal belongs to *)
        let vars = T.vars_list [l;r] in
        assert (vars <> []);
        let x = List.hd vars in
        (* Add lit to the list of lits for the given variable. All variables
           of the lit have the same representative in components. *)
        UF.add cluster x [lits.(i)];
        find_components lits (i+1)
      | Lit.Prop (p, _) when T.is_ground p ->
        branch := lits.(i) :: !branch;
        find_components lits (i+1)  (* branch part *)
      | Lit.Prop (p, _) ->
        let vars = T.vars p in
        let x = List.hd vars in
        UF.add cluster x [lits.(i)];
        find_components lits (i+1)
      | Lit.True
      | Lit.False -> find_components lits (i+1)
    end
  in
  find_components c.C.hclits 0;
  let components = ref [] in
  UF.iter cluster (fun _ l ->
    Util.debug 4 "component %a" (Util.pp_list Lit.pp) l;
    components := l :: !components);
  let n = List.length !components in
  if n > 1 && List.for_all (fun l -> List.length l >= 2) !components then begin
    (* Do the split. But only because we have several components, that contain several
       literals each. *)
    Util.incr_stat stat_splits;
    (* create a list of symbols *)
    let symbols = Util.times (n-1) next_split_term in
    let proof c' = Proof.mk_c_step c' "split" [c.C.hcproof] in
    (* the guard clause, plus the first component, plus all negated split symbols *)
    let guard =
      let lits = List.map Lit.mk_false symbols
               @ List.hd !components @ !branch in
      C.create ~parents:[c] ~ctx lits proof
    in
    (* one new clause for each other component *)
    let new_clauses = List.map2
      (fun component split_symbol ->
        let split_lit = Lit.mk_true split_symbol in
        let lits = split_lit :: (component @ !branch) in
        C.create ~parents:[c] ~ctx lits proof)
      (List.tl !components) symbols
    in
    let new_clauses = guard :: new_clauses in
    Util.debug 3 "split on %a yields %a" C.pp c
      (Util.pp_list C.pp) new_clauses;
    Util.exit_prof prof_split;
    new_clauses
  end else (Util.exit_prof prof_split; [])
  end

(* ----------------------------------------------------------------------
 * simplifications
 * ---------------------------------------------------------------------- *)

exception RewriteInto of Term.t * Substs.t

(** Compute normal form of term w.r.t active set. Clauses used to
    rewrite are added to the clauses hashset.
    restrict is an option for restricting demodulation in positive maximal terms *)
let demod_nf ?(restrict=false) (simpl_set : PS.SimplSet.t) clauses t =
  let ord = Ctx.ord simpl_set#ctx in
  (* compute normal form of subterm. If restrict is true, substitutions that
     are variable renamings are forbidden (since we are at root of a max term) *) 
  let rec normal_form ~restrict t =
    (* do not rewrite non-atomic formulas *)
    if not (T.atomic t)
      then t  (* do not rewrite such formulas *)
      else begin
        (* find equations l=r that match subterm *)
        try
          UnitIdx.retrieve ~sign:true simpl_set#idx_simpl 1 t 0 ()
            (fun () l r (_,_,_,unit_clause) subst ->
              (* r is the term subterm is going to be rewritten into *)
              assert (C.is_unit_clause unit_clause);
              if (not restrict || not (S.is_renaming subst))
              && (C.is_oriented_rule unit_clause ||
                 O.compare ord (S.apply_no_renaming subst l 1) (S.apply_no_renaming subst r 1) = Comp.Gt)
                (* subst(l) > subst(r) and restriction does not apply, we can rewrite *)
                then begin
                  assert (O.compare ord (S.apply_no_renaming subst l 1) (S.apply_no_renaming subst r 1) = Comp.Gt);
                  clauses := unit_clause :: !clauses;
                  Util.incr_stat stat_demodulate_step;
                  raise (RewriteInto (r, subst))
                end);
          t (* not found any match, normal form found *)
        with RewriteInto (t', subst) ->
          traverse ~restrict subst t' 1 (* done one rewriting step, continue *)
      end
  (* rewrite innermost-leftmost of [subst(t,scope)]. The initial scope is
     0, but then we traverse terms in which variables are really the variables
     of the RHS of a previously applied rule (in context 1); all those
     variables are bound to terms in context 0 *)
  and traverse ~restrict subst t scope =
    match t.T.term with
    | T.Var _ -> S.apply_no_renaming subst t scope
    | T.BoundVar _ -> t
    | T.Bind (s, t') ->
      let t'' = traverse ~restrict subst t' scope in
      let new_t = T.mk_bind s t'' in
      (* rewrite term at root *)
      normal_form ~restrict new_t
    | T.Node (s, l) ->
      (* rewrite subterms *)
      let l' = List.map (fun t' -> traverse ~restrict:false subst t' scope) l in
      let t' = if List.for_all2 (==) l l'
        then t
        else T.mk_node s l' in
      (* rewrite term at root *)
      normal_form ~restrict t'
    | T.At (t1, t2) ->
      let t1' = traverse ~restrict:false subst t1 scope in
      let t2' = traverse ~restrict:false subst t2 scope in
      let t' = T.mk_at t1' t2' in
      normal_form ~restrict t'
  in
  traverse ~restrict S.empty t 0

(** Demodulate the clause, with restrictions on which terms to rewrite *)
let demodulate (simpl_set : PS.SimplSet.t) c =
  Util.enter_prof prof_demodulate;
  Util.incr_stat stat_demodulate_call;
  let ctx = c.C.hcctx in
  let ord = Ctx.ord ctx in
  (* clauses used to rewrite *)
  let clauses = ref [] in
  (* literals that are eligible for resolution *)
  let eligible_res = C.eligible_res c 0 S.empty in
  (* demodulate literals *)
  let demod_lit i lit =
    match lit with
    | Lit.Equation (l, r, false, _) ->
      Lit.mk_neq ~ord
        (demod_nf simpl_set clauses l)
        (demod_nf simpl_set clauses r)
    | Lit.True
    | Lit.False -> lit
    | Lit.Prop (p, true) when BV.get eligible_res i ->
      Lit.mk_true (demod_nf ~restrict:true simpl_set clauses p)
    | Lit.Prop (p, sign) ->
      Lit.mk_prop (demod_nf simpl_set clauses p) sign
    | Lit.Equation (l, r, true, Comp.Gt) when BV.get eligible_res i ->
      Lit.mk_eq ~ord
        (demod_nf ~restrict:true simpl_set clauses l)
        (demod_nf simpl_set clauses r)
    | Lit.Equation (l, r, true, Comp.Lt) when BV.get eligible_res i ->
      Lit.mk_eq ~ord
        (demod_nf simpl_set clauses l)
        (demod_nf ~restrict:true simpl_set clauses r)
    | Lit.Equation (l, r, true, _) ->
      Lit.mk_eq ~ord
        (demod_nf simpl_set clauses l)
        (demod_nf simpl_set clauses r)
  in
  (* demodulate every literal *)
  let lits = Array.mapi demod_lit c.C.hclits in
  if !clauses = []
    then (* no rewriting performed *)
      let _ = Util.exit_prof prof_demodulate in
      c
    else begin  (* construct new clause *)
      let proof c' = Proof.mk_c_step c' "demod"
        (c.C.hcproof :: List.map (fun hc -> hc.C.hcproof) !clauses) in
      let parents = c :: c.C.hcparents in
      let new_c = C.create_a ~parents ~ctx lits proof in
      Util.debug 3 "demodulate %a into %a using\n %a"
        C.pp c C.pp new_c (Util.pp_list C.pp) !clauses;
      (* return simplified clause *)
      Util.exit_prof prof_demodulate;
      new_c
    end

(** Find clauses that [given] may demodulate, add them to set *)
let backward_demodulate (active_set : PS.ActiveSet.t) set given =
  Util.enter_prof prof_back_demodulate;
  let ctx = given.C.hcctx in
  let ord = Ctx.ord ctx in
  let renaming = Ctx.renaming_clear ~ctx in
  let scope = T.max_var given.C.hcvars + 1 in
  (* find clauses that might be rewritten by l -> r *)
  let recurse ~oriented set l r =
    I.retrieve_specializations active_set#idx_back_demod scope l 0 set
      (fun set t' with_pos subst ->
        let c = with_pos.C.WithPos.clause in
        (* subst(l) matches t' and is > subst(r), very likely to rewrite! *)
        if oriented || O.compare ord
        (S.apply ~renaming subst l 0) (S.apply ~renaming subst r 0) = Comp.Gt
          then  (* add the clause to the set, it may be rewritten by l -> r *)
            C.CSet.add set c
          else set)
  in
  let set' = match given.C.hclits with
  | [|Lit.Equation (l,r,true,Comp.Gt)|] -> recurse ~oriented:true set l r
  | [|Lit.Equation (l,r,true,Comp.Lt)|] -> recurse ~oriented:true set r l
  | [|Lit.Equation (l,r,true,_)|] ->
    let set' = recurse ~oriented:false set l r in
    recurse ~oriented:false set' r l  (* both sides can rewrite, but we
                                         need to check ordering *)
  | _ -> set
  in
  Util.exit_prof prof_back_demodulate;
  set'

let is_tautology c =
  let rec check lits i =
    if i = Array.length lits then false
    else match lits.(i) with
    | Lit.True -> true
    | Lit.False -> check lits (i+1)
    | Lit.Prop (p, sign) ->
      Util.array_exists
        (function
          | Lit.Prop (p', sign') ->
            sign = (not sign') && T.eq p p'
          | _ -> false)
        lits
      || check lits (i+1)
    | Lit.Equation (l, r, true, _) when l == r -> true
    | Lit.Equation (l, r, sign, _) ->
      Util.array_exists
        (function
          | Lit.Equation (l', r', sign', _) ->
            sign = (not sign') &&
            ((T.eq l l' && T.eq r r') || (T.eq l r' && T.eq l' r))
          | _ -> false)
        lits
      || check lits (i+1)
  in
  let is_tauto = check c.C.hclits 0 in
  (if is_tauto then Util.debug 3 "%a is a tautology" C.pp c);
  is_tauto

(* TODO: create CC on terms here? This function is seldom used...
(** semantic tautology deletion, using a congruence closure algorithm
    to see if negative literals imply some positive literal *)
let is_semantic_tautology c =
  Util.enter_prof prof_semantic_tautology;
  (* create the congruence closure of all negative equations of [c] *)
  let cc = T.TCC.create 5 in
  let cc =
    Array.fold_left
      (fun cc lit -> match lit with
        | Lit.Equation (l, r, false, _) ->
          T.TCC.merge cc (T.cc_curry l) (T.cc_curry r)
        | _ -> cc)
      cc c.C.hclits in
  let res =
    Util.array_exists
      (function
        | Lit.Equation (_, _, false, _) -> false
        | Lit.Equation (l, r, true, _) ->
          (* if l=r is implied by the congruence, then the clause is redundant *)
          T.TCC.eq cc (T.cc_curry l) (T.cc_curry r))
      c.C.hclits
  in
  (if res then begin
    Util.incr_stat stat_semantic_tautology;
    Util.debug 2 "@[<h>%a is a semantic tautology@]" !C.pp_clause#pp_h c;
    end);
  Util.exit_prof prof_semantic_tautology;
  res
*)

let basic_simplify c =
  Util.enter_prof prof_basic_simplify;
  let ctx = c.C.hcctx in
  let ord = Ctx.ord ctx in
  Util.incr_stat stat_basic_simplify;
  let absurd_lit lit = match lit with
  | Lit.Equation (l, r, false, _) when T.eq l r -> true
  | Lit.Equation (l, r, true, _)
    when (l == T.true_term && r == T.false_term)
      || (l == T.false_term && r == T.true_term) -> true
  | Lit.Prop (p, false) when T.eq p T.true_term -> true
  | Lit.Prop (p, true) when T.eq p T.false_term -> true
  | Lit.False -> true
  | _ -> false
  in
  let lits = Array.to_list c.C.hclits in
  (* remove s!=s literals *)
  let new_lits = List.filter (fun lit -> not (absurd_lit lit)) lits in
  (* remove duplicate literals *)
  let new_lits = Util.list_uniq Lit.eq_com new_lits in
  (* destructive equality resolution *)
  let rec er lits =
    match Util.list_find er_check lits with
    | Some (i, Lit.Equation (l, r, sign, _)) ->
        assert (not sign);
        assert (T.is_var l || T.is_var r);
        begin try
          let subst = Unif.unification l 0 r 0 in
          let renaming = Ctx.renaming_clear ~ctx in
          (* remove the literal, and apply the substitution to the remaining literals
             before trying to find another x!=t *)
          er (Lit.apply_subst_list ~ord ~renaming subst (Util.list_remove lits i) 0)
        with Unif.Fail -> lits
        end
    | None -> lits
    | _ -> assert false
  (* finds candidate literals for destructive ER (lits with >= 1 variable) *)
  and er_check = function
    | Lit.Equation (l, r, sign, _) -> (not sign) && (T.is_var l || T.is_var r)
    | _ -> false
  in
  let new_lits = er new_lits in
  if List.length new_lits = Array.length c.C.hclits
  then (Util.exit_prof prof_basic_simplify; c) (* no change *)
  else begin
    let proof = Proof.adapt_c c.C.hcproof in  (* do not bother printing this *)
    let parents = c :: c.C.hcparents in
    let new_clause = C.create ~parents ~ctx new_lits proof in
    Util.debug 3 "%a basic_simplifies into %a" C.pp c C.pp new_clause;
    Util.exit_prof prof_basic_simplify;
    new_clause
  end

exception FoundMatch of Term.t * Clause.t * Substs.t

let positive_simplify_reflect (simpl_set : PS.SimplSet.t) c =
  Util.enter_prof prof_pos_simplify_reflect;
  let ctx = c.C.hcctx in
  let scope = T.max_var c.C.hcvars + 1 in
  (* iterate through literals and try to resolve negative ones *)
  let rec iterate_lits acc lits clauses = match lits with
  | [] -> List.rev acc, clauses
  | (Lit.Equation (s, t, false, _) as lit)::lits' ->
    begin match equatable_terms clauses s t with
    | None -> (* keep literal *)
      iterate_lits (lit::acc) lits' clauses
    | Some new_clauses -> (* drop literal, remember clauses *)
      iterate_lits acc lits' new_clauses
    end
  | lit::lits' -> iterate_lits (lit::acc) lits' clauses
  (** try to make the terms equal using some positive unit clauses
      from active_set *)
  and equatable_terms clauses t1 t2 =
    match t1.T.term, t2.T.term with
    | _ when T.eq t1 t2 -> Some clauses  (* trivial *)
    | T.Node (f, ss), T.Node (g, ts)
    when Symbol.eq f g && List.length ss = List.length ts ->
      (* try to make the terms equal directly *)
      (match equate_root clauses t1 t2 with
      | None -> (* otherwise try to make subterms pairwise equal *)
        let ok, clauses = List.fold_left2
          (fun (ok, clauses) t1' t2' ->
            if ok
              then match equatable_terms clauses t1' t2' with
              | None -> false, []
              | Some clauses -> true, clauses
              else false, [])
          (true, clauses) ss ts
        in
        if ok then Some clauses else None
      | Some clauses -> Some clauses)
    | _ -> equate_root clauses t1 t2 (* try to solve it with a unit equality *)
  (** try to equate terms with a positive unit clause that match them *)
  and equate_root clauses t1 t2 =
    try UnitIdx.retrieve ~sign:true simpl_set#idx_simpl scope t1 0 ()
      (fun () l r (_,_,_,c') subst ->
        let renaming = Ctx.renaming_clear ~ctx in
        if t2 == S.apply ~renaming subst r scope
        then begin  (* t1!=t2 is refuted by l\sigma = r\sigma *)
          Util.debug 4 "equate %a and %a using %a" T.pp t1 T.pp t2 C.pp c';
          raise (FoundMatch (r, c', subst)) (* success *)
        end else ());
      None (* no match *)
    with FoundMatch (r, c', subst) ->
      Some (c'.C.hcproof :: clauses)  (* success *)
  in
  (* fold over literals *)
  let lits, premises = iterate_lits [] (Array.to_list c.C.hclits) [] in
  if List.length lits = Array.length c.C.hclits
    then (Util.exit_prof prof_pos_simplify_reflect; c) (* no literal removed, keep c *)
    else 
      let proof c' = Proof.mk_c_step c' "simplify_reflect+" (c.C.hcproof::premises) in
      let parents = c :: c.C.hcparents in
      let new_c = C.create ~parents ~ctx lits proof in
      Util.debug 3 "%a pos_simplify_reflect into %a" C.pp c C.pp new_c;
      Util.exit_prof prof_pos_simplify_reflect;
      new_c
    
let negative_simplify_reflect (simpl_set : PS.SimplSet.t) c =
  Util.enter_prof prof_neg_simplify_reflect;
  let ctx = c.C.hcctx in
  let scope = T.max_var c.C.hcvars + 1 in
  (* iterate through literals and try to resolve positive ones *)
  let rec iterate_lits acc lits clauses = match lits with
  | [] -> List.rev acc, clauses
  | (Lit.Equation (s, t, true, _) as lit)::lits' ->
    begin match can_refute s t, can_refute t s with
    | None, None -> (* keep literal *)
      iterate_lits (lit::acc) lits' clauses
    | Some new_clause, _ | _, Some new_clause -> (* drop literal, remember clause *)
      iterate_lits acc lits' (new_clause :: clauses)
    end
  | lit::lits' -> iterate_lits (lit::acc) lits' clauses
  (** try to remove the literal using a negative unit clause *)
  and can_refute s t =
    try UnitIdx.retrieve ~sign:false simpl_set#idx_simpl scope s 0 ()
      (fun () l r (_,_,_,c') subst ->
        let renaming = Ctx.renaming_clear ~ctx in
        if t == S.apply ~renaming subst r scope
        then begin
          Util.debug 3 "neg_reflect eliminates %a=%a with %a" T.pp s T.pp t C.pp c';
          raise (FoundMatch (r, c', subst)) (* success *)
        end else ());
      None (* no match *)
    with FoundMatch (r, c', subst) ->
      Some c'.C.hcproof  (* success *)
  in
  (* fold over literals *)
  let lits, premises = iterate_lits [] (Array.to_list c.C.hclits) [] in
  if List.length lits = Array.length c.C.hclits
    then (Util.exit_prof prof_neg_simplify_reflect; c) (* no literal removed *)
    else 
      let proof c' = Proof.mk_c_step c' "simplify_reflect-" (c.C.hcproof::premises) in
      let parents = c :: c.C.hcparents in
      let new_c = C.create ~parents ~ctx lits proof in
      Util.debug 3 "%a neg_simplify_reflect into %a" C.pp c C.pp new_c;
      Util.exit_prof prof_neg_simplify_reflect;
      new_c

(* ----------------------------------------------------------------------
 * subsumption
 * ---------------------------------------------------------------------- *)

(** raised when a subsuming substitution is found *)
exception SubsumptionFound of Substs.t

(** checks whether subst(lit_a) subsumes subst(lit_b). Returns a list of
    substitutions s such that s(lit_a) = lit_b and s contains subst. The list
    is empty if lit_a does not subsume lit_b. *)
let match_lits subst lit_a sc_a lit_b sc_b =
  (* match t1 with t2, then t1' with t2' *)
  let match4 subst t1 t2 t1' t2' =
    try let subst = Unif.matching ~subst t1 sc_a t2 sc_b in
        [Unif.matching ~subst t1' sc_a t2' sc_b]
    with Unif.Fail -> []
  and match2 subst t1 sc1 t2 sc2 =
    try [Unif.matching ~subst t1 sc1 t2 sc2] with Unif.Fail -> []
  in
  match lit_a, lit_b with
  | Lit.Prop (pa, signa), Lit.Prop (pb, signb) ->
    if signa = signb
      then match2 subst pa sc_a pb sc_b
      else []
  | Lit.Equation (_, _, signa, _), Lit.Equation (_, _, signb, _)
    when signa <> signb -> [] (* different sign *)
  | Lit.Equation (la, ra, _, Comp.Lt), Lit.Equation (lb, rb, _, Comp.Lt)
  | Lit.Equation (la, ra, _, Comp.Gt), Lit.Equation (lb, rb, _, Comp.Gt) -> (* use monotonicity *)
    match4 subst la lb ra rb
  | Lit.Equation (la, ra, _, Comp.Gt), Lit.Equation (lb, rb, _, Comp.Lt)
  | Lit.Equation (la, ra, _, Comp.Lt), Lit.Equation (lb, rb, _, Comp.Gt) -> (* use monotonicity *)
    match4 subst la rb ra lb
  | Lit.Equation (la, ra, _, _), Lit.Equation (lb, rb, _, _) -> (* general case *)
    (match4 subst la lb ra rb) @ (match4 subst la rb ra lb)
  | Lit.True, Lit.True
  | Lit.False, Lit.False -> [subst]
  | _ -> []

(** check that every literal in a matches at least one literal in b *)
let all_lits_match a sc_a b sc_b =
  Util.array_forall
    (fun lita ->
      Util.array_exists
        (fun litb -> match_lits S.empty lita sc_a litb sc_b <> [])
        b)
    a

(** Compare literals by subsumption difficulty (see "towards efficient subsumption", Tammet).
    We sort by increasing order, so non-ground, deep, heavy literals are smaller
    (thus tested early) *)
let compare_literals_subsumption lita litb =
  (* ground literal is bigger *)
  if Lit.is_ground lita && not (Lit.is_ground litb) then 1
  else if not (Lit.is_ground lita) && Lit.is_ground litb then -1
  (* deep literal is smaller *)
  else let deptha, depthb = Lit.depth lita, Lit.depth litb in 
  if deptha <> depthb then depthb - deptha
  (* heavy literal is smaller *)
  else if Lit.weight lita <> Lit.weight litb
  then Lit.weight litb - Lit.weight lita
  else 0

(** Check whether [a] subsumes [b], and if it does, return the
    corresponding substitution *)
let subsumes_with a sc_a b sc_b =
  Util.incr_stat stat_subsumption_call;
  (* a must not have more literals *)
  if Array.length a > Array.length b then None else
  (* variables that cannot be bound during subsumption *)
  if not (all_lits_match a sc_a b sc_b) then None else
  (* sort a copy of [a] by decreasing difficulty *)
  let a = Array.copy a in
  (* try to subsumes literals of b whose index are not in bv, with [subst] *)
  let rec try_permutations i subst bv =
    if i = Array.length a then raise (SubsumptionFound subst) else
    let lita = a.(i) in
    find_matched lita i subst bv 0
  (* find literals of b that are not bv and that are matched by lita *)
  and find_matched lita i subst bv j =
    if j = Array.length b then ()
    (* if litb is already matched, continue *)
    else if BV.get bv j then find_matched lita i subst bv (j+1) else begin
    let litb = b.(j) in
    (* match lita and litb, then flag litb as used, and try with next literal of a *)
    let substs = match_lits subst lita sc_a litb sc_b in
    BV.set bv j;
    List.iter (fun subst' -> try_permutations (i+1) subst' bv) substs;
    BV.reset bv j;
    (* some variable of lita occur in a[j+1...], try another literal of b *)
    if substs <> [] && not (check_vars lita (i+1))
      then () (* no backtracking for litb *)
      else find_matched lita i subst bv (j+1)
    end
  (* does some literal in a[j...] contain a variable in l or r? *)
  and check_vars lit j =
    let vars = Lit.vars lit in
    if vars = []
      then false
      else
        try
          for k = j to Array.length a - 1 do
            if List.exists (fun v -> Lit.var_occurs v a.(k)) vars
              then raise Exit
          done;
          false
        with Exit -> true
  in
  try
    Array.sort compare_literals_subsumption a;
    let bv = BV.empty () in
    try_permutations 0 S.empty bv;
    None
  with (SubsumptionFound subst) -> Some subst

let subsumes a b =
  Util.enter_prof prof_subsumption;
  let res = match subsumes_with a 0 b 1 with
  | None -> false
  | Some _ ->
    Util.debug 2 "%a subsumes %a" Lits.pp a Lits.pp b;
    true
  in
  Util.debug 2 "%a subsumes %a" Lits.pp a Lits.pp b;
  Util.exit_prof prof_subsumption;
  res

let eq_subsumes a b =
  (* subsume a literal using a = b *)
  let rec equate_lit_with a b lit =
    match lit with
    | Lit.Equation (u, v, true, _) -> equate_terms a b u v
    | _ -> false
  (* make u and v equal using a = b (possibly several times) *)
  and equate_terms a b u v =
    match u.T.term, v.T.term with
    | _ when u == v -> true 
    | _ when equate_root a b u v -> true
    | T.Node (f, ss), T.Node (g, ts)
    when Symbol.eq f g && List.length ss = List.length ts ->
      List.for_all2 (equate_terms a b) ss ts
    | T.At (t11, t12), T.At (t21, t22) ->
      equate_terms a b t11 t21 && equate_terms a b t12 t22
    | _ -> false
  (* check whether a\sigma = u and b\sigma = v, for some sigma; or the commutation thereof *)
  and equate_root a b u v =
        (try let subst = Unif.matching a 1 u 0 in
              let _ = Unif.matching ~subst b 1 v 0 in
              true
         with Unif.Fail -> false)
    ||  (try let subst = Unif.matching b 1 u 0 in
              let _ = Unif.matching ~subst a 1 v 0 in
              true
         with Unif.Fail -> false)
  in
  (* check for each literal *)
  Util.enter_prof prof_eq_subsumption;
  Util.incr_stat stat_eq_subsumption_call;
  let res = match a with
  | [|Lit.Equation (s, t, true, _)|] ->
    let res = Util.array_exists (equate_lit_with s t) b in
    (if res then Util.debug 3 "%a eq-subsumes %a"  Lits.pp a Lits.pp b);
    res
  | _ -> false  (* only a positive unit clause unit-subsumes a clause *)
  in
  Util.exit_prof prof_eq_subsumption;
  res

let subsumed_by_set set c =
  Util.enter_prof prof_subsumption_set;
  Util.incr_stat stat_subsumed_by_set_call;
  (* if there is an equation in c, try equality subsumption *)
  let try_eq_subsumption = Util.array_exists Lit.equational c.C.hclits in
  (* use feature vector indexing *)
  try
    SubsumIdx.retrieve_subsuming_c set#idx_fv c ()
      (fun () c' -> if (try_eq_subsumption && eq_subsumes c'.C.hclits c.C.hclits)
                    || subsumes c'.C.hclits c.C.hclits then raise Exit);
    Util.exit_prof prof_subsumption_set;
    false
  with Exit ->
    Util.debug 3 "%a subsumed by active set" C.pp c;
    Util.exit_prof prof_subsumption_set;
    true

let subsumed_in_set set c =
  Util.enter_prof prof_subsumption_in_set;
  Util.incr_stat stat_subsumed_in_set_call;
  (* if c is a single unit clause *)
  let try_eq_subsumption = C.is_unit_clause c && Lit.is_pos c.C.hclits.(0) in
  (* use feature vector indexing *)
  let l = ref [] in
  SubsumIdx.retrieve_subsumed_c set#idx_fv c ()
    (fun () hc' -> if (try_eq_subsumption && eq_subsumes c.C.hclits hc'.C.hclits)
                    || subsumes c.C.hclits hc'.C.hclits then l := hc' :: !l);
  Util.exit_prof prof_subsumption_in_set;
  !l

(** Number of equational lits. Used as an estimation for the difficulty of the subsumption
    check for this clause. *)
let rec num_equational lits i =
  if i = Array.length lits then 0
  else if Lit.equational lits.(i) then 1 + (num_equational lits (i+1))
  else num_equational lits (i+1)

(* ----------------------------------------------------------------------
 * contextual literal cutting
 * ---------------------------------------------------------------------- *)

exception RemoveLit of int * Clause.t

(** Performs successive contextual literal cuttings *)
let rec contextual_literal_cutting active_set c =
  Util.enter_prof prof_clc;
  let ctx = c.C.hcctx in
  if Array.length c.C.hclits <= 1
  || num_equational c.C.hclits 0 > 3
  || Array.length c.C.hclits > 8
    then (Util.exit_prof prof_clc; c) else
  (* do we need to try to use equality subsumption? *)
  let try_eq_subsumption = Util.array_exists Lit.equational c.C.hclits in
  (* try to remove one literal from the literal array *)
  let rec remove_one_lit lits = 
    try
      for i = 0 to Array.length lits - 1 do
        (* negate literal *)
        lits.(i) <- Lit.negate lits.(i);
        (* test for subsumption *)
        SubsumIdx.retrieve_subsuming active_set#idx_fv (Lits.to_seq lits) ()
          (fun () c' -> if (try_eq_subsumption && eq_subsumes c'.C.hclits lits)
                        || subsumes c'.C.hclits lits
             (* some clause subsumes the literals with i-th literal flipped *)
             then (lits.(i) <- Lit.negate lits.(i); raise (RemoveLit (i, c'))));
        (* restore literal *)
        lits.(i) <- Lit.negate lits.(i);
      done;
      None (* no change *)
    with (RemoveLit (i, c')) ->
      (* remove the literal and recurse *)
      Some (Util.array_except_idx lits i, c')
  in
  match remove_one_lit c.C.hclits with
  | None -> (Util.exit_prof prof_clc; c) (* no literal removed *)
  | Some (new_lits, c') -> begin
      (* hc' allowed us to cut a literal *)
      assert (List.length new_lits + 1 = Array.length c.C.hclits);
      let proof c'' = Proof.mk_c_step c'' "clc" [c.C.hcproof; c'.C.hcproof] in
      let parents = c :: c.C.hcparents in
      let new_c = C.create ~parents ~ctx new_lits proof in
      Util.debug 3 "contextual literal cutting in %a using %a gives\n\t%a"
        C.pp c C.pp c' C.pp new_c;
      (* try to cut another literal *)
      Util.exit_prof prof_clc; 
      contextual_literal_cutting active_set new_c
    end

(* ----------------------------------------------------------------------
 * contraction
 * ---------------------------------------------------------------------- *)

exception CondensedInto of Lit.t array * Substs.t

(** performs condensation on the clause. It looks for two literals l1 and l2 of same
    sign such that l1\sigma = l2, and hc\sigma \ {l2} subsumes hc. Then
    hc is simplified into hc\sigma \ {l2}.
    If there are too many equational literals, the simplification is disabled to
    avoid pathologically expensive subsumption checks.
    TODO remove this limitation after an efficient subsumption check is implemented. *)
let rec condensation c =
  Util.enter_prof prof_condensation;
  let ctx = c.C.hcctx in
  if Array.length c.C.hclits <= 1 || num_equational c.C.hclits 0 > 3
  || Array.length c.C.hclits > 8
    then (Util.exit_prof prof_condensation; c) else
  (* scope is used to rename literals for subsumption *)
  let lits = c.C.hclits in
  let n = Array.length lits in
  try
    for i = 0 to n - 1 do
      let lit = lits.(i) in
      for j = i+1 to n - 1 do
        let lit' = lits.(j) in
        (* try to match lit with lit' (and vice versa), then check if subst(c) subsumes c *)
        let substs = match_lits S.empty lit 0 lit' 0 @
                     match_lits S.empty lit' 0 lit 0 in
        List.iter
          (fun subst ->
            let new_lits = Array.sub lits 0 (n - 1) in
            (if i <> n-1 then new_lits.(i) <- lits.(n-1));  (* remove i-th lit *)
            let renaming = Ctx.renaming_clear ~ctx in
            let ord = Ctx.ord ctx in
            let new_lits = Lits.apply_subst ~renaming ~ord subst new_lits 0 in
            (* check subsumption *)
            if subsumes new_lits lits
              then raise (CondensedInto (new_lits, subst)))
          substs
      done;
    done;
    Util.exit_prof prof_condensation; 
    c
  with CondensedInto (new_lits, subst) ->
    (* clause is simplified *)
    let proof c' = Proof.mk_c_step c' "condensation" [c.C.hcproof] in
    let parents = c :: c.C.hcparents in
    let new_c = C.create_a ~parents ~ctx new_lits proof in
    Util.debug 3 "condensation in %a (with %a) gives\n\t %a"
      C.pp c S.pp subst C.pp new_c;
    (* try to condense further *)
    Util.exit_prof prof_condensation; 
    condensation new_c

(** {2 Contributions to Env} *)

let setup_env ~env =
  let rw_simplify (simpl : PS.SimplSet.t) c =
    let c = basic_simplify (demodulate simpl c) in
    let c = positive_simplify_reflect simpl c in
    let c = negative_simplify_reflect simpl c in
    c
  and active_simplify actives c =
    (* condensation *)
    let c = condensation c in
    (* contextual literal cutting *)
    let c = contextual_literal_cutting actives c in
    c
  and backward_simplify actives c =
    let set = C.CSet.empty in
    backward_demodulate actives set c
  and redundant = subsumed_by_set
  and backward_redundant = subsumed_in_set
  and list_simplify c =
    if is_tautology c then [] else [c]
  and is_trivial = is_tautology
  and constrs =
    [Precedence.min_constraint
      [Symbol.split_symbol; Symbol.false_symbol; Symbol.true_symbol]]
  and rule_remove_trivial = ("remove_trivial", fun ~ctx -> Transform.remove_trivial)
  and rule_reduce_cnf = ("cnf", fun ~ctx -> Cnf.as_transform ~ctx:(Ctx.skolem_ctx ~ctx))
  in
  Env.add_binary_inf ~env "superposition_passive" infer_passive;
  Env.add_binary_inf ~env "superposition_active" infer_active;
  Env.add_unary_inf ~env "equality_factoring" infer_equality_factoring;
  Env.add_unary_inf ~env "equality_resolution" infer_equality_resolution;
  env.Env.rw_simplify <- rw_simplify;
  env.Env.basic_simplify <- basic_simplify;
  env.Env.active_simplify <- active_simplify;
  env.Env.backward_simplify <- backward_simplify;
  env.Env.redundant <- redundant;
  env.Env.backward_redundant <- backward_redundant;
  env.Env.list_simplify <- list_simplify;
  env.Env.is_trivial <- is_trivial;
  Env.add_constrs env (Sequence.of_list constrs);
  Env.add_preprocess_rule ~env rule_remove_trivial;
  Env.add_preprocess_rule ~env rule_reduce_cnf;
  ()
