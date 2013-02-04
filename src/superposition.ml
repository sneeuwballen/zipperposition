(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

open Types
open Symbols

module T = Terms
module C = Clauses
module O = Orderings
module S = FoSubst
module I = Index
module BV = Bitvector
module Lits = Literals
module FV = FeatureVector
module PS = ProofState
module Unif = FoUnif
module Utils = FoUtils

(* statistics *)
let stat_basic_simplify = mk_stat "basic_simplify calls"
let stat_superposition_call = mk_stat "superposition calls"
let stat_equality_resolution_call = mk_stat "equality_resolution calls"
let stat_equality_factoring_call = mk_stat "equality_factoring calls"
let stat_subsumption_call = mk_stat "subsumption calls"
let stat_eq_subsumption_call = mk_stat "equality subsumption calls"
let stat_subsumed_in_set_call = mk_stat "subsumed_in_set calls"
let stat_subsumed_by_set_call = mk_stat "subsumed_by_set calls"
let stat_demodulate_call = mk_stat "demodulate calls"
let stat_demodulate_step = mk_stat "demodulate steps"
let stat_splits = mk_stat "splits"

let prof_demodulate = Utils.mk_profiler "demodulate"
let prof_back_demodulate = Utils.mk_profiler "backward_demodulate"
let prof_pos_simplify_reflect = Utils.mk_profiler "simplify_reflect+"
let prof_neg_simplify_reflect = Utils.mk_profiler "simplify_reflect-"
let prof_clc = Utils.mk_profiler "contextual_literal_cutting"
let prof_condensation = Utils.mk_profiler "condensation"
let prof_basic_simplify = Utils.mk_profiler "basic_simplify"
let prof_subsumption = Utils.mk_profiler "subsumption"
let prof_eq_subsumption = Utils.mk_profiler "equality_subsumption"
let prof_subsumption_set = Utils.mk_profiler "forward_subsumption"
let prof_subsumption_in_set = Utils.mk_profiler "backward_subsumption"
let prof_infer_active = Utils.mk_profiler "infer_active"
let prof_infer_passive = Utils.mk_profiler "infer_passive"
let prof_infer_equality_resolution = Utils.mk_profiler "infer_equality_resolution"
let prof_infer_equality_factoring = Utils.mk_profiler "infer_equality_factoring"
let prof_split = Utils.mk_profiler "infer_split"

(* ----------------------------------------------------------------------
 * combinators
 * ---------------------------------------------------------------------- *)

(** returns the first (f e) that is not None, with e element of list *)
let rec list_first f = function
  | [] -> None
  | x::tl -> match f x with
    | Some _ as x -> x
    | _ -> list_first f tl


(** apply f to all non-variable positions in t, accumulating the
    results along. f is given the subterm, the position and the context
    at each such position, and returns a list of objects; all lists
    returned by f are concatenated.

    position -> term
    -> (term -> position -> 'b list)
    -> 'b list
    *)
let all_positions pos t f =
  let rec aux pos t = match t.term with
  | Var _ | BoundVar _ -> []
  | Bind (_, t') ->
    let acc = f t pos in  (* apply to term itself *)
    List.rev_append (aux (pos @ [0]) t') acc
  | Node (hd, tl) ->
    let acc = f t pos in  (* apply to term itself *)
    let acc, _ =
      List.fold_left
      (fun (acc,idx) t ->
        let acc = List.rev_append (aux (pos @ [idx]) t) acc in (* recurse in subterm *)
        acc, idx+1)
      (acc, 0) tl
    in
    acc
  in
  aux pos t

(* ----------------------------------------------------------------------
 * inferences
 * ---------------------------------------------------------------------- *)

(** all the elements of a, but the i-th, into a list *)
let array_except_idx a i =
  Utils.array_foldi
    (fun acc j elt -> if i = j then acc else elt::acc)
    [] a

(* Helper that does one or zero superposition inference, with all
   the given parameters. Clauses have an offset. *)
let do_superposition ~ctx (active_clause, o_a) active_pos
                          (passive_clause, o_p) passive_pos subst acc =
  let ord = ctx.ctx_ord in
  assert (List.length active_pos = 2);
  incr_stat stat_superposition_call;
  match passive_pos with
  | [] | _::[] -> assert false
  | passive_idx::passive_side::subterm_pos ->
  let active_idx = List.hd active_pos
  and u, v, sign_uv = Calculus.get_equations_sides passive_clause [passive_idx; passive_side]
  and s, t, sign_st = Calculus.get_equations_sides active_clause active_pos in
  Utils.debug 3 (lazy (Utils.sprintf ("sup @[<hov>@[<h>%a s=%a t=%a@]@ @[<h>%a " ^^
                                      "u=%a v=%a p=%a@]@ subst=%a@]")
                       !C.pp_clause#pp active_clause !T.pp_term#pp s !T.pp_term#pp t
                       !C.pp_clause#pp passive_clause !T.pp_term#pp u !T.pp_term#pp v
                       pp_pos passive_pos S.pp_substitution subst));
  assert (T.db_closed s);
  if not sign_st 
  then (Utils.debug 3 (lazy "... active literal is negative"); acc)
  else if not (T.atomic s) (* do not rewrite non-atomic formulas *)
  then (Utils.debug 3 (lazy "... active term is not atomic or DB-closed"); acc)
  else if not (T.db_closed (T.at_pos u subterm_pos))
    && (List.exists (fun x -> S.is_in_subst subst (x,o_p)) (T.vars (T.at_pos u subterm_pos)))
  then (Utils.debug 3 (lazy "... narrowing with De Bruijn indices"); acc)
  else
  let t' = S.apply_subst subst (t, o_a)
  and v' = S.apply_subst subst (v, o_p) in
  if sign_uv && t' == v' && subterm_pos = []
  then (Utils.debug 3 (lazy "... will yield a tautology"); acc)
  else begin
    if (ord#compare (S.apply_subst subst (s, o_a)) t' = Lt ||
        ord#compare (S.apply_subst subst (u, o_p)) v' = Lt ||
        not (BV.get (C.eligible_res (passive_clause, o_p) subst) passive_idx) ||
        not (BV.get (C.eligible_param (active_clause, o_a) subst) active_idx))
      then (Utils.debug 3 (lazy "... has bad ordering conditions"); acc)
      else begin (* ordering constraints are ok *)
        let lits_a = array_except_idx active_clause.hclits active_idx in
        let lits_p = array_except_idx passive_clause.hclits passive_idx in
        (* replace s\sigma by t\sigma in u|_p\sigma *)
        let u' = S.apply_subst subst (u, o_p) in
        let new_u = T.replace_pos u' subterm_pos t' in
        (* apply substitution to other literals *)
        let new_lits =
          Lits.mk_lit ~ord new_u v' sign_uv :: 
          (Literals.apply_subst_list ~ord subst (lits_a, o_a)) @
          (Literals.apply_subst_list ~ord subst (lits_p, o_p))
        in
        let rule = if sign_uv then "sup+" else "sup-" in
        let proof c = Proof (c, rule, [active_clause.hcproof;
                                       passive_clause.hcproof]) in
        let parents = [active_clause; passive_clause] in
        let new_clause = C.mk_hclause ~parents ~ctx new_lits proof in
        Utils.debug 3 (lazy (Utils.sprintf "... ok, conclusion @[<h>%a@]"
                            !C.pp_clause#pp_h new_clause));
        new_clause :: acc
      end
  end

let infer_active (actives : ProofState.active_set) clause : hclause list =
  Utils.enter_prof prof_infer_active;
  let ctx = actives#ctx in
  let offset = T.max_var clause.hcvars + 1 in
  (* no literal can be eligible for paramodulation if some are selected.
     This checks if inferences with i-th literal are needed? *)
  let eligible_lit =
    let bv = C.eligible_param (clause,0) S.id_subst in
    fun i lit -> BV.get bv i
  in
  (* do the inferences where clause is active; for this,
     we try to rewrite conditionally other clauses using
     non-minimal sides of every positive literal *)
  let new_clauses = Calculus.fold_lits ~both:true eligible_lit
    (fun acc s t _ s_pos ->
      (* rewrite clauses using s *)
      actives#idx_sup_into#retrieve_unifiables s acc
        (fun acc u_p set ->
          try (* rewrite u_p with s, if they are unifiable *)
            let subst = Unif.unification S.id_subst (s, 0) (u_p, offset) in
            I.ClauseSet.fold
              (fun (hc, u_pos, u_p) acc ->
                let passive = hc in
                do_superposition ~ctx (clause, 0) s_pos (passive, offset) u_pos subst acc)
              set acc
          with
            UnificationFailure -> acc)
    )
    [] clause.hclits
  in
  Utils.exit_prof prof_infer_active;
  new_clauses

let infer_passive (actives:ProofState.active_set) clause : hclause list =
  Utils.enter_prof prof_infer_passive;
  let ctx = actives#ctx in
  let offset = T.max_var clause.hcvars + 1 in
  (* perform inference on this lit? *)
  let eligible =
    let bv = C.eligible_res (clause,0) S.id_subst in
    fun i lit -> BV.get bv i
  in
  (* do the inferences in which clause is passive (rewritten),
     so we consider both negative and positive literals *)
  let new_clauses = Calculus.fold_lits ~both:true eligible
    (fun acc u v _ u_pos ->
      (* rewrite subterms of u *)
      let new_clauses = all_positions u_pos u
        (fun u_p p ->
          (* all terms that occur in an equation in the active_set
             and that are potentially unifiable with u_p (u at position p) *)
          actives#idx_sup_from#retrieve_unifiables u_p acc
            (fun acc s set ->
              try
                let subst = Unif.unification S.id_subst (s, offset) (u_p, 0) in
                I.ClauseSet.fold
                  (fun (hc, s_pos, s) acc ->
                    let active = hc in
                    do_superposition ~ctx (active, offset) s_pos (clause, 0) p subst acc)
                  set acc
              with
                UnificationFailure -> acc))
      in List.rev_append new_clauses acc)
    [] clause.hclits
  in
  Utils.exit_prof prof_infer_passive;
  new_clauses

let infer_equality_resolution clause =
  Utils.enter_prof prof_infer_equality_resolution;
  let ctx = clause.hcctx in
  (* literals that can potentially be eligible for resolution *)
  let eligible =
    let bv = C.eligible_res (clause,0) S.id_subst in
    fun i lit -> Lits.is_neg lit && BV.get bv i
  in
  (* iterate on those literals *)
  let new_clauses = Calculus.fold_lits ~both:false eligible
    (fun acc l r sign l_pos ->
      assert (not sign);
      match l_pos with
      | [] -> assert false
      | pos::_ ->
      try
        let subst = Unif.unification S.id_subst (l,0) (r,0) in
        if BV.get (C.eligible_res (clause,0) subst) pos
          (* subst(lit) is maximal, we can do the inference *)
          then begin
            incr_stat stat_equality_resolution_call;
            let proof c = Proof (c, "eq_res", [clause.hcproof])
            and new_lits = array_except_idx clause.hclits pos in
            let new_lits = Lits.apply_subst_list ~ord:ctx.ctx_ord subst (new_lits, 0) in
            let new_clause = C.mk_hclause ~parents:[clause] ~ctx new_lits proof in
            Utils.debug 3 (lazy (Utils.sprintf
                          "equality resolution on @[<h>%a@] yields @[<h>%a@]"
                          !C.pp_clause#pp clause !C.pp_clause#pp_h new_clause));
            new_clause::acc
          end else
            acc
      with UnificationFailure -> acc) (* l and r not unifiable, try next *)
    [] clause.hclits
  in
  Utils.exit_prof prof_infer_equality_resolution;
  new_clauses

let infer_equality_factoring clause =
  Utils.enter_prof prof_infer_equality_factoring;
  let ctx = clause.hcctx in
  let ord = ctx.ctx_ord in
  (* is the literal eligible for paramodulation? *)
  let eligible =
    let bv = C.eligible_param (clause,0) S.id_subst in
    fun i lit -> BV.get bv i
  in
  (* find root terms that are unifiable with s and are not in the
     literal at s_pos. This returns a list of position and substitution *)
  let find_unifiable_lits s s_pos =
    Utils.array_foldi
      (fun acc i lit ->
        match lit with
        | Equation (_, _, false, _) -> acc
        | _ when List.hd s_pos = i -> acc (* same index *) 
        | Equation (u, v, true, _) ->
          let try_u =  (* try inference between s and u *)
            try
              let subst = Unif.unification S.id_subst (s,0) (u,0) in [[i; left_pos], subst]
            with UnificationFailure -> []
          and try_v =  (* try inference between s and v *)
            try
              let subst = Unif.unification S.id_subst (s,0) (v,0) in [[i; right_pos], subst]
            with UnificationFailure -> []
          in try_u @ try_v @ acc)
      [] clause.hclits
  (* do the inference between given positions, if ordering
     conditions are respected *)
  and do_inference active_pos passive_pos subst =
    let s, t, sign_st = Calculus.get_equations_sides clause active_pos
    and u, v, sign_uv = Calculus.get_equations_sides clause passive_pos
    and active_idx = List.hd active_pos in
    assert (sign_st && sign_uv);
    (* check whether subst(lit) is maximal, and not (subst(s) < subst(t)) *)
    if ord#compare (S.apply_subst subst (s,0)) (S.apply_subst subst (t,0)) <> Lt &&
       BV.get (C.eligible_param (clause,0) subst) active_idx
      then begin
        incr_stat stat_equality_factoring_call;
        let proof c = Proof (c, "eq_fact", [clause.hcproof])
        (* new_lits: literals of the new clause. remove active literal
           and replace it by a t!=v one, and apply subst *)
        and new_lits = array_except_idx clause.hclits active_idx in
        let new_lits = (Lits.mk_neq ~ord t v) :: new_lits in
        let new_lits = Lits.apply_subst_list ~ord subst (new_lits,0) in
        let new_clause = C.mk_hclause ~parents:[clause] ~ctx new_lits proof in
        Utils.debug 3 (lazy (Utils.sprintf
                      "equality factoring on @[<h>%a@] yields @[<h>%a@]"
                      !C.pp_clause#pp clause !C.pp_clause#pp_h new_clause));
        [new_clause]
      end else
        []
  (* try to do inferences with each positive literal *)
  in
  let new_clauses = Calculus.fold_lits ~both:true eligible
    (fun acc s t _ s_pos -> (* try with s=t *)
      let unifiables = find_unifiable_lits s s_pos in
      List.fold_left
        (fun acc (passive_pos, subst) ->
          (do_inference s_pos passive_pos subst) @ acc)
        acc unifiables)
    [] clause.hclits
  in
  Utils.exit_prof prof_infer_equality_factoring;
  new_clauses

let split_count = ref 0
let split_limit = ref 100

(* union-find that maps terms to list of literals *)
module UF = UnionFind.Make(
  struct
    type key = term
    type value = literal list
    let equal = (==)
    let hash t = t.hkey
    let zero = []
    let merge = List.rev_append
  end)

(** Hyper-splitting *)
let infer_split hc =
  let ctx = hc.hcctx in
  let ord = ctx.ctx_ord in
  (* only do splitting on large clauses *)
  if Array.length hc.hclits < 4 || !split_count >= !split_limit then [] else begin
  Utils.enter_prof prof_split;
  (* get a fresh split symbol *)
  let rec next_split_term () = 
    let s = "$$split_" ^ (string_of_int !split_count) in
    incr split_count;
    if Symbols.is_used s
      then next_split_term ()
      else T.mk_const (mk_symbol ~attrs:attr_split s) bool_sort
  in
  (* is the term made of a split symbol? *)
  let is_split_term t = match t.term with
  | Node (s, []) when has_attr attr_split s -> true
  | _ -> false
  in
  (* literals that are ground, or split symbols *)
  let branch = ref [] in
  (* maps variables to a list of literals *)
  let cluster = UF.create hc.hcvars in
  (* for each literal, merge the list of all variables occurring in
     the literal *)
  Array.iter
    (fun lit ->
      match Lits.vars lit with
      | [] -> ()
      | x::vars' -> List.iter (fun y -> UF.union cluster x y) vars')
    hc.hclits;
  (* Divide clause into components (that do not share variables and do not contain
     any split symbol), and a remaining (branch) part. Ground terms go in the "branch" part.
     [components] is a list of (vars, literal list ref), *)
  let rec find_components lits i =
    if i = Array.length lits then () else begin
      match lits.(i) with
      | Equation (l, r, _, _) when ((is_split_term l && r == T.true_term)
                                  ||(is_split_term r && l == T.true_term)) ->
        branch := lits.(i) :: !branch;
        find_components lits (i+1)  (* branch part *)
      | Equation (l, r, _, _) when T.is_ground_term l && T.is_ground_term r ->
        branch := lits.(i) :: !branch;
        find_components lits (i+1)  (* branch part *)
      | Equation (l, r, _, _) ->
        (* find which component this literal belongs to *)
        let vars = T.vars_list [l;r] in
        assert (vars <> []);
        let x = List.hd vars in
        (* Add lit to the list of lits for the given variable. All variables
           of the lit have the same representative in components. *)
        UF.add cluster x [lits.(i)];
        find_components lits (i+1)
    end
  in
  find_components hc.hclits 0;
  let components = ref [] in
  UF.iter cluster (fun _ l ->
    Utils.debug 4 (lazy (Utils.sprintf "component @[<h>%a@]"
                   (Utils.pp_list Lits.pp_literal) l));
    components := l :: !components);
  let n = List.length !components in
  if n > 1 && List.for_all (fun l -> List.length l >= 2) !components then begin
    (* Do the split. But only because we have several components, that contain several
       literals each. *)
    incr_stat stat_splits;
    (* create a list of symbols *)
    let symbols = Utils.times (n-1) next_split_term in
    let proof c = Proof (c, "split", [hc.hcproof]) in
    (* the guard clause, plus the first component, plus all negated split symbols *)
    let guard =
      let lits = List.map (Lits.mk_neq ~ord T.true_term) symbols @ List.hd !components @ !branch in
      C.mk_hclause ~parents:[hc] ~ctx lits proof
    in
    (* one new clause for each other component *)
    let new_clauses = List.map2
      (fun component split_symbol ->
        let split_lit = Lits.mk_eq ~ord split_symbol T.true_term in
        let lits = split_lit :: (component @ !branch) in
        C.mk_hclause ~parents:[hc] ~ctx lits proof)
      (List.tl !components) symbols
    in
    let new_clauses = guard :: new_clauses in
    Utils.debug 3 (lazy (Utils.sprintf
                  "split on @[<h>%a@] yields @[<h>%a@]"
                  !C.pp_clause#pp_h hc (Utils.pp_list !C.pp_clause#pp_h) new_clauses));
    Utils.exit_prof prof_split;
    new_clauses
  end else (Utils.exit_prof prof_split; [])
  end

(* ----------------------------------------------------------------------
 * simplifications
 * ---------------------------------------------------------------------- *)

exception RewriteInto of term

(** Compute normal form of term w.r.t active set. Clauses used to
    rewrite are added to the clauses hashset.
    restrict is an option for restricting demodulation in positive maximal terms *)
let demod_nf ?(restrict=false) simpl_set clauses t =
  let ord = simpl_set#ctx.ctx_ord in
  let oriented_hclause hc = match hc.hclits with
  | [|Equation (_,_,true,Gt)|] | [|Equation (_,_,true,Lt)|] -> true (* oriented *)
  | _ -> false in
  (* compute normal form of subterm. If restrict is true, substitutions that
     are variable renamings are forbidden (since we are at root of a max term) *) 
  let rec normal_form ~restrict t =
    (* do not rewrite non-atomic formulas *)
    if not (T.atomic t)
      then t  (* do not rewrite such formulas *)
      else begin
        (* find equations l=r that match subterm *)
        try
          simpl_set#idx_simpl#retrieve ~sign:true 1 (t,0)
            (fun l r subst unit_hclause ->
              (* r is the term subterm is going to be rewritten into *)
              assert (C.is_unit_clause unit_hclause);
              let new_l = S.apply_subst subst l
              and new_r = S.apply_subst subst r in
              if (not restrict || not (S.is_renaming subst))
              && (oriented_hclause unit_hclause || ord#compare new_l new_r = Gt)
                (* subst(l) > subst(r) and restriction does not apply, we can rewrite *)
                then begin
                  assert (ord#compare new_l new_r = Gt);
                  clauses := unit_hclause :: !clauses;
                  incr_stat stat_demodulate_step;
                  raise (RewriteInto new_r)
                end);
          t (* not found any match, normal form found *)
        with RewriteInto t' ->
          normal_form ~restrict t' (* done one rewriting step, continue *)
      end
  (* rewrite innermost-leftmost *)
  and traverse ~restrict t =
    match t.term with
    | Var _ | BoundVar _ -> t
    | Bind (s, t') ->
      let t'' = traverse ~restrict:false t' in
      let new_t = T.mk_bind s t'' in
      (* rewrite term at root *)
      normal_form ~restrict new_t
    | Node (s, l) ->
      (* rewrite subterms *)
      let l' = List.map (traverse ~restrict:false) l in
      let t' = if List.for_all2 (==) l l' then t else T.mk_node s t.sort l' in
      (* rewrite term at root *)
      normal_form ~restrict t'
  in
  traverse ~restrict t

(** Demodulate the clause, with restrictions on which terms to rewrite *)
let demodulate simpl_set c =
  Utils.enter_prof prof_demodulate;
  incr_stat stat_demodulate_call;
  let ctx = c.hcctx in
  let ord = ctx.ctx_ord in
  (* clauses used to rewrite *)
  let clauses = ref [] in
  (* literals that are eligible for resolution *)
  let eligible_res = C.eligible_res (c,0) S.id_subst in
  (* demodulate literals *)
  let demod_lit i lit =
    match lit with
    | Equation (l, r, false, _) ->
      Lits.mk_neq ~ord (demod_nf simpl_set clauses l) (demod_nf simpl_set clauses r)
    | Equation (l, r, true, Gt) when BV.get eligible_res i ->
      Lits.mk_eq ~ord
        (demod_nf ~restrict:true simpl_set clauses l)
        (demod_nf simpl_set clauses r)
    | Equation (l, r, true, Lt) when BV.get eligible_res i ->
      Lits.mk_eq ~ord
        (demod_nf simpl_set clauses l)
        (demod_nf ~restrict:true simpl_set clauses r)
    | Equation (l, r, true, _) ->
      Lits.mk_eq ~ord (demod_nf simpl_set clauses l) (demod_nf simpl_set clauses r)
  in
  (* demodulate every literal *)
  let lits = Array.mapi demod_lit c.hclits in
  if !clauses = []
    then begin (* no rewriting performed *)
      assert (Utils.array_forall2 Lits.eq_com c.hclits lits);
      Utils.exit_prof prof_demodulate;
      c
    end else begin  (* construct new clause *)
      let proof c' = Proof (c', "demod", c.hcproof :: List.map (fun hc -> hc.hcproof) !clauses) in
      let parents = c :: c.hcparents in
      let new_hc = C.mk_hclause_a ~parents ~ctx lits proof in
      Utils.debug 3 (lazy (Utils.sprintf "@[<h>demodulate %a into %a using @[<hv>%a@]@]"
                     !C.pp_clause#pp c !C.pp_clause#pp_h new_hc
                     (Utils.pp_list !C.pp_clause#pp_h) !clauses));
      (* return simplified clause *)
      Utils.exit_prof prof_demodulate;
      new_hc
    end

(** Find clauses that [given] may demodulate, add them to set *)
let backward_demodulate (active_set : PS.active_set) set given =
  Utils.enter_prof prof_back_demodulate;
  let ctx = given.hcctx in
  let ord = ctx.ctx_ord in
  let offset = T.max_var given.hcvars + 1 in
  (* find clauses that might be rewritten by l -> r *)
  let recurse ~oriented set l r =
    active_set#idx_back_demod#retrieve_specializations l set
      (fun set t' clauses ->
        try
          let subst = Unif.matching S.id_subst (l,0) (t',offset) in
          (* subst(l) matches t' and is > subst(r), very likely to rewrite! *)
          if oriented || ord#compare (S.apply_subst subst (l,0))
                                     (S.apply_subst subst (r,0)) = Gt
            then
              (* add clauses to the set, they may be rewritten by l -> r *)
              Index.ClauseSet.fold (fun (hc,_,_) set -> C.CSet.add set hc) clauses set
            else set
        with UnificationFailure -> set)
  in
  let set' = match given.hclits with
  | [|Equation (l,r,true,Gt)|] -> recurse ~oriented:true set l r
  | [|Equation (l,r,true,Lt)|] -> recurse ~oriented:true set r l
  | [|Equation (l,r,true,_)|] ->
    let set' = recurse ~oriented:false set l r in
    recurse ~oriented:false set' r l  (* both sides can rewrite, but we
                                         need to check ordering *)
  | _ -> set
  in
  Utils.exit_prof prof_back_demodulate;
  set'

let is_tautology hc =
  let rec check lits i =
    if i = Array.length lits then false
    else match lits.(i) with
    | Equation (l, r, true, _) when l == r -> true
    | Equation (l, r, sign, _) ->
      Utils.array_exists
        (fun (Equation (l', r', sign', _)) ->
          (sign = not sign') &&
          (((T.eq_term l l') && (T.eq_term r r')) ||
          ((T.eq_term l r') && (T.eq_term l' r))))
        lits
      || check lits (i+1)
  in
  let is_tauto = check hc.hclits 0 in
  (if is_tauto then
    Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a@] is a tautology" !C.pp_clause#pp_h hc)));
  is_tauto

(** semantic tautology deletion, using a congruence closure algorithm
    to see if negative literals imply some positive literal *)
let is_semantic_tautology c = false (* TODO *)

let basic_simplify hc =
  Utils.enter_prof prof_basic_simplify;
  let ctx = hc.hcctx in
  let ord = ctx.ctx_ord in
  incr_stat stat_basic_simplify;
  let absurd_lit lit = match lit with
  | Equation (l, r, false, _) when T.eq_term l r -> true
  | Equation (l, r, true, _) when (l == T.true_term && r == T.false_term)
                               || (l == T.false_term && r == T.true_term) -> true
  | _ -> false in
  let lits = Array.to_list hc.hclits in
  (* remove s!=s literals *)
  let new_lits = List.filter (fun lit -> not (absurd_lit lit)) lits in
  (* remove duplicate literals *)
  let new_lits = Utils.list_uniq Lits.eq_com new_lits in
  (* destructive equality resolution *)
  let rec er lits =
    match Utils.list_find er_check lits with
    | None -> lits
    | Some (i, Equation (l, r, sign, _)) ->
        assert (not sign);
        assert (T.is_var l || T.is_var r);
        try
          let subst = Unif.unification S.id_subst (l,0) (r,0) in
          (* remove the literal, and apply the substitution to the remaining literals
             before trying to find another x!=t *)
          er (Lits.apply_subst_list ~ord subst ((Utils.list_remove lits i),0))
        with UnificationFailure -> lits
  (* finds candidate literals for destructive ER (lits with >= 1 variable) *)
  and er_check (Equation (l, r, sign, _)) = (not sign) && (T.is_var l || T.is_var r) in
  let new_lits = er new_lits in
  if List.length new_lits = Array.length hc.hclits
  then (Utils.exit_prof prof_basic_simplify; hc) (* no change *)
  else begin
    let proof = C.adapt_proof hc.hcproof in  (* do not bother printing this *)
    let parents = hc :: hc.hcparents in
    let new_clause = C.mk_hclause ~parents ~ctx new_lits proof in
    Utils.debug 3 (lazy (Utils.sprintf "@[<hov 4>@[<h>%a@]@ basic_simplifies into @[<h>%a@]@]"
                   !C.pp_clause#pp_h hc !C.pp_clause#pp_h new_clause));
    Utils.exit_prof prof_basic_simplify;
    new_clause
  end

exception FoundMatch of (term * hclause * substitution)

let positive_simplify_reflect simpl_set c =
  Utils.enter_prof prof_pos_simplify_reflect;
  let ctx = c.hcctx in
  let offset = T.max_var c.hcvars + 1 in
  (* iterate through literals and try to resolve negative ones *)
  let rec iterate_lits acc lits clauses = match lits with
  | [] -> List.rev acc, clauses
  | (Equation (s, t, false, _) as lit)::lits' ->
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
    match t1.term, t2.term with
    | _ when T.eq_term t1 t2 -> Some clauses  (* trivial *)
    | Node (f, ss), Node (g, ts) when f == g && List.length ss = List.length ts ->
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
    try simpl_set#idx_simpl#retrieve ~sign:true offset (t1,0)
      (fun l (r,_) subst hc ->
        if t2 == (S.apply_subst subst (r,offset))
        then begin  (* t1!=t2 is refuted by l\sigma = r\sigma *)
          Utils.debug 4 (lazy (Utils.sprintf "equate %a and %a using %a"
                      !T.pp_term#pp t1 !T.pp_term#pp t2 !C.pp_clause#pp_h hc));
          raise (FoundMatch (r, hc, subst)) (* success *)
        end else ());
      None (* no match *)
    with FoundMatch (r, hc, subst) ->
      Some (hc.hcproof :: clauses)  (* success *)
  in
  (* fold over literals *)
  let lits, premises = iterate_lits [] (Array.to_list c.hclits) [] in
  if List.length lits = Array.length c.hclits
    then (Utils.exit_prof prof_pos_simplify_reflect; c) (* no literal removed, keep c *)
    else 
      let proof c' = Proof (c', "simplify_reflect+", c.hcproof::premises) in
      let parents = c :: c.hcparents in
      let new_hc = C.mk_hclause ~parents ~ctx lits proof in
      Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a pos_simplify_reflect into %a@]"
                    !C.pp_clause#pp c !C.pp_clause#pp_h new_hc));
      Utils.exit_prof prof_pos_simplify_reflect;
      new_hc
    
let negative_simplify_reflect simpl_set c =
  Utils.enter_prof prof_neg_simplify_reflect;
  let ctx = c.hcctx in
  let offset = T.max_var c.hcvars + 1 in
  (* iterate through literals and try to resolve positive ones *)
  let rec iterate_lits acc lits clauses = match lits with
  | [] -> List.rev acc, clauses
  | (Equation (s, t, true, _) as lit)::lits' ->
    begin match can_refute s t, can_refute t s with
    | None, None -> (* keep literal *)
      iterate_lits (lit::acc) lits' clauses
    | Some new_clause, _ | _, Some new_clause -> (* drop literal, remember clause *)
      iterate_lits acc lits' (new_clause :: clauses)
    end
  | lit::lits' -> iterate_lits (lit::acc) lits' clauses
  (** try to remove the literal using a negative unit clause *)
  and can_refute s t =
    try simpl_set#idx_simpl#retrieve ~sign:false offset (s,0)
      (fun l (r,_) subst hc ->
        if t == (S.apply_subst subst (r,offset))
        then begin
          Utils.debug 3 (lazy (Utils.sprintf "neg_reflect eliminates %a=%a with %a"
                         !T.pp_term#pp s !T.pp_term#pp t !C.pp_clause#pp_h hc));
          raise (FoundMatch (r, hc, subst)) (* success *)
        end else ());
      None (* no match *)
    with FoundMatch (r, hc, subst) ->
      Some hc.hcproof  (* success *)
  in
  (* fold over literals *)
  let lits, premises = iterate_lits [] (Array.to_list c.hclits) [] in
  if List.length lits = Array.length c.hclits
    then (Utils.exit_prof prof_neg_simplify_reflect; c) (* no literal removed *)
    else 
      let proof c' = Proof (c', "simplify_reflect-", c.hcproof::premises) in
      let parents = c :: c.hcparents in
      let new_hc = C.mk_hclause ~parents ~ctx lits proof in
      Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a neg_simplify_reflect into %a@]"
                    !C.pp_clause#pp c !C.pp_clause#pp_h new_hc));
      Utils.exit_prof prof_neg_simplify_reflect;
      new_hc

(* ----------------------------------------------------------------------
 * subsumption
 * ---------------------------------------------------------------------- *)

(** raised when a subsuming substitution is found *)
exception SubsumptionFound of substitution

(** checks whether subst(lit_a) subsumes subst(lit_b). Returns a list of
    substitutions s such that s(lit_a) = lit_b and s contains subst. The list
    is empty if lit_a does not subsume lit_b. *)
let match_lits subst (lit_a, o_a) (lit_b, o_b) =
  (* match t1 with t2, then t1' with t2' *)
  let match4 subst t1 t2 t1' t2' =
    try let subst' = Unif.matching subst (t1,o_a) (t2,o_b) in
        [Unif.matching subst' (t1',o_a) (t2',o_b)]
    with UnificationFailure -> []
  in
  match lit_a, lit_b with
  | Equation (_, _, signa, _), Equation (_, _, signb, _)
    when signa <> signb -> [] (* different sign *)
  | Equation (la, ra, _, Lt), Equation (lb, rb, _, Lt)
  | Equation (la, ra, _, Gt), Equation (lb, rb, _, Gt) -> (* use monotonicity *)
    match4 subst la lb ra rb
  | Equation (la, ra, _, Gt), Equation (lb, rb, _, Lt)
  | Equation (la, ra, _, Lt), Equation (lb, rb, _, Gt) -> (* use monotonicity *)
    match4 subst la rb ra lb
  | Equation (la, ra, _, _), Equation (lb, rb, _, _) -> (* general case *)
    (match4 subst la lb ra rb) @ (match4 subst la rb ra lb)

(** check that every literal in a matches at least one literal in b *)
let all_lits_match (a,o_a) (b,o_b) =
  Utils.array_forall
    (fun lita ->
      Utils.array_exists
        (fun litb -> match_lits S.id_subst (lita,o_a) (litb,o_b) <> [])
        b)
    a

(** Compare literals by subsumption difficulty (see "towards efficient subsumption", Tammet).
    We sort by increasing order, so non-ground, deep, heavy literals are smaller
    (thus tested early) *)
let compare_literals_subsumption lita litb =
  let rec is_ground (Equation (l,r,_,_)) = T.is_ground_term l && T.is_ground_term r
  and depth (Equation (l,r,_,_)) = max (term_depth l) (term_depth r)
  and term_depth t = match t.term with
    | Var _ | BoundVar _ -> 1
    | Bind (_, t') -> 1 + term_depth t'
    | Node (_, l) -> 1 + List.fold_left (fun m t' -> max m (term_depth t')) 0 l
  and size (Equation (l,r,_,_)) = l.tsize + r.tsize
  in
  (* ground literal is bigger *)
  if is_ground lita && not (is_ground litb) then 1
  else if not (is_ground lita) && is_ground litb then -1
  (* deep literal is smaller *)
  else let deptha, depthb = depth lita, depth litb in 
  if deptha <> depthb then depthb - deptha
  (* heavy literal is smaller *)
  else if size lita <> size litb
  then size litb - size lita
  else 0

(** Check whether [a] subsumes [b], and if it does, return the
    corresponding substitution *)
let subsumes_with (a,o_a) (b,o_b) =
  incr_stat stat_subsumption_call;
  (* a must not have more literals *)
  if Array.length a > Array.length b then None else
  (* variables that cannot be bound during subsumption *)
  if not (all_lits_match (a,o_a) (b,o_b)) then None else
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
    let substs = match_lits subst (lita,o_a) (litb,o_b) in
    let bv' = BV.set bv j in
    List.iter (fun subst' -> try_permutations (i+1) subst' bv') substs;
    (* some variable of lita occur in a[j+1...], try another literal of b *)
    if substs <> [] && not (check_vars lita (i+1))
      then () (* no backtracking for litb *)
      else find_matched lita i subst bv (j+1)
    end
  (* does some literal in a[j...] contain a variable in l or r? *)
  and check_vars lit j =
    if j = Array.length a then false else match lit, a.(j) with
    | Equation (l, r, _, _), _ when T.is_ground_term l && T.is_ground_term r -> false
    | Equation (l, r, _, _), Equation (l', r',_,_) ->
      let vars = T.vars_list [l;r] in
      if (List.exists (fun v -> T.var_occurs v l' || T.var_occurs v r') vars)
        then true
        else check_vars lit (j+1)
  in
  try
    Array.sort compare_literals_subsumption a;
    try_permutations 0 S.id_subst BV.empty;
    None
  with (SubsumptionFound subst) -> Some subst

let subsumes a b =
  Utils.enter_prof prof_subsumption;
  let offset = T.max_var (Lits.vars_lits a) + 1 in
  let res = match subsumes_with (a,0) (b,offset) with
  | None -> false
  | Some _ -> true
  in
  Utils.debug 2 (lazy (Utils.sprintf "%% @[<h>%a subsumes %a@]"
                Lits.pp_lits a Lits.pp_lits b));
  Utils.exit_prof prof_subsumption;
  res

let eq_subsumes a b =
  let offset = T.max_var (Lits.vars_lits b) + 1 in
  (* subsume a literal using a = b *)
  let rec equate_lit_with a b lit =
    match lit with
    | Equation (u, v, true, _) -> equate_terms a b u v
    | _ -> false
  (* make u and v equal using a = b (possibly several times) *)
  and equate_terms a b u v =
    match u.term, v.term with
    | _ when u == v -> true 
    | _ when equate_root a b u v -> true
    | Node (f, ss), Node (g, ts) when f == g && List.length ss = List.length ts ->
      List.for_all2 (equate_terms a b) ss ts
    | _ -> false
  (* check whether a\sigma = u and b\sigma = v, for some sigma; or the commutation thereof *)
  and equate_root a b u v =
        (try let subst = Unif.matching S.id_subst (a,offset) (u,0) in
              S.apply_subst subst (b,offset) == v
         with UnificationFailure -> false)
    ||  (try let subst = Unif.matching S.id_subst (b,offset) (u,0) in
              S.apply_subst subst (a,offset) == v
         with UnificationFailure -> false)
  in
  (* check for each literal *)
  Utils.enter_prof prof_eq_subsumption;
  incr_stat stat_eq_subsumption_call;
  let res = match a with
  | [|Equation (s, t, true, _)|] ->
    let res = Utils.array_exists (equate_lit_with s t) b in
    (if res then Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a eq-subsumes@ %a@]"
                                Lits.pp_lits a Lits.pp_lits b)));
    res
  | _ -> false  (* only a positive unit clause unit-subsumes a clause *)
  in
  Utils.exit_prof prof_eq_subsumption;
  res

let subsumed_by_set set c =
  Utils.enter_prof prof_subsumption_set;
  incr_stat stat_subsumed_by_set_call;
  (* if there is an equation in c, try equality subsumption *)
  let try_eq_subsumption = Utils.array_exists Lits.equational c.hclits in
  (* use feature vector indexing *)
  try
    FV.retrieve_subsuming set#idx_fv c.hclits
      (fun hc' -> if (try_eq_subsumption && eq_subsumes hc'.hclits c.hclits)
                  || subsumes hc'.hclits c.hclits then raise Exit);
    Utils.exit_prof prof_subsumption_set;
    false
  with Exit ->
    Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a@] subsumed by active set"
                         !C.pp_clause#pp c));
    Utils.exit_prof prof_subsumption_set;
    true

let subsumed_in_set set c =
  Utils.enter_prof prof_subsumption_in_set;
  incr_stat stat_subsumed_in_set_call;
  (* if c is a single unit clause *)
  let try_eq_subsumption = C.is_unit_clause c && Lits.is_pos c.hclits.(0) in
  (* use feature vector indexing *)
  let l = ref [] in
  FV.retrieve_subsumed
    set#idx_fv c.hclits
    (fun hc' -> if (try_eq_subsumption && eq_subsumes c.hclits hc'.hclits)
                || subsumes c.hclits hc'.hclits then l := hc' :: !l);
  Utils.exit_prof prof_subsumption_in_set;
  !l

(** Number of equational lits. Used as an estimation for the difficulty of the subsumption
    check for this clause. *)
let rec num_equational lits i =
  if i = Array.length lits then 0
  else if Lits.equational lits.(i) then 1 + (num_equational lits (i+1))
  else num_equational lits (i+1)

(* ----------------------------------------------------------------------
 * contextual literal cutting
 * ---------------------------------------------------------------------- *)

exception RemoveLit of int * hclause

(** Performs successive contextual literal cuttings *)
let rec contextual_literal_cutting active_set c =
  Utils.enter_prof prof_clc;
  let ctx = c.hcctx in
  if Array.length c.hclits <= 1 || num_equational c.hclits 0 > 3 || Array.length c.hclits > 8
    then (Utils.exit_prof prof_clc; c) else
  (* do we need to try to use equality subsumption? *)
  let try_eq_subsumption = Utils.array_exists Lits.equational c.hclits in
  (* try to remove one literal from the literal array *)
  let rec remove_one_lit lits = 
    try
      for i = 0 to Array.length lits - 1 do
        (* negate literal *)
        lits.(i) <- Lits.negate lits.(i);
        (* test for subsumption *)
        FV.retrieve_subsuming active_set#idx_fv lits
          (fun c' -> if (try_eq_subsumption && eq_subsumes c'.hclits lits)
                      || subsumes c'.hclits lits
             (* some clause subsumes the literals with i-th literal flipped *)
             then (lits.(i) <- Lits.negate lits.(i); raise (RemoveLit (i, c'))));
        (* restore literal *)
        lits.(i) <- Lits.negate lits.(i);
      done;
      None (* no change *)
    with (RemoveLit (i, c')) ->
      (* remove the literal and recurse *)
      Some (array_except_idx lits i, c')
  in
  match remove_one_lit c.hclits with
  | None -> (Utils.exit_prof prof_clc; c) (* no literal removed *)
  | Some (new_lits, c') -> begin
      (* hc' allowed us to cut a literal *)
      assert (List.length new_lits + 1 = Array.length c.hclits);
      let proof c'' = Proof (c'', "clc", [c.hcproof; c'.hcproof]) in
      let parents = c :: c.hcparents in
      let new_hc = C.mk_hclause ~parents ~ctx new_lits proof in
      Utils.debug 3 (lazy (Utils.sprintf
                    "@[<h>contextual literal cutting in %a using %a gives %a@]"
                    !C.pp_clause#pp_h c !C.pp_clause#pp_h c' !C.pp_clause#pp_h new_hc));
      (* try to cut another literal *)
      Utils.exit_prof prof_clc; 
      contextual_literal_cutting active_set new_hc
    end

(* ----------------------------------------------------------------------
 * contraction
 * ---------------------------------------------------------------------- *)

exception CondensedInto of literal array * substitution

(** performs condensation on the clause. It looks for two literals l1 and l2 of same
    sign such that l1\sigma = l2, and hc\sigma \ {l2} subsumes hc. Then
    hc is simplified into hc\sigma \ {l2}.
    If there are too many equational literals, the simplification is disabled to
    avoid pathologically expensive subsumption checks.
    TODO remove this limitation after an efficient subsumption check is implemented. *)
let rec condensation hc =
  Utils.enter_prof prof_condensation;
  let ctx = hc.hcctx in
  if Array.length hc.hclits <= 1 || num_equational hc.hclits 0 > 3 || Array.length hc.hclits > 8
    then (Utils.exit_prof prof_condensation; hc) else
  (* offset is used to rename literals for subsumption *)
  let lits = hc.hclits in
  let n = Array.length lits in
  try
    for i = 0 to n - 1 do
      let lit = lits.(i) in
      for j = i+1 to n - 1 do
        let lit' = lits.(j) in
        (* try to match lit with lit' (and vice versa), then check if subst(hc) subsumes hc *)
        let substs =  match_lits S.id_subst (lit,0) (lit',0) @
                      match_lits S.id_subst (lit',0) (lit,0) in
        List.iter
          (fun subst ->
            let new_lits = Array.sub lits 0 (n - 1) in
            (if i <> n-1 then new_lits.(i) <- lits.(n-1));  (* remove i-th lit *)
            let new_lits = Lits.apply_subst_lits ~ord:ctx.ctx_ord subst (new_lits,0) in
            (* check subsumption *)
            if subsumes new_lits lits
              then raise (CondensedInto (new_lits, subst)))
          substs
      done;
    done;
    Utils.exit_prof prof_condensation; 
    hc
  with CondensedInto (new_lits, subst) ->
    (* clause is simplified *)
    let proof c = Proof (c, "condensation", [hc.hcproof]) in
    let parents = hc :: hc.hcparents in
    let new_hc = C.mk_hclause_a ~parents ~ctx new_lits proof in
    Utils.debug 3 (lazy (Utils.sprintf
                  "@[<h>condensation in %a (with %a) gives %a@]"
                  !C.pp_clause#pp_h hc S.pp_substitution subst !C.pp_clause#pp_h new_hc));
    (* try to condense further *)
    Utils.exit_prof prof_condensation; 
    condensation new_hc

(* ----------------------------------------------------------------------
 * the Calculus object
 * ---------------------------------------------------------------------- *)

let superposition : Calculus.calculus =
  object
    method binary_rules = ["superposition_active", infer_active;
                           "superposition_passive", infer_passive]

    method unary_rules = ["equality_resolution", infer_equality_resolution;
                          "equality_factoring", infer_equality_factoring]

    method basic_simplify c = basic_simplify c

    method rw_simplify (simpl : PS.simpl_set) hc =
      let hc = basic_simplify (demodulate simpl hc) in
      let hc = positive_simplify_reflect simpl hc in
      let hc = negative_simplify_reflect simpl hc in
      hc

    method active_simplify actives hc =
      (* condensation *)
      let hc = condensation hc in
      (* contextual literal cutting *)
      let hc = contextual_literal_cutting actives hc in
      hc

    method backward_simplify actives hc =
      let set = C.CSet.empty in
      backward_demodulate actives set hc

    method redundant actives hc =
      subsumed_by_set actives hc

    method backward_redundant actives hc =
      subsumed_in_set actives hc

    method list_simplify hc =
      if is_tautology hc then [] else [hc]

    method is_trivial hc = is_tautology hc

    method axioms = []

    method constr _ = [Precedence.min_constraint [split_symbol; false_symbol; true_symbol]]

    method preprocess ~ctx l =
      List.fold_left
        (fun acc hc ->
          (* reduction to CNF *)
          let clauses = Cnf.cnf_of hc in
          List.fold_left
            (fun acc hc ->
              let hc = C.clause_of_fof (C.update_ctx ~ctx hc) in
              let hc = basic_simplify hc in
              if is_tautology hc then acc else hc :: acc)
            acc clauses)
        [] l
  end
