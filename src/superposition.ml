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
  | Var _ -> []
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

(* helper that does one or zero superposition inference, with all
   the given parameters *)
let do_superposition ~ord active_clause active_pos passive_clause passive_pos subst acc =
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
                       C.pp_pos passive_pos S.pp_substitution subst));
  assert ((Utils.list_inter T.eq_term active_clause.cvars passive_clause.cvars) = []);
  assert (T.db_closed s);
  if not sign_st 
  then (Utils.debug 3 (lazy "... active literal is negative"); acc)
  else if not (T.atomic s) (* do not rewrite non-atomic formulas *)
  then (Utils.debug 3 (lazy "... active term is not atomic or DB-closed"); acc)
  else if not (T.db_closed (T.at_pos u subterm_pos))
    && (List.exists (fun x -> S.is_in_subst x subst) passive_clause.cvars)
  then (Utils.debug 3 (lazy "... narrowing with De Bruijn indices"); acc)
  else
  let t' = S.apply_subst subst t
  and v' = S.apply_subst subst v in
  if sign_uv && T.eq_term t' v' && subterm_pos = []
  then (Utils.debug 3 (lazy "... will yield a tautology"); acc)
  else begin
    if (ord#compare (S.apply_subst subst s) t' = Lt ||
        ord#compare (S.apply_subst subst u) v' = Lt ||
        not (C.eligible_res ~ord passive_clause passive_idx subst) ||
        not (C.eligible_param ~ord active_clause active_idx subst))
      then (Utils.debug 3 (lazy "... has bad ordering conditions"); acc)
      else begin (* ordering constraints are ok *)
        let new_lits = array_except_idx  active_clause.clits active_idx in
        let new_lits = (array_except_idx passive_clause.clits passive_idx) @ new_lits in
        let new_u = T.replace_pos u subterm_pos t' in (* replace s by t in u|_p *)
        let new_lits = (C.mk_lit ~ord new_u v' sign_uv) :: new_lits in
        (* apply substitution *)
        let new_lits = List.map (C.apply_subst_lit ~ord subst) new_lits in
        let rule = if sign_uv then "sup+" else "sup-" in
        let proof = Proof (rule, [(active_clause, active_pos, subst);
                                  (passive_clause, passive_pos, subst)]) in
        let new_clause = C.mk_hclause ~ord new_lits proof [active_clause.cref; passive_clause.cref] in
        Utils.debug 3 (lazy (Utils.sprintf "... ok, conclusion @[<h>%a@]"
                            !C.pp_clause#pp_h new_clause));
        new_clause :: acc
      end
  end

let infer_active (actives : ProofState.active_set) clause : hclause list =
  Utils.enter_prof prof_infer_active;
  let ord = actives#ord in
  (* no literal can be eligible for paramodulation if some are selected *)
  if C.has_selected_lits clause.cref
    then (Utils.exit_prof prof_infer_active; []) else
  (* perform inferences with i-th literal? *)
  let eligible_lit i lit = C.pos_lit lit && C.is_maxlit clause.cref i in
  (* do the inferences where clause is active; for this,
     we try to rewrite conditionally other clauses using
     non-minimal sides of every positive literal *)
  let new_clauses = Calculus.fold_lits ~both:true eligible_lit
    (fun acc s t _ s_pos ->
      (* rewrite clauses using s *)
      actives#idx_sup_into#retrieve_unifiables s acc
        (fun acc u_p set ->
          try (* rewrite u_p with s, if they are unifiable *)
            let subst = Unif.unification S.id_subst s u_p in
            I.ClauseSet.fold
              (fun (hc, u_pos, u_p) acc ->
                let passive = C.base_clause hc in
                do_superposition ~ord clause s_pos passive u_pos subst acc)
              set acc
          with
            UnificationFailure -> acc)
    )
    [] clause.clits
  in
  Utils.exit_prof prof_infer_active;
  new_clauses

let infer_passive (actives:ProofState.active_set) clause : hclause list =
  Utils.enter_prof prof_infer_passive;
  let ord = actives#ord in
  let hc = clause.cref in
  (* perform inference on this lit? *)
  let eligible i lit = if C.has_selected_lits hc then C.is_selected hc i else C.is_maxlit hc i in
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
                let subst = Unif.unification S.id_subst s u_p in
                I.ClauseSet.fold
                  (fun (hc, s_pos, s) acc ->
                      let active = C.base_clause hc in
                      do_superposition ~ord active s_pos clause p subst acc)
                  set acc
              with
                UnificationFailure -> acc))
      in List.rev_append new_clauses acc)
    [] clause.clits
  in
  Utils.exit_prof prof_infer_passive;
  new_clauses

let infer_equality_resolution ~ord hc =
  Utils.enter_prof prof_infer_equality_resolution;
  let clause = C.base_clause hc in
  (* literals that can potentially be eligible for resolution *)
  let eligible i lit =
    C.neg_lit lit && (if C.has_selected_lits hc then C.is_selected hc i else C.is_maxlit hc i) in
  (* iterate on those literals *)
  let new_clauses = Calculus.fold_lits ~both:false eligible
    (fun acc l r sign l_pos ->
      assert (not sign);
      match l_pos with
      | [] -> assert false
      | pos::_ ->
      try
        let subst = Unif.unification S.id_subst l r in
        if C.eligible_res ~ord clause pos subst
          (* subst(lit) is maximal, we can do the inference *)
          then begin
            incr_stat stat_equality_resolution_call;
            let proof = Proof ("eq_res", [clause, [pos], subst])
            and new_lits = array_except_idx clause.clits pos in
            let new_lits = List.map (C.apply_subst_lit ~ord subst) new_lits in
            let new_clause = C.mk_hclause ~ord new_lits proof [hc] in
            Utils.debug 3 (lazy (Utils.sprintf
                          "equality resolution on @[<h>%a@] yields @[<h>%a@]"
                          !C.pp_clause#pp clause !C.pp_clause#pp_h new_clause));
            new_clause::acc
          end else
            acc
      with UnificationFailure -> acc) (* l and r not unifiable, try next *)
    [] hc.hclits
  in
  Utils.exit_prof prof_infer_equality_resolution;
  new_clauses

let infer_equality_factoring ~ord hc =
  Utils.enter_prof prof_infer_equality_factoring;
  if C.has_selected_lits hc (* no literal is eligible for paramodulation *)
    then (Utils.exit_prof prof_infer_equality_factoring; []) else
  let clause = C.base_clause hc in
  let eligible i lit = C.pos_lit lit && C.is_maxlit hc i in
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
              let subst = Unif.unification S.id_subst s u in [[i; C.left_pos], subst]
            with UnificationFailure -> []
          and try_v =  (* try inference between s and v *)
            try
              let subst = Unif.unification S.id_subst s v in [[i; C.right_pos], subst]
            with UnificationFailure -> []
          in try_u @ try_v @ acc)
      [] clause.clits
  (* do the inference between given positions, if ordering
     conditions are respected *)
  and do_inference active_pos passive_pos subst =
    let s, t, sign_st = Calculus.get_equations_sides clause active_pos
    and u, v, sign_uv = Calculus.get_equations_sides clause passive_pos
    and active_idx = List.hd active_pos in
    assert (sign_st && sign_uv);
    (* check whether subst(lit) is maximal, and not (subst(s) < subst(t)) *)
    if ord#compare (S.apply_subst subst s) (S.apply_subst subst t) <> Lt &&
       C.eligible_param ~ord clause active_idx subst
      then begin
        incr_stat stat_equality_factoring_call;
        let proof = Proof ("eq_fact",
          [(clause, active_pos, subst); (clause, passive_pos, subst)])
        (* new_lits: literals of the new clause. remove active literal
           and replace it by a t!=v one, and apply subst *)
        and new_lits = array_except_idx clause.clits active_idx in
        let new_lits = (C.mk_neq ~ord t v) :: new_lits in
        let new_lits = List.map (C.apply_subst_lit ~ord subst) new_lits in
        let new_clause = C.mk_hclause ~ord new_lits proof [hc] in
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
    [] hc.hclits
  in
  Utils.exit_prof prof_infer_equality_factoring;
  new_clauses

(* ----------------------------------------------------------------------
 * simplifications
 * ---------------------------------------------------------------------- *)

exception RewriteInto of term

(** Compute normal form of term w.r.t active set. Clauses used to
    rewrite are added to the clauses hashset.
    restrict is an option for restricting demodulation in positive maximal terms *)
let demod_nf ?(restrict=false) simpl_set clauses t =
  let ord = simpl_set#ord in
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
          simpl_set#idx_simpl#retrieve ~sign:true t
            (fun l r subst unit_hclause ->
              (* r is the term subterm is going to be rewritten into *)
              assert (C.is_unit_clause unit_hclause);
              let new_l = t
              and new_r = S.apply_subst subst r in
              if (not restrict || not (S.is_renaming subst))
              && (oriented_hclause unit_hclause || ord#compare new_l new_r = Gt)
                (* subst(l) > subst(r) and restriction does not apply, we can rewrite *)
                then begin
                  assert (ord#compare new_l new_r = Gt);
                  clauses := (C.base_clause unit_hclause, [0], subst) :: !clauses;
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
    | Var _ -> t
    | Node (s, l) ->
      (* rewrite subterms *)
      let l' = List.map (traverse ~restrict:false) l in
      let t' = if List.for_all2 (==) l l' then t else T.mk_node s t.sort l' in
      (* rewrite term at root *)
      normal_form ~restrict t'
  in
  traverse ~restrict t

let demodulate simpl_set c =
  Utils.enter_prof prof_demodulate;
  incr_stat stat_demodulate_call;
  let ord = simpl_set#ord in
  (* clauses used to rewrite *)
  let clauses = ref [] in
  (* demodulate literals *)
  let demod_lit i lit =
    match lit with
    | Equation (l, r, false, _) ->
      C.mk_neq ~ord (demod_nf simpl_set clauses l) (demod_nf simpl_set clauses r)
    | Equation (l, r, true, Gt) when C.eligible_res ~ord c i S.id_subst ->
      C.mk_eq ~ord (demod_nf ~restrict:true simpl_set clauses l) (demod_nf simpl_set clauses r)
    | Equation (l, r, true, Lt) when C.eligible_res ~ord c i S.id_subst ->
      C.mk_eq ~ord (demod_nf simpl_set clauses l) (demod_nf ~restrict:true simpl_set clauses r)
    | Equation (l, r, true, _) when C.eligible_res ~ord c i S.id_subst ->
      C.mk_eq ~ord (demod_nf ~restrict:true simpl_set clauses l)
                   (demod_nf ~restrict:true simpl_set clauses r)
    | Equation (l, r, true, _) ->
      C.mk_eq ~ord (demod_nf simpl_set clauses l) (demod_nf simpl_set clauses r)
  in
  (* demodulate every literal *)
  let lits = Array.mapi demod_lit c.clits in
  if !clauses = []
    then begin (* no rewriting performed *)
      assert (Utils.array_forall2 C.eq_literal_com c.clits lits);
      Utils.exit_prof prof_demodulate;
      c.cref
    end else begin  (* construct new clause *)
      let proof = Proof ("demod", (c, [], S.id_subst) :: !clauses) in
      (* parents are clauses used to simplify the clause, plus parents of the clause *)
      let clauses = List.map (fun (c,_,_) -> c.cref) !clauses in
      let parents = clauses @ c.cref.hcparents in
      let new_hc = C.mk_hclause_a ~ord lits proof parents in
      Utils.debug 3 (lazy (Utils.sprintf "@[<h>demodulate %a into %a using @[<hv>%a@]@]"
                     !C.pp_clause#pp c !C.pp_clause#pp_h new_hc
                     (Utils.pp_list !C.pp_clause#pp_h) clauses)); 
      (* return simplified clause *)
      Utils.exit_prof prof_demodulate;
      new_hc
    end

(** Find clauses that [given] may demodulate, add them to set *)
let backward_demodulate (active_set : PS.active_set) set given =
  Utils.enter_prof prof_back_demodulate;
  let ord = active_set#ord in
  (* find clauses that might be rewritten by l -> r *)
  let recurse ~oriented set l r =
    active_set#idx_back_demod#retrieve_specializations l set
      (fun set t' clauses ->
        try
          let subst = FoUnif.matching FoSubst.id_subst l t' in
          (* subst(l) matches t' and is > subst(r), very likely to rewrite! *)
          if oriented || ord#compare (S.apply_subst subst l) (S.apply_subst subst r) = Gt
            then
              (* add clauses to the set, they may be rewritten by l -> r *)
              Index.ClauseSet.fold (fun (hc,_,_) set -> C.CSet.add set hc) clauses set
            else set
        with UnificationFailure -> set)
  in
  let set' = match given.clits with
  | [|Equation (l,r,true,Gt)|] -> recurse ~oriented:true set l r
  | [|Equation (l,r,true,Lt)|] -> recurse ~oriented:true set r l
  | [|Equation (l,r,true,_)|] ->
    let set' = recurse ~oriented:false set l r in
    recurse ~oriented:false set' r l  (* both sides can rewrite, but we need to check ordering *)
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

let basic_simplify ~ord hc =
  Utils.enter_prof prof_basic_simplify;
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
  let new_lits = Utils.list_uniq C.eq_literal_com new_lits in
  (* destructive equality resolution *)
  let rec er lits =
    match Utils.list_find er_check lits with
    | None -> lits
    | Some (i, Equation (l, r, sign, _)) ->
        assert (not sign);
        assert (T.is_var l || T.is_var r);
        try
          let subst = Unif.unification S.id_subst l r in
          (* remove the literal, and apply the substitution to the remaining literals
             before trying to find another x!=t *)
          er (List.map (C.apply_subst_lit ~ord subst) (Utils.list_remove lits i))
        with UnificationFailure -> lits
  (* finds candidate literals for destructive ER (lits with >= 1 variable) *)
  and er_check (Equation (l, r, sign, _)) = (not sign) && (T.is_var l || T.is_var r) in
  let new_lits = er new_lits in
  if List.length new_lits = Array.length hc.hclits
  then (Utils.exit_prof prof_basic_simplify; hc) (* no change *)
  else begin
    let parents = hc :: hc.hcparents in
    let proof = hc.hcproof in  (* do not bother printing this *)
    let new_clause = C.mk_hclause ~ord new_lits proof parents in
    Utils.debug 3 (lazy (Utils.sprintf "@[<hov 4>@[<h>%a@]@ basic_simplifies into @[<h>%a@]@]"
                   !C.pp_clause#pp_h hc !C.pp_clause#pp_h new_clause));
    Utils.exit_prof prof_basic_simplify;
    new_clause
  end

exception FoundMatch of (term * hclause * substitution)

let positive_simplify_reflect simpl_set c =
  Utils.enter_prof prof_pos_simplify_reflect;
  let ord = simpl_set#ord in
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
    try simpl_set#idx_simpl#retrieve ~sign:true t1
      (fun l r subst hc ->
        if T.eq_term t2 (S.apply_subst subst r)
        then begin
          Utils.debug 4 (lazy (Utils.sprintf "equate %a and %a using %a"
                      !T.pp_term#pp t1 !T.pp_term#pp t2 !C.pp_clause#pp_h hc));
          raise (FoundMatch (r, hc, subst)) (* success *)
        end else ());
      None (* no match *)
    with FoundMatch (r, hc, subst) ->
      Some ((C.base_clause hc, [0], subst) :: clauses)  (* success *)
  in
  (* fold over literals *)
  let lits, premises = iterate_lits [] (Array.to_list c.clits) [] in
  if List.length lits = Array.length c.clits
    then (Utils.exit_prof prof_pos_simplify_reflect; c.cref) (* no literal removed, same clause *)
    else 
      let proof = Proof ("simplify_reflect+", (c, [], S.id_subst)::premises) in
      let premises = List.map (fun (c,_,_) -> c.cref) premises in
      let new_hc = C.mk_hclause ~ord lits proof (c.cref::premises) in
      Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a pos_simplify_reflect into %a@]"
                    !C.pp_clause#pp c !C.pp_clause#pp_h new_hc));
      Utils.exit_prof prof_pos_simplify_reflect;
      new_hc
    
let negative_simplify_reflect simpl_set c =
  Utils.enter_prof prof_neg_simplify_reflect;
  let ord = simpl_set#ord in
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
    try simpl_set#idx_simpl#retrieve ~sign:false s
      (fun l r subst hc ->
        if T.eq_term t (S.apply_subst subst r)
        then begin
          Utils.debug 3 (lazy (Utils.sprintf "neg_reflect eliminates %a=%a with %a"
                         !T.pp_term#pp s !T.pp_term#pp t !C.pp_clause#pp_h hc));
          raise (FoundMatch (r, hc, subst)) (* success *)
        end else ());
      None (* no match *)
    with FoundMatch (r, hc, subst) ->
      Some (C.base_clause hc, [0], subst)  (* success *)
  in
  (* fold over literals *)
  let lits, premises = iterate_lits [] (Array.to_list c.clits) [] in
  if List.length lits = Array.length c.clits
    then (Utils.exit_prof prof_neg_simplify_reflect; c.cref) (* no literal removed *)
    else 
      let proof = Proof ("simplify_reflect-", (c, [], S.id_subst)::premises) in
      let premises = List.map (fun (c,_,_) -> c.cref) premises in
      let new_hc = C.mk_hclause ~ord lits proof (c.cref::premises) in
      Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a neg_simplify_reflect into %a@]"
                    !C.pp_clause#pp c !C.pp_clause#pp_h new_hc));
      Utils.exit_prof prof_neg_simplify_reflect;
      new_hc

(* ----------------------------------------------------------------------
 * subsumption
 * ---------------------------------------------------------------------- *)

(** raised when a subsuming substitution is found *)
exception SubsumptionFound of substitution

(** Hashset containing all variables of the array of literals *)
let vars_of_lits lits =
  let set = T.THashSet.create () in
  let update_set t = List.iter (T.THashSet.add set) t.vars in
  Array.iter
    (function (Equation (l,r,_,_)) -> update_set l; update_set r)
    lits;
  set

(** checks whether subst(lit_a) subsumes subst(lit_b). Returns a list of
    substitutions s such that s(lit_a) = lit_b and s contains subst. The list
    is empty if lit_a does not subsume lit_b. *)
let match_lits ~locked subst lit_a lit_b =
  (* match t1 with t2, then t1' with t2' *)
  let match4 subst t1 t2 t1' t2' =
    try let subst' = Unif.matching_locked ~locked subst t1 t2 in
        [Unif.matching_locked ~locked subst' t1' t2']
    with UnificationFailure -> []
  and match2 subst t1 t2 =
    try [Unif.matching_locked ~locked subst t1 t2]
    with UnificationFailure -> []
  in
  match lit_a, lit_b with
  | Equation (_, _, signa, _), Equation (_, _, signb, _) when signa <> signb -> [] (* different sign *)
  | Equation (la, ra, _, _), Equation (lb, rb, _, _) when T.eq_term la lb && T.eq_term ra rb -> [S.id_subst]
  | Equation (la, ra, _, _), Equation (lb, rb, _, _) when T.eq_term la rb && T.eq_term ra lb -> [S.id_subst]
  | Equation (la, ra, _, _), Equation (lb, rb, _, _) when T.eq_term la lb -> match2 subst ra rb
  | Equation (la, ra, _, _), Equation (lb, rb, _, _) when T.eq_term la rb -> match2 subst ra lb
  | Equation (la, ra, _, _), Equation (lb, rb, _, _) when T.eq_term ra rb -> match2 subst la lb
  | Equation (la, ra, _, _), Equation (lb, rb, _, _) when T.eq_term ra lb -> match2 subst la rb
  | Equation (la, ra, _, Lt), Equation (lb, rb, _, Lt)
  | Equation (la, ra, _, Gt), Equation (lb, rb, _, Gt) -> (* use monotonicity *)
    match4 subst la lb ra rb
  | Equation (la, ra, _, Gt), Equation (lb, rb, _, Lt)
  | Equation (la, ra, _, Lt), Equation (lb, rb, _, Gt) -> (* use monotonicity *)
    match4 subst la rb ra lb
  | Equation (la, ra, _, _), Equation (lb, rb, _, _) -> (* general case *)
    (match4 subst la lb ra rb) @ (match4 subst la rb ra lb)

(** check that every literal in a matches at least one literal in b *)
let all_lits_match ~locked a b =
  Utils.array_forall
    (fun lita ->
      Utils.array_exists
        (fun litb -> match_lits ~locked S.id_subst lita litb <> [])
        b)
    a

(** Compare literals by subsumption difficulty (see "towards efficient subsumption", Tammet).
    We sort by increasing order, so non-ground, deep, heavy literals are smaller
    (thus tested early) *)
let compare_literals_subsumption lita litb =
  let rec is_ground (Equation (l,r,_,_)) = T.is_ground_term l && T.is_ground_term r
  and depth (Equation (l,r,_,_)) = max (term_depth l) (term_depth r)
  and term_depth t = match t.term with
    | Var _ -> 1
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

open Bitvector

(** Check whether [a] subsumes [b], and if it does, return the
    corresponding substitution *)
let subsumes_with a b =
  incr_stat stat_subsumption_call;
  (* a must not have more literals *)
  if Array.length a > Array.length b then None else
  (* variables that cannot be bound during subsumption *)
  let locked = vars_of_lits b in
  if not (all_lits_match ~locked a b) then None else
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
    else if bv_get bv j then find_matched lita i subst bv (j+1) else begin
    let litb = b.(j) in
    (* match lita and litb, then flag litb as used, and try with next literal of a *)
    let substs = match_lits ~locked subst lita litb in
    let bv' = bv_set bv j in
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
      if (List.exists (fun v -> T.var_occurs v l' || T.var_occurs v r') l.vars ||
          List.exists (fun v -> T.var_occurs v l' || T.var_occurs v r') r.vars)
        then true
        else check_vars lit (j+1)
  in
  try
    Array.sort compare_literals_subsumption a;
    try_permutations 0 S.id_subst 0;
    None
  with (SubsumptionFound subst) -> Some subst

let subsumes a b =
  Utils.enter_prof prof_subsumption;
  let res = match subsumes_with a b with
  | None -> false
  | Some _ -> true
  in
  Utils.exit_prof prof_subsumption;
  res

let eq_subsumes a b =
  (* subsume a literal using a = b *)
  let rec equate_lit_with a b lit =
    match lit with
    | Equation (u, v, true, _) -> equate_terms a b u v
    | _ -> false
  (* make u and v equal using a = b (possibly several times) *)
  and equate_terms a b u v =
    match u.term, v.term with
    | _ when T.eq_term u v -> true 
    | _ when equate_root a b u v -> true
    | Node (f, ss), Node (g, ts) when f == g && List.length ss = List.length ts ->
      List.for_all2 (equate_terms a b) ss ts
    | _ -> false
  (* check whether a\sigma = u and b\sigma = v, for some sigma; or the commutation thereof *)
  and equate_root a b u v =
        (try let subst = Unif.matching S.id_subst a u in S.apply_subst subst b == v
         with UnificationFailure -> false)
    ||  (try let subst = Unif.matching S.id_subst b u in S.apply_subst subst a == v
         with UnificationFailure -> false)
  in
  (* check for each literal *)
  Utils.enter_prof prof_eq_subsumption;
  incr_stat stat_eq_subsumption_call;
  let res = match a with
  | [|Equation (s, t, true, _)|] ->
    let res = Utils.array_exists (equate_lit_with s t) b in
    (if res then Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a eq-subsumes@ %a@]"
                                C.pp_lits a C.pp_lits b)));
    res
  | _ -> false  (* only a positive unit clause unit-subsumes a clause *)
  in
  Utils.exit_prof prof_eq_subsumption;
  res

let subsumed_by_set set c =
  Utils.enter_prof prof_subsumption_set;
  incr_stat stat_subsumed_by_set_call;
  (* if there is an equation in c, try equality subsumption *)
  let try_eq_subsumption = Utils.array_exists C.equational_lit c.clits in
  (* use feature vector indexing *)
  try
    FV.retrieve_subsuming set#idx_fv c.cref.hclits
      (fun hc' -> if (try_eq_subsumption && eq_subsumes hc'.hclits c.clits)
                  || subsumes hc'.hclits c.clits then raise Exit);
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
  let try_eq_subsumption = C.is_unit_clause c.cref && C.pos_lit c.clits.(0) in
  (* use feature vector indexing *)
  let l = ref [] in
  FV.retrieve_subsumed
    set#idx_fv c.cref.hclits
    (fun hc' -> if (try_eq_subsumption && eq_subsumes c.clits hc'.hclits)
                || subsumes c.clits hc'.hclits then l := hc' :: !l);
  Utils.exit_prof prof_subsumption_in_set;
  !l

(* ----------------------------------------------------------------------
 * contextual literal cutting
 * ---------------------------------------------------------------------- *)

exception RemoveLit of int * hclause

(** Performs successive contextual literal cuttings *)
let rec contextual_literal_cutting active_set hc =
  Utils.enter_prof prof_clc;
  if Array.length hc.hclits <= 1 then (Utils.exit_prof prof_clc; hc) else
  let ord = active_set#ord in
  (* do we need to try to use equality subsumption? *)
  let try_eq_subsumption = Utils.array_exists C.equational_lit hc.hclits in
  (* rename literals clause *)
  let c = active_set#relocate hc in
  (* try to remove one literal from the literal array *)
  let rec remove_one_lit lits = 
    try
      for i = 0 to Array.length lits - 1 do
        (* negate literal *)
        lits.(i) <- C.negate_lit lits.(i);
        (* test for subsumption *)
        FV.retrieve_subsuming active_set#idx_fv lits
          (fun hc' -> if (try_eq_subsumption && eq_subsumes hc'.hclits lits)
                      || subsumes hc'.hclits lits
             (* some clause subsumes the literals with i-th literal flipped *)
             then (lits.(i) <- C.negate_lit lits.(i); raise (RemoveLit (i, hc'))));
        (* restore literal *)
        lits.(i) <- C.negate_lit lits.(i);
      done;
      None (* no change *)
    with (RemoveLit (i, hc')) ->
      (* remove the literal and recurse *)
      Some (array_except_idx lits i, hc')
  in
  match remove_one_lit c.clits with
  | None -> (Utils.exit_prof prof_clc; hc) (* no literal removed *)
  | Some (new_lits, hc') -> begin
      (* hc' allowed us to cut a literal *)
      assert (List.length new_lits + 1 = Array.length hc.hclits);
      let proof = Proof ("clc", [c, [], S.id_subst; C.base_clause hc', [], S.id_subst])
      and parents = c.cref :: c.cref.hcparents in
      let new_hc = C.mk_hclause ~ord new_lits proof parents in
      Utils.debug 3 (lazy (Utils.sprintf
                    "@[<h>contextual literal cutting in %a using %a gives %a@]"
                    !C.pp_clause#pp_h hc !C.pp_clause#pp_h hc' !C.pp_clause#pp_h new_hc));
      (* try to cut another literal *)
      Utils.exit_prof prof_clc; 
      contextual_literal_cutting active_set new_hc
    end

(* ----------------------------------------------------------------------
 * contraction
 * ---------------------------------------------------------------------- *)

exception CondensedInto of literal array * substitution

(** match literals. Returns 0, 1 or 2 matching substs. *)
let match_literals lit lit' =
  match lit, lit' with
  | Equation (s,t,sign,_), Equation (l,r,sign',_) when sign = sign' ->
    (try let subst = Unif.matching S.id_subst s l in [Unif.matching subst t r]
    with UnificationFailure -> [])
    @
    (try let subst = Unif.matching S.id_subst s r in [Unif.matching subst t l]
    with UnificationFailure -> [])
  | _ -> []

(** number of equational lits *)
let rec num_equational lits i =
  if i = Array.length lits then 0
  else if C.equational_lit lits.(i) then 1 + (num_equational lits (i+1))
  else num_equational lits (i+1)

(** performs condensation on the clause. It looks for two literals l1 and l2 of same
    sign such that l1\sigma = l2, and hc\sigma \ {l2} subsumes hc. Then
    hc is simplified into hc\sigma \ {l2}.
    If there are too many equational literals, the simplification is disabled to
    avoid pathologically expensive subsumption checks.
    TODO remove this limitation after an efficient subsumption check is implemented. *)
let rec condensation ~ord hc =
  Utils.enter_prof prof_condensation;
  if Array.length hc.hclits <= 1 || num_equational hc.hclits 0 > 3 || Array.length hc.hclits > 8
    then (Utils.exit_prof prof_condensation; hc) else
  (* offset is used to rename literals for subsumption *)
  let offset = T.max_var hc.hcvars +1 in
  let lits = hc.hclits in
  let n = Array.length lits in
  try
    for i = 0 to n - 1 do
      let lit = lits.(i) in
      for j = i+1 to n - 1 do
        let lit' = lits.(j) in
        (* try to match lit with lit' (and vice versa), then check if subst(hc) subsumes hc *)
        let substs = match_literals lit lit' @ match_literals lit' lit in
        List.iter
          (fun subst ->
            let new_lits = Array.sub lits 0 (n - 1) in
            (if i <> n-1 then new_lits.(i) <- lits.(n-1));  (* remove i-th lit *)
            (* rename literals for the subsumption check *)
            let renaming = S.relocate offset hc.hcvars in
            (* apply substitution then renaming *)
            for k = 0 to n-2 do
              new_lits.(k) <- C.apply_subst_lit ~ord renaming (C.apply_subst_lit ~ord subst new_lits.(k));
            done;
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
    let proof = Proof ("condensation", [C.base_clause hc, [], subst])
    and parents = hc :: hc.hcparents in
    let new_hc = C.mk_hclause_a ~ord new_lits proof parents in
    Utils.debug 3 (lazy (Utils.sprintf
                  "@[<h>condensation in %a (with %a) gives %a@]"
                  !C.pp_clause#pp_h hc S.pp_substitution subst !C.pp_clause#pp_h new_hc));
    (* try to condense further *)
    Utils.exit_prof prof_condensation; 
    condensation ~ord new_hc

(* ----------------------------------------------------------------------
 * reduction to CNF
 * ---------------------------------------------------------------------- *)

(** Transform the clause into proper CNF; returns a list of clauses *)
let cnf_of ~ord hc =
  (* unique counter for variable indexes *)
  let varindex = ref 0 in
  (* convert literal to term (reify equality) *)
  let rec lit_to_term (Equation (l,r,sign,_)) =
    if T.eq_term l T.true_term then (if sign then r else T.mk_not r)
    else if T.eq_term r T.true_term then (if sign then l else T.mk_not l)
    else (if sign then T.mk_eq l r else T.mk_not (T.mk_eq l r))
  (* negation normal form (also remove equivalence and implications) *) 
  and nnf t =
    if t.sort <> bool_sort then t else
    match t.term with
    | Var _ | Node (_, []) -> t
    | Node (s, [{term=Node (s', [a; b])}]) when s = not_symbol && s' = and_symbol ->
      nnf (T.mk_or (T.mk_not a) (T.mk_not b))  (* de morgan *)
    | Node (s, [{term=Node (s', [a; b])}]) when s = not_symbol && s' = or_symbol ->
      nnf (T.mk_and (T.mk_not a) (T.mk_not b)) (* de morgan *)
    | Node (s, [a; b]) when s = imply_symbol ->
      nnf (T.mk_or (T.mk_not a) b) (* (a => b) -> (not a or b) *)
    | Node (s, [a; b]) when s = eq_symbol && a.sort = bool_sort ->
      (* (a <=> b) -> (not a or b) and (not b or a) *)
      nnf (T.mk_and
        (T.mk_or (T.mk_not a) b)
        (T.mk_or (T.mk_not b) a))
    | Node (s, [{term=Node (s', [a; b])}]) when s = not_symbol && s' = imply_symbol ->
      nnf (T.mk_and a (T.mk_not b)) (* not (a => b) -> (a and not b) *)
    | Node (s, [{term=Node (s', [a; b])}]) when s = not_symbol && s' = eq_symbol && a.sort = bool_sort ->
      (* not (a <=> b) -> (a <=> (not b)) *)
      nnf (T.mk_or
        (T.mk_and a (T.mk_not b))
        (T.mk_and b (T.mk_not a)))
    | Node (s, [{term=Node (s', [{term=Node (s'', [t'])}])}]) when s = not_symbol && s' = forall_symbol ->
      assert (s'' = lambda_symbol);
      nnf (T.mk_exists (T.mk_not t')) (* not forall -> exists not *)
    | Node (s, [{term=Node (s', [{term=Node (s'', [t'])}])}]) when s = not_symbol && s' = exists_symbol ->
      assert (s'' = lambda_symbol);
      nnf (T.mk_forall (T.mk_not t')) (* not exists -> forall not *)
    | Node (s, [{term=Node (s', [t])}]) when s = not_symbol && s' = not_symbol ->
      nnf t (* double negation *)
    | Node (s, l) ->
      let t' = T.mk_node s t.sort (List.map nnf l) in
      if T.eq_term t t' then t' else nnf t'
  (* skolemization of existentials, removal of forall *)
  and skolemize t = match t.term with
    | Var _ | Node (_, []) -> t
    | Node (s, [{term=Node (s', [t])}]) when s = not_symbol && s' = not_symbol ->
      skolemize t (* double negation *)
    | Node (s, [{term=Node (s', [t'])}]) when s = forall_symbol ->
      assert (s' = lambda_symbol);
      (* a fresh variable *)
      let sort = match T.look_db_sort 0 t with
        | None -> univ_sort
        | Some s -> s in
      let v = T.mk_var (!varindex) sort in
      incr varindex;
      let new_t' = T.db_unlift (T.db_replace t' v) in
      skolemize new_t' (* remove forall *)
    | Node (s, [{term=Node (s', [t'])}]) when s = exists_symbol ->
      assert (s' = lambda_symbol);
      (* make a skolem symbol *)
      let sort = match T.look_db_sort 0 t with
        | None -> univ_sort
        | Some s -> s in
      let new_t' = !T.skolem ~ord t' sort in
      skolemize new_t' (* remove forall *)
    | Node (s, l) -> T.mk_node s t.sort (List.map skolemize l)
  (* reduction to cnf using De Morgan. Returns a list of list of terms *)
  and to_cnf t =
    if t.sort <> bool_sort then [[t, true]]
    else match t.term with
    | Var _ | Node (_, []) -> [[t, true]]
    | Node (s, [t']) when s = not_symbol ->
      assert (T.atomic_rec t' ||
              match t'.term with Node (s', _) when s' = eq_symbol -> true | _ -> false);
      [[t', false]]
    | Node (s, [a; b]) when s = and_symbol ->
      let ca = to_cnf a
      and cb = to_cnf b in
      List.rev_append ca cb
    | Node (s, [a; b]) when s = or_symbol ->
      product (to_cnf a) (to_cnf b)
    | Node _ -> [[t, true]]
  (* cartesian product of lists of lists *)
  and product a b =
    List.fold_left
      (fun acc litsa -> List.fold_left
        (fun acc' litsb -> (litsa @ litsb) :: acc')
        acc b)
      [] a
  (* check whether the clause is already in CNF *)
  and is_cnf hc =
    Utils.array_forall
      (fun (Equation (l, r, sign, _)) -> T.atomic_rec l && T.atomic_rec r &&
                                        (l.sort != bool_sort || (l == T.true_term || r == T.true_term)))
      hc.hclits
  in
  Utils.debug 3 (lazy (Utils.sprintf "input clause %a@." !C.pp_clause#pp_h hc));
  if is_cnf hc
    then begin
      Utils.debug 3 (lazy (Utils.sprintf "clause @[<h>%a@] is cnf" !C.pp_clause#pp_h hc));
      [hc] (* already cnf, perfect *)
    end else
      let lits = Array.to_list hc.hclits in
      let nnf_lits = List.map (fun lit -> nnf (lit_to_term lit)) lits in
      let skolem_lits = List.map (fun t -> skolemize t) nnf_lits in
      let clauses_of_lits = List.map to_cnf skolem_lits in
      (* list of list of literals, by or-product *)
      let lit_list_list = match clauses_of_lits with
        | [] -> assert false  (* is in cnf ;) *)
        | hd::tl -> List.fold_left product hd tl in
      (* build clauses from lits *)
      let proof = Proof ("to_cnf", [C.base_clause hc, [], S.id_subst]) in
      let clauses = List.map
        (fun lits ->
          let lits = List.map (fun (t, sign) -> C.mk_lit ~ord t T.true_term sign) lits in
          let new_hc = C.mk_hclause ~ord lits proof [hc] in
          Utils.debug 4 (lazy (Utils.sprintf "mk_clause %a@." !C.pp_clause#pp_h new_hc));
          C.clause_of_fof ~ord new_hc)
        lit_list_list
      in
      Utils.debug 3 (lazy (Utils.sprintf "%% clause @[<h>%a@] to_cnf -> @[<h>%a@]"
                    !C.pp_clause#pp_h hc (Utils.pp_list !C.pp_clause#pp_h) clauses));
      List.iter (fun hc -> assert (is_cnf hc)) clauses;
      clauses

(* ----------------------------------------------------------------------
 * the Calculus object
 * ---------------------------------------------------------------------- *)

let superposition : Calculus.calculus =
  object
    method binary_rules = ["superposition_active", infer_active;
                           "superposition_passive", infer_passive]

    method unary_rules = ["equality_resolution", infer_equality_resolution;
                          "equality_factoring", infer_equality_factoring]

    method basic_simplify ~ord c = basic_simplify ~ord c

    method rw_simplify ~select (simpl : PS.simpl_set) hc =
      let ord = simpl#ord in
      let hc = C.select_clause ~select hc in
      (* rename for demodulation *)
      let c = simpl#relocate hc in
      let hc = basic_simplify ~ord (demodulate simpl c) in
      (* rename for simplify reflect *)
      let c = simpl#relocate hc in
      let hc = positive_simplify_reflect simpl c in
      (* rename for simplify reflect *)
      let c =  simpl#relocate hc in
      let hc = negative_simplify_reflect simpl c in
      let hc = C.select_clause ~select hc in
      hc

    method active_simplify ~select actives hc =
      (* condensation *)
      let hc = condensation ~ord:actives#ord hc in
      (* contextual literal cutting *)
      let hc = contextual_literal_cutting actives hc in
      let hc = C.select_clause ~select hc in
      hc

    method backward_simplify actives hc =
      let set = C.CSet.empty in
      let c = actives#relocate hc in
      backward_demodulate actives set c

    method redundant actives hc =
      let c = actives#relocate hc in
      subsumed_by_set actives c

    method backward_redundant actives hc =
      let c = actives#relocate hc in
      subsumed_in_set actives c

    method list_simplify ~ord ~select hc =
      if is_tautology hc then [] else [hc]

    method is_trivial hc = is_tautology hc

    method axioms = []

    method constr _ = [Precedence.min_constraint [false_symbol; true_symbol]]

    method preprocess ~ord ~select l =
      List.fold_left
        (fun acc hc ->
          (* reduction to CNF *)
          let clauses = cnf_of ~ord hc in
          List.fold_left
            (fun acc hc ->
              let hc = C.reord_hclause ~ord (C.clause_of_fof ~ord hc) in
              let hc = basic_simplify ~ord hc in
              let hc = C.select_clause ~select hc in
              if is_tautology hc then acc else hc :: acc)
            acc clauses)
        [] l
  end
