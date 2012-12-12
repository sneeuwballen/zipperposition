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

let print_stats () =
  List.iter
    (fun (name, cnt) -> Format.printf "%% %-30s ... %s@." name (Int64.to_string !cnt))
    [stat_superposition_call; stat_equality_resolution_call; stat_equality_factoring_call;
     stat_subsumption_call; stat_subsumed_in_set_call; stat_subsumed_by_set_call;
     stat_basic_simplify; stat_demodulate_call; stat_demodulate_step;
     C.stat_fresh; C.stat_mk_hclause; C.stat_new_clause]

(* for profiling *)
let enable = true

let prof_demodulate = HExtlib.profile ~enable "demodulate"
let prof_clc = HExtlib.profile ~enable "contextual_literal_cutting"
let prof_condensation = HExtlib.profile ~enable "condensation"
let prof_basic_simplify = HExtlib.profile ~enable "basic_simplify"
let prof_subsumption = HExtlib.profile ~enable "subsumption"
let prof_eq_subsumption = HExtlib.profile ~enable "equality_subsumption"
let prof_subsumption_set = HExtlib.profile ~enable "forward_subsumption"
let prof_subsumption_in_set = HExtlib.profile ~enable "backward_subsumption"
let prof_infer_active = HExtlib.profile ~enable "infer_active"
let prof_infer_passive = HExtlib.profile ~enable "infer_passive"
let prof_infer_equality_resolution = HExtlib.profile ~enable "infer_equality_resolution"
let prof_infer_equality_factoring = HExtlib.profile ~enable "infer_equality_factoring"

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
  assert ((Utils.list_inter T.eq_term (Lazy.force active_clause.cvars) (Lazy.force passive_clause.cvars)) = []);
  assert (T.db_closed s);
  if not sign_st 
  then (Utils.debug 3 (lazy "... active literal is negative"); acc)
  else if not (T.atomic s) (* do not rewrite non-atomic formulas *)
  then (Utils.debug 3 (lazy "... active term is not atomic or DB-closed"); acc)
  else if not (T.db_closed (T.at_pos u subterm_pos))
    && (List.exists (fun x -> S.is_in_subst x subst) (Lazy.force passive_clause.cvars))
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
        let proof = lazy (Proof (rule, [(active_clause, active_pos, subst);
                                        (passive_clause, passive_pos, subst)])) in
        let new_clause = C.mk_hclause ~ord new_lits proof [active_clause.cref; passive_clause.cref] in
        Utils.debug 3 (lazy (Utils.sprintf "... ok, conclusion @[<h>%a@]"
                            !C.pp_clause#pp_h new_clause));
        new_clause :: acc
      end
  end

let infer_active_ (actives : ProofState.active_set) clause : hclause list =
  let ord = actives#ord in
  (* no literal can be eligible for paramodulation if some are selected *)
  if C.has_selected_lits clause.cref then [] else
  (* perform inferences with i-th literal? *)
  let eligible_lit i lit = C.pos_lit lit && C.is_maxlit clause.cref i in
  (* do the inferences where clause is active; for this,
     we try to rewrite conditionally other clauses using
     non-minimal sides of every positive literal *)
  Calculus.fold_lits ~both:true eligible_lit
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

let infer_active actives clause =
  prof_infer_active.HExtlib.profile (infer_active_ actives) clause

let infer_passive_ (actives:ProofState.active_set) clause : hclause list =
  let ord = actives#ord in
  let hc = clause.cref in
  (* perform inference on this lit? *)
  let eligible i lit = if C.has_selected_lits hc then C.is_selected hc i else C.is_maxlit hc i in
  (* do the inferences in which clause is passive (rewritten),
     so we consider both negative and positive literals *)
  Calculus.fold_lits ~both:true eligible
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

let infer_passive actives clause =
  prof_infer_passive.HExtlib.profile (infer_passive_ actives) clause

let infer_equality_resolution_ ~ord hc =
  let clause = C.base_clause hc in
  (* literals that can potentially be eligible for resolution *)
  let eligible i lit =
    C.neg_lit lit && (if C.has_selected_lits hc then C.is_selected hc i else C.is_maxlit hc i) in
  (* iterate on those literals *)
  Calculus.fold_lits ~both:false eligible
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
            let proof = lazy (Proof ("eq_res", [clause, [pos], subst]))
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

let infer_equality_resolution ~ord clause =
  prof_infer_equality_resolution.HExtlib.profile (infer_equality_resolution_ ~ord) clause

let infer_equality_factoring_ ~ord hc =
  if C.has_selected_lits hc then [] else (* no literal is eligible for paramodulation *)
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
        let proof = lazy (Proof ("eq_fact",
          [(clause, active_pos, subst); (clause, passive_pos, subst)]))
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
  in Calculus.fold_lits ~both:true eligible
    (fun acc s t _ s_pos -> (* try with s=t *)
      let unifiables = find_unifiable_lits s s_pos in
      List.fold_left
        (fun acc (passive_pos, subst) ->
          (do_inference s_pos passive_pos subst) @ acc)
        acc unifiables)
    [] hc.hclits

let infer_equality_factoring ~ord clause =
  prof_infer_equality_factoring.HExtlib.profile (infer_equality_factoring_ ~ord) clause

(* ----------------------------------------------------------------------
 * simplifications
 * ---------------------------------------------------------------------- *)

exception RewriteInto of term

(** Compute normal form of term w.r.t active set. Clauses used to
    rewrite are added to the clauses hashset. *)
let demod_nf ?(subterms_only=false) simpl_set clauses t =
  let ord = simpl_set#ord in
  let oriented_hclause hc = match hc.hclits with
  | [|Equation (_,_,true,Gt)|] | [|Equation (_,_,true,Lt)|] -> true (* oriented *)
  | _ -> false in
  (* compute normal form of subterm *) 
  let rec normal_form t =
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
              if oriented_hclause unit_hclause || ord#compare new_l new_r = Gt
                (* subst(l) > subst(r), we can rewrite *)
                then begin
                  assert (ord#compare new_l new_r = Gt);
                  Utils.debug 4 (lazy (Utils.sprintf "rewrite %a into %a using %a"
                                 !T.pp_term#pp new_l !T.pp_term#pp new_r
                                 !C.pp_clause#pp_h unit_hclause));
                  clauses := (C.base_clause unit_hclause, [0], subst) :: !clauses;
                  incr_stat stat_demodulate_step;
                  raise (RewriteInto new_r)
                end else Utils.debug 4
                  (lazy (Utils.sprintf "could not rewrite %a into %a using %a, bad order"
                         !T.pp_term#pp new_l !T.pp_term#pp new_r
                         !C.pp_clause#pp_h unit_hclause)));
          t (* not found any match, normal form found *)
        with RewriteInto t' ->
          normal_form t' (* done one rewriting step, continue *)
      end
  (* rewrite innermost-leftmost *)
  and traverse t =
    match t.term with
    | Var _ -> t
    | Node (s, l) ->
      (* rewrite subterms *)
      let l' = List.map traverse l in
      let t' = if List.for_all2 (==) l l' then t else T.mk_node s t.sort l' in
      (* rewrite term at root *)
      normal_form t'
  in
  match t.term with
  | Node (f, ts) when subterms_only -> (* rewrite only subterms *)
    T.mk_node f t.sort (List.map traverse ts)
  | _ -> traverse t

let demodulate_ simpl_set c =
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
      C.mk_eq ~ord (demod_nf ~subterms_only:true simpl_set clauses l) (demod_nf simpl_set clauses r)
    | Equation (l, r, true, Lt) when C.eligible_res ~ord c i S.id_subst ->
      C.mk_eq ~ord (demod_nf simpl_set clauses l) (demod_nf ~subterms_only:true simpl_set clauses r)
    | Equation (l, r, true, _) when C.eligible_res ~ord c i S.id_subst ->
      C.mk_eq ~ord (demod_nf ~subterms_only:true simpl_set clauses l)
                   (demod_nf ~subterms_only:true simpl_set clauses r)
    | Equation (l, r, true, _) ->
      C.mk_eq ~ord (demod_nf simpl_set clauses l) (demod_nf simpl_set clauses r)
  in
  (* demodulate every literal *)
  let lits = Array.mapi demod_lit c.clits in
  if !clauses = []
    then begin (* no rewriting performed *)
      assert (Utils.array_forall2 C.eq_literal_com c.clits lits);
      c.cref
    end else begin  (* construct new clause *)
      let proof = lazy (Proof ("demod", (c, [], S.id_subst) :: !clauses)) in
      (* parents are clauses used to simplify the clause, plus parents of the clause *)
      let parents = (List.map (fun (c,_,_) -> c.cref) !clauses) @ c.cref.hcparents in
      C.mk_hclause_a ~ord lits proof parents
    end

let demodulate simpl_set c = prof_demodulate.HExtlib.profile (demodulate_ simpl_set) c

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
    match HExtlib.list_index er_check lits with
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
  then hc (* no change *)
  else begin
    let parents = hc :: hc.hcparents in
    let proof = hc.hcproof in  (* do not bother printing this *)
    let new_clause = C.mk_hclause ~ord new_lits proof parents in
    Utils.debug 3 (lazy (Utils.sprintf "@[<hov 4>@[<h>%a@]@ basic_simplifies into @[<h>%a@]@]"
    !C.pp_clause#pp_h hc !C.pp_clause#pp_h new_clause));
    new_clause
  end

exception FoundMatch of (term * hclause * substitution)

let positive_simplify_reflect simpl_set c =
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
    then c.cref (* no literal removed, same clause *)
    else 
      let proof = lazy (Proof ("simplify_reflect+", (c, [], S.id_subst)::premises)) in
      let premises = List.map (fun (c,_,_) -> c.cref) premises in
      let new_hc = C.mk_hclause ~ord lits proof (c.cref::premises) in
      Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a pos_simplify_reflect into %a@]"
                    !C.pp_clause#pp c !C.pp_clause#pp_h new_hc));
      new_hc
    
let negative_simplify_reflect simpl_set c =
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
    then c.cref (* no literal removed *)
    else 
      let proof = lazy (Proof ("simplify_reflect-", (c, [], S.id_subst)::premises)) in
      let premises = List.map (fun (c,_,_) -> c.cref) premises in
      let new_hc = C.mk_hclause ~ord lits proof (c.cref::premises) in
      Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a neg_simplify_reflect into %a@]"
                    !C.pp_clause#pp c !C.pp_clause#pp_h new_hc));
      new_hc

(* ----------------------------------------------------------------------
 * subsumption
 * ---------------------------------------------------------------------- *)

(** raised when a subsuming substitution is found *)
exception SubsumptionFound of substitution

(** Check compatibility of two (normalized) substitutions *)
let compatible_substs s1 s2 =
  S.reset_bindings s1;
  S.reset_bindings s2;
  S.apply_subst_bind s2;  (* apply s2, but not s1 *)
  List.for_all
    (fun (v1, t1) ->
      (* either s2 does not bind v1, or it binds it to the same term *)
      T.eq_term v1 v1.binding || T.eq_term v1.binding t1)
    s1

(** Merge two compatible substs (merge s1 into s2) *)
let merge_substs s1 s2 =
  (* reset bindings occurring in substitutions *)
  S.reset_bindings s1;
  S.reset_bindings s2;
  S.apply_subst_bind s2;
  List.fold_left
    (fun subst (v, t) ->
      if T.eq_term v v.binding
        then (v, t) :: subst  (* not bound in s2 *)
        else subst)
    s2 s1

(** checks whether subst(lit_a) subsumes subst(lit_b). Returns a list of
    substitutions s such that s(lit_a) = lit_b and s contains subst. The list
    is empty if lit_a does not subsume lit_b. *)
let match_lits ~locked subst lit_a lit_b =
  match lit_a, lit_b with
  | Equation (_, _, signa, _), Equation (_, _, signb, _) when signa <> signb -> [] (* different sign *)
  | Equation (la, ra, signa, _), Equation (lb, rb, signb, _) when T.eq_term la lb && T.eq_term ra rb ->
    [S.id_subst]
  | Equation (la, ra, signa, _), Equation (lb, rb, signb, _) when T.eq_term la rb && T.eq_term ra lb ->
    [S.id_subst]
  | Equation (la, ra, signa, _), Equation (lb, rb, signb, _) when T.eq_term la lb ->
    (try [Unif.matching_locked ~locked subst ra rb]
    with UnificationFailure -> [])
  | Equation (la, ra, signa, _), Equation (lb, rb, signb, _) when T.eq_term la rb ->
    (try [Unif.matching_locked ~locked subst ra lb]
    with UnificationFailure -> [])
  | Equation (la, ra, signa, _), Equation (lb, rb, signb, _) when T.eq_term ra rb ->
    (try [Unif.matching_locked ~locked subst la lb]
    with UnificationFailure -> [])
  | Equation (la, ra, signa, _), Equation (lb, rb, signb, _) when T.eq_term ra lb ->
    (try [Unif.matching_locked ~locked subst la rb]
    with UnificationFailure -> [])
  | Equation (la, ra, signa, _), Equation (lb, rb, signb, _) -> (* general case *)
    (try
      let subst = Unif.matching_locked ~locked subst la lb in
      let subst = Unif.matching_locked ~locked subst ra rb in
      [subst]
    with UnificationFailure -> []) @
    (try
      let subst = Unif.matching_locked ~locked subst la rb in
      let subst = Unif.matching_locked ~locked subst ra lb in
      [subst]
    with UnificationFailure -> [])

(** finds all literals matched by this literal in clause, with the
    corresponding substitutions. It returns a list of
    (index of subsumed lit, subst) *)
let matched_lits ~locked lit lits =
  let ans = ref [] 
  and count = ref 0 in
  Array.iteri
    (fun idx lit' -> (* match lit with lit', add substitutions to ans *)
      List.iter
        (fun subst -> ans := (idx, subst) :: !ans; incr count)
        (match_lits ~locked S.id_subst lit lit'))
    lits;
  !count, !ans

(** Hashset containing all variables of the list of literals *)
let vars_of_lits lits =
  let set = T.THashSet.create () in
  let update_set t = List.iter (T.THashSet.add set) t.vars in
  Array.iter
    (function (Equation (l,r,_,_)) -> update_set l; update_set r)
    lits;
  set

(** Check whether a subsumes b, and if it does, return the
    corresponding substitution *)
let subsumes_with a b =
  incr_stat stat_subsumption_call;
  (* a must not have more literals *)
  if Array.length a > Array.length b then None else
  (* variables that cannot be bound during subsumption *)
  let locked = vars_of_lits b in
  (* list of matched literals in b, for each literal of a *)
  let ans_array = Array.map (fun lit -> matched_lits ~locked lit b) a in
  (* sort by increasing number of solutions *)
  Array.sort (fun (count1, _) (count2, _) -> count1 - count2) ans_array;
  let ans_array = Array.map snd ans_array in
  (* find a compatible superset of all those substitutions *)
  let rec find_compatible subst indexes i =
    if i = Array.length ans_array
    then raise (SubsumptionFound subst) (* done the whole array *)
    else
      let substs = ans_array.(i) in
      List.iter
        (fun (idx, s) ->
          match () with
          | _ when Ptset.mem idx indexes -> ()
          | _ when compatible_substs s subst ->
            (* merge subst with lits, continue *)
            let indexes' = Ptset.add idx indexes
            and subst' = merge_substs s subst in
            find_compatible subst' indexes' (i+1)
          | _ -> ()
        ) substs;
  in
  if Utils.array_exists (fun substs -> substs = []) ans_array
    then None  (* some literal matches no literal in b *)
    else try
      find_compatible S.id_subst Ptset.empty 0;
      None  (* no subsuming substitution found *)
    with SubsumptionFound subst -> Some subst

let subsumes a b =
  let check a b = match subsumes_with a b with
  | None -> false
  | Some _ -> true
  in
  prof_subsumption.HExtlib.profile (check a) b

let eq_subsumes_ a b =
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
  incr_stat stat_eq_subsumption_call;
  match a with
  | [|Equation (s, t, true, _)|] ->
    let res = Utils.array_exists (equate_lit_with s t) b in
    (if res then Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a eq-subsumes@ %a@]"
                                C.pp_lits a C.pp_lits b)));
    res
  | _ -> false  (* only a positive unit clause unit-subsumes a clause *)

let eq_subsumes a b = prof_eq_subsumption.HExtlib.profile (eq_subsumes_ a) b

let subsumed_by_set_ set c =
  incr_stat stat_subsumed_by_set_call;
  (* if there is an equation in c, try equality subsumption *)
  let try_eq_subsumption = Utils.array_exists C.equational_lit c.clits in
  (* use feature vector indexing *)
  try
    FV.retrieve_subsuming set#idx_fv c.cref.hclits
      (fun hc' -> if (try_eq_subsumption && eq_subsumes hc'.hclits c.clits)
                  || subsumes hc'.hclits c.clits then raise Exit);
    false
  with Exit ->
    Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a@] subsumed by active set"
                         !C.pp_clause#pp c));
    true

let subsumed_by_set set clause =
  prof_subsumption_set.HExtlib.profile (subsumed_by_set_ set) clause

let subsumed_in_set_ set c =
  incr_stat stat_subsumed_in_set_call;
  (* if c is a single unit clause *)
  let try_eq_subsumption = C.is_unit_clause c.cref && C.pos_lit c.clits.(0) in
  (* use feature vector indexing *)
  let l = ref [] in
  FV.retrieve_subsumed
    set#idx_fv c.cref.hclits
    (fun hc' -> if (try_eq_subsumption && eq_subsumes c.clits hc'.hclits)
                || subsumes c.clits hc'.hclits then l := hc' :: !l);
  !l

let subsumed_in_set set clause =
  prof_subsumption_in_set.HExtlib.profile (subsumed_in_set_ set) clause

(* ----------------------------------------------------------------------
 * contextual literal cutting
 * ---------------------------------------------------------------------- *)

exception RemoveLit of int * hclause

(** Performs successive contextual literal cuttings *)
let rec contextual_literal_cutting_ active_set hc =
  if Array.length hc.hclits <= 1 then hc else
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
  | None -> hc (* no literal removed *)
  | Some (new_lits, hc') -> begin
      (* hc' allowed us to cut a literal *)
      assert (List.length new_lits + 1 = Array.length hc.hclits);
      let proof = lazy (Proof ("clc", [c, [], S.id_subst;
                                       C.base_clause hc', [], S.id_subst]))
      and parents = c.cref :: c.cref.hcparents in
      let new_hc = C.mk_hclause ~ord new_lits proof parents in
      Utils.debug 3 (lazy (Utils.sprintf
                    "@[<h>contextual literal cutting in %a using %a gives %a@]"
                    !C.pp_clause#pp_h hc !C.pp_clause#pp_h hc' !C.pp_clause#pp_h new_hc));
      (* try to cut another literal *)
      contextual_literal_cutting_ active_set new_hc
    end

let contextual_literal_cutting active_set hc =
  prof_clc.HExtlib.profile (contextual_literal_cutting_ active_set) hc

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

(** performs condensation on the clause. It looks for two literals l1 and l2 of same
    sign such that l1\sigma = l2, and hc\sigma \ {l2} subsumes hc. Then
    hc is simplified into hc\sigma \ {l2} *)
let rec condensation_ ~ord hc =
  if Array.length hc.hclits <= 1 then hc else
  (* offset is used to rename literals for subsumption *)
  let offset = T.max_var hc.hcvars +1 in
  let lits = hc.hclits in
  let n = Array.length lits in
  try
    for i = 0 to n - 1 do
      let lit = lits.(i) in
      for j = i+1 to n - 1 do
        let lit' = lits.(j) in
        (* try to match lit with lit', then check if subst(hc) subsumes hc *)
        let substs = match_literals lit lit' in
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
    hc
  with CondensedInto (new_lits, subst) ->
    (* clause is simplified *)
    let proof = lazy (Proof ("condensation", [C.base_clause hc, [], subst]))
    and parents = hc :: hc.hcparents in
    let new_hc = C.mk_hclause_a ~ord new_lits proof parents in
    Utils.debug 3 (lazy (Utils.sprintf
                  "@[<h>condensation in %a (with %a) gives %a@]"
                  !C.pp_clause#pp_h hc S.pp_substitution subst !C.pp_clause#pp_h new_hc));
    (* try to condense further *)
    condensation_ ~ord new_hc

let condensation ~ord hc =
  prof_condensation.HExtlib.profile (condensation_ ~ord) hc

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
      let new_t' = !Calculus.skolem ~ord t' sort in
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
      (fun (Equation (l, r, sign, _)) -> T.atomic_rec l && T.atomic_rec r)
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
      let proof = lazy (Proof ("to_cnf", [C.base_clause hc, [], S.id_subst])) in
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

    method simplify ~select actives simpl hc =
      let ord = actives#ord in
      let hc = basic_simplify ~ord hc in
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
      (* condensation *)
      let hc = condensation ~ord hc in
      let hc = C.select_clause ~select hc in
      hc

    method redundant actives hc =
      let c = actives#relocate hc in
      subsumed_by_set actives c

    method redundant_set actives hc =
      let c = actives#relocate hc in
      subsumed_in_set actives c

    method list_simplify ~ord ~select hc =
      if is_tautology hc then Some [] else None  (* no other list simplification *)

    method axioms = []

    method constr _ = Precedence.min_constraint [false_symbol; true_symbol]

    method preprocess ~ord ~select l =
      List.fold_left
        (fun acc hc ->
          (* reduction to CNF *)
          let clauses = cnf_of ~ord hc in
          List.fold_left
            (fun acc hc ->
              let hc = C.reord_hclause ~ord (C.clause_of_fof ~ord hc) in
              let hc = basic_simplify ~ord hc in
              if is_tautology hc then acc else hc :: acc)
            acc clauses)
        [] l
  end
