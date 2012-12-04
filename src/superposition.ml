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
open Calculus

module T = Terms
module C = Clauses
module O = Orderings
module S = FoSubst
module I = Index
module FV = FeatureVector
module PS = ProofState
module Unif = FoUnif
module Utils = FoUtils

(** a conclusion is a clause, plus the clauses used to infer it *)
type conclusion = clause

(** inferences *)
type inference_rule = ProofState.active_set -> clause -> conclusion list

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
     stat_basic_simplify; stat_demodulate_call; stat_demodulate_step; C.stat_fresh]

(* for profiling *)
let enable = true

let prof_demodulate = HExtlib.profile ~enable "demodulate"
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
    let acc, _, _ =
      List.fold_left
      (fun (acc,idx,post) t ->
          let acc = List.rev_append (aux (pos @ [idx]) t) acc in (* recurse in subterm *)
          if post = [] then acc, idx, []
          else acc, idx+1, List.tl post)
      (acc, 0, tl) tl
    in
    acc
  in
  aux pos t

(** iterate through positions that are common to both terms.
    f has type
    'a -> position -> term -> term -> 'a option,
    so it can choose to stop by returning None. *)
let parallel_positions pos t1 t2 acc f =
  (** fold on both lists *)
  let rec fold acc pos idx l1 l2 = match l1, l2 with
  | [], [] -> Some acc
  | [], _ | _, [] -> None  (* different length *)
  | hd1::tl1, hd2::tl2 ->
    begin
      match aux acc (idx::pos) hd1 hd2 with
      | None -> None
      | Some acc -> fold acc pos (idx+1) tl1 tl2
    end
  (** fold through common positions *)
  and aux acc pos t1 t2 =
    match t1.term, t2.term with
    | Var _, _ | _, Var _ ->
      f acc (List.rev pos) t1 t2
    | Node (hd1, tl1), Node (hd2, tl2) ->
      begin match f acc (List.rev pos) t1 t2 with
      | None when hd1 = hd2 -> fold acc pos 0 tl1 tl2  (* recurse in subterms *)
      | None -> None (* not the same, and not accepted by f *)
      | Some acc -> Some acc (* f is ok on this pair of terms *)
      end
  in aux acc pos t1 t2

(* ----------------------------------------------------------------------
 * inferences
 * ---------------------------------------------------------------------- *)

(* helper that does one or zero superposition inference, with all
   the given parameters *)
let do_superposition ~ord active_clause active_pos passive_clause passive_pos subst acc =
  assert (List.length active_pos = 2);
  incr_stat stat_superposition_call;
  match passive_pos with
  | [] | _::[] -> assert false
  | passive_idx::passive_side::subterm_pos ->
  let active_idx = List.hd active_pos
  and u, v, sign_uv = get_equations_sides passive_clause [passive_idx; passive_side]
  and s, t, sign_st = get_equations_sides active_clause active_pos in
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
        let new_lits = Utils.list_remove active_clause.clits active_idx in
        let new_lits = (Utils.list_remove passive_clause.clits passive_idx) @ new_lits in
        let new_u = T.replace_pos u subterm_pos t' in (* replace s by t in u|_p *)
        let new_lits = (C.mk_lit ~ord new_u v' sign_uv) :: new_lits in
        (* apply substitution *)
        let new_lits = List.map (C.apply_subst_lit ~ord subst) new_lits in
        let rule = if sign_uv then "sup+" else "sup-" in
        let proof = lazy (Proof (rule, [(active_clause, active_pos, subst);
                                        (passive_clause, passive_pos, subst)])) in
        let new_clause = C.mk_clause ~ord new_lits ~selected:[]
          proof [active_clause; passive_clause] in
        Utils.debug 3 (lazy (Utils.sprintf "... ok, conclusion @[<h>%a@]"
                            !C.pp_clause#pp new_clause));
        new_clause :: acc
      end
  end

let infer_active_ actives clause =
  let ord = actives.PS.a_ord in
  if C.selected clause <> [] then []  (* no literal can be eligible for paramodulation *)
  else
  (* do the inferences where clause is active; for this,
     we try to rewrite conditionally other clauses using
     non-minimal sides of every positive literal *)
  fold_positive ~both:true
    (fun acc s t _ s_pos ->
      (* rewrite clauses using s *)
      let subterm_idx = actives.PS.idx#subterm_index in
      subterm_idx#retrieve_unifiables s acc
        (fun acc u_p set ->
          try (* rewrite u_p with s, if they are unifiable *)
            let subst = Unif.unification S.id_subst s u_p in
            I.ClauseSet.fold
              (fun (hc, u_pos, u_p) acc ->
                do_superposition ~ord clause s_pos hc u_pos subst acc)
              set acc
          with
            UnificationFailure -> acc)
    )
    [] (C.maxlits clause)

let infer_active actives clause =
  prof_infer_active.HExtlib.profile (infer_active_ actives) clause

let infer_passive_ actives clause =
  let ord = actives.PS.a_ord in
  let lits = if C.selected clause = []
    then C.maxlits clause
    else C.selected_lits clause in
  (* do the inferences in which clause is passive (rewritten),
     so we consider both negative and positive literals *)
  fold_lits ~both:true ~pos:true ~neg:true
    (fun acc u v _ u_pos ->
      (* rewrite subterms of u *)
      let new_clauses = all_positions u_pos u
        (fun u_p p ->
          (* all terms that occur in an equation in the active_set
             and that are potentially unifiable with u_p (u at position p) *)
          actives.PS.idx#root_index#retrieve_unifiables u_p acc
            (fun acc s set ->
              try
                let subst = Unif.unification S.id_subst s u_p in
                I.ClauseSet.fold
                  (fun (hc, s_pos, s) acc ->
                      do_superposition ~ord hc s_pos clause p subst acc)
                  set acc
              with
                UnificationFailure -> acc))
      in List.rev_append new_clauses acc
    )
    [] lits

let infer_passive actives clause =
  prof_infer_passive.HExtlib.profile (infer_passive_ actives) clause

let infer_equality_resolution_ ~ord clause =
  (* literals that can potentially be eligible for resolution *)
  let lits = if C.selected clause = []
    then C.maxlits clause
    else Utils.list_pos clause.clits in
  (* iterate on those literals *)
  fold_negative ~both:false
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
            and new_lits = Utils.list_remove clause.clits pos in
            let new_lits = List.map (C.apply_subst_lit ~ord subst) new_lits in
            let new_clause = C.mk_clause ~ord new_lits ~selected:[] proof [clause] in
            Utils.debug 3 (lazy (Utils.sprintf
                          "equality resolution on @[<h>%a@] yields @[<h>%a@]"
                          !C.pp_clause#pp clause !C.pp_clause#pp new_clause));
            new_clause::acc
          end else
            acc
      with UnificationFailure -> acc (* l and r not unifiable, try next *)
    )
    [] lits

let infer_equality_resolution ~ord clause =
  prof_infer_equality_resolution.HExtlib.profile (infer_equality_resolution_ ~ord) clause

let infer_equality_factoring_ ~ord clause =
  if C.selected clause <> [] then [] (* no eligible literal *)
  else
  let lits_pos = Utils.list_pos clause.clits in
  (* find root terms that are unifiable with s and are not in the
     literal at s_pos. This returns a list of position and substitution *)
  let find_unifiable_lits s s_pos =
    List.fold_left
      (fun acc (Equation (u, v, sign, _), idx) ->
        if not sign then acc
        else if List.hd s_pos = idx then acc (* same index *)
        else
          let try_u =  (* try inference between s and u *)
            try
              let subst = Unif.unification S.id_subst s u in [[idx; C.left_pos], subst]
            with UnificationFailure -> []
          and try_v =  (* try inference between s and v *)
            try
              let subst = Unif.unification S.id_subst s v in [[idx; C.right_pos], subst]
            with UnificationFailure -> []
          in try_u @ try_v @ acc
      )
      [] lits_pos
  (* do the inference between given positions, if ordering
     conditions are respected *)
  and do_inference active_pos passive_pos subst =
    let s, t, sign_st = get_equations_sides clause active_pos
    and u, v, sign_uv = get_equations_sides clause passive_pos
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
        and new_lits = Utils.list_remove clause.clits active_idx in
        let new_lits = (C.mk_neq ~ord t v) :: new_lits in
        let new_lits = List.map (C.apply_subst_lit ~ord subst) new_lits in
        let new_clause = C.mk_clause ~ord new_lits ~selected:[] proof [clause] in
        Utils.debug 3 (lazy (Utils.sprintf
                      "equality factoring on @[<h>%a@] yields @[<h>%a@]"
                      !C.pp_clause#pp clause !C.pp_clause#pp new_clause));
        [new_clause]
      end else
        []
  (* try to do inferences with each positive literal *)
  in fold_positive ~both:true
    (fun acc s t _ s_pos -> (* try with s=t *)
      let unifiables = find_unifiable_lits s s_pos in
      List.fold_left
        (fun acc (passive_pos, subst) ->
          (do_inference s_pos passive_pos subst) @ acc)
        acc unifiables)
    [] (C.maxlits clause)

let infer_equality_factoring ~ord clause =
  prof_infer_equality_factoring.HExtlib.profile (infer_equality_factoring_ ~ord) clause

(* ----------------------------------------------------------------------
 * simplifications
 * ---------------------------------------------------------------------- *)

exception RewriteInto of term

(** Compute normal form of term w.r.t active set. Clauses used to
    rewrite are added to the clauses hashset. *)
let demod_nf ?(subterms_only=false) ~ord idx clauses t =
  let oriented_hclause hc = match hc.clits with
  | [Equation (_,_,true,Gt)] | [Equation (_,_,true,Lt)] -> true (* oriented *)
  | _ -> false in
  (* compute normal form of subterm *) 
  let rec normal_form t =
    (* do not rewrite non-atomic formulas *)
    if not (T.atomic t)
      then t  (* do not rewrite such formulas *)
      else begin
        (* find equations l=r that match subterm *)
        try
          idx#retrieve ~sign:true t
            (fun l r subst unit_hclause ->
              (* r is the term subterm is going to be rewritten into *)
              let new_l = t
              and new_r = S.apply_subst subst r in
              if oriented_hclause unit_hclause || ord#compare new_l new_r = Gt
                (* subst(l) > subst(r), we can rewrite *)
                then begin
                  assert (ord#compare new_l new_r = Gt); (* TODO remove *)
                  Utils.debug 4 (lazy (Utils.sprintf "rewrite %a into %a using %a"
                                 !T.pp_term#pp new_l !T.pp_term#pp new_r
                                 !C.pp_clause#pp_h unit_hclause));
                  C.CHashSet.add clauses unit_hclause;
                  incr_stat stat_demodulate_step;
                  raise (RewriteInto new_r)
                end else Utils.debug 4
                  (lazy (Utils.sprintf "could not rewrite %a into %a using %a, bad order"
                         !T.pp_term#pp new_l !T.pp_term#pp new_r
                         !C.pp_clause#pp_h unit_hclause)));
          t (* not found any match, normal form found *)
        with RewriteInto t' -> normal_form t'
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

let demodulate ~ord idx clause =
  incr_stat stat_demodulate_call;
  (* clauses used to rewrite *)
  let clauses = C.CHashSet.create () in
  (* demodulate literals *)
  let demod_lit i lit =
    match lit with
    | Equation (l, r, false, _) ->
      C.mk_neq ~ord (demod_nf ~ord idx clauses l) (demod_nf ~ord idx clauses r)
    | Equation (l, r, true, Gt) when C.eligible_res ~ord clause i S.id_subst ->
      C.mk_eq ~ord (demod_nf ~subterms_only:true ~ord idx clauses l) (demod_nf ~ord idx clauses r)
    | Equation (l, r, true, Lt) when C.eligible_res ~ord clause i S.id_subst ->
      C.mk_eq ~ord (demod_nf ~ord idx clauses l) (demod_nf ~subterms_only:true ~ord idx clauses r)
    | Equation (l, r, true, _) ->  (* rewrite unconditionally *)
      C.mk_eq ~ord (demod_nf ~ord idx clauses l) (demod_nf ~ord idx clauses r)
  in
  (* demodulate every literal *)
  let lits = Utils.list_mapi clause.clits demod_lit in
  if C.CHashSet.is_empty clauses
    then begin (* no rewriting performed *)
      assert (List.for_all2 C.eq_literal clause.clits lits);
      clause
    end else begin  (* construct new clause *)
      let clauses = C.CHashSet.to_list clauses in
      let clauses_subst = List.map (fun c -> (c, [], S.id_subst)) clauses in
      let proof = lazy (Proof ("demod", (clause, [], S.id_subst)::clauses_subst)) in
      (* parents are clauses used to simplify the clause, plus parents of the clause *)
      let parents = clauses @ clause.cparents in
      C.mk_clause ~ord lits ~selected:clause.cselected proof parents
    end

let is_tautology c =
  let rec check lits = match lits with
  | [] -> false
  | (Equation (l, r, true, _))::_ when l == r -> true
  | (Equation (l, r, sign, _))::lits' ->
    List.exists
      (fun (Equation (l', r', sign', _)) ->
          (sign = not sign') &&
          (((T.eq_term l l') && (T.eq_term r r')) ||
          ((T.eq_term l r') && (T.eq_term l' r))))
      c.clits
   || check lits'
  in
  let is_tauto = check c.clits in
  (if is_tauto then
    Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a@] is a tautology" !C.pp_clause#pp c)));
  is_tauto

(** semantic tautology deletion, using a congruence closure algorithm
    to see if negative literals imply some positive literal *)
let is_semantic_tautology c = false (* TODO *)

let basic_simplify ~ord clause =
  incr_stat stat_basic_simplify;
  (* convert some fof to literals *)
  let clause = C.clause_of_fof ~ord clause in
  let absurd_lit lit = match lit with
  | Equation (l, r, false, _) when T.eq_term l r -> true
  | _ -> false in
  (* remove s!=s literals *)
  let new_lits = List.filter (fun lit -> not (absurd_lit lit)) clause.clits in
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
  let parents = clause :: C.parents clause in
  let proof = clause.cproof in  (* do not bother printing this *)
  let new_clause = C.mk_clause ~ord new_lits ~selected:[] proof parents in
  (* XXX note: we should keep same selected literals if no literal has been removed,
     but it looks like it makes the prover much slower... *)
  (if not (C.eq_clause new_clause clause) then
      (Utils.debug 3 (lazy (Utils.sprintf "@[<hov 4>@[<h>%a@]@ basic_simplifies into @[<h>%a@]@]"
      !C.pp_clause#pp clause !C.pp_clause#pp new_clause))));
  new_clause

exception FoundMatch of (term * hclause)

let positive_simplify_reflect ~ord idx clause =
  (* iterate through literals and try to resolve negative ones *)
  let rec iterate_lits acc lits clauses = match lits with
  | [] -> List.rev acc, clauses
  | (Equation (s, t, false, _) as lit, idx)::lits' ->
    begin match equatable_terms clauses s t with
    | None -> (* keep literal *)
      iterate_lits (lit::acc) lits' clauses
    | Some new_clauses -> (* drop literal, remember clauses *)
      iterate_lits acc lits' new_clauses
    end
  | (lit, _)::lits' -> iterate_lits (lit::acc) lits' clauses
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
    try idx#retrieve ~sign:true t1
      (fun l r subst hc ->
        if T.eq_term t2 (S.apply_subst subst r)
        then begin
          Utils.debug 4 (lazy (Utils.sprintf "equate %a and %a using %a"
                      !T.pp_term#pp t1 !T.pp_term#pp t2 !C.pp_clause#pp hc));
          raise (FoundMatch (r, hc)) (* success *)
        end else ());
      None (* no match *)
    with FoundMatch (r, clause) ->
      Some (clause :: clauses)  (* success *)
  in
  (* fold over literals *)
  let lits, premises = iterate_lits [] (Utils.list_pos clause.clits) [] in
  if List.length lits = List.length clause.clits
    then clause (* no literal removed *)
    else 
      let proof = lazy (Proof ("simplify_reflect+",
        (clause, [], S.id_subst)::(List.map (fun c -> (c, [], S.id_subst)) premises))) in
      let c = C.mk_clause ~ord lits ~selected:[] proof (clause::premises) in
      Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a pos_simplify_reflect into %a@]"
                    !C.pp_clause#pp clause !C.pp_clause#pp c));
      c
    
let negative_simplify_reflect ~ord idx clause =
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
    try idx#retrieve ~sign:false s
      (fun l r subst hc ->
        if T.eq_term t (S.apply_subst subst r)
        then begin
          Utils.debug 3 (lazy (Utils.sprintf "neg_reflect eliminates %a=%a with %a"
                         !T.pp_term#pp s !T.pp_term#pp t !C.pp_clause#pp hc));
          raise (FoundMatch (r, hc)) (* success *)
        end else ());
      None (* no match *)
    with FoundMatch (r, hc) ->
      Some hc  (* success *)
  in
  (* fold over literals *)
  let lits, premises = iterate_lits [] clause.clits [] in
  if List.length lits = List.length clause.clits
    then clause (* no literal removed *)
    else 
      let proof = lazy (Proof ("simplify_reflect-",
        (clause, [], S.id_subst)::(List.map (fun c -> (c, [], S.id_subst)) premises))) in
      let c = C.mk_clause ~ord lits ~selected:[] proof (clause::premises) in
      Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a neg_simplify_reflect into %a@]"
                    !C.pp_clause#pp clause !C.pp_clause#pp c));
      c

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
        then (v, t) :: subst  (* not bound in t2 *)
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
let matched_lits ~locked lit clause =
  let ans = ref [] 
  and count = ref 0 in
  Utils.list_iteri clause.clits
    (fun idx lit' -> (* match lit with lit', add substitutions to ans *)
      List.iter
        (fun subst -> ans := (idx, subst) :: !ans; incr count)
        (match_lits ~locked S.id_subst lit lit'));
  !count, !ans

(** Check whether a subsumes b, and if it does, return the
    corresponding substitution *)
let subsumes_with a b =
  incr_stat stat_subsumption_call;
  (* a must not have more literals *)
  if List.length a.clits > List.length b.clits then None else
  let locked = T.THashSet.from_list b.cvars in
  (* list of matched literals in b, for each literal of a *)
  let ans_list = List.map
    (fun lit -> matched_lits ~locked lit b)
    a.clits in
  (* find a compatible superset of all those substitutions *)
  let rec find_compatible subst indexes l =
    match l with
    | [] -> raise (SubsumptionFound subst)
    | substs::l' ->
      List.iter
        (fun (idx, s) ->
          match () with
          | _ when Ptset.mem idx indexes -> ()
          | _ when compatible_substs s subst ->
            (* merge subst with lits, continue *)
            let indexes' = Ptset.add idx indexes
            and subst' = merge_substs s subst in
            find_compatible subst' indexes' l'
          | _ -> ()
        ) substs;
  in
  (* sort by increasing number of solutions *)
  let ans_list = List.sort (fun (count1, _) (count2, _) -> count1 - count2) ans_list in
  let ans_list = List.map snd ans_list in
  if List.exists (fun substs -> substs = []) ans_list
    then None  (* some literal matches no literal in b *)
    else try
      find_compatible S.id_subst Ptset.empty ans_list;
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
  match a.clits with
  | [Equation (s, t, true, _)] ->
    let res = List.exists (equate_lit_with s t) b.clits in
    (if res then Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a eq-subsumes@ %a@]"
                                !C.pp_clause#pp a !C.pp_clause#pp b)));
    res
  | _ -> false  (* only a positive unit clause unit-subsumes a clause *)

let eq_subsumes a b = prof_eq_subsumption.HExtlib.profile (eq_subsumes_ a) b

let subsumed_by_set_ set c =
  incr_stat stat_subsumed_by_set_call;
  (* if there is an equation in c, try equality subsumption *)
  let try_eq_subsumption = List.exists C.equational_lit c.clits in
  (* use feature vector indexing *)
  try
    FV.retrieve_subsuming set.PS.fv_idx c
      (fun c' -> if (try_eq_subsumption && eq_subsumes c' c)
                  || subsumes c' c then raise Exit);
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
  let try_eq_subsumption = match c.clits with
    | [Equation (_, _, true, _)] -> true
    | _ -> false in
  (* use feature vector indexing *)
  let l = ref [] in
  FV.retrieve_subsumed
    set.PS.fv_idx c
    (fun c' -> if (try_eq_subsumption && eq_subsumes c c')
                || subsumes c c' then l := c' :: !l);
  !l

let subsumed_in_set set clause =
  prof_subsumption_in_set.HExtlib.profile (subsumed_in_set_ set) clause

(* ----------------------------------------------------------------------
 * reduction to CNF
 * ---------------------------------------------------------------------- *)

(** Transform the clause into proper CNF; returns a list of clauses *)
let cnf_of ~ord clause =
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
  and is_cnf c =
    List.for_all
      (fun (Equation (l, r, sign, _)) -> T.atomic_rec l && T.atomic_rec r)
      c.clits
  in
  Utils.debug 3 (lazy (Utils.sprintf "input clause %a@." !C.pp_clause#pp clause));
  if is_cnf clause
    then begin
      Utils.debug 3 (lazy (Utils.sprintf "clause @[<h>%a@] is cnf" !C.pp_clause#pp clause));
      [clause] (* already cnf, perfect *)
    end else
      let nnf_lits = List.map (fun lit -> nnf (lit_to_term lit)) clause.clits in
      let skolem_lits = List.map (fun t -> skolemize t) nnf_lits in
      let clauses_of_lits = List.map to_cnf skolem_lits in
      (* list of list of literals, by or-product *)
      let lit_list_list = match clauses_of_lits with
        | [] -> assert false  (* is in cnf ;) *)
        | hd::tl -> List.fold_left product hd tl in
      (* build clauses from lits *)
      let proof = lazy (Proof ("to_cnf", [clause, [], S.id_subst])) in
      let clauses = List.map
        (fun lits ->
          let clause = C.mk_clause ~ord
              (List.map (fun (t, sign) -> C.mk_lit ~ord t T.true_term sign) lits)
              ~selected:[] proof [clause] in
          Utils.debug 4 (lazy (Utils.sprintf "mk_clause %a@." !C.pp_clause#pp clause));
          C.clause_of_fof ~ord clause)
        lit_list_list
      in
      Utils.debug 3 (lazy (Utils.sprintf "%% clause @[<h>%a@] to_cnf -> @[<h>%a@]"
                    !C.pp_clause#pp clause (Utils.pp_list !C.pp_clause#pp) clauses));
      List.iter (fun c -> assert (is_cnf c)) clauses;
      clauses

(* ----------------------------------------------------------------------
 * the Calculus object
 * ---------------------------------------------------------------------- *)

let superposition : calculus =
  object
    method binary_rules = ["superposition_active", infer_active;
                           "superposition_passive", infer_passive]

    method unary_rules = ["equality_resolution", infer_equality_resolution;
                          "equality_factoring", infer_equality_factoring]

    method basic_simplify ~ord c = basic_simplify ~ord c

    method simplify actives idx c =
      let ord = actives.PS.a_ord in
      let c = basic_simplify ~ord c in
      let c = basic_simplify ~ord (demodulate ~ord idx c) in
      let c = positive_simplify_reflect ~ord idx c in
      let c = negative_simplify_reflect ~ord idx c in
      c

    method redundant actives c = subsumed_by_set actives c

    method redundant_set actives c = subsumed_in_set actives c

    method list_simplify ~ord ~select c =
      if is_tautology c then Some [] else None  (* no other list simplification *)

    method axioms = []

    method constr _ = Precedence.min_constraint [false_symbol; true_symbol]

    method preprocess ~ord ~select l =
      List.fold_left
        (fun acc c ->
          (* reduction to CNF *)
          let clauses = cnf_of ~ord c in
          List.fold_left
            (fun acc c ->
              let c = C.reord_clause ~ord (C.clause_of_fof ~ord c) in
              let c = basic_simplify ~ord c in
              if is_tautology c
                then acc
                else c :: acc)
            acc clauses)
        [] l
  end
