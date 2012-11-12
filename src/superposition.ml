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
open Hashcons
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
let stat_superposition_call = mk_stat "superposition calls"
let stat_equality_resolution_call = mk_stat "equality_resolution calls"
let stat_equality_factoring_call = mk_stat "equality_factoring calls"
let stat_subsumption_call = mk_stat "subsumption calls"
let stat_subsumed_in_set_call = mk_stat "subsumed_in_set calls"
let stat_subsumed_by_set_call = mk_stat "subsumed_by_set calls"
let stat_demodulate_call = mk_stat "demodulate calls"
let stat_demodulate_step = mk_stat "demodulate steps"

let print_stats () =
  List.iter
    (fun (name, cnt) -> Format.printf "%% %-30s ... %s@." name (Int64.to_string !cnt))
    [stat_superposition_call; stat_equality_resolution_call; stat_equality_factoring_call;
     stat_subsumption_call; stat_subsumed_in_set_call; stat_subsumed_by_set_call;
     stat_demodulate_call; stat_demodulate_step]

(* for profiling *)
let enable = true

let prof_demodulate = HExtlib.profile ~enable "demodulate"
let prof_basic_simplify = HExtlib.profile ~enable "basic_simplify"
let prof_subsumption = HExtlib.profile ~enable "subsumption"
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

(** returns the term t where the first sutbterm t'
    such that f t' is not None, is replaced by f t'.
    Does not visit variables.

    position -> term
      -> (term -> (term * 'b) option)
      -> (term * 'b * term * position) option
    *)
let first_position pos t f =
  (* re-build context from the result *)
  let rec inject_pos pos ctx = function
    | None -> None
    | Some (t,b) -> Some (ctx t,b,t, List.rev pos)
  and aux pos ctx t = match t.term with
  | Leaf _ -> inject_pos pos ctx (f t)
  | Var _ -> None
  | Node l ->
      match f t with  (* try f at the current composite term *)
      | Some _ as x -> inject_pos pos ctx x
      | None ->
          (* pre is the list of subterm before, post the list of subterms
             after the current subterm *)
          let rec first pre idx post l = match l with
          | [] -> None
          | t :: tl ->
             (* newctx will re-build the current term around the result on a subterm *)
             let newctx = fun x -> ctx (T.mk_node (pre@[x]@post)) in
             match aux (idx :: pos (* extend position *)) newctx t with
             | Some _ as x -> x
             | None ->
                 if post = [] then None (* tl is also empty *)
                 else first (pre @ [t]) (idx+1) (List.tl post) tl
          in
          first [] 1 (List.tl l) l
  in
  aux pos (fun x -> x) t

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
  | Leaf _ -> f t pos
  | Var _ -> []
  | Node [] -> assert false
  | Node (hd::tl) ->
      let acc, _, _, _ =
        List.fold_left
        (fun (acc,pre,idx,post) t -> (* Invariant: pre @ [t] @ post = hd::tl *)
            let acc = List.rev_append (aux (pos @ [idx]) t) acc in (* recurse in subterm *)
            if post = [] then acc, pre, idx, []
            else acc, pre @ [t], idx+1, List.tl post)
        (f t pos (* apply f to t *), [hd], 1, tl) tl
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
    | Var _, _ | _, Var _ | Leaf _, _ | _, Leaf _ ->
      f acc (List.rev pos) t1 t2
    | Node (hd1::tl1), Node (hd2::tl2) ->
      begin match f acc (List.rev pos) t1 t2 with
      | None when T.eq_term hd1 hd2 -> fold acc pos 1 tl1 tl2  (* recurse in subterms *)
      | None -> None (* not the same, and not accepted by f *)
      | Some acc -> Some acc (* f is ok on this pair of terms *)
      end
    | _ -> assert false
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
  Utils.debug 3 (lazy (Utils.sprintf ("@[<h>sup @[<h>%a@] s=%a t=%a @[<h>%a@] " ^^
                                      "u=%a v=%a p=%a subst=%a@]")
                       !C.pp_clause#pp active_clause !T.pp_term#pp s !T.pp_term#pp t
                       !C.pp_clause#pp passive_clause !T.pp_term#pp u !T.pp_term#pp v
                       C.pp_pos passive_pos S.pp_substitution subst));
  assert ((Utils.list_inter T.eq_term active_clause.cvars passive_clause.cvars) = []);
  assert (T.db_closed s);
  if not sign_st 
  then (Utils.debug 3 (lazy "active literal is negative"); acc)
  else if not (T.db_closed (T.at_pos u subterm_pos))
  then (Utils.debug 3 (lazy "passive subterm is not DB closed"); acc)
  else
  let t' = S.apply_subst subst t
  and v' = S.apply_subst subst v in
  if sign_uv && T.eq_term t' v' && subterm_pos = []
  then (Utils.debug 3 (lazy "will yield a tautology"); acc)
  else begin
    assert (T.eq_term (S.apply_subst subst (T.at_pos u subterm_pos))
                        (S.apply_subst subst s));
    if (ord#compare (S.apply_subst subst s) t' = Lt ||
        ord#compare (S.apply_subst subst u) v' = Lt ||
        not (C.eligible_res ~ord passive_clause passive_idx subst) ||
        not (C.eligible_param ~ord active_clause active_idx subst))
      then acc
      else begin (* ordering constraints are ok *)
        let new_lits = Utils.list_remove active_clause.clits active_idx in
        let new_lits = (Utils.list_remove passive_clause.clits passive_idx) @ new_lits in
        let new_u = T.replace_pos u subterm_pos t in (* replace s by t in u|_p *)
        let new_lits = (C.mk_lit ~ord new_u v sign_uv) :: new_lits in
        (* apply substitution *)
        let new_lits = List.map (C.apply_subst_lit ~ord subst) new_lits in
        let rule = if sign_uv then "superposition_right" else "superposition_left" in
        let proof = lazy (Proof (rule, [(active_clause, active_pos, subst);
                                        (passive_clause, passive_pos, subst)])) in
        let new_clause = C.mk_clause ~ord new_lits ~selected:(lazy [])
          proof (lazy [active_clause; passive_clause]) in
        Utils.debug 3 (lazy (Utils.sprintf "ok, conclusion @[<h>%a@]"
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
            let proof = lazy (Proof ("equality_resolution", [clause, [pos], subst]))
            and new_lits = Utils.list_remove clause.clits pos in
            let new_lits = List.map (C.apply_subst_lit ~ord subst) new_lits in
            let new_clause = C.mk_clause ~ord new_lits ~selected:(lazy [])
              proof (lazy [clause]) in
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
    assert (T.db_closed u);
    assert (T.db_closed s);
    (* check whether subst(lit) is maximal, and not (subst(s) < subst(t)) *)
    if ord#compare (S.apply_subst subst s) (S.apply_subst subst t) <> Lt &&
       C.eligible_param ~ord clause active_idx subst
      then begin
        incr_stat stat_equality_factoring_call;
        let proof = lazy (Proof ("equality_factoring",
          [(clause, active_pos, subst); (clause, passive_pos, subst)]))
        (* new_lits: literals of the new clause. remove active literal
           and replace it by a t!=v one, and apply subst *)
        and new_lits = Utils.list_remove clause.clits active_idx in
        let new_lits = (C.mk_neq ~ord t v) :: new_lits in
        let new_lits = List.map (C.apply_subst_lit ~ord subst) new_lits in
        let new_clause = C.mk_clause ~ord new_lits ~selected:(lazy [])
          proof (lazy [clause]) in
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

exception FoundMatch of (term * substitution * clause * position)

(** Do one step of demodulation on subterm. *)
let demod_subterm ~ord blocked_ids active_set subterm =
  Utils.debug 4 (lazy (Utils.sprintf "  demod subterm %a" !T.pp_term#pp subterm));
  (* do not rewrite non closed subterms *)
  if not (T.db_closed subterm)
    then (Utils.debug 3 (lazy (Utils.sprintf "demod: %a not closed" !T.pp_term#pp subterm));
          None)
  (* no rewriting on non-atomic formulae *)
  else if subterm.sort = bool_sort && not (T.atomic subterm)
    then (Utils.debug 3 (lazy (Utils.sprintf "demod: %a not atomic" !T.pp_term#pp subterm));
          None)
  (* try to rewrite using unit positive clauses *)
  else try
    (* if ground, try to rewrite directly *)
    (try
      if T.is_ground_term subterm
        then
          let new_t, (hc, pos, l) = Ptmap.find subterm.tag
            active_set.PS.idx#ground_rewrite_index in
          assert (T.eq_term l subterm);
          raise (FoundMatch (new_t, S.id_subst, hc, pos))
        else ()
    with Not_found -> ());
    (* unit clause+pos that potentially match subterm *)
    active_set.PS.idx#unit_root_index#retrieve_generalizations subterm ()
      (fun () l set ->
        assert (T.db_closed l);
        try
          let subst = Unif.matching S.id_subst l subterm in
          (* iterate on all clauses for this term *)
          I.ClauseSet.iter 
            (fun (unit_hclause, pos, l) ->
              (* do we have to ignore the clause? *)
              if List.mem unit_hclause.ctag blocked_ids then () else begin
                match pos with
                | [0; side] ->
                  (* r is the term subterm is going to be rewritten into *)
                  let r = C.get_pos unit_hclause [0; C.opposite_pos side] in
                  let new_l = subterm
                  and new_r = S.apply_subst subst r in
                  if ord#compare new_l new_r = Gt
                    (* subst(l) > subst(r), we can rewrite *)
                    then begin
                      Utils.debug 4 (lazy (Utils.sprintf "rewrite %a into %a using %a"
                                     !T.pp_term#pp subterm !T.pp_term#pp new_r
                                     !C.pp_clause#pp_h unit_hclause));
                      raise (FoundMatch (new_r, subst, unit_hclause, pos))
                    end else Utils.debug 4
                      (lazy (Utils.sprintf "could not rewrite %a into %a using %a, bad order"
                             !T.pp_term#pp subterm !T.pp_term#pp new_r
                             !C.pp_clause#pp_h unit_hclause));
                | _ -> assert false
              end
          ) set
        with
          UnificationFailure -> ()
      );
    None  (* not found any match *)
  with
    FoundMatch (new_t, subst, unit_hclause, pos) ->
      begin
        incr_stat stat_demodulate_step;
        Some (new_t, (unit_hclause, pos, subst))  (* return the new term, and proof *)
      end

(** Normalize term (which is at pos pos in the clause) w.r.t. active set.
    This returns a list of clauses and positions in clauses that have
    been used for rewriting. *)
let demod_term ~ord blocked_ids active_set term =
  let rec one_step term clauses =
    let pos = [] in
    match first_position pos term (demod_subterm ~ord blocked_ids active_set) with
    | None -> term, clauses
    | Some (new_term, (unit_hc, active_pos, subst), _, _) ->
      let new_clauses =  (unit_hc, active_pos, subst) :: clauses in
      one_step new_term new_clauses (* do another step *)
  in one_step term []

(** demodulate a whole clause w.r.t the active_set, but ignores
    the blocked clauses (generally the clause itself, if it
    is already in the active_set)
    TODO ensure the conditions for rewrite of positive literals are ok (cf paper) *)
let demodulate active_set blocked_ids clause =
  Utils.debug 4 (lazy (Utils.sprintf "demodulate %a..." !C.pp_clause#pp clause));
  incr_stat stat_demodulate_call;
  let ord = active_set.PS.a_ord in
  (* rewrite the literal lit (at pos), returning a new lit
     and clauses used to rewrite it *)
  let rec demodulate_literal pos lit = match lit with
  | Equation (l, r, sign, _) ->
      if sign && C.eligible_res ~ord clause pos S.id_subst
        then lit, []  (* do not rewrite in this case *)
        else
          let new_l, l_clauses = demod_term ~ord blocked_ids active_set l in
          let new_r, r_clauses = demod_term ~ord blocked_ids active_set r in
          (C.mk_lit ~ord new_l new_r sign), (l_clauses @ r_clauses)
  (* rewrite next lit, and get more clauses *)
  and iterate_lits pos lits new_lits clauses = match lits with
  | [] -> List.rev new_lits, clauses
  | lit::tail ->
    let new_lit, new_clauses = demodulate_literal pos lit in
    iterate_lits (pos+1) tail (new_lit::new_lits) (new_clauses@clauses)
  in
  let new_lits, clauses = iterate_lits 0 clause.clits [] [] in
  if try List.for_all2 C.eq_literal clause.clits new_lits with Invalid_argument _ -> false
    (* if the literals are the same, no simplification occurred *)
    then clause
    (* add the initial clause itself (without pos or subst, too complicated) to
       the proof before returning the simplified clause *)
    else
      let proof = lazy (Proof ("demodulation", (clause, [], S.id_subst)::clauses)) in
      (* parents are clauses used to simplify the clause, plus parents of the clause *)
      let parents = lazy ((List.map (fun (c,_,_) -> c) clauses) @ (C.parents clause)) in
      C.mk_clause ~ord new_lits ~selected:(lazy []) proof parents

let is_tautology c =
  let is_tauto =
    (* s=s literal *)
    (List.exists
      (fun (Equation (l, r, sign, _)) ->
          (sign && T.eq_term l r))
      c.clits) ||
    (* both l=r and l!=r are literals *)
    (List.exists
      (fun (Equation (l, r, sign, _)) ->
        List.exists
          (fun (Equation (l', r', sign', _)) ->
              (sign = not sign') &&
              (((T.eq_term l l') && (T.eq_term r r')) ||
              ((T.eq_term l r') && (T.eq_term l' r)))
          )
        c.clits
      )
      c.clits)
  in
  (if is_tauto then
    Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a@] is a tautology" !C.pp_clause#pp c)));
  is_tauto

(** semantic tautology deletion, using a congruence closure algorithm
    to see if negative literals imply some positive literal *)
let is_semantic_tautology c = false (* TODO *)

let basic_simplify ~ord clause =
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
  let parents = lazy (clause :: C.parents clause) in
  let proof =
    try if List.for_all2 C.eq_literal_com clause.clits new_lits
      then clause.cproof else lazy (Proof ("basic_simplify", [clause, [], S.id_subst]))
    with Invalid_argument _ -> lazy (Proof ("basic_simplify", [clause, [], S.id_subst])) in
  let new_clause = C.mk_clause ~ord new_lits ~selected:(lazy []) proof parents in
  (if not (C.eq_clause new_clause clause) then
      (Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a@] basic_simplifies into @[<h>%a@]"
      !C.pp_clause#pp clause !C.pp_clause#pp new_clause))));
  new_clause

let positive_simplify_reflect active_set clause =
  let ord = active_set.PS.a_ord in
  (* iterate through literals and try to resolve negative ones *)
  let rec iterate_lits acc lits clauses = match lits with
  | [] -> List.rev acc, clauses
  | (Equation (s, t, false, _) as lit, idx)::lits' ->
    begin match parallel_positions [C.left_pos; idx] s t [] equatable_lits with
    | None -> (* keep literal *)
      iterate_lits (lit::acc) lits' clauses
    | Some new_clauses -> (* drop literal, remember clauses *)
      iterate_lits acc lits' (new_clauses @ clauses)
    end
  | (lit, _)::lits' -> iterate_lits (lit::acc) lits' clauses
  (** try to remove the literal using some positive unit clauses
      from active_set *)
  and equatable_lits clauses pos t1 t2 =
    if T.eq_term t1 t2
      then Some clauses  (* trivial *)
      else  (* try to solve it with a unit equality *)
        try active_set.PS.idx#unit_root_index#retrieve_generalizations t1 ()
          (fun () l set ->
            try
              let subst = Unif.matching S.id_subst l t1 in
              (* find some r in the set, such that subst(r) = t2 *)
              I.ClauseSet.iter
                (fun (clause, pos, _) ->
                  match pos with
                  | [idx; side] ->
                    (* get the other side of the equation *)
                    let r = C.get_pos clause [idx; C.opposite_pos side] in
                    if T.eq_term t2 (S.apply_subst subst r)
                    then begin
                      Utils.debug 4 (lazy (Utils.sprintf "equate %a and %a using %a"
                                  !T.pp_term#pp t1 !T.pp_term#pp t2 !C.pp_clause#pp clause));
                      raise (FoundMatch (r, subst, clause, pos)) (* success *)
                    end else ()
                  | _ -> assert false)
                set
            with UnificationFailure -> ());
          None (* no match *)
        with FoundMatch (r, subst, clause, pos) ->
          Some ((clause, pos, subst) :: clauses)  (* success *)
  in
  (* fold over literals *)
  let lits, premises = iterate_lits [] (Utils.list_pos clause.clits) [] in
  if List.length lits = List.length clause.clits
    then clause (* no literal removed *)
    else 
      let proof = lazy (Proof ("pos_simplify_reflect", (clause, [], S.id_subst)::premises))
      and clauses = List.map (fun (c,_,_) -> c) premises in
      let c = C.mk_clause ~ord lits ~selected:(lazy []) proof (lazy (clause::clauses)) in
      Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a pos_simplify_reflect into %a@]"
                    !C.pp_clause#pp clause !C.pp_clause#pp c));
      c
    
let negative_simplify_reflect active_set clause =
  let ord = active_set.PS.a_ord in
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
    try active_set.PS.idx#root_index#retrieve_generalizations s ()
      (fun () l set ->
        try
          let subst = Unif.matching S.id_subst l s in
          (* find some r in the set, such that subst(r) = t *)
          I.ClauseSet.iter
            (fun (clause, pos, _) ->
              match pos with
              | [idx; side] ->
                (* get the other side of the equation *)
                let r = C.get_pos clause [idx; C.opposite_pos side] in
                if List.length clause.clits = 1 &&
                   C.neg_lit (List.hd clause.clits) &&
                   T.eq_term t (S.apply_subst subst r)
                then begin
                  Utils.debug 3 (lazy (Utils.sprintf "neg_reflect eliminates %a=%a with %a"
                                 !T.pp_term#pp s !T.pp_term#pp t !C.pp_clause#pp clause));
                  raise (FoundMatch (r, subst, clause, pos)) (* success *)
                end else ()
              | _ -> assert false)
            set
        with UnificationFailure -> ());
      None (* no match *)
    with FoundMatch (r, subst, clause, pos) ->
      Some (clause, pos, subst)  (* success *)
  in
  (* fold over literals *)
  let lits, premises = iterate_lits [] clause.clits [] in
  if List.length lits = List.length clause.clits
    then clause (* no literal removed *)
    else 
      let proof = lazy (Proof ("neg_simplify_reflect", (clause, [], S.id_subst)::premises))
      and clauses = List.map (fun (c,_,_) -> c) premises in
      let c = C.mk_clause ~ord lits ~selected:(lazy []) proof (lazy (clause::clauses)) in
      Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a neg_simplify_reflect into %a@]"
                    !C.pp_clause#pp clause !C.pp_clause#pp c));
      c

(* ----------------------------------------------------------------------
 * subsumption
 * ---------------------------------------------------------------------- *)

(** checks whether subst(lit_a) subsumes subst(lit_b). Returns a list of
    substitutions s such that s(lit_a) = lit_b and s contains subst. The list
    is empty if lit_a does not subsume lit_b. *)
let match_lits ~locked subst lit_a lit_b =
  match lit_a, lit_b with
  | Equation (la, ra, signa, _), Equation (lb, rb, signb, _) ->
    if signa <> signb then [] else
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

(** raised when a subsuming substitution is found *)
exception SubsumptionFound of substitution

let subsumes_with a b =
  incr_stat stat_subsumption_call;
  let locked = b.cvars in
  (* a must not have more literals *)
  if List.length a.clits > List.length b.clits then None else
  (* does the list subst(l1) subsume subst(l2)? *)
  let rec aux l1 l2 subst = match l1 with
  | [] -> (* no lits in subsuming clause *)
    raise (SubsumptionFound subst)
  | x::l1_tail ->
    (* is there a y in l2 with subst'(subst(x)) = subst(y)? if yes, remove
       x from l1 and y from l2 and continue *)
    attempt_with x l1_tail [] l2 subst
  (* try to match x against literals in l2 (l2_pre are literals
     in l2 that have already been tried), with subst *)
  and attempt_with x l1 l2_pre l2 subst = match l2 with
  | [] -> None
  | y::l2_tail ->
    let possible_matches = match_lits ~locked subst x y in
    if possible_matches = []  (* x and y do not match *)
      then attempt_with x l1 (y::l2_pre) l2_tail subst
      else
        let l2' = List.rev_append l2_pre l2_tail in  (* l2 without y *)
        (* try to recurse with each possible match of x,y *)
        List.iter (fun subst' -> ignore (aux l1 l2' subst')) possible_matches;
        attempt_with x l1 (y::l2_pre) l2_tail subst
  (* used to sort literals by decreasing number of variables *)
  and compare_lit_vars (Equation (l1,l2,_,_)) (Equation (r1,r2,_,_)) =
    (List.length r1.vars) + (List.length r2.vars) 
    - (List.length l1.vars) - (List.length l2.vars)
  in
  let res =
    (* try aux with the whole list of literals l1 *)
    let l1 = List.sort compare_lit_vars a.clits
    and l2 = List.sort compare_lit_vars b.clits in
    try aux l1 l2 S.id_subst
    with SubsumptionFound subst -> Some subst
  in
  (if res <> None then
    Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a@] subsumes @[<h>%a@]"
                  !C.pp_clause#pp a !C.pp_clause#pp b)));
  res

let subsumes a b =
  let check a b = match subsumes_with a b with
  | None -> false
  | Some _ -> true
  in
  prof_subsumption.HExtlib.profile (check a) b

let subsumed_by_set_ set c =
  incr_stat stat_subsumed_by_set_call;
  (* use feature vector indexing *)
  try
    FV.retrieve_subsuming set.PS.fv_idx c
      (fun c' -> if subsumes c' c then raise Exit);
    false
  with Exit ->
    Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a@] subsumed by active set"
                         !C.pp_clause#pp c));
    true

let subsumed_by_set set clause =
  prof_subsumption_set.HExtlib.profile (subsumed_by_set_ set) clause

let subsumed_in_set_ set c =
  incr_stat stat_subsumed_in_set_call;
  (* use feature vector indexing *)
  let l = ref [] in
    FV.retrieve_subsumed
      set.PS.fv_idx c
      (fun c' -> if subsumes c c' then l := c' :: !l);
  !l

let subsumed_in_set set clause =
  prof_subsumption_in_set.HExtlib.profile (subsumed_in_set_ set) clause

let orphan_murder set clause = set (* TODO *)


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
    | Var _ | Leaf _ -> t
    | Node [{term=Leaf s}; {term=Node [{term=Leaf s'}; a; b]}]
      when s = not_symbol && s' = and_symbol ->
      nnf (T.mk_or (T.mk_not a) (T.mk_not b))  (* de morgan *)
    | Node [{term=Leaf s}; {term=Node [{term=Leaf s'}; a; b]}]
      when s = not_symbol && s' = or_symbol ->
      nnf (T.mk_and (T.mk_not a) (T.mk_not b)) (* de morgan *)
    | Node [{term=Leaf s}; a; b] when s = imply_symbol ->
      nnf (T.mk_or (T.mk_not a) b) (* (a => b) -> (not a or b) *)
    | Node [{term=Leaf s}; a; b] when s = eq_symbol && a.sort = bool_sort ->
      (* (a <=> b) -> (not a or b) and (not b or a) *)
      nnf (T.mk_and
        (T.mk_or (T.mk_not a) b)
        (T.mk_or (T.mk_not b) a))
    | Node [{term=Leaf s}; {term=Node [{term=Leaf s'}; a; b]}]
      when s = not_symbol && s' = imply_symbol ->
      nnf (T.mk_and a (T.mk_not b)) (* not (a => b) -> (a and not b) *)
    | Node [{term=Leaf s}; {term=Node [{term=Leaf s'}; a; b]}]
      when s = not_symbol && s' = eq_symbol && a.sort = bool_sort ->
      (* not (a <=> b) -> (a <=> (not b)) *)
      nnf (T.mk_or
        (T.mk_and a (T.mk_not b))
        (T.mk_and b (T.mk_not a)))
    | Node [{term=Leaf s}; {term=Node [{term=Leaf s'};
        {term=Node [{term=Leaf s''}; t']}]}]
      when s = not_symbol && s' = forall_symbol ->
      assert (s'' = lambda_symbol);
      nnf (T.mk_exists (T.mk_not t')) (* not forall -> exists not *)
    | Node [{term=Leaf s}; {term=Node [{term=Leaf s'};
        {term=Node [{term=Leaf s''}; t']}]}]
      when s = not_symbol && s' = exists_symbol ->
      assert (s'' = lambda_symbol);
      nnf (T.mk_forall (T.mk_not t')) (* not exists -> forall not *)
    | Node [{term=Leaf s}; {term=Node [{term=Leaf s'}; t]}]
      when s = not_symbol && s' = not_symbol -> nnf t (* double negation *)
    | Node l ->
      let t' = T.mk_node (List.map nnf l) in
      if T.eq_term t t' then t' else nnf t'
  (* skolemization of existentials, removal of forall *)
  and skolemize t = match t.term with
    | Var _ | Leaf _ -> t
    | Node [{term=Leaf s}; {term=Node [{term=Leaf s'}; t]}]
      when s = not_symbol && s' = not_symbol -> skolemize t (* double negation *)
    | Node [{term=Leaf s}; {term=Node [{term=Leaf s'}; t']}]
      when s = forall_symbol ->
      assert (s' = lambda_symbol);
      (* a fresh variable *)
      let sort = match T.look_db_sort 0 t with
        | None -> univ_sort
        | Some s -> s in
      let v = T.mk_var (!varindex) sort in
      incr varindex;
      let new_t' = T.db_unlift (T.db_replace t' v) in
      skolemize new_t' (* remove forall *)
    | Node [{term=Leaf s}; {term=Node [{term=Leaf s'}; t']}]
      when s = exists_symbol ->
      assert (s' = lambda_symbol);
      (* make a skolem symbol *)
      let sort = match T.look_db_sort 0 t with
        | None -> univ_sort
        | Some s -> s in
      let new_t' = Calculus.skolem ~ord t' sort in
      skolemize new_t' (* remove forall *)
    | Node l -> T.mk_node (List.map skolemize l)
  (* reduction to cnf using De Morgan. Returns a list of list of terms *)
  and to_cnf t =
    if t.sort <> bool_sort then [[t, true]]
    else match t.term with
    | Var _ | Leaf _ -> [[t, true]]
    | Node [{term=Leaf s}; t'] when s = not_symbol ->
      assert (T.hd_symbol t' = Some eq_symbol || T.atomic_rec t');
      [[t', false]]
    | Node [{term=Leaf s}; a; b] when s = and_symbol ->
      let ca = to_cnf a
      and cb = to_cnf b in
      List.rev_append ca cb
    | Node [{term=Leaf s}; a; b] when s = or_symbol ->
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
      let proof = lazy (Proof ("cnf_reduction", [clause, [], S.id_subst])) in
      let clauses = List.map
        (fun lits ->
          let clause = C.mk_clause ~ord
              (List.map (fun (t, sign) -> C.mk_lit ~ord t T.true_term sign) lits)
              ~selected:(lazy []) proof (lazy [clause]) in
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

    method simplify actives c =
      let ord = actives.PS.a_ord in
      let c = basic_simplify ~ord c in
      let c = basic_simplify ~ord (demodulate actives [] c) in
      let c = basic_simplify ~ord (positive_simplify_reflect actives c) in
      let c = basic_simplify ~ord (negative_simplify_reflect actives c) in
      c

    method redundant actives c = subsumed_by_set actives c

    method redundant_set actives c = subsumed_in_set actives c

    method trivial c = is_tautology c

    method axioms = []

    method constr = O.consts_constraint 

    method preprocess ~ord l =
      List.fold_left
        (fun acc c ->
          (* reduction to CNF *)
          let clauses = cnf_of ~ord c in
          let clauses = List.map
            (fun c -> C.reord_clause ~ord (C.clause_of_fof ~ord c))
            clauses in
          let clauses = List.filter (fun c -> not (is_tautology c)) clauses in
          List.rev_append clauses acc)
        [] l
  end
