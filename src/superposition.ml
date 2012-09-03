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

    ctx is to be applied to the result, to build a bigger term
    pos is the position of the term

    position -> (foterm -> 'a) -> foterm
      -> (foterm -> (foterm * 'b) option)
      -> ('a * 'b * foterm * position) option
    *)
let first_position pos ctx t f =
  (* re-build context from the result *)
  let rec inject_pos pos ctx = function
    | None -> None
    | Some (t,b) -> Some (ctx t,b,t, List.rev pos)
  and aux pos ctx t = match t.node.term with
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
  aux pos ctx t

(** apply f to all non-variable positions in t, accumulating the
    results along. f is given the subterm, the position and the context
    at each such position, and returns a list of objects; all lists
    returned by f are concatenated.

    position -> (foterm -> 'a) -> foterm
    -> (foterm -> position -> (foterm -> 'a) -> 'b list)
    -> 'b list
    *)
let all_positions pos ctx t f =
  let rec aux pos ctx t = match t.node.term with
  | Leaf _ -> f t pos ctx
  | Var _ -> []
  | Node l ->
      let acc, _, _, _ =
        List.fold_left
        (fun (acc,pre,idx,post) t -> (* Invariant: pre @ [t] @ post = l *)
            let newctx = fun x -> ctx (T.mk_node (pre@[x]@post)) in
            let acc = (aux (pos @ [idx]) newctx t) @ acc in (* recurse in subterm *)
            if post = [] then acc, l, idx, []
            else acc, pre @ [t], idx+1, List.tl post)
        (f t pos ctx (* apply f to t *), [], 0, List.tl l) l
      in
      acc
  in
  aux pos ctx t


(* ----------------------------------------------------------------------
 * inferences
 * ---------------------------------------------------------------------- *)

(* helper that does one or zero superposition inference, with all
   the given parameters *)
let do_superposition ~ord active_clause active_pos passive_clause passive_pos subst acc =
  assert (List.length active_pos = 2);
  match passive_pos with
  | [] | _::[] -> assert false
  | passive_idx::passive_side::subterm_pos ->
  let active_idx = List.hd active_pos
  and u, v, sign_uv = get_equations_sides passive_clause [passive_idx; passive_side]
  and s, t, sign_st = get_equations_sides active_clause active_pos in
  Utils.debug 3 (lazy (Utils.sprintf ("@[<h>sup @[<h>%a@] s=%a t=%a @[<h>%a@] " ^^
                                      "u=%a v=%a p=%a subst=%a@]")
                       (C.pp_clause ~sort:false) active_clause T.pp_foterm s T.pp_foterm t
                       (C.pp_clause ~sort:false) passive_clause T.pp_foterm u T.pp_foterm v
                       C.pp_pos passive_pos (S.pp_substitution ~sort:false) subst));
  assert ((Utils.list_inter T.eq_foterm active_clause.cvars passive_clause.cvars) = []);
  assert (T.db_closed s);
  if not sign_st 
  then (Utils.debug 3 (lazy "active literal is negative"); acc)
  else if not (T.db_closed (T.at_pos u subterm_pos))
  then (Utils.debug 3 (lazy "passive subterm is not DB closed"); acc)
  else begin
    assert (T.eq_foterm (S.apply_subst subst (T.at_pos u subterm_pos))
                        (S.apply_subst subst s));
    if (ord#compare (S.apply_subst subst s) (S.apply_subst subst t) = Lt ||
        ord#compare (S.apply_subst subst u) (S.apply_subst subst v) = Lt ||
        not (C.check_maximal_lit ~ord active_clause active_idx subst) ||
        not (C.check_maximal_lit ~ord passive_clause passive_idx subst))
      then begin
        Utils.debug 3 (lazy (Utils.sprintf "ordering constraint failed %s %s %B %B"
          (C.string_of_comparison (ord#compare
            (S.apply_subst subst s) (S.apply_subst subst t)))
          (C.string_of_comparison (ord#compare
            (S.apply_subst subst u) (S.apply_subst subst v)))
          (C.check_maximal_lit ~ord active_clause active_idx subst)
          (C.check_maximal_lit ~ord passive_clause passive_idx subst)));
        acc
      end else begin (* ordering constraints are ok *)
        let new_lits = Utils.list_remove active_clause.clits active_idx in
        let new_lits = (Utils.list_remove passive_clause.clits passive_idx) @ new_lits in
        let new_u = T.replace_pos u subterm_pos t in (* replace s by t in u|_p *)
        let new_lits = (C.mk_lit ~ord new_u v sign_uv) :: new_lits in
        (* apply substitution *)
        let new_lits = List.map (C.apply_subst_lit ~ord subst) new_lits in
        let rule = if sign_uv then "superposition_right" else "superposition_left" in
        let proof = lazy (Proof (rule, [(active_clause, active_pos, subst);
                                        (passive_clause, passive_pos, subst)])) in
        let new_clause = C.mk_clause ~ord new_lits proof in
        Utils.debug 3 (lazy (Utils.sprintf "ok, conclusion @[<h>%a@]"
                            (C.pp_clause ~sort:false) new_clause));
        new_clause :: acc
      end
  end

let infer_active_ actives clause =
  let ord = actives.PS.a_ord in
  (* do the inferences where clause is active; for this,
     we try to rewrite conditionally other clauses using
     non-minimal sides of every positive literal *)
  fold_positive ~both:true
    (fun acc s t _ s_pos ->
      (* rewrite clauses using s *)
      let subterm_idx = actives.PS.idx.I.subterm_index in
      let unifiables = I.DT.retrieve_unifiables subterm_idx s in
      I.ClauseSet.fold
        (fun (hc, u_pos, u_p) acc ->
          try (* rewrite u_p with s, if they are unifiable *)
            let subst = Unif.unification s u_p in
            do_superposition ~ord clause s_pos hc.node u_pos subst acc
          with
            UnificationFailure _ -> acc)
        unifiables acc
    )
    [] (C.maxlits clause)

let infer_active actives clause =
  prof_infer_active.HExtlib.profile (infer_active_ actives) clause

let infer_passive_ actives clause =
  let ord = actives.PS.a_ord in
  (* do the inferences in which clause is passive (rewritten),
     so we consider both negative and positive literals *)
  fold_lits ~both:true ~pos:true ~neg:true
    (fun acc u v _ u_pos ->
      (* rewrite subterms of u *)
      let ctx x = x in
      let new_clauses = all_positions u_pos ctx u
        (fun u_p p ctx ->
          (* u at position p is u_p *)
          let root_idx = actives.PS.idx.I.root_index in
          (* all terms that occur in an equation in the active_set
             and that are potentially unifiable with u_p *)
          let unifiables = I.DT.retrieve_unifiables root_idx u_p in
          I.ClauseSet.fold
            (fun (hc, s_pos, s) acc ->
              try
                let subst = Unif.unification s u_p in
                do_superposition ~ord hc.node s_pos clause p subst acc
              with
                UnificationFailure _ -> acc)
            unifiables [])
      in List.rev_append new_clauses acc
    )
    [] (C.maxlits clause)

let infer_passive actives clause =
  prof_infer_passive.HExtlib.profile (infer_passive_ actives) clause

let infer_equality_resolution_ ~ord clause =
  fold_negative ~both:false
    (fun acc l r sign l_pos ->
      assert (not sign);
      match l_pos with
      | [] -> assert false
      | pos::_ ->
      try
        let subst = Unif.unification l r in
        if C.check_maximal_lit ~ord clause pos subst
          (* subst(lit) is maximal, we can do the inference *)
          then
            let proof = lazy (Proof ("equality_resolution", [clause, [pos], subst]))
            and new_lits = Utils.list_remove clause.clits pos in
            let new_lits = List.map (C.apply_subst_lit ~ord subst) new_lits in
            let new_clause = C.mk_clause ~ord new_lits proof in
            Utils.debug 3 (lazy (Utils.sprintf
                          "equality resolution on @[<h>%a@] yields @[<h>%a@]"
                          (C.pp_clause ~sort:false) clause (C.pp_clause ~sort:false) new_clause));
            new_clause::acc
          else
            acc
      with UnificationFailure _ -> acc (* l and r not unifiable, try next *)
    )
    [] (C.maxlits clause)

let infer_equality_resolution ~ord clause =
  prof_infer_equality_resolution.HExtlib.profile (infer_equality_resolution_ ~ord) clause

let infer_equality_factoring_ ~ord clause =
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
              let subst = Unif.unification s u in [[idx; C.left_pos], subst]
            with UnificationFailure _ -> []
          and try_v =  (* try inference between s and v *)
            try
              let subst = Unif.unification s v in [[idx; C.right_pos], subst]
            with UnificationFailure _ -> []
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
    if C.check_maximal_lit ~ord clause active_idx subst &&
       ord#compare (S.apply_subst subst s) (S.apply_subst subst t) <> Lt
      then
        let proof = lazy (Proof ("equality_factoring",
          [(clause, active_pos, subst); (clause, passive_pos, subst)]))
        (* new_lits: literals of the new clause. remove active literal
           and replace it by a t!=v one, and apply subst *)
        and new_lits = Utils.list_remove clause.clits active_idx in
        let new_lits = (C.mk_neq ~ord t v) :: new_lits in
        let new_lits = List.map (C.apply_subst_lit ~ord subst) new_lits in
        let new_clause = C.mk_clause ~ord new_lits proof in
        Utils.debug 3 (lazy (Utils.sprintf
                      "equality factoring on @[<h>%a@] yields @[<h>%a@]"
                      (C.pp_clause ~sort:false) clause (C.pp_clause ~sort:false) new_clause));
        [new_clause]
      else
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

exception FoundMatch of (foterm * substitution * clause * position)

(** Do one step of demodulation on subterm. *)
let demod_subterm ~ord blocked_ids active_set subterm =
  (* do not rewrite non closed subterms *)
  if not (T.db_closed subterm) then None else
  (* no rewriting on non-atomic formulae *)
  if subterm.node.sort = bool_sort && not (T.atomic subterm) then None else 
  (* unit clause+pos that potentially match subterm *)
  let matches =
    I.DT.retrieve_generalizations active_set.PS.idx.I.unit_root_index subterm in
  try
    I.ClauseSet.iter
      (fun (unit_hclause, pos, l) ->
        (* do we have to ignore the clause? *)
        if List.mem unit_hclause.tag blocked_ids then () else
        assert (T.db_closed l);
        try
          let subst = Unif.matching l subterm in
          match pos with
          | [0; side] ->
              (* r is the term subterm is going to be rewritten into *)
              let r = C.get_pos unit_hclause.node [0; C.opposite_pos side] in
              let new_l = subterm
              and new_r = S.apply_subst subst r in
              if ord#compare new_l new_r = Gt
                (* subst(l) > subst(r), we can rewrite *)
                then raise (FoundMatch (new_r, subst, unit_hclause.node, pos))
                else ()
          | _ -> assert false
        with
          UnificationFailure _ -> ()
      )
      matches;
    None  (* not found any match *)
  with
    FoundMatch (new_t, subst, unit_hclause, pos) ->
      Some (new_t, (unit_hclause, pos, subst))  (* return the new term, and proof *)

(** Normalize term (which is at pos pos in the clause) w.r.t. active set.
    This returns a list of clauses and positions in clauses that have
    been used for rewriting. *)
let demod_term ~ord blocked_ids active_set term =
  let rec one_step term clauses =
    let ctx = fun t -> t
    and pos = [] in
    match first_position pos ctx term (demod_subterm ~ord blocked_ids active_set) with
    | None -> term, clauses
    | Some (new_term, (unit_hc, active_pos, subst), _, _) ->
      let new_clauses =  (unit_hc, active_pos, subst) :: clauses in
      one_step new_term new_clauses
  in one_step term []

(** demodulate a whole clause w.r.t the active_set, but ignores
    the blocked clauses (generally the clause itself, if it
    is already in the active_set)
    TODO ensure the conditions for rewrite of positive literals are ok (cf paper) *)
let demodulate_ active_set blocked_ids clause =
  let ord = active_set.PS.a_ord in
  (* rewrite the literal lit (at pos), returning a new lit
     and clauses used to rewrite it *)
  let rec demodulate_literal pos lit = match lit with
  | Equation (l, r, sign, _) ->
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
      C.mk_clause ~ord new_lits proof

let demodulate active_set clause =
  prof_demodulate.HExtlib.profile (demodulate_ active_set) clause

let is_tautology c =
  let is_tauto =
    (* s=s literal *)
    (List.exists
      (fun (Equation (l, r, sign, _)) ->
          (sign && T.eq_foterm l r))
      c.clits) ||
    (* both l=r and l!=r are literals *)
    (List.exists
      (fun (Equation (l, r, sign, _)) ->
        List.exists
          (fun (Equation (l', r', sign', _)) ->
              (sign = not sign') &&
              (((T.eq_foterm l l') && (T.eq_foterm r r')) ||
              ((T.eq_foterm l r') && (T.eq_foterm l' r)))
          )
        c.clits
      )
      c.clits)
  in
  (if is_tauto then
    Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a@] is a tautology"
                  (C.pp_clause ~sort:false) c)));
  is_tauto

let basic_simplify ~ord clause =
  (* convert some fof to literals *)
  let clause = C.clause_of_fof ~ord clause in
  let absurd_lit lit = match lit with
  | Equation (l, r, false, _) when T.eq_foterm l r -> true
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
          let subst = Unif.unification l r in
          (* remove the literal, and apply the substitution to the remaining literals
             before trying to find another x!=t *)
          er (List.map (C.apply_subst_lit ~ord subst) (Utils.list_remove lits i))
        with UnificationFailure _ -> lits
  (* finds candidate literals for destructive ER (lits with >= 1 variable) *)
  and er_check (Equation (l, r, sign, _)) = (not sign) && (T.is_var l || T.is_var r) in
  let new_lits = er new_lits in
  let new_clause = C.mk_clause ~ord new_lits clause.cproof in
  (if not (C.eq_clause new_clause clause) then
      (Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a@] basic_simplifies into @[<h>%a@]"
      (C.pp_clause ~sort:false) clause (C.pp_clause ~sort:false) new_clause))));
  new_clause

(** checks whether subst(lit_a) subsumes subst(lit_b). Returns a list of
    substitutions s such that s(lit_a) = lit_b and s contains subst. The list
    is empty if lit_a does not subsume lit_b. *)
let match_lits ~locked lit_a lit_b subst =
  match lit_a, lit_b with
  | Equation (la, ra, signa, _), Equation (lb, rb, signb, _) ->
    if signa <> signb then [] else
    (try
      let s = Unif.matching_locked ~locked
        (S.apply_subst subst la) (S.apply_subst subst lb) in
      let s = S.concat s subst in
      let s' = Unif.matching_locked ~locked
        (S.apply_subst s ra) (S.apply_subst s rb) in
      [S.concat s' s]
    with UnificationFailure _ -> []) @
    (try
      let s = Unif.matching_locked ~locked
        (S.apply_subst subst la) (S.apply_subst subst rb) in
      let s = S.concat s subst in
      let s' = Unif.matching_locked ~locked
        (S.apply_subst s ra) (S.apply_subst s lb) in
      [S.concat s' s]
    with UnificationFailure _ -> [])

(** raised when a subsuming substitution is found *)
exception SubsumptionFound of substitution

let subsumes_with a b =
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
    let possible_matches = match_lits ~locked x y subst in
    if possible_matches = []  (* x and y do not match *)
      then attempt_with x l1 (y::l2_pre) l2_tail subst
      else
        let l2' = List.rev_append l2_pre l2_tail in  (* l2 without y *)
        (* try to recurse with each possible match of x,y *)
        List.iter (fun subst' -> ignore (aux l1 l2' subst')) possible_matches;
        attempt_with x l1 (y::l2_pre) l2_tail subst
  (* used to sort literals by decreasing number of variables *)
  and compare_lit_vars (Equation (l1,l2,_,_)) (Equation (r1,r2,_,_)) =
    (List.length (T.vars_of_term r1)) + (List.length (T.vars_of_term r2)) 
    - (List.length (T.vars_of_term l1)) - (List.length (T.vars_of_term l2))
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
                  (C.pp_clause ~sort:false) a (C.pp_clause ~sort:false) b)));
  res

let subsumes a b =
  let check a b = match subsumes_with a b with
  | None -> false
  | Some _ -> true
  in
  prof_subsumption.HExtlib.profile (check a) b

let subsumed_by_set_ set clause =
  (* use feature vector indexing *)
  let candidates = FV.retrieve_subsuming set.PS.fv_idx clause in
  try
    List.iter
      (fun hc ->
        if subsumes hc.node clause then raise Exit else ())
      candidates;
    false
  with Exit ->
    Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a@] subsumed by active set"
                         (C.pp_clause ~sort:false) clause));
    true

let subsumed_by_set set clause =
  prof_subsumption_set.HExtlib.profile (subsumed_by_set_ set) clause

let subsumed_in_set_ set clause =
  (* use feature vector indexing *)
  let candidates = FV.retrieve_subsumed set.PS.fv_idx clause in
  List.filter
    (fun hc -> subsumes clause hc.node)
    candidates

let subsumed_in_set set clause =
  prof_subsumption_in_set.HExtlib.profile (subsumed_in_set_ set) clause

let orphan_murder set clause = set (* TODO *)


(* ----------------------------------------------------------------------
 * reduction to CNF
 * ---------------------------------------------------------------------- *)

(** Transform the clause into proper CNF; returns a list of clauses *)
let cnf_of ~ord clause =
  (* convert literal to term (reify equality) *)
  let rec lit_to_term (Equation (l,r,sign,_)) =
    if T.eq_foterm l T.true_term then (if sign then r else T.mk_not r)
    else if T.eq_foterm r T.true_term then (if sign then l else T.mk_not l)
    else (if sign then T.mk_eq l r else T.mk_not (T.mk_eq l r))
  (* negation normal form (also remove equivalence and implications) *) 
  and nnf t =
    if t.node.sort <> bool_sort then t else
    match t.node.term with
    | Var _ | Leaf _ -> t
    | Node [{node={term=Leaf s}}; {node={term=Node [{node={term=Leaf s'}}; a; b]}}]
      when s = not_symbol && s' = and_symbol ->
      nnf (T.mk_or (T.mk_not a) (T.mk_not b))  (* de morgan *)
    | Node [{node={term=Leaf s}}; {node={term=Node [{node={term=Leaf s'}}; a; b]}}]
      when s = not_symbol && s' = or_symbol ->
      nnf (T.mk_and (T.mk_not a) (T.mk_not b)) (* de morgan *)
    | Node [{node={term=Leaf s}}; a; b] when s = imply_symbol ->
      nnf (T.mk_or (T.mk_not a) b) (* (a => b) -> (not a or b) *)
    | Node [{node={term=Leaf s}}; a; b] when s = eq_symbol && a.node.sort = bool_sort ->
      (* (a <=> b) -> (not a or b) and (not b or a) *)
      nnf (T.mk_and
        (T.mk_or (T.mk_not a) b)
        (T.mk_or (T.mk_not b) a))
    | Node [{node={term=Leaf s}}; {node={term=Node [{node={term=Leaf s'}}; a; b]}}]
      when s = not_symbol && s' = imply_symbol ->
      nnf (T.mk_and a (T.mk_not b)) (* not (a => b) -> (a and not b) *)
    | Node [{node={term=Leaf s}}; {node={term=Node [{node={term=Leaf s'}}; a; b]}}]
      when s = not_symbol && s' = eq_symbol && a.node.sort = bool_sort ->
      (* not (a <=> b) -> (a <=> (not b)) *)
      nnf (T.mk_or
        (T.mk_and a (T.mk_not b))
        (T.mk_and b (T.mk_not a)))
    | Node [{node={term=Leaf s}}; {node={term=Node [{node={term=Leaf s'}};
        {node={term=Node [{node={term=Leaf s''}}; t']}}]}}]
      when s = not_symbol && s' = forall_symbol ->
      assert (s'' = lambda_symbol);
      nnf (T.mk_exists (T.mk_not t')) (* not forall -> exists not *)
    | Node [{node={term=Leaf s}}; {node={term=Node [{node={term=Leaf s'}};
        {node={term=Node [{node={term=Leaf s''}}; t']}}]}}]
      when s = not_symbol && s' = exists_symbol ->
      assert (s'' = lambda_symbol);
      nnf (T.mk_forall (T.mk_not t')) (* not exists -> forall not *)
    | Node [{node={term=Leaf s}}; {node={term=Node [{node={term=Leaf s'}}; t]}}]
      when s = not_symbol && s' = not_symbol -> nnf t (* double negation *)
    | Node l ->
      let t' = T.mk_node (List.map nnf l) in
      if T.eq_foterm t t' then t' else nnf t'
  (* skolemization of existentials, removal of forall *)
  and skolemize varindex t = match t.node.term with
    | Var _ | Leaf _ -> t
    | Node [{node={term=Leaf s}}; {node={term=Node [{node={term=Leaf s'}}; t]}}]
      when s = not_symbol && s' = not_symbol -> skolemize varindex t (* double negation *)
    | Node [{node={term=Leaf s}}; {node={term=Node [{node={term=Leaf s'}}; t']}}]
      when s = forall_symbol ->
      assert (s' = lambda_symbol);
      (* a fresh variable *)
      let v = T.mk_var (!varindex) t.node.sort in
      incr varindex;
      let new_t' = T.db_unlift (T.db_replace t' v) in
      skolemize varindex new_t' (* remove forall *)
    | Node [{node={term=Leaf s}}; {node={term=Node [{node={term=Leaf s'}}; t']}}]
      when s = exists_symbol ->
      assert (s' = lambda_symbol);
      (* make a skolem symbol *)
      let sk = Calculus.skolem ord (T.vars_of_term t') t.node.sort in
      let new_t' = T.db_unlift (T.db_replace t' sk) in
      skolemize varindex new_t' (* remove forall *)
    | Node l -> T.mk_node (List.map (skolemize varindex) l)
  (* reduction to cnf using De Morgan. Returns a list of list of terms *)
  and to_cnf t =
    if t.node.sort <> bool_sort then [[t, true]]
    else match t.node.term with
    | Var _ | Leaf _ -> [[t, true]]
    | Node [{node={term=Leaf s}}; t'] when s = not_symbol ->
      assert (T.hd_symbol t' = Some eq_symbol || T.atomic_rec t');
      [[t', false]]
    | Node [{node={term=Leaf s}}; a; b] when s = and_symbol ->
      let ca = to_cnf a
      and cb = to_cnf b in
      List.rev_append ca cb
    | Node [{node={term=Leaf s}}; a; b] when s = or_symbol ->
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
  if is_cnf clause
    then begin
      Utils.debug 3 (lazy (Utils.sprintf "clause @[<h>%a@] is cnf"
                    (C.pp_clause ~sort:false) clause));
      [clause] (* already cnf, perfect *)
    end else
      let nnf_lits = List.map (fun lit -> nnf (lit_to_term lit)) clause.clits in
      let skolem_lits = List.map (fun t -> skolemize (ref 0) t) nnf_lits in
      let clauses_of_lits = List.map to_cnf skolem_lits in
      (* list of list of literals, by or-product *)
      let lit_list_list = match clauses_of_lits with
        | [] -> assert false  (* is in cnf ;) *)
        | hd::tl -> List.fold_left product hd tl in
      (* build clauses from lits *)
      let clauses = List.map
        (fun lits ->
          C.clause_of_fof ~ord
            (C.mk_clause ~ord
              (List.map (fun (t, sign) -> C.mk_lit ~ord t T.true_term sign) lits)
              clause.cproof))
        lit_list_list
      in
      Utils.debug 3 (lazy (Utils.sprintf "%% clause @[<h>%a@] to_cnf -> @[<h>%a@]"
                    (C.pp_clause ~sort:false) clause
                    (Utils.pp_list (C.pp_clause ~sort:false)) clauses));
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

    method simplify actives c = demodulate actives [] c

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
          let clauses = List.map (C.clause_of_fof ~ord) clauses in
          List.rev_append clauses acc)
        [] l
  end
