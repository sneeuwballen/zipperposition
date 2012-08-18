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

module T = Terms
module C = Clauses
module O = Orderings
module S = FoSubst
module I = Index
module PS = ProofState
module Unif = FoUnif
module Utils = FoUtils

(** a conclusion is a clause, plus the clauses used to infer it *)
type conclusion = clause

(** raised when the empty clause is found *)
exception Success of hclause

(** inferences *)
type inference_rule = ProofState.active_set -> clause -> conclusion list

(* for profiling *)
let enable = true

let prof_check_max_lit = HExtlib.profile ~enable "check_max_lit"
let prof_demodulate = HExtlib.profile ~enable "demodulate"
let prof_basic_simplify = HExtlib.profile ~enable "basic_simplify"
let prof_subsumption = HExtlib.profile ~enable "subsumption"
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


(** fold f over all literals sides, with their positions.
    f is given (acc, left side, right side, sign, position of left side)
    if pos, then positive literals will be visited.
    if neg, then negative literals will be visited.
    if both, then both sides of a non-oriented equation
      will be visited

    ?pos:bool -> ?neg:bool -> ?both:bool
    -> ('a -> Types.foterm -> Types.foterm -> bool -> int list -> 'a)
    -> 'a -> (Types.literal * int) list
    -> 'a *)
let rec fold_lits ?(pos=true) ?(neg=true) ?(both=true) f acc lits =
  if (not pos) && (not neg) then acc else
  (* is the sign ok, given the parameters? *)
  let sign_ok sign = if sign then pos else neg in
  List.fold_left
    (fun acc (lit, idx) ->
      match lit with
      | Equation (l,r,sign,Gt) when sign_ok sign ->
        f acc l r sign [idx; C.left_pos]
      | Equation (l,r,sign,Lt) when sign_ok sign ->
        f acc r l sign [idx; C.right_pos]
      | Equation (l,r,sign,_) when sign_ok sign ->
        if both
        then (* visit both sides of the equation *)
          let acc = f acc r l sign [idx; C.right_pos] in
          f acc l r sign [idx; C.left_pos]
        else (* only visit one side (arbitrary) *)
          f acc l r sign [idx; C.left_pos]
      | _ -> acc)
    acc lits

(** Visit all non-minimal sides of positive equations *)
let rec fold_positive ?(both=true) f acc lits =
  fold_lits ~pos:true ~neg:false ~both f acc lits

(** Visit all non-minimal sides of negative equations *)
let rec fold_negative ?(both=true) f acc lits =
  fold_lits ~pos:false ~neg:true ~both f acc lits

(** compare literals as multisets of multisets of terms *)
let compare_lits_partial ~ord l1 l2 =
  match l1, l2 with
  | Equation (s, t, sign_st, _), Equation (u, v, sign_uv, _) ->
    let s_u = ord#compare s u
    and s_v = ord#compare s v
    and t_u = ord#compare t u
    and t_v = ord#compare t v in
    match s_u, s_v, t_u, t_v, sign_uv, sign_st with
    | Eq, _, _, Eq, _, _
    | _, Eq, Eq, _, _, _ ->
      if sign_st = sign_uv then Eq
      else if sign_st then Lt
      else (assert sign_uv; Gt)
    | Gt, Gt, _, _, _, _        (* s dominates *)
    | _, _, Gt, Gt, _, _ -> Gt  (* t dominates *)
    | Gt, Eq, _, _, false, true (* s = v & s > u *)
    | Eq, Gt, _, _, false, true (* s = u & s > v *)
    | _, _, Gt, Eq, false, true (* t = v & t > u *)
    | _, _, Eq, Gt, false, true -> Gt (* t = u & t > v *)
    | Lt, _, Lt, _, _, _        (* u dominates *)
    | _, Lt, _, Lt, _, _ -> Lt  (* v dominates *)
    | Eq, _, Lt, _, true, false (* s = u, t < u *)
    | Lt, _, Eq, _, true, false (* t = u, s < u *)
    | _, Eq, _, Lt, true, false (* s = v, t < v *)
    | _, Lt, _, Eq, true, false -> Lt (* t = v, s < v *)
    | _ -> Incomparable

(** check that the literal subst(clause[i]) is maximal in subst(clause) *)
let check_maximal_lit_ ~ord clause pos subst =
  let lits_pos = Utils.list_pos clause.clits in
  let lit = Utils.list_get clause.clits pos in
  let slit = C.apply_subst_lit ~ord subst lit in
  List.for_all
    (fun (lit', idx) ->
      if idx = pos
        then (assert (C.eq_literal lit lit'); true)
        else
          let slit' = C.apply_subst_lit ~ord subst lit' in
          match compare_lits_partial ~ord slit slit' with
          | Eq | Gt | Invertible | Incomparable -> true
          | Lt -> begin
            Utils.debug 3 (lazy (Utils.sprintf "%a < %a" (C.pp_literal ~sort:false) slit
                                 (C.pp_literal ~sort:false) slit'));
            false  (* slit is not maximal *)
          end
    )
    lits_pos

let check_maximal_lit ~ord clause pos subst =
  prof_check_max_lit.HExtlib.profile (check_maximal_lit_ ~ord clause pos) subst

(** decompose the literal at given position *)
let get_equations_sides clause pos = match pos with
  | idx::eq_side::[] ->
    (match Utils.list_get clause.clits idx with
    | Equation (l,r,sign,_) when eq_side = C.left_pos -> (l, r, sign)
    | Equation (l,r,sign,_) when eq_side = C.right_pos -> (r, l, sign)
    | _ -> invalid_arg "wrong side")
  | _ -> invalid_arg "wrong kind of position (expected binary list)"

(* ----------------------------------------------------------------------
 * inferences
 * ---------------------------------------------------------------------- *)

(** do inferences that involve the given clause *)
let do_inferences active_set rules c =
  assert ((T.min_var c.cvars) > active_set.PS.active_clauses.C.bag_maxvar);
  (* apply every inference rule *)
  List.fold_left
    (fun acc (name, rule) ->
      Utils.debug 3 (lazy ("#  apply rule " ^ name));
      let new_clauses = rule active_set c in
      List.rev_append new_clauses acc)
    [] rules

(* helper that does one or zero superposition inference, with all
   the given parameters *)
let do_superposition ~ord active_clause active_pos passive_clause passive_pos subst acc =
  assert (List.length active_pos = 2);
  assert ((Utils.list_inter T.eq_foterm active_clause.cvars passive_clause.cvars) = []);
  match passive_pos with
  | [] | _::[] -> assert false
  | passive_idx::passive_side::subterm_pos ->
  let active_idx = List.hd active_pos
  and u, v, sign_uv = get_equations_sides passive_clause [passive_idx; passive_side]
  and s, t, sign_st = get_equations_sides active_clause active_pos in
  Utils.debug 3 (lazy (Utils.sprintf "@[<h>sup %a s=%a t=%a %a u=%a v=%a p=%a subst=%a@]"
                       (C.pp_clause ~sort:false) active_clause T.pp_foterm s T.pp_foterm t
                       (C.pp_clause ~sort:false) passive_clause T.pp_foterm u T.pp_foterm v
                       C.pp_pos passive_pos (S.pp_substitution ~sort:false) subst));
  if not sign_st
  then (Utils.debug 3 (lazy "active literal is negative"); acc)
  else begin
    assert (T.eq_foterm (S.apply_subst subst (T.at_pos u subterm_pos))
                        (S.apply_subst subst s));
    if (ord#compare (S.apply_subst subst s) (S.apply_subst subst t) = Lt ||
        ord#compare (S.apply_subst subst u) (S.apply_subst subst v) = Lt ||
        not (check_maximal_lit ~ord active_clause active_idx subst) ||
        not (check_maximal_lit ~ord passive_clause passive_idx subst))
      then begin
        Utils.debug 3 (lazy (Utils.sprintf "ordering constraint failed %s %s %B %B"
          (C.string_of_comparison (ord#compare
            (S.apply_subst subst s) (S.apply_subst subst t)))
          (C.string_of_comparison (ord#compare
            (S.apply_subst subst u) (S.apply_subst subst v)))
          (check_maximal_lit ~ord active_clause active_idx subst)
          (check_maximal_lit ~ord passive_clause passive_idx subst)));
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
        let new_clause = C.mk_clause new_lits proof in
        Utils.debug 3 (lazy (Utils.sprintf "ok, conclusion %a"
                            (C.pp_clause ~sort:false) new_clause));
        new_clause :: acc
      end
  end

let infer_active_ actives clause =
  let ord = actives.PS.a_ord
  and lits_pos = Utils.list_pos clause.clits in
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
    [] lits_pos

let infer_active actives clause =
  prof_infer_active.HExtlib.profile (infer_active_ actives) clause

let infer_passive_ actives clause =
  let ord = actives.PS.a_ord
  and lits_pos = Utils.list_pos clause.clits in
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
    [] lits_pos

let infer_passive actives clause =
  prof_infer_passive.HExtlib.profile (infer_passive_ actives) clause

let infer_equality_resolution_ actives clause =
  let ord = actives.PS.a_ord in
  fold_negative ~both:false
    (fun acc l r sign l_pos ->
      assert (not sign);
      match l_pos with
      | [] -> assert false
      | pos::_ ->
      try
        let subst = Unif.unification l r in
        if check_maximal_lit ~ord clause pos subst
          (* subst(lit) is maximal, we can do the inference *)
          then
            let proof = lazy (Proof ("equality_resolution", [clause, [pos], subst]))
            and new_lits = Utils.list_remove clause.clits pos in
            let new_lits = List.map (C.apply_subst_lit ~ord subst) new_lits in
            let new_clause = C.mk_clause new_lits proof in
            new_clause::acc
          else
            acc
      with UnificationFailure _ -> acc (* l and r not unifiable, try next *)
    )
    [] (Utils.list_pos clause.clits)

let infer_equality_resolution actives clause =
  prof_infer_equality_resolution.HExtlib.profile (infer_equality_resolution_ actives) clause

let infer_equality_factoring_ actives clause =
  let ord = actives.PS.a_ord
  and lits_pos = Utils.list_pos clause.clits in
  (* find root terms that are unifiable with s and are not in the
     literal at s_pos. This returns a list of position and substitution *)
  let find_unifiable_lits s s_pos candidate_lits =
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
    (* check whether subst(lit) is maximal *)
    if check_maximal_lit ~ord clause active_idx subst
      then
        let proof = lazy (Proof ("equality_factoring",
          [(clause, active_pos, subst); (clause, passive_pos, subst)]))
        (* new_lits: literals of the new clause. remove active literal
           and replace it by a t!=v one, and apply subst *)
        and new_lits = Utils.list_remove clause.clits active_idx in
        let new_lits = (C.mk_neq ~ord t v) :: new_lits in
        let new_lits = List.map (C.apply_subst_lit ~ord subst) new_lits in
        [C.mk_clause new_lits proof]
      else
        []
  (* try to do inferences with each positive literal *)
  in fold_positive ~both:true
    (fun acc s t _ s_pos -> (* try with s=t *)
      let unifiables = find_unifiable_lits s s_pos lits_pos in
      List.fold_left
        (fun acc (passive_pos, subst) ->
          (do_inference s_pos passive_pos subst) @ acc)
        acc unifiables)
    [] lits_pos

let infer_equality_factoring actives clause =
  prof_infer_equality_factoring.HExtlib.profile (infer_equality_factoring_ actives) clause

(* ----------------------------------------------------------------------
 * simplifications
 * ---------------------------------------------------------------------- *)

exception FoundMatch of (foterm * substitution * clause * position)

(** Do one step of demodulation on subterm. *)
let demod_subterm ~ord blocked_ids active_set subterm =
  (* unit clause+pos that potentially match subterm *)
  let matches =
    I.DT.retrieve_generalizations active_set.PS.idx.I.unit_root_index subterm in
  try
    I.ClauseSet.iter
      (fun (unit_hclause, pos, l) ->
        (* do we have to ignore the clause? *)
        if List.mem unit_hclause.tag blocked_ids then ()
        else
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
    is already in the active_set) *)
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
      C.mk_clause new_lits proof

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
    Utils.debug 3 (lazy (Utils.sprintf "%a is a tautology" (C.pp_clause ~sort:false) c)));
  is_tauto

let basic_simplify ~ord clause =
  let absurd_lit lit = match lit with
  | Equation (l, r, false, _) when T.eq_foterm l r -> true
  | _ -> false in
  (* remove s!=s literals *)
  let new_lits = List.filter (fun lit -> not (absurd_lit lit)) clause.clits in
  (* remove duplicate literals *)
  let new_lits = Utils.list_uniq C.eq_literal new_lits in
  (* destructive equality resolution *)
  let rec er lits =
    match HExtlib.list_index er_check lits with
    | None -> lits
    | Some (i, Equation (l, r, sign, _)) ->
        assert (not sign);
        assert (T.is_var l || T.is_var r);
        try
          let subst = Unif.unification l r in
          er (List.map (C.apply_subst_lit ~ord subst) (Utils.list_remove lits i))
        with UnificationFailure _ -> lits
  (* finds candidate literals for destructive ER (lits with >= 1 variable) *)
  and er_check (Equation (l, r, sign, _)) = (not sign) && (T.is_var l || T.is_var r) in
  let new_lits = er new_lits in
  let new_clause = C.mk_clause new_lits clause.cproof in
  (Utils.debug 3 (lazy (Utils.sprintf "%a basic_simplifies into %a" (C.pp_clause ~sort:false)
                        clause (C.pp_clause ~sort:false) new_clause)));
  new_clause

let subsumes a b =
  if List.length a.clits > List.length b.clits then false else false (* TODO *)


let subsumed_by_set set clause = false (* TODO *)
let subsumed_in_set set clause = [] (* TODO *)
let orphan_murder set clause = set (* TODO *)
