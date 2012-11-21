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


(** apply f to all non-variable positions in t. f is given the subterm, the
    position and the context at each such position. *)
let all_positions pos t f =
  let rec aux pos t = match t.term with
  | Var _ -> ()
  | Node (hd, tl) ->
    f t pos;  (* apply to term itself *)
    ignore
      (List.fold_left
        (fun idx t ->
          aux (pos @ [idx]) t; (* recurse in subterm *)
          idx+1)
      0 tl)
  in
  aux pos t

(* ----------------------------------------------------------------------
 * inferences
 * ---------------------------------------------------------------------- *)

(* helper that does one or zero superposition inference, with all
   the given parameters *)
let do_superposition ~cs active_clause active_pos passive_clause passive_pos subst acc =
  assert (List.length active_pos = 2);
  let ord = cs#ord in
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
  if not sign_st 
  then Utils.debug 3 (lazy "... active literal is negative")
  else if T.db_var (T.at_pos u subterm_pos)
  then Utils.debug 3 (lazy "... passive subterm is a DB variable")
  else
  let t' = S.apply_subst subst t
  and v' = S.apply_subst subst v in
  if sign_uv && T.eq_term t' v' && subterm_pos = []
  then Utils.debug 3 (lazy "... will yield a tautology")
  else begin
    assert (T.eq_term (S.apply_subst subst (T.at_pos u subterm_pos))
                        (S.apply_subst subst s));
    if (ord#compare (S.apply_subst subst s) t' = Lt ||
        ord#compare (S.apply_subst subst u) v' = Lt ||
        not (C.eligible_param ~cs active_clause active_idx subst) ||
        not (C.eligible_res ~cs passive_clause passive_idx subst))
      then ()
      else begin (* ordering constraints are ok *)
        let eqns = Vector.create 10 in
        (* copy other literals *)
        for i = 0 to Array.length active_clause.clits - 1 do
          if i <> active_idx then
            Vector.push eqns (C.apply_subst_eqn subst active_clause.clits.(i).lit_eqn)
        done;
        for i = 0 to Array.length passive_clause.clits - 1 do
          if i <> passive_idx then
            Vector.push eqns (C.apply_subst_eqn subst passive_clause.clits.(i).lit_eqn)
        done;
        let new_u = T.replace_pos u subterm_pos t in (* replace s by t in u|_p *)
        Vector.push eqns (C.mk_eqn new_u v sign_uv);
        let rule = if sign_uv then "sup+" else "sup-" in
        let proof = lazy (Proof (rule, [(active_clause, active_pos, subst);
                                        (passive_clause, passive_pos, subst)])) in
        let new_clause =
          C.mk_clause ~cs (Vector.to_array eqns) proof [active_clause; passive_clause] in
        Utils.debug 3 (lazy (Utils.sprintf "... ok, conclusion @[<h>%a@]"
                            !C.pp_clause#pp new_clause));
        Vector.push acc new_clause
      end
  end

let infer_active_ actives clause =
  let cs = actives.PS.a_cs in
  let acc = Vector.create 10 in
  if clause.cselected = 0
  then acc  (* no literal can be eligible for paramodulation *)
  else begin
    (* do the inferences where clause is active; for this,
       we try to rewrite conditionally other clauses using
       non-minimal sides of every positive literal *)
    fold_positive ~both:true ~ord:cs#ord
      (fun () lit s t _ s_pos ->
        if not lit.lit_maximal then () else
        (* rewrite clauses using s *)
        let subterm_idx = actives.PS.idx#subterm_index in
        subterm_idx#retrieve_unifiables s ()
          (fun () u_p set ->
            try (* rewrite u_p with s, if they are unifiable *)
              let subst = Unif.unification S.id_subst s u_p in
              I.ClauseSet.iter
                (fun (hc, u_pos, u_p) ->
                  do_superposition ~cs clause s_pos hc u_pos subst acc)
                set
            with
              UnificationFailure -> ()))
      () clause.clits;
    acc
  end

let infer_active actives clause =
  prof_infer_active.HExtlib.profile (infer_active_ actives) clause

let infer_passive_ actives clause =
  let acc = Vector.create 10 in
  let cs = actives.PS.a_cs in
  let eligible lit =
    if clause.cselected = 0 then lit.lit_maximal else lit.lit_selected
  in
  (* do the inferences in which clause is passive (rewritten),
     so we consider both negative and positive literals *)
  fold_lits ~both:true ~pos:true ~neg:true ~ord:cs#ord
    (fun () lit u v _ u_pos ->
      if not (eligible lit) then () else
      (* rewrite subterms of u *)
      all_positions u_pos u
        (fun u_p p ->
          (* all terms that occur in an equation in the active_set
             and that are potentially unifiable with u_p (u at position p) *)
          actives.PS.idx#root_index#retrieve_unifiables u_p ()
            (fun () s set ->
              try
                let subst = Unif.unification S.id_subst s u_p in
                I.ClauseSet.iter
                  (fun (hc, s_pos, s) ->
                      do_superposition ~cs hc s_pos clause p subst acc)
                  set
              with
                UnificationFailure -> ())))
    () clause.clits;
  acc

let infer_passive actives clause =
  prof_infer_passive.HExtlib.profile (infer_passive_ actives) clause

let infer_equality_resolution_ ~cs clause =
  let acc = Vector.create 4 in
  (* literals that can potentially be eligible for resolution *)
  let eligible lit =
    if clause.cselected = 0 then lit.lit_maximal else lit.lit_selected in
  (* iterate on those literals *)
  fold_negative ~both:false ~ord:cs#ord
    (fun () lit l r sign l_pos ->
      assert (not sign);
      if not (eligible lit) then () else
      match l_pos with
      | [] -> assert false
      | pos::_ ->
      try
        let subst = Unif.unification S.id_subst l r in
        if C.eligible_res ~cs clause pos subst
          (* subst(lit) is maximal, we can do the inference *)
          then begin
            incr_stat stat_equality_resolution_call;
            let proof = lazy (Proof ("eq_res", [clause, [pos], subst])) in
            let eqns = Vector.create (Array.length clause.clits - 1) in
            for i = 0 to Array.length clause.clits - 1 do
              if i <> pos
                then Vector.push eqns (C.apply_subst_eqn subst clause.clits.(i).lit_eqn)
            done;
            let new_clause = C.mk_clause ~cs (Vector.to_array eqns) proof [clause] in
            Utils.debug 3 (lazy (Utils.sprintf
                          "equality resolution on @[<h>%a@] yields @[<h>%a@]"
                          !C.pp_clause#pp clause !C.pp_clause#pp new_clause));
            Vector.push acc new_clause
          end 
      with UnificationFailure -> () (* l and r not unifiable, try next *)
    )
    () clause.clits;
  acc

let infer_equality_resolution ~cs clause =
  prof_infer_equality_resolution.HExtlib.profile (infer_equality_resolution_ ~cs) clause

let infer_equality_factoring_ ~cs clause =
  let acc = Vector.create 5 in
  let ord = cs#ord in
  (* try to perform an inference where not(s < t) *)
  let rec try_inference i j u v s t =
    (try
      let subst = Unif.unification S.id_subst s u in
      if ord#compare (S.apply_subst subst s) (S.apply_subst subst t) <> Lt
        && C.eligible_param ~cs clause i subst
        then do_inference i j subst u v s t
    with UnificationFailure -> ());
    (try
      let subst = Unif.unification S.id_subst s v in
      if ord#compare (S.apply_subst subst s) (S.apply_subst subst t) <> Lt
        && C.eligible_param ~cs clause i subst
        then do_inference i j subst v u s t
    with UnificationFailure -> ());
  (* perform the inference *)
  and do_inference i j subst u v s t =
    incr_stat stat_equality_factoring_call;
    let eqns = Vector.create (Array.length clause.clits) in
    (* add the other literals *)
    for k = 0 to Array.length clause.clits - 1 do
      if k <> i
        then Vector.push eqns (C.apply_subst_eqn subst clause.clits.(k).lit_eqn)
    done;
    Vector.push eqns (C.apply_subst_eqn subst (C.mk_neq t v));
    let proof = lazy (Proof ("eq_fact",
      [(clause, [i], subst); (clause, [j], subst)])) in
    let new_clause = C.mk_clause ~cs (Vector.to_array eqns) proof [clause] in
    Utils.debug 3 (lazy (Utils.sprintf
                  "equality factoring on @[<h>%a@] yields @[<h>%a@]"
                  !C.pp_clause#pp clause !C.pp_clause#pp new_clause));
    Vector.push acc new_clause
  in
  (if clause.cselected = 0 then (* try to factorize every pair of literals *)
    for i = 0 to Array.length clause.clits - 1 do
      let lit = clause.clits.(i) in
      (* literal is positive maximal, hence eligible for paramodulation *)
      if C.pos_eqn lit.lit_eqn && lit.lit_maximal
        then
          match lit.lit_eqn with
          | Equation (s, t, sign) ->
            assert sign;
            let cmp_st = ord#compare s t in
            for j = 0 to Array.length clause.clits - 1 do
              let lit' = clause.clits.(j) in
              (* only try with positive literals *)
              if j = i || C.neg_eqn lit'.lit_eqn then () else
              match lit'.lit_eqn with
              | Equation (u, v, _) ->
                (* now try to factorize those two equations *)
                (match cmp_st with
                | Gt -> try_inference i j u v s t
                | Lt -> try_inference i j u v t s
                | _ -> try_inference i j u v s t; try_inference i j u v t s)
            done
    done);
  acc

let infer_equality_factoring ~cs clause =
  prof_infer_equality_factoring.HExtlib.profile (infer_equality_factoring_ ~cs) clause

(* ----------------------------------------------------------------------
 * simplifications
 * ---------------------------------------------------------------------- *)

exception RewriteInto of term

(** Compute normal form of term w.r.t active set. Clauses used to
    rewrite are added to the clauses hashset. *)
let demod_nf ~ord active_set clauses t =
  (* compute normal form of subterm *) 
  let rec normal_form t =
    (* do not rewrite non-atomic formulas *)
    if t.sort = bool_sort  && not (T.atomic t)
      then t  (* do not rewrite such formulas *)
      else begin
        (* try to rewrite using unit positive clauses *) 
        let t' = ground_rewrite t in
        (* find equations l=r that match subterm *)
        try
          active_set.PS.idx#unit_root_index#retrieve ~sign:true t'
            (fun l r subst unit_hclause ->
              (* r is the term subterm is going to be rewritten into *)
              C.CHashSet.add clauses unit_hclause;
              let new_l = t'
              and new_r = S.apply_subst subst r in
              if ord#compare new_l new_r = Gt
                (* subst(l) > subst(r), we can rewrite *)
                then begin
                  Utils.debug 4 (lazy (Utils.sprintf "rewrite %a into %a using %a"
                                 !T.pp_term#pp new_l !T.pp_term#pp new_r
                                 !C.pp_clause#pp_h unit_hclause));
                  raise (RewriteInto new_r)
                end else Utils.debug 4
                  (lazy (Utils.sprintf "could not rewrite %a into %a using %a, bad order"
                         !T.pp_term#pp new_l !T.pp_term#pp new_r
                         !C.pp_clause#pp_h unit_hclause)));
          t' (* not found any match, normal form found *)
        with RewriteInto t'' -> normal_form t''
      end
  (* rewrite ground terms *)
  and ground_rewrite t = 
    if T.is_ground_term t
      then
        let t' =
          try 
            let t', (hc, _, _) = Ptmap.find t.tag active_set.PS.idx#ground_rewrite_index in
            C.CHashSet.add clauses hc;  (* remember we used this clause *)
            t'
          with Not_found -> t
        in
        if T.eq_term t t' then t else ground_rewrite t' (* continue rewriting *)
      else t
  (* rewrite innermost-leftmost *)
  and traverse t =
    match t.term with
    | Var _ -> t
    | Node (s, l) ->
      (* rewrite subterms *)
      let l' = List.map traverse l in
      let t' = T.mk_node s t.sort l' in
      (* rewrite term at root *)
      normal_form t'
  in
  traverse t

let demodulate active_set clause =
  let cs = active_set.PS.a_cs in
  (* clauses used to rewrite *)
  let clauses = C.CHashSet.create () in
  (* demodulate literals *)
  let demod_lit idx lit =
    match lit.lit_eqn with
    | Equation (_, _, true) when C.eligible_res ~cs clause idx S.id_subst ->
      lit.lit_eqn (* do not rewrite literals eligible for resolution *)
    | Equation (l, r, sign) ->
      C.mk_eqn
        (demod_nf ~ord:cs#ord active_set clauses l)
        (demod_nf ~ord:cs#ord active_set clauses r)
        sign
  in
  (* demodulate every literal *)
  let eqns = Array.mapi demod_lit clause.clits in
  if C.CHashSet.is_empty clauses
    then clause (* no rewriting performed *)
    else begin  (* construct new clause *)
      let clauses = C.CHashSet.to_list clauses in
      let clauses_subst = List.map (fun c -> (c, [], S.id_subst)) clauses in
      let proof = lazy (Proof ("demod", (clause, [], S.id_subst)::clauses_subst)) in
      (* parents are clauses used to simplify the clause, plus parents of the clause *)
      let parents = clauses @ clause.cparents in
      C.mk_clause ~cs:active_set.PS.a_cs eqns proof parents
    end

let is_tautology c =
  try
    for i = 0 to Array.length c.clits - 1 do
      let lit = c.clits.(i) in
      (* check for s=s, and s=t v s!=t *)
      (match lit.lit_eqn with
       | Equation (l, r, true) when T.eq_term l r -> raise Exit
       | Equation (l, r, sign) -> 
          let eqn = C.negate_eqn lit.lit_eqn in (* flip equation for tests *)
          for j = i+1 to Array.length c.clits - 1 do
            let eqn' = c.clits.(j).lit_eqn in
            if C.eq_eqn eqn eqn' then raise Exit
          done)
    done;
    false
  with Exit ->
    Utils.debug 3 (lazy (Utils.sprintf "@[<h>%a@] is a tautology" !C.pp_clause#pp c));
    true

(** semantic tautology deletion, using a congruence closure algorithm
    to see if negative literals imply some positive literal *)
let is_semantic_tautology c = false (* TODO *)

let basic_simplify ~cs clause =
  let simplified = ref false in
  (* convert some fof to literals *)
  let clause = C.clause_of_fof ~cs clause in
  (* new equations, and substitution for DER (destructive equality resolution) *)
  let eqns = Vector.create (Array.length clause.clits)
  and subst = ref S.id_subst in
  (* iterate through literals for DER. Add surviving literals
     to the vector eqns. *)
  for i = 0 to Array.length clause.clits - 1 do
    let lit = clause.clits.(i) in
    match C.apply_subst_eqn !subst lit.lit_eqn with
    | Equation (l, r, false) when T.eq_term l r ->
      simplified := true  (* remove s!=s *)
    | Equation (x, r, false) when T.is_var x && not (T.var_occurs x r) ->
      (* add x -> r to the substitution, remove literal *)
      subst := S.build_subst !subst x r;
      simplified := true
    | eqn -> Vector.push eqns eqn (* keep this equation *)
  done;
  (* apply substitution consistently to eqns *)
  let eqns = Vector.map eqns (C.apply_subst_eqn !subst) in
  (* remove s!=s once again *)
  let eqns = Vector.filter eqns
    (function | Equation (l,r,false) when T.eq_term l r -> false | _ -> true) in
  (* remove duplicate literals *)
  let eqns = Vector.uniq_sort ~cmp:C.compare_eqn eqns in
  (* build clause if something changed *)
  if not !simplified && Vector.size eqns = Array.length clause.clits
    then clause
    else begin
      let parents = clause.cparents in
      let proof = clause.cproof in  (* do not bother printing this *)
      let new_clause = C.mk_clause ~cs (Vector.to_array eqns) proof parents in
      Utils.debug 3 (lazy (Utils.sprintf ("@[<hov 4>@[<h>%a@]@ basic_simplifies "^^
                      "into @[<h>%a@]@]") !C.pp_clause#pp clause !C.pp_clause#pp new_clause));
      new_clause
    end

exception FoundMatch of (term * hclause)

(* Perform simplify reflect on the clause (positive and negative) *)
let simplify_reflect active_set clause =
  let cs = active_set.PS.a_cs in
  let eqns = Vector.create (Array.length clause.clits) in
  let clauses = ref [] in  (* clauses used for simplification *)
  (* equate those two terms using a negative equation *)
  let refute_neg s t =
    try active_set.PS.idx#unit_root_index#retrieve ~sign:false s
      (fun l r subst hc ->
        if T.eq_term t (S.apply_subst subst r)
        then raise (FoundMatch (r, hc))); (* success *)
      None (* no match *)
    with FoundMatch (r, hc) ->
      Some hc  (* success *)
  (* equate the terms using a positive equation *)
  and refute_pos s t =
    let pairs = Unif.disunify s t in
    match pairs with
    | [s', t'] ->  (* try to prove s = t with a positive unit clause *)
      (try active_set.PS.idx#unit_root_index#retrieve ~sign:true s'
        (fun l r subst hc ->
          if T.eq_term t' (S.apply_subst subst r)
          then raise (FoundMatch (r, hc))); (* success *)
        None (* no match *)
      with FoundMatch (r, hc) ->
        Some hc)
    | _ ->
      None(* not 1 diff, but 0 or several, do nothing *)
  in
  (* iterate through literals and try to resolve negative ones *)
  for i = 0 to Array.length clause.clits - 1 do
    let lit = clause.clits.(i) in
    match lit.lit_eqn with
    | Equation (s, t, true) ->
      (* negative simplify reflect : remove the literal using a negative unit clause *)
      (match refute_neg s t with
      | None -> Vector.push eqns lit.lit_eqn  (* keep literal *)
      | Some hc -> clauses := hc :: !clauses)
    | Equation (s, t, false) ->
      (* positive simplify reflect : remove literal using a positive unit clause *)
      (match refute_pos s t with
      | None -> Vector.push eqns lit.lit_eqn  (* keep literal *)
      | Some hc -> clauses := hc :: !clauses)
  done;
  (* rebuild clause if different *)
  if !clauses = []
    then clause  (* no simplification performed *)
    else begin
      let proof = lazy (Proof ("simplify_reflect",
        (clause, [], S.id_subst)::(List.map (fun c -> (c, [], S.id_subst)) !clauses)))
      and parents = clause.cparents in
      let new_clause = C.mk_clause ~cs (Vector.to_array eqns) proof parents in
      Utils.debug 3 (lazy (Utils.sprintf ("@[<hov 4>@[<h>%a@]@ simplify-reflect "^^
                      "into @[<h>%a@]@]") !C.pp_clause#pp clause !C.pp_clause#pp new_clause));
      new_clause
    end

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
  match lit_a.lit_eqn, lit_b.lit_eqn with
  | Equation (_, _, signa), Equation (_, _, signb) when signa <> signb -> [] (* different sign *)
  | Equation (la, ra, signa), Equation (lb, rb, signb) when T.eq_term la lb && T.eq_term ra rb ->
    [S.id_subst]
  | Equation (la, ra, signa), Equation (lb, rb, signb) when T.eq_term la rb && T.eq_term ra lb ->
    [S.id_subst]
  | Equation (la, ra, signa), Equation (lb, rb, signb) when T.eq_term la lb ->
    (try [Unif.matching_locked ~locked subst ra rb]
    with UnificationFailure -> [])
  | Equation (la, ra, signa), Equation (lb, rb, signb) when T.eq_term la rb ->
    (try [Unif.matching_locked ~locked subst ra lb]
    with UnificationFailure -> [])
  | Equation (la, ra, signa), Equation (lb, rb, signb) when T.eq_term ra rb ->
    (try [Unif.matching_locked ~locked subst la lb]
    with UnificationFailure -> [])
  | Equation (la, ra, signa), Equation (lb, rb, signb) when T.eq_term ra lb ->
    (try [Unif.matching_locked ~locked subst la rb]
    with UnificationFailure -> [])
  | Equation (la, ra, signa), Equation (lb, rb, signb) -> (* general case *)
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
  let ans = Vector.create (Array.length clause.clits) in
  Array.iteri
    (fun idx lit' -> (* match lit with lit', add substitutions to ans *)
      List.iter
        (fun subst -> Vector.push ans (idx, subst))
        (match_lits ~locked S.id_subst lit lit'))
    clause.clits;
  Vector.to_array ans

(** Check whether a subsumes b, and if it does, return the
    corresponding substitution *)
let subsumes_with a b =
  incr_stat stat_subsumption_call;
  (* trivial case, plus filter: a must not have more literals *)
  if a.clits = [||] then Some S.id_subst
  else if Array.length a.clits > Array.length b.clits then None else
  let locked = T.THashSet.from_array b.cvars in
  (* array of matched literals in b, for each literal of a *)
  let subst_array = Array.map
    (fun lit -> matched_lits ~locked lit b)
    a.clits in
  (* sort by increasing number of solutions *)
  Array.sort (fun a1 a2 -> (Array.length a1) - (Array.length a2)) subst_array;
  (* find a compatible superset of all those substitutions *)
  let rec find_compatible subst indexes i =
    if i = Array.length subst_array
    then raise (SubsumptionFound subst) (* explored the whole array *)
    else
      let substs = subst_array.(i) in  (* substitutions for literal i *)
      Array.iter  (* continue by merging with compatible substs *)
        (fun (idx, s) ->
          match () with
          | _ when Ptset.mem idx indexes -> ()
          | _ when compatible_substs s subst ->
            (* merge subst with lits, continue *)
            let indexes' = Ptset.add idx indexes
            and subst' = merge_substs s subst in
            find_compatible subst' indexes' (i+1)
          | _ -> ()
        ) substs
  in
  if Array.length subst_array.(0) = 0
    then None  (* some literal of a matches no literal of b *)
    else
      try
        find_compatible S.id_subst Ptset.empty 0;
        None  (* no subsuming substitution found *)
      with SubsumptionFound subst ->
        Some subst

let subsumes a b =
  let check () = subsumes_with a b <> None in
  prof_subsumption.HExtlib.profile check ()

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
  let v = Vector.create 10 in
    FV.retrieve_subsumed
      set.PS.fv_idx c
      (fun c' -> if subsumes c c' then Vector.push v c');
  v

let subsumed_in_set set clause =
  prof_subsumption_in_set.HExtlib.profile (subsumed_in_set_ set) clause

(* ----------------------------------------------------------------------
 * reduction to CNF
 * ---------------------------------------------------------------------- *)

(** Transform the clause into proper CNF; returns a list of clauses *)
let cnf_of ~cs clause =
  (* unique counter for variable indexes *)
  let varindex = ref 0 in
  (* negation normal form (also remove equivalence and implications) *) 
  let rec nnf t =
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
      let new_t' = Calculus.skolem ~ord:cs#ord t' sort in
      skolemize new_t' (* remove forall *)
    | Node (s, l) -> T.mk_node s t.sort (List.map skolemize l)
  (* vector singleton *)
  and singleton x = Vector.from_list [Vector.from_list [x]]
  (* reduction to cnf using De Morgan. Returns a list of list of terms *)
  and to_cnf t =
    if t.sort <> bool_sort then singleton (t, true)
    else match t.term with
    | Var _ | Node (_, []) -> singleton (t, true)
    | Node (s, [t']) when s = not_symbol ->
      assert (T.atomic_rec t' ||
              match t'.term with Node (s', _) when s' = eq_symbol -> true | _ -> false);
      singleton (t', false)
    | Node (s, [a; b]) when s = and_symbol ->
      let va = to_cnf a
      and vb = to_cnf b in
      Vector.append va vb;  (* concatenate both lists of clauses *)
      va
    | Node (s, [a; b]) when s = or_symbol ->
      Vector.product (to_cnf a) (to_cnf b)
    | Node _ -> singleton (t, true)
  (* check whether the clause is already in CNF *)
  and is_cnf c =
    try
      Array.iter
        (fun {lit_eqn=Equation (l, r, sign)} ->
          if T.atomic_rec l && T.atomic_rec r then () else raise Exit)
        c.clits;
      true
    with Exit -> false
  in
  (* main part, apply all transformations *)
  Utils.debug 3 (lazy (Utils.sprintf "input clause %a@." !C.pp_clause#pp clause));
  if is_cnf clause
    then begin
      Utils.debug 3 (lazy (Utils.sprintf "clause @[<h>%a@] is cnf" !C.pp_clause#pp clause));
      Vector.from_list [clause] (* already cnf, perfect *)
    end else
      let nnf_eqns = Array.map (fun lit -> nnf (C.term_of_eqn lit.lit_eqn)) clause.clits in
      let skolem_eqns = Array.map skolemize nnf_eqns in
      let eqn_vec_vec_vec = Array.map to_cnf skolem_eqns in
      (* vector of vectors of equations, by or-product *)
      assert (Array.length eqn_vec_vec_vec > 0);
      let eqn_vec_vec = ref eqn_vec_vec_vec.(0) in
      for i = 1 to Array.length eqn_vec_vec_vec - 1 do
        eqn_vec_vec := Vector.product !eqn_vec_vec eqn_vec_vec_vec.(i);
      done;
      (* build clauses from lits *)
      let proof = lazy (Proof ("to_cnf", [clause, [], S.id_subst])) in
      let clauses = Vector.map !eqn_vec_vec
        (fun eqn_vec ->
          let eqns = Vector.to_array eqn_vec in
          let eqns = Array.map (fun (t, sign) -> C.mk_eqn t T.true_term sign) eqns in
          let clause = C.mk_clause ~cs eqns proof [clause] in
          C.clause_of_fof ~cs clause)
      in
      Utils.debug 3 (lazy (Utils.sprintf "%% @[<h>clause %a@]@ @[<h>to_cnf -> %a@]"
                    !C.pp_clause#pp clause (Utils.pp_vector (fun f _ c -> !C.pp_clause#pp f c)) clauses));
      Vector.iter clauses (fun c -> assert (is_cnf c));
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

    method basic_simplify ~cs c = basic_simplify ~cs c

    method simplify actives c =
      let cs = actives.PS.a_cs in
      let c = basic_simplify ~cs c in
      let c = basic_simplify ~cs (demodulate actives c) in
      let c = basic_simplify ~cs (simplify_reflect actives c) in
      c

    method redundant actives c = subsumed_by_set actives c

    method redundant_set actives c = subsumed_in_set actives c

    method list_simplify ~cs c =
      if is_tautology c then Some (Vector.create 0) else None  (* no other list simplification *)

    method axioms = Vector.create 0

    method constr _ = O.consts_constraint 

    method preprocess ~cs v =
      let v' = Vector.create (Vector.size v * 2) in
      Vector.iter v
        (fun c ->
          (* reduction to CNF *)
          let clauses = cnf_of ~cs c in
          Vector.iter clauses
            (fun c' ->
              (* add resulting clauses to v' if they are not tautologies *)
              let c' = C.clause_of_fof ~cs c' in
              if not (is_tautology c') then Vector.push v' c'));
      v'
  end
