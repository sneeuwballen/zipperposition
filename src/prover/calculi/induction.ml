
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Induction through Cut} *)

open Libzipperposition

module Lits = Literals
module TI = InnerTerm
module T = FOTerm
module Su = Substs
module Ty = Type

module type S = Induction_intf.S

let section = Util.Section.make ~parent:Const.section "induction"
let section_guess = Util.Section.make ~parent:Const.section "lemma_guess"

let stats_lemmas = Util.mk_stat "induction.inductive_lemmas"
let stats_guess_lemmas_absurd = Util.mk_stat "induction.guess_lemmas_absurd"
let stats_guess_lemmas_trivial = Util.mk_stat "induction.guess_lemmas_trivial"
let stats_guess_lemmas = Util.mk_stat "induction.guess_lemmas"
let stats_min = Util.mk_stat "induction.assert_min"

let prof_guess_lemma = Util.mk_profiler "induction.guess_lemma"
let prof_check_lemma = Util.mk_profiler "induction.check_lemma"

let k_enable : bool Flex_state.key = Flex_state.create_key()
let k_lemma_guess : bool Flex_state.key = Flex_state.create_key()
let k_ind_depth : int Flex_state.key = Flex_state.create_key()

(* TODO
 in any ground inductive clause C[n]<-Gamma, containing inductive [n]:
   - find path [p], if any (empty at worst)
   - extract context C[]
   - find coverset of [n]
   - for every [n' < n] in the coverset, strengthen the path
     (i.e. if there is [c=t, D[]] in the path with the proper
       type, add [not D[n']], because [n' < n ... < c])
   - add C[t] <- [n=t · p], Gamma
     for every [t] in coverset
   - add boolean clause  [n=t1] or [n=t2] .. or [n=tk] <= Gamma
     where coverset = {t1, t2, ... tk}

  rule to make trivial any clause with >= 2 incompatible path literals
*)

module Cst_set = CCSet.Make(struct
    type t = Ind_cst.cst
    let compare = Ind_cst.cst_compare
  end)

let scan_term (t:T.t): Cst_set.t =
  Ind_cst.find_cst_in_term t
  |> Cst_set.of_seq

(* scan terms for inductive constants *)
let scan_terms (seq:T.t Sequence.t) : Cst_set.t =
  seq
  |> Sequence.flat_map Ind_cst.find_cst_in_term
  |> Cst_set.of_seq

(** {2 Guess new Lemmas} *)

module Make_guess_lemma
    (E : Env.S)(A : Avatar_intf.S with module E = E) : sig
  val inf_guess_lemma : E.unary_inf_rule
end = struct
  module C = E.C
  module SubsumptionIndex = FeatureVector.Make(struct
      type t = Literals.t
      let compare = Literals.compare
      let to_lits c = Array.map Literal.Conv.to_form c |> Sequence.of_array
    end)

  let section = section_guess

  let has_pos_lit_ c = CCArray.exists Literal.is_pos (C.lits c)

  let is_ind_conjecture_ c =
    match C.distance_to_goal c with
    | Some (0 | 1) -> true
    | Some _
    | None -> false

  (* sub-terms that have an inductive (non-arrow) type, candidates
     for "subterm generalization" *)
  let generalizable_subterms c =
    C.Seq.terms c
      |> Sequence.flat_map T.Seq.subterms
      |> Sequence.filter
        (fun t ->
           assert (T.is_ground t);
           Type.is_app (T.ty t) &&
           Ind_ty.is_inductive_type (T.ty t))
      |> Sequence.sort_uniq ~cmp:T.compare
      |> Sequence.to_rev_list

  type lemma_candidate = Literals.t list

  let compare_lemma : lemma_candidate CCOrd.t = CCOrd.list_ Literals.compare

  (* set of all lemmas so far, to check if a new candidate lemma
     has already been tried *)
  let all_candidates_ = ref (SubsumptionIndex.empty ())

  (* update [all_candidates_] when a lemma is added *)
  let () =
    Signal.on_every A.on_lemma
      (fun cut ->
         let cs = cut.A.cut_pos |> List.map (fun c -> C.lits c) in
         all_candidates_ := SubsumptionIndex.add_list !all_candidates_ cs)

  let pp_lemma out (l:lemma_candidate) =
    Format.fprintf out "{@[<v>%a@]}" (Util.pp_list Literals.pp) l

  type generalization = (T.Set.t * Type.t) list
  (** One generalization: a list of clusters, each cluster being
      a set of terms that should map to the same variable,
      with some specific type *)

  let pp_generalization out (g:generalization) =
    let pp_set out (s,_) =
      Format.fprintf out "[@[<hv>%a@]]" (Util.pp_list T.pp) (T.Set.to_list s)
    in Format.fprintf out "{@[<hv>%a@]}" (Util.pp_list pp_set) g

  let common_cst c1 c2 : bool =
    not (Cst_set.is_empty (Cst_set.inter c1 c2))

  (* check predicate [p] on subterms, starting from the root towards the
     leaves. *)
  let check_subterms_order_ (c:C.t) (p:T.t -> [`True | `False | `Recurse]): bool =
    let rec check_term t =
      match p t with
        | `True -> true
        | `False -> false
        | `Recurse ->
          match T.view t with
            | T.AppBuiltin (_, l)
            | T.App (_, l) -> List.for_all check_term l
            | T.Const _ | T.DB _ | T.Var _ -> true
    in
    C.Seq.terms c |> Sequence.for_all check_term

  (* does every inductive constant of [c] occur only as a
     non-strict subterm of some element of [g]? *)
  let covers_all_csts (c:C.t) (g:generalization): bool =
    let cst_set = C.Seq.terms c |> scan_terms in
    check_subterms_order_ c
      (fun t -> match T.view t with
         | _ when List.exists (fun (set,_) -> T.Set.mem t set) g -> `True
         | T.Const id ->
           if Cst_set.exists (fun c -> ID.equal id (Ind_cst.cst_id c)) cst_set
           then `False else `True
         | _ -> `Recurse)

  (* does the generalization contain at least one term that is not
     an inductive constant? *)
  let has_some_non_cst (g:generalization): bool =
    let check_t t = match T.view t with
      | T.Const id -> not (Ind_cst.is_cst id)
      | _ -> true
    in
    List.exists (fun (set,_) -> T.Set.exists check_t set) g

  (* do those terms share at least one common constant? *)
  let share_common_cst (t1:T.t) (t2:T.t) : bool =
    common_cst (scan_term t1) (scan_term t2)

  (* TODO:
     check some things in the generalization. It must be realistic that
     terms in the same cluster can be equal
     (e.g. a cluster [t, succ(t)] should flag a generalization as impossible).
  *)

  (* add a term to the generalization, possibly merging with some existing
     sets. returns [None] if addition failed. *)
  let add_term (g:generalization) (t:T.t): generalization option =
    try
      let others, set =
        List.fold_left
          (fun (others,set_t) (set',ty') ->
             if T.Set.exists (share_common_cst t) set'
             then
               if Type.equal (T.ty t) ty'
               then others, T.Set.union set_t set'
               else raise Exit (* should be merged, but can't *)
             else (set',ty') :: others, set_t)
          ([], T.Set.singleton t) g
      in
      Some ((set, T.ty t) :: others)
    with Exit -> None

  (* given a set of terms of inductive type that occur in a clause [c], we
     return all the combinations of those terms that satisfy some criteria.

     Simple generalization:
     - all the occurrences of inductive constants in [c] are subterms of
       some cluster in the generalization.
     - clusters are maximal, that is, two terms that share some sub-constant
       will belong in the same cluster.

     A cluster is made of different terms that share some ind. constant
     and will map to the same variable, assuming they all have the same type.
  *)
  let enumerate_generalizations (c:C.t) (terms:T.t list): generalization Sequence.t =
    let cst_set = C.Seq.terms c |> scan_terms in
    Util.debugf ~section 3
      "@[<2>try to guess lemmas@ from %a@ \
       can generalize on [@[%a@]]@ must eliminate [@[%a@]]@]"
      (fun k->k C.pp c (Util.pp_list T.pp) terms
          (Util.pp_list Ind_cst.pp_cst) (Cst_set.elements cst_set));
    (* enumerate states *)
    let rec aux (g:generalization)(to_process:T.t list): generalization Sequence.t =
      match to_process with
        | [] -> Sequence.return g (* done *)
        | t :: to_process_tail ->
          let seq1 = aux g to_process_tail (* try without [t] *)
          and seq2 = match add_term g t with
            | None -> Sequence.empty  (* skip [t] *)
            | Some g' -> aux g' to_process_tail
          in
          Sequence.append seq1 seq2
    in
    aux [] terms
    |> Sequence.filter
      (fun g -> has_some_non_cst g && covers_all_csts c g)

  (* check lemma on small instances. It returns [true] iff none of the
     instances reduces to [false] *)
  let small_check (lemma:lemma_candidate): bool =
    (* generate instances of [lits]. If [last=true], instantiating variables
       with parametrized constructors is forbidden (must return a leaf) *)
    let gen_instances ~(last:bool) (lits:Literals.t): Literals.t Sequence.t =
      let subst_add subst v t =
        Substs.FO.bind subst ((v:Type.t HVar.t:>TI.t HVar.t),0) (t,0)
      and subst_add_ty subst v ty =
        Substs.Ty.bind subst ((v:Type.t HVar.t:>TI.t HVar.t),0) (ty,0)
      and subst_mem subst v =
        Substs.mem subst ((v:Type.t HVar.t:>TI.t HVar.t),0)
      and subst_apply_ty subst ty =
        Substs.Ty.apply_no_renaming subst (ty,0)
      in
      let rec aux offset subst vars = match vars with
        | [] ->
          let renaming = E.Ctx.renaming_clear() in
          Sequence.return (Literals.apply_subst ~renaming subst (lits,0))
        | v :: vars' when subst_mem subst v ->
          (* ignore bound variables *)
          aux offset subst vars'
        | v :: vars' ->
          begin match Ind_ty.as_inductive_type (HVar.ty v) with
            | None when Type.equal (HVar.ty v) Type.prop ->
              (* try [true] and [false] *)
              Sequence.of_list [T.true_; T.false_]
              |> Sequence.flat_map
                (fun b ->
                   let subst = subst_add subst v b in
                   aux offset subst vars')
            | None -> aux offset subst vars' (* ignore [v] *)
            | Some ({ Ind_ty.ty_constructors; ty_vars; _ }, ind_ty_args) ->
              assert (List.length ty_vars = List.length ind_ty_args);
              let ind_ty_args' = List.map (subst_apply_ty subst) ind_ty_args in
              (* try to replace [v] by each constructor *)
              Sequence.of_list ty_constructors
              |> Sequence.flat_map
                (fun {Ind_ty.cstor_ty=c_ty; cstor_name=c_id} ->
                   let n, _, _ = Type.open_poly_fun c_ty in
                   assert (n = List.length ty_vars);
                   let c_ty_args, _ =
                     Type.apply c_ty ind_ty_args'
                     |> Type.open_fun
                   in
                   if last && c_ty_args <> []
                   then Sequence.empty (* fail *)
                   else (
                     (* fresh variables as arguments to the constructor *)
                     let sub_vars =
                       List.mapi
                         (fun i ty' -> HVar.make ~ty:ty' (i+offset) |> T.var)
                         c_ty_args
                     in
                     let t =
                       T.app_full
                         (T.const ~ty:c_ty c_id)
                         ind_ty_args'
                         sub_vars
                     in
                     let subst = subst_add subst v t in
                     aux (offset+List.length c_ty_args) subst vars'
                   )
                )
          end
      in
      let vars = Literals.vars lits in
      (* replace type variables by [prop], easy to test *)
      let ty_vars = List.filter (fun v -> Type.is_tType (HVar.ty v)) vars in
      let subst =
        List.fold_left
          (fun subst v -> subst_add_ty subst v Type.prop)
          Substs.empty
          ty_vars
      in
      (* offset to allocate new variables without collision *)
      let offset = 1 + T.Seq.max_var (Sequence.of_list vars) in
      aux offset subst vars
    in
    Util.debugf ~section 3 "@[<hv2>small_check lemma@ @[%a@]@]"
      (fun k->k (Util.pp_list Literals.pp_vars) lemma);
    Sequence.of_list lemma
    |> Sequence.flat_map (gen_instances ~last:false) (* depth 1 *)
    |> Sequence.flat_map (gen_instances ~last:false) (* depth 2 *)
    |> Sequence.flat_map (gen_instances ~last:true) (* close leaves *)
    |> Sequence.for_all
      (fun lits ->
         let c = C.create_a ~trail:Trail.empty lits ProofStep.mk_trivial in
         let ds, _ = E.all_simplify c in
         let res = not (List.exists C.is_empty ds) in
         Util.debugf ~section 5
           "@[<hv2>... small_check case@ @[%a@]@ simplified: (@[%a@])@ pass: %B@]"
           (fun k->k C.pp c (Util.pp_list C.pp) ds res);
         res
      )

  (* do only a few steps of inferences for checking if a candidate lemma
     is trivial/absurd *)
  let max_steps_ = 20

  exception Lemma_yields_false of C.t

  (* check that [lemma] is not obviously absurd or trivial, by making a few steps of
     superposition inferences between [lemma] and the Active Set.
     The strategy here is set of support: no inference between clauses of [lemma]
     and no inferences among active clauses, just between active clauses and
     those derived from [lemma]. Inferences with trails are dropped because
     the lemma should be inconditionally true. *)
  let check_not_absurd_or_trivial (lemma:lemma_candidate): bool =
    let q : C.t Queue.t = Queue.create() in (* clauses waiting *)
    let push_c c = Queue.push c q in
    let n : int ref = ref 0 in (* number of steps *)
    let trivial = ref true in
    List.iter
      (fun lits ->
         let c = C.create_a ~trail:Trail.empty lits ProofStep.mk_trivial in
         if not (E.is_trivial c) then push_c c)
      lemma;
    try
      while not (Queue.is_empty q) && !n < max_steps_ do
        incr n;
        let c = Queue.pop q in
        let c, _ = E.simplify c in
        assert (C.trail c |> Trail.is_empty);
        (* check for empty clause *)
        if C.is_empty c then raise (Lemma_yields_false c)
        else if E.is_trivial c then ()
        else (
          trivial := false; (* at least one clause does not simplify to [true] *)
          (* now make inferences with [c] and push non-trivial clauses to [q] *)
          E.generate c
          |> Sequence.filter_map
            (fun new_c ->
               let new_c, _ = E.simplify new_c in
               (* discard trivial/conditional clauses; scan for empty clauses *)
               if not (Trail.is_empty (C.trail new_c)) then None
               else if E.is_trivial new_c then None
               else if C.is_empty new_c then raise (Lemma_yields_false new_c)
               else Some new_c)
          |> Sequence.iter push_c
        )
      done;
      Util.debugf ~section 2
        "@[<2>lemma @[%a@]@ apparently not absurd (after %d steps; trivial:%B)@]"
        (fun k->k pp_lemma lemma !n !trivial);
      if !trivial then Util.incr_stat stats_guess_lemmas_trivial;
      not !trivial
    with Lemma_yields_false c ->
      assert (C.is_empty c);
      Util.debugf ~section 2
        "@[<2>lemma @[%a@] absurd:@ leads to empty clause %a (after %d steps)@]"
        (fun k->k pp_lemma lemma C.pp c !n);
      Util.incr_stat stats_guess_lemmas_absurd;
      false

  let check_not_already_tried (lemma:lemma_candidate): bool =
    let check_lits lits =
      SubsumptionIndex.retrieve_alpha_equiv_c !all_candidates_ lits
      |> Sequence.is_empty
    in
    let res = List.for_all check_lits lemma in
    if not res then (
      Util.debugf ~section 5 "@[lemma @[%a@]@ already tried@]" (fun k->k pp_lemma lemma)
    );
    res

  (* some checks that [l] should be considered as a lemma *)
  let is_acceptable_lemma_ l : bool =
    check_not_already_tried l &&
    small_check l &&
    check_not_absurd_or_trivial l

  let is_acceptable_lemma x = Util.with_prof prof_check_lemma is_acceptable_lemma_ x

  (* make an inference by generalizing on the list of variables [on] in
     the clause [c] *)
  let generalize ~(on:generalization) (c:C.t): lemma_candidate =
    (* fresh var generator, starting high enough *)
    let mk_fresh_var_ =
      let r = ref (C.Seq.vars c |> T.Seq.max_var |> succ) in
      fun ty ->
        let v = T.var_of_int ~ty !r in
        incr r;
        v
    in
    (* replace generalized terms by fresh variables *)
    let replacements : T.t T.Map.t =
      List.fold_left
        (fun m (set,ty) ->
           assert (not (T.Set.is_empty set));
           T.Set.to_seq set
           |> Sequence.map (fun t->t,mk_fresh_var_ ty)
           |> T.Map.add_seq m)
        T.Map.empty on
    in
    (* replace constants by variables in [c], then
        let [f] be [forall... bigAnd_{l in c} not l] *)
    let lemma =
      C.lits c
      |> Literals.map (fun t -> T.replace_m t replacements)
      |> Array.map (fun lit -> [| Literal.negate lit |])
      |> Array.to_list
    in
    Util.debugf ~section 2
      "@[<2>generalizing on %a@ in %a:@ returns @[<hv0>%a@]@]"
      (fun k->k pp_generalization on C.pp c pp_lemma lemma);
    lemma

  (* TODO: check conjecture by trying every substitutions of ind. variables
     with each cstor (fresh variable arguments), and simplifying *)

  (*
     NOTE: maybe, actually, NEVER do a generalization if there are
     clusters with several terms. First introduce lemmas to prove those
     terms are equal; if the lemma is proved, the clause will turn into
     one with singleton clusters (which will be generalizable!). So,
     actually, current process should distinguish between
     - generalization of subterms (accept singleton clusters only!)
     - candidate equality lemmas otherwise (for non-singleton clusters)
       to reduce to first case
  *)

  (* TODO:
     when a generalization [g] in [c] is obtained, with a cluster [t1, t2…]
     such that [t1] and [t2] :
     - are NOT toplevel terms of [c]
     - at least one is not a constant (but they share >= 1 cst)
     - look "simpler" than [not c]

     then we can also try the lemma [t1=t2], if it passes the filter.

     For instance, in [f (rev(rev(l))) != f(l)]
     we might want to prove [l=rev(rev(l))],
     because we might be able to prove it by induction.
  *)

  (* check if lemma is relevant/redundant/acceptable, and if yes
     then turn it into clauses *)
  let check_and_add_lemma (c:C.t)(lemma:lemma_candidate): C.t list =
    (* if [box f] already exists or is too deep, no need to re-do inference *)
    if is_acceptable_lemma lemma
    then (
      (* introduce cut now *)
      let proof = ProofStep.mk_trivial in
      let cut = A.introduce_cut lemma proof in
      A.add_lemma cut;
      let clauses = cut.A.cut_pos @ cut.A.cut_neg in
      List.iter (fun c -> C.set_flag SClause.flag_lemma c true) clauses;
      Util.incr_stat stats_guess_lemmas;
      Util.debugf ~section 2
        "@[<2>guessed lemma from %a:@ obtaining @[<hv0>%a@]@]"
        (fun k->k C.pp c (Util.pp_list C.pp) clauses);
      clauses
    )
    else []

  (* when a clause has inductive constants, take its negation
      and add it as a lemma (some restrictions apply) *)
  let inf_guess_lemma_ c =
    let terms = lazy (generalizable_subterms c) in
    if C.is_ground c
    && not (is_ind_conjecture_ c)
    && not (C.get_flag SClause.flag_lemma c)
    && not (has_pos_lit_ c) (* only positive lemmas, therefore C negative *)
    && (C.trail c |> Trail.exists BBox.is_inductive) (* in inductive branch *)
    && (Lazy.force terms <> [])
    then (
      let lazy terms = terms in
      (* each possible way to generalize a subterm occurring multiple times *)
      enumerate_generalizations c terms
      |> Sequence.map (fun terms' -> generalize c ~on:terms')
      |> Sequence.sort_uniq ~cmp:compare_lemma
      |> Sequence.flat_map_l (check_and_add_lemma c)
      |> Sequence.to_rev_list
    )
    else []

  let inf_guess_lemma c = Util.with_prof prof_guess_lemma inf_guess_lemma_ c
end

(** {2 Perform Induction} *)

module Make
(E : Env.S)
(A : Avatar_intf.S with module E = E)
= struct
  module Env = E
  module Ctx = E.Ctx
  module C = E.C
  module BoolBox = BBox
  module BoolLit = BoolBox.Lit

  (* scan clauses for ground terms of an inductive type,
     and declare those terms *)
  let scan_clause c : Cst_set.t =
    C.lits c
    |> Lits.Seq.terms
    |> scan_terms

  let is_eq_ ~path (t1:Ind_cst.cst) (t2:Ind_cst.case) ctxs =
    let p = Ind_cst.path_cons t1 t2 ctxs path in
    BoolBox.inject_case p

  (* TODO: rephrase this in the context of induction *)
  (* exhaustivity (inference):
    if some term [t : tau] is maximal in a clause, [tau] is inductive,
    and [t] was never split on, then introduce
    [t = c1(...) or t = c2(...) or ... or t = ck(...)] where the [ci] are
    constructors of [tau], and [...] are new Skolems of [t];
    if [t] is ground then Avatar splitting (with xor) should apply directly
      instead, as an optimization, with [k] unary clauses and 1 bool clause
  *)

  (* data required for asserting that a constant is the smallest one
     taht makes a conjunction of clause contexts true in the model *)
  type min_witness = {
    mw_cst: Ind_cst.cst;
      (* the constant *)
    mw_generalize_on: Ind_cst.cst list;
      (* list of other constants we can generalize in the strengthening.
         E.g. in [not p(a,b)], with [mw_cst=a], [mw_generalize_on=[b]],
         we obtain [not p(0,b)], [not p(s(a'),b)], [p(a',X)] where [b]
         was generalized *)
    mw_contexts: ClauseContext.t list;
      (* the conjunction of contexts for which [cst] is minimal
         (that is, in the model, any term smaller than [cst] makes at
         least one context false) *)
    mw_coverset : Ind_cst.cover_set;
      (* minimality should be asserted for each case of the coverset *)
    mw_path: Ind_cst.path;
      (* path leading to this *)
    mw_proof: ProofStep.t;
      (* proof for the result *)
    mw_trail: Trail.t;
      (* trail to carry *)
  }

  (* recover the (possibly empty) path from a boolean trail *)
  let path_of_trail trail : Ind_cst.path =
    Trail.to_seq trail
    |> Sequence.filter_map BBox.as_case
    |> Sequence.max ~lt:(fun a b -> Ind_cst.path_dominates b a)
    |> CCOpt.get Ind_cst.path_empty

  (* the rest of the trail *)
  let trail_rest trail : Trail.t =
    Trail.filter
      (fun lit -> match BBox.as_case lit with
         | None -> true
         | Some _ -> false)
      trail

  (* TODO: incremental strenghtening.
     - when expanding a coverset in clauses_of_min_witness, see if there
       are other constants in the path with same type, in which case
     strenghten! *)

  (* replace the constants by fresh variables *)
  let generalize_lits (lits:Lits.t) ~(generalize_on:Ind_cst.cst list) : Lits.t =
    if generalize_on=[] then lits
    else (
      let offset = (Lits.Seq.vars lits |> T.Seq.max_var) + 1 in
      (* (constant -> variable) list *)
      let pairs =
        List.mapi
          (fun i c ->
             let ty = Ind_cst.cst_ty c in
             let id = Ind_cst.cst_id c in
             T.const ~ty id, T.var (HVar.make ~ty (i+offset)))
          generalize_on
      in
      Util.debugf ~section 5 "@[<2>generalize_lits `@[%a@]`:@ subst (@[%a@])@]"
        (fun k->k Lits.pp lits CCFormat.(list (pair T.pp T.pp)) pairs);
      Lits.map
        (fun t ->
           List.fold_left
             (fun t (cst,var) -> T.replace ~old:cst ~by:var t)
             t pairs)
        lits
    )

  (* for each member [t] of the cover set:
     for each ctx in [mw.mw_contexts]:
      - add ctx[t] <- [cst=t]
      - for each [t' subterm t] of same type, add clause ~[ctx[t']] <- [cst=t]
    @param path the current induction branch
    @param trail precondition to this minimality
  *)
  let clauses_of_min_witness ~trail mw : (C.t list * BoolBox.t list list) =
    let b_lits = ref [] in
    let clauses =
      Ind_cst.cover_set_cases ~which:`All mw.mw_coverset
      |> Sequence.flat_map
        (fun (case:Ind_cst.case) ->
           let b_lit = is_eq_ mw.mw_cst case mw.mw_contexts ~path:mw.mw_path in
           CCList.Ref.push b_lits b_lit;
           (* clauses [ctx[case] <- b_lit] *)
           let pos_clauses =
             List.map
               (fun ctx ->
                  let t = Ind_cst.case_to_term case in
                  let lits = ClauseContext.apply ctx t in
                  C.create_a lits mw.mw_proof ~trail:(Trail.add b_lit mw.mw_trail))
               mw.mw_contexts
           in
           (* clauses [CNF(¬ And_i ctx_i[t']) <- b_lit] for
              each t' subterm of case, with generalization on other
              inductive constants *)
           let neg_clauses =
             Ind_cst.case_sub_constants case
             |> Sequence.filter_map
               (fun sub ->
                  (* only keep sub-constants that have the same type as [cst] *)
                  let sub = Ind_cst.cst_to_term sub in
                  let ty = Ind_cst.cst_ty mw.mw_cst in
                  if Type.equal (T.ty sub) ty
                  then Some sub else None)
             |> Sequence.flat_map
               (fun sub ->
                  (* for each context, apply it to [sub] and negate its
                     literals, obtaining a DNF of [¬ And_i ctx_i[t']];
                     then turn DNF into CNF *)
                  let clauses =
                    mw.mw_contexts
                    |> Util.map_product
                      ~f:(fun ctx ->
                         let lits = ClauseContext.apply ctx sub in
                         let lits = Array.map Literal.negate lits in
                         [Array.to_list lits])
                    |> List.map
                      (fun l ->
                         let lits =
                           Array.of_list l
                           |> generalize_lits ~generalize_on:mw.mw_generalize_on
                         in
                         C.create_a lits mw.mw_proof
                           ~trail:(Trail.add b_lit mw.mw_trail))
                  in
                  Sequence.of_list clauses)
            |> Sequence.to_rev_list
           in
           (* all new clauses *)
           let res = List.rev_append pos_clauses neg_clauses in
           Util.debugf ~section 2
             "@[<2>minimality of `%a`@ in case `%a` \
              @[generalize_on (@[%a@])@]:@ @[<hv>%a@]@]"
             (fun k-> k
                 Ind_cst.pp_cst mw.mw_cst T.pp (Ind_cst.case_to_term case)
                 (Util.pp_list Ind_cst.pp_cst) mw.mw_generalize_on
                 (Util.pp_list C.pp) res);
           Sequence.of_list res)
      |> Sequence.to_rev_list
    in
    (* boolean constraint(s) *)
    let b_clauses =
      (* trail => \Or b_lits *)
      let pre = trail |> Trail.to_list |> List.map BoolLit.neg in
      let post = !b_lits in
      [ pre @ post ]
    in
    clauses, b_clauses

  (* ensure the proper declarations are done for this constant *)
  let decl_cst_ cst =
    Util.debugf ~section 3 "@[<2>declare ind.cst. `%a`@]" (fun k->k Ind_cst.pp_cst cst);
    Ind_cst.declarations_of_cst cst
    |> Sequence.iter (fun (id,ty) -> Ctx.declare id ty)

  (* [cst] is the minimal term for which contexts [ctxs] holds, returns
     clauses expressing that, and assert boolean constraints *)
  let assert_min
      ~trail ~proof ~(generalize_on:Ind_cst.cst list) ctxs (cst:Ind_cst.cst) =
    let path = path_of_trail trail in
    let trail' = trail_rest trail in
    match Ind_cst.cst_cover_set cst with
      | Some set when not (Ind_cst.path_contains_cst path cst) ->
        decl_cst_ cst;
        let mw = {
          mw_cst=cst;
          mw_generalize_on=generalize_on;
          mw_contexts=ctxs;
          mw_coverset=set;
          mw_path=path;
          mw_proof=proof;
          mw_trail=trail';
        } in
        let clauses, b_clauses = clauses_of_min_witness ~trail mw in
        A.Solver.add_clauses ~proof b_clauses;
        Util.debugf ~section 2 "@[<2>add boolean constraints@ @[<hv>%a@]@ proof: %a@]"
          (fun k->k (Util.pp_list BBox.pp_bclause) b_clauses
              ProofPrint.pp_normal_step proof);
        Util.incr_stat stats_min;
        clauses
      | Some _ (* path already contains [cst] *)
      | None -> []  (* too deep for induction *)

  (* TODO: trail simplification that removes all path literals except
     the longest? *)

  (* checks whether the trail is trivial, that is:
     - contains two literals [i = t1] and [i = t2] with [t1], [t2]
        distinct cover set members, or
     - two literals [loop(i) minimal by a] and [loop(i) minimal by b], or
     - two literals [C in loop(i)], [D in loop(j)] if i,j do not depend
        on one another *)
  let trail_is_trivial trail =
    let seq = Trail.to_seq trail in
    (* all boolean literals that express paths *)
    let relevant_cases = Sequence.filter_map BoolBox.as_case seq in
    (* are there two distinct incompatible paths in the trail? *)
    Sequence.product relevant_cases relevant_cases
    |> Sequence.exists
      (fun (p1, p2) ->
         let res =
           not (Ind_cst.path_equal p1 p2) &&
           not (Ind_cst.path_dominates p1 p2) &&
           not (Ind_cst.path_dominates p2 p1)
         in
         if res
         then (
           Util.debugf ~section 4
             "@[<2>trail@ @[%a@]@ is trivial because of@ \
              {@[@[%a@],@,@[%a@]}@]@]"
             (fun k->k C.pp_trail trail Ind_cst.pp_path p1 Ind_cst.pp_path p2)
         );
         res)

  (* TODO: only do this when the clause already has some induction
     in its trail (must comes from lemma/goal) *)
  (* when a clause contains new inductive constants, assert minimality
     of the clause for all those constants independently *)
  let inf_assert_minimal c =
    let consts = scan_clause c in
    let proof =
      ProofStep.mk_inference [C.proof c] ~rule:(ProofStep.mk_rule "min")
    in
    let clauses =
      Cst_set.elements consts
      |> CCList.flat_map
        (fun cst ->
           decl_cst_ cst;
           let ctx = ClauseContext.extract_exn (C.lits c) (Ind_cst.cst_to_term cst) in
           (* no generalization, we have no idea whether [consts]
              originate from a universal quantification *)
           assert_min ~trail:(C.trail c) ~proof ~generalize_on:[] [ctx] cst)
    in
    clauses

  (* clauses when we do induction on [cst], generalizing the constants
     [generalize_on] *)
  let induction_on_
      ?(trail=Trail.empty) (clauses:C.t list) ~cst ~generalize_on : C.t list =
    decl_cst_ cst;
    Util.debugf ~section 1 "@[<2>perform induction on `%a`@ in `@[%a@]`@]"
      (fun k->k Ind_cst.pp_cst cst (Util.pp_list C.pp) clauses);
    (* extract a context from every clause, even those that do not contain [cst] *)
    let ctxs =
      List.map
        (fun c ->
           let sub = Ind_cst.cst_to_term cst in
           match ClauseContext.extract (C.lits c) sub with
             | Some ctx -> ctx
             | None -> ClauseContext.trivial (C.lits c) sub)
        clauses
    in
    (* proof: one step from all the clauses above *)
    let proof =
      ProofStep.mk_inference (List.map C.proof clauses)
        ~rule:(ProofStep.mk_rule "min")
    in
    assert_min ~trail ~proof ~generalize_on ctxs cst

  (* find inductive constants within the skolems *)
  let ind_consts_of_skolems (l:(ID.t*Type.t) list) : Ind_cst.cst list =
    l
    |> List.filter (CCFun.uncurry Ind_cst.is_potential_cst)
    |> List.map (CCFun.uncurry Ind_cst.cst_of_id)

  (* hook for converting some statements to clauses.
     It check if [Negated_goal l] contains inductive clauses, in which case
     it states their collective minimality.
     It also handles inductive Lemmas *)
  let convert_statement st =
    match Statement.view st with
    | Statement.NegatedGoal (skolems, _) ->
      (* find inductive constants *)
      begin match ind_consts_of_skolems skolems with
        | [] -> E.CR_skip
        | consts ->
          (* first, get "proper" clauses, with proofs *)
          let clauses = C.of_statement st in
          (* for each new inductive constant, assert minimality of
             this constant w.r.t the set of clauses that contain it *)
          consts
          |> CCList.flat_map
            (fun cst ->
               (* generalize on the other constants *)
               let generalize_on =
                 CCList.remove ~eq:Ind_cst.cst_equal ~x:cst consts
               in
               induction_on_ clauses ~cst ~generalize_on)
          |> E.cr_return (* do not add the clause itself *)
        end
    | _ -> E.cr_skip

  let new_lemmas_ : C.t list ref = ref []

  (* look whether, to prove the lemma, we need induction *)
  let on_lemma cut =
    (* find inductive constants within the skolems *)
    let consts = ind_consts_of_skolems cut.A.cut_skolems in
    begin match consts with
      | [] -> () (* regular lemma *)
      | consts ->
        (* add the condition that the lemma is false *)
        let trail = Trail.singleton (BoolLit.neg cut.A.cut_lit) in
        let l =
          CCList.flat_map
            (fun cst ->
               let generalize_on =
                 CCList.remove ~eq:Ind_cst.cst_equal ~x:cst consts
               in
               induction_on_ ~trail ~generalize_on cut.A.cut_neg ~cst)
            consts
        in
        Util.incr_stat stats_lemmas;
        new_lemmas_ := List.rev_append l !new_lemmas_;
    end

  let inf_new_lemmas ~full:_ () =
    let l = !new_lemmas_ in
    new_lemmas_ := [];
    l

  (** {2 Register} *)

  let register () =
    Util.debug ~section 2 "register induction";
    let d = Env.flex_get k_ind_depth in
    Util.debugf ~section 2 "maximum induction depth: %d" (fun k->k d);
    Ind_cst.max_depth_ := d;
    Env.add_unary_inf "induction.ind" inf_assert_minimal;
    Env.add_clause_conversion convert_statement;
    Env.add_is_trivial_trail trail_is_trivial;
    Signal.on_every A.on_lemma on_lemma;
    Env.add_generate "ind.lemmas" inf_new_lemmas;
    if Env.flex_get k_lemma_guess then (
      Util.debug ~section 2 "enable lemma guessing";
      let module G = Make_guess_lemma(E)(A) in
      Env.add_unary_inf "ind.guess_lemma" G.inf_guess_lemma;
    );
    (* declare new constants to [Ctx] *)
    Signal.on_every Ind_cst.on_new_cst decl_cst_;
    ()
end

(** {2 Options and Registration} *)

let enabled_ = ref true
let lemma_guess = ref true
let depth_ = ref !Ind_cst.max_depth_

(* if induction is enabled AND there are some inductive types,
   then perform some setup after typing, including setting the key
   [k_enable].
   It will update the parameters. *)
let post_typing_hook stmts state =
  let p = Flex_state.get_exn Params.key state in
  (* only enable if there are inductive types *)
  let should_enable =
    CCVector.exists
      (fun st -> match Statement.view st with
        | Statement.Data _ -> true
        | _ -> false)
      stmts
  in
  if !enabled_ && should_enable then (
    Util.debug ~section 1
      "Enable induction: requires ord=rpo6; select=NoSelection";
    let p = {
      p with Params.
      param_ord = "rpo6";
      param_select = "NoSelection";
    } in
    state
    |> Flex_state.add Params.key p
    |> Flex_state.add k_enable true
    |> Flex_state.add k_lemma_guess !lemma_guess
    |> Flex_state.add k_ind_depth !depth_
    |> Flex_state.add Ctx.Key.lost_completeness true
  ) else Flex_state.add k_enable false state

(* if enabled: register the main functor, with inference rules, etc. *)
let env_action (module E : Env.S) =
  let is_enabled = E.flex_get k_enable in
  if is_enabled then (
    let (module A) = Avatar.get_env (module E) in
    (* XXX here we do not use E anymore, because we do not know
       that A.E = E. Therefore, it is simpler to use A.E. *)
    let module E = A.E in
    E.Ctx.lost_completeness ();
    E.Ctx.set_selection_fun Selection.no_select;
    let module M = Make(A.E)(A) in
    M.register ();
  )

let extension =
  Extensions.(
    {default with
     name="induction_simple";
     post_typing_actions=[post_typing_hook];
     env_actions=[env_action];
    })

let () =
  Params.add_opts
    [ "--induction", Options.switch_set true enabled_, " enable induction"
    ; "--no-induction", Options.switch_set false enabled_, " disable induction"
    ; "--lemma-guess", Options.switch_set true lemma_guess, " enable lemma guess"
    ; "--no-lemma-guess", Options.switch_set false lemma_guess, " disable lemma guess"
    ; "--induction-depth", Arg.Set_int depth_, " maximum depth of nested induction"
    ]
