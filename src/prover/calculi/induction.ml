
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Induction through Cut} *)

open Logtk

module Lits = Literals
module T = FOTerm
module Su = Substs
module Ty = Type
module Fmt = CCFormat
module RT = Rewrite_term

module type S = Induction_intf.S

type term = T.t
type var = T.var

let section = Util.Section.make ~parent:Const.section "induction"

let stats_lemmas = Util.mk_stat "induction.inductive_lemmas"
let stats_trivial_lemmas = Util.mk_stat "induction.trivial_lemmas"
let stats_absurd_lemmas = Util.mk_stat "induction.absurd_lemmas"
let stats_inductions = Util.mk_stat "induction.inductions"

let k_enable : bool Flex_state.key = Flex_state.create_key()
let k_ind_depth : int Flex_state.key = Flex_state.create_key()
let k_test_depth : int Flex_state.key = Flex_state.create_key()
let k_limit_to_active : bool Flex_state.key = Flex_state.create_key()

(** {2 Formula to be Proved Inductively *)
module Goal : sig
  type t

  val trivial : t
  (** trivial goal *)

  val make : Literals.t list -> t

  val form : t -> Cut_form.t
  val cs : t -> Literals.t list
  val vars : t -> T.VarSet.t

  val ind_vars : t -> var list
  (** the inductive variables *)

  type status =
    | S_trivial
    | S_ok
    | S_falsifiable of Subst.t

  val test : t -> status

  val pp : t CCFormat.printer
end = struct
  type status =
    | S_trivial
    | S_ok
    | S_falsifiable of Subst.t

  (* formula to be proved inductively. The clauses share some variables,
     they are not independent *)
  type t = {
    cut: Cut_form.t;
    test_res: status lazy_t;
  }

  let trivial: t =
    {cut=Cut_form.trivial; test_res=Lazy.from_val S_trivial}

  let test_ (cs:Literals.t list): status =
    (* test and save *)
    let form = List.map Literals.Conv.to_forms cs in
    begin match Test_prop.small_check form with
      | Test_prop.R_ok -> S_ok
      | Test_prop.R_fail subst -> S_falsifiable subst
    end

  let make cs: t =
    let cut = Cut_form.make cs in
    let test_res = lazy (test_ cs) in
    {cut; test_res}

  let form t = t.cut
  let cs t = Cut_form.cs t.cut
  let vars t = Cut_form.vars t.cut
  let test (t:t): status = Lazy.force t.test_res
  let ind_vars t = Cut_form.ind_vars t.cut

  let pp out (f:t): unit = Cut_form.pp out f.cut
end

(** {2 More Thorough Testing of Clauses} *)
module Goal_test(E : Env_intf.S) : sig
  type goal = Goal.t

  val check_not_absurd_or_trivial : goal -> bool

  val is_acceptable_goal : goal -> bool
end = struct
  module C = E.C

  type goal = Goal.t

  exception Yield_false of C.t

  (* check that [lemma] is not obviously absurd or trivial, by using
     the corresponding inference rules from env [E].

     We do not try to refute it with a few superposition steps because
     in cases it would work (lemma is wrong, attempted nonetheless,
     and refutable in a few steps), the lemma will be disproved and
     simplified away by saturation anyway.
  *)
  let check_not_absurd_or_trivial (g:goal): bool =
    Util.debugf ~section 2 "@[<2>@{<green>assess goal@}@ %a@]"
      (fun k->k Goal.pp g);
    let trivial = ref true in
    try
      (* fully simplify each goal's clause, check if absurd or trivial *)
      List.iter
        (fun lits ->
           let c = C.create_a ~trail:Trail.empty lits ProofStep.mk_trivial in
           let cs, _ = E.all_simplify c in
           begin match CCList.find_pred C.is_empty cs with
             | Some c_empty -> raise (Yield_false c_empty)
             | None ->
               if not (List.for_all E.is_trivial cs) then trivial := false
           end)
        (Goal.cs g);
      Util.debugf ~section 2
        "@[<2>lemma @[%a@]@ apparently not absurd (trivial:%B)@]"
        (fun k->k Goal.pp g !trivial);
      if !trivial then Util.incr_stat stats_trivial_lemmas;
      not !trivial
    with Yield_false c ->
      assert (C.is_empty c);
      Util.debugf ~section 2
        "@[<2>lemma @[%a@] absurd:@ leads to empty clause `%a`@]"
        (fun k->k Goal.pp g C.pp c);
      Util.incr_stat stats_absurd_lemmas;
      false

  (* some checks that [g] should be considered as a goal *)
  let is_acceptable_goal (g:goal) : bool =
    Goal.test g = Goal.S_ok &&
    check_not_absurd_or_trivial g
end

module T_view : sig
  type 'a t =
    | T_var of T.var
    | T_app_defined of ID.t * Rewrite_term.defined_cst * 'a list
    | T_app_cstor of ID.t * 'a list
    | T_app_unin of ID.t * 'a list
    | T_app of 'a * 'a list
    | T_builtin of Builtin.t * 'a list

  val view : term -> term t
end = struct
  type 'a t =
    | T_var of T.var
    | T_app_defined of ID.t * Rewrite_term.defined_cst * 'a list
    | T_app_cstor of ID.t * 'a list
    | T_app_unin of ID.t * 'a list
    | T_app of 'a * 'a list
    | T_builtin of Builtin.t * 'a list

  let view (t:term): term t = match T.view t with
    | T.AppBuiltin (b, l) -> T_builtin (b,l)
    | T.Var v -> T_var v
    | T.Const id when Ind_ty.is_constructor id -> T_app_cstor (id, [])
    | T.Const id when Classify_cst.id_is_defined id ->
      begin match RT.as_defined_cst id with
        | Some c -> T_app_defined (id, c, [])
        | None -> T_app_unin (id, [])
      end
    | T.Const id -> T_app_unin (id, [])
    | T.App (f, l) ->
      begin match T.view f with
        | T.Const id when Ind_ty.is_constructor id -> T_app_cstor (id, l)
        | T.Const id when Classify_cst.id_is_defined id ->
          begin match RT.as_defined_cst id with
            | Some c -> T_app_defined (id, c, l)
            | None -> T_app_unin (id, l)
          end
        | T.Const id -> T_app_unin (id, l)
        | _ -> T_app (f,l)
      end
    | T.DB _ -> assert false
end

(* data flow for induction:

   1) Introduce lemmas
     - some lemmas come directly from the input, and are directly
       asserted in Avatar
     - some other lemmas are "guessed" from regular clauses that
       contain inductive skolems. From these clauses (where we do not know
       what to do with these skolems in general), a lemma is
       built by negating the clauses and replacing the skolems by fresh
       variables.

       The goals obtained from this second source (given clauses)
       are pre-processed:
       - they are tested (see {!Test_prop}) to avoid trying to prove
         trivially false lemmas
       - they might be generalized using a collection of heuristics.
         Each generalization is also tested.

       Then, the surviving goals are added to Avatar using [A.introduce_cut].

   2) new lemmas from Avatar (coming from 1)) are checked for {b variables}
      with an inductive type.
      For each such variable satisfying some side condition (e.g. occurring
      at least once in active position), a fresh coverset of the variable's
      type is built, and fresh skolems are created for the other variables.

      Clauses of the goal (they share variables) are then instantiated
      to the ground with these skolems (and each case of the coverset)
      and then negated. We add a trail to them. The trail contains [not lemma]
        and the corresponding case literal.
      The resulting formulas are re-normalized  into CNF and added to
      the resulting set of clauses.
      In addition, for each recursive case of the coverset, induction
      hypothesis are added (instantiating the goal with this case,
      keeping the other variables identical).

    3) the resulting set of clauses
*)

(* TODO: strong induction? instead of using sub-constants of the case
   in the induction hypothesis, use a constraint [x < top] *)

(** {2 Calculus of Induction} *)
module Make
    (E : Env.S)
    (A : Avatar_intf.S with module E = E)
= struct
  module Env = E
  module Ctx = E.Ctx
  module C = E.C
  module BoolBox = BBox
  module BoolLit = BoolBox.Lit
  module Goal_test = Goal_test(E)

  let max_depth = Env.flex_get k_ind_depth

  let is_ind_conjecture_ c =
    match C.distance_to_goal c with
      | Some (0 | 1) -> true
      | Some _
      | None -> false

  let has_pos_lit_ c = CCArray.exists Literal.is_pos (C.lits c)

  (* TODO: remove or adapt (using notion of position of defined symbols) *)
  (* sub-terms of an inductive type, that occur several times (candidate
     for "subterm generalization" *)
  let generalizable_subterms c: term list =
    let count = T.Tbl.create 16 in
    C.Seq.terms c
    |> Sequence.flat_map T.Seq.subterms
    |> Sequence.filter
      (fun t -> Ind_ty.is_inductive_type (T.ty t) && not (T.is_const t))
    |> Sequence.iter
      (fun t ->
         let n = try T.Tbl.find count t with Not_found -> 0 in
         T.Tbl.replace count t (n+1));
    (* terms that occur more than once *)
    T.Tbl.to_seq count
    |> Sequence.filter_map (fun (t,n) -> if n>1 then Some t else None)
    |> Sequence.to_rev_list

  (* fresh var generator *)
  let fresh_var_gen_ (): Type.t -> T.t =
    let r = ref 0 in
    fun ty ->
      let v = T.var_of_int ~ty !r in
      incr r;
      v

  (* scan terms for inductive skolems. *)
  let scan_terms (seq:term Sequence.t) : Ind_cst.ind_skolem list =
    seq
    |> Sequence.flat_map Ind_cst.find_ind_skolems
    |> Sequence.to_rev_list
    |> CCList.sort_uniq ~cmp:Ind_cst.ind_skolem_compare

  (* scan clauses for ground terms of an inductive type,
     and perform induction on these terms.  *)
  let scan_clause (c:C.t) : Ind_cst.ind_skolem list =
    begin
      C.lits c
      |> Lits.Seq.terms
      |> scan_terms
    end

  (* goal for induction *)
  (* ensure the proper declarations are done for this coverset *)
  let decl_cst_of_set (set:Cover_set.t): unit =
    Util.debugf ~section 3
      "@[<2>declare coverset@ `%a`@]" (fun k->k Cover_set.pp set);
    begin
      Cover_set.declarations set
      |> Sequence.iter (fun (id,ty) -> Ctx.declare id ty)
    end

  (* induction on the given variable *)
  let ind_on_var (cut:A.cut_res)(v:T.var): C.t list =
    let g = A.cut_form cut in
    let ty = HVar.ty v in
    let depth = A.cut_depth cut in
    let cut_blit = A.cut_lit cut in
    (* proof step *)
    let proof =
      ProofStep.mk_inference (List.map C.proof (A.cut_pos cut))
        ~rule:(ProofStep.mk_rulef "induction(%a)" HVar.pp v)
    and c_set = Cover_set.make ~depth ty in
    decl_cst_of_set c_set;
    Util.debugf ~section 2
      "(@[ind_on_var `%a`@ :form %a@ cover_set %a@])"
      (fun k->k HVar.pp v Cut_form.pp g Cover_set.pp c_set);
    (* other variables -> become skolems *)
    let subst_skolems: Subst.t =
      Cut_form.vars g
      |> T.VarSet.remove v
      |> T.VarSet.to_list
      |> List.map
        (fun v ->
           let ty_v = HVar.ty v in
           let id = Ind_cst.make_skolem ty_v in
           Ctx.declare id ty_v;
           ((v:var:>InnerTerm.t HVar.t),0), (T.const ~ty:ty_v id,0))
      |> Subst.FO.of_list ?init:None
    in
    (* set of boolean literal. We will add their exclusive disjonction to
       the SAT solver. *)
    let b_lits = ref [] in
    (* build clauses for the induction on [v] *)
    let clauses =
      Cover_set.cases ~which:`All c_set
      |> Sequence.flat_map_l
        (fun (case:Cover_set.case) ->
           (* literal for this case *)
           let b_lit_case = BBox.inject_case case in
           CCList.Ref.push b_lits b_lit_case;
           (* clauses [goal[v := t'] <- b_lit(case), ¬cut.blit]
              for every [t'] sub-constant of [case] *)
           let pos_clauses =
             Cover_set.Case.sub_constants case
             |> CCList.filter_map
               (fun sub_cst ->
                  (* only keep sub-constants that have the same type as [cst] *)
                  if Type.equal (Ind_cst.ty sub_cst) ty
                  then (
                    let t = Ind_cst.to_term sub_cst in
                    Some t
                  ) else None)
             |> CCList.flat_map
               (fun t' ->
                  let g' = Cut_form.subst1 v t' g in
                  Cut_form.cs g'
                  |> List.map
                    (fun lits ->
                       let trail =
                         [ b_lit_case;
                           BoolLit.neg cut_blit;
                         ] |> Trail.of_list
                       in
                       C.create_a lits proof ~trail))
           in
           (* clauses [CNF[¬goal[case]) <- b_lit(case), ¬cut.blit] with
              other variables being replaced by skolem symbols *)
           let neg_clauses =
             let subst =
               Subst.FO.bind
                 subst_skolems
                 ((v:var:>InnerTerm.t HVar.t),0)
                 (Cover_set.Case.to_term case,0)
             in
             let renaming = Ctx.renaming_clear () in
             (* for each clause, apply [subst] to it and negate its
                literals, obtaining a DNF of [¬ And_i ctx_i[case]];
                then turn DNF into CNF *)
             begin
               Cut_form.apply_subst ~renaming subst (g,0)
               |> Cut_form.cs
               |> Util.map_product
                 ~f:(fun lits ->
                   let lits = Array.map Literal.negate lits in
                   [Array.to_list lits])
               |> List.map
                 (fun l ->
                    let lits = Array.of_list l in
                    let trail =
                      [ BoolLit.neg cut_blit;
                        b_lit_case;
                      ] |> Trail.of_list
                    in
                    C.create_a lits proof ~trail)
             end
           in
           (* all new clauses *)
           let res = List.rev_append pos_clauses neg_clauses in
           Util.debugf ~section 2
             "(@[<2>induction on `%a`@ :form %a@ @[<2>:case `%a`@]@ \
              @[<2>:res [@[<hv>%a@]]@]@])"
             (fun k-> k HVar.pp v Cut_form.pp g Cover_set.Case.pp case
                 (Util.pp_list C.pp) res);
           res)
      |> Sequence.to_rev_list
    in
    (* boolean constraint(s) *)
    let b_clauses =
      (* [\Or_{t in cases} b_lit(t)] *)
      let b_at_least_one = !b_lits
      (* for each case t!=u, [¬b_lit(t) ∨ ¬b_lit(u)] *)
      and b_at_most_one =
        CCList.diagonal !b_lits
        |> List.rev_map
          (fun (l1,l2) -> [BoolLit.neg l1; BoolLit.neg l2])
      in
      b_at_least_one :: b_at_most_one
    in
    A.Solver.add_clauses ~proof b_clauses;
    Util.debugf ~section 2 "@[<2>add boolean constraints@ @[<hv>%a@]@ proof: %a@]"
      (fun k->k (Util.pp_list BBox.pp_bclause) b_clauses
          ProofPrint.pp_normal_step proof);
    Util.incr_stat stats_inductions;
    (* return the clauses *)
    clauses

  (* does the variable occur in an active position in [f],
     or under some uninterpreted position? *)
  let occurs_in_active_pos (f:Cut_form.t)(x:T.var): bool =
    let open T_view in
    let is_x (t:term): bool = match T.view t with
      | T.Var y -> HVar.equal Type.equal x y
      | _ -> false
    in
    (* true if [x] occurs in active positions somewhere in [t] *)
    let rec check_sub(t:term): bool = match T_view.view t with
      | T_app_defined (_, c, l) ->
        let pos = RT.Defined_cst.defined_positions c in
        assert (IArray.length pos >= List.length l);
        (* only look under active positions *)
        begin
          Sequence.of_list l
          |> Sequence.zip_i |> Sequence.zip
          |> Sequence.exists
            (fun (i,u) ->
               IArray.get pos i = RT.Pos_active &&
               ( is_x u || check_sub u ))
        end
      | T_var _ -> false
      | T_app (f,l) -> check_sub f || List.exists check_eq_or_sub l
      | T_app_cstor (_,l) -> List.exists check_sub l
      | T_builtin (_,l)
      | T_app_unin (_,l) -> List.exists check_eq_or_sub l (* approx *)
    (* true if [t=x] or if [x] occurs in active positions somewhere in [t] *)
    and check_eq_or_sub (t:term): bool =
      is_x t || check_sub t
    in
    begin
      Cut_form.cs f
      |> Sequence.of_list
      |> Sequence.flat_map Sequence.of_array
      |> Sequence.flat_map Literal.Seq.terms
      |> Sequence.exists check_sub
    end

  (* should we do induction on [x] in [c]? *)
  let should_do_ind_on_var (f:Cut_form.t) (x:T.var): bool =
    not (E.flex_get k_limit_to_active) ||
    occurs_in_active_pos f x

  (* main inductive proof of lemmas that have inductive variables *)
  let prove_lemma (cut:A.cut_res): C.t list =
    let g = A.cut_form cut in
    begin match Cut_form.ind_vars g with
      | [] -> []
      | ivars ->
        (* filter on which variables we do induction *)
        let ivars =
          List.filter
            (fun v ->
               let ok = should_do_ind_on_var g v in
               if not ok then (
                 Util.debugf ~section 3
                   "(@[ind: inactive variable `%a`@ :in %a@])"
                   (fun k->k HVar.pp v Cut_form.pp g);
               );
               ok)
            ivars
        in
        (* for each variable, build a coverset of its type,
           and do a case distinction on the [top] constant of this
           coverset. *)
        CCList.flat_map (ind_on_var cut) ivars
    end

  (* replace the constants by fresh variables in the given clauses,
     returning a goal *)
  let generalize_clauses
      (cs:Lits.t list)
      ~(generalize_on:Ind_cst.ind_skolem list) : Goal.t =
    if generalize_on=[] then Goal.trivial
    else (
      (* offset to allocate new variables *)
      let offset =
        Sequence.of_list cs
        |> Sequence.flat_map Lits.Seq.vars
        |> T.Seq.max_var
        |> succ
      in
      (* (constant -> variable) list *)
      let pairs =
        List.mapi
          (fun i (id,ty) ->
             T.const ~ty id, T.var (HVar.make ~ty (i+offset)))
          generalize_on
        |> T.Map.of_list
      in
      Util.debugf ~section 5
        "@[<2>generalize_lits@ :in `@[<hv>%a@]`@ :subst (@[%a@])@]"
        (fun k->k (Util.pp_list ~sep:"∧" Lits.pp) cs
            (T.Map.pp T.pp T.pp) pairs);
      (* replace skolems by the new variables, then negate the formula
         and re-CNF the negation *)
      begin
        cs
        |> Util.map_product
          ~f:(fun lits ->
             lits
             |> Array.map
               (fun lit ->
                  lit
                  |> Literal.map (fun t -> T.replace_m t pairs)
                  |> Literal.negate
                  |> CCList.return)
             |> Array.to_list)
        |> List.map Array.of_list
        |> Goal.make
      end
    )

  (* try to prove theses clauses by turning the given constants into
     variables, negating the clauses, adn introducing the result
     as a lemma to be proved by induction *)
  let prove_by_ind (clauses:C.t list) ~generalize_on : unit =
    Util.debugf ~section 5 "(@[<2>consider_proving_by_induction@ :clauses [@[%a@]]@]"
      (fun k->k (Util.pp_list C.pp) clauses);
    let goal =
      generalize_clauses
        (List.map C.lits clauses)
        ~generalize_on
    in
    let depth =
      Sequence.of_list generalize_on
      |> Sequence.map (fun (id,_) -> Ind_cst.ind_skolem_depth id)
      |> Sequence.max
      |> CCOpt.get_or ~default:0
    in
    if depth <= max_depth then (
      (* check if goal is worth the effort *)
      if Goal_test.is_acceptable_goal goal then (
        Util.debugf ~section 1
          "(@[<2>@{<green>prove_by_induction@}@ :clauses [@[%a@]]@]"
          (fun k->k (Util.pp_list C.pp) clauses);
        let proof = ProofStep.mk_lemma in
        let cut = A.introduce_cut ~depth (Goal.form goal) proof in
        A.add_lemma cut
      );
    );
    ()

  (* Try to prove the given clause by introducing an inductive lemma. *)
  let inf_prove_by_ind (c:C.t): C.t list =
    let consts = scan_clause c in
    if consts<>[] then (
      prove_by_ind [c] ~generalize_on:consts;
    );
    []

  (* hook for converting some statements to clauses.
     It check if [Negated_goal l] contains clauses with inductive skolems,
     in which case it tries to prove these clauses by induction in a lemma.
  *)
  let convert_statement st =
    begin match Statement.view st with
      | Statement.NegatedGoal (skolems, _) ->
        (* find inductive skolems in there *)
        let ind_skolems =
          List.filter
            (fun (id,ty) -> Ind_cst.id_is_ind_skolem id ty)
            skolems
        in
        begin match ind_skolems with
          | [] -> E.CR_skip
          | consts ->
            (* introduce one lemma where all the skolems are
               replaced by variables *)
            let clauses = C.of_statement st in
            prove_by_ind clauses ~generalize_on:consts;
            (* "skip" in any case, because the proof is done in a cut anyway *)
            E.CR_skip
        end
      | _ -> E.cr_skip
    end

(* checks whether the trail is trivial, that is, it contains
   two literals [i = t1] and [i = t2] with [t1], [t2] distinct cover set cases *)
  let trail_is_trivial trail =
    let seq = Trail.to_seq trail in
    (* all boolean literals that express paths *)
    let relevant_cases = Sequence.filter_map BoolBox.as_case seq in
    (* are there two distinct incompatible cases in the trail? *)
    Sequence.product relevant_cases relevant_cases
    |> Sequence.exists
      (fun (c1, c2) ->
         let res =
           Cover_set.Case.same_cst c1 c2 &&
           not (Cover_set.Case.equal c1 c2)
         in
         if res then (
           Util.debugf ~section 4
             "(@[<2>trail@ @[%a@]@ is trivial because of@ \
              {@[@[%a@],@,@[%a@]}@]@])"
             (fun k->k C.pp_trail trail Cover_set.Case.pp c1 Cover_set.Case.pp c2)
         );
         res)

  let new_clauses_from_lemmas_ : C.t list ref = ref []

  (* look whether, to prove the lemma, we need induction *)
  let on_lemma cut =
    let l = prove_lemma cut in
    if l<>[] then (
      Util.incr_stat stats_lemmas;
      new_clauses_from_lemmas_ := List.rev_append l !new_clauses_from_lemmas_;
    )

  let inf_new_lemmas ~full:_ () =
    let l = !new_clauses_from_lemmas_ in
    new_clauses_from_lemmas_ := [];
    l

  let register () =
    Util.debug ~section 2 "register induction";
    let d = Env.flex_get k_ind_depth in
    Util.debugf ~section 2 "maximum induction depth: %d" (fun k->k d);
    Ind_cst.max_depth_ := d;
    Env.add_unary_inf "induction.ind" inf_prove_by_ind;
    Env.add_clause_conversion convert_statement;
    Env.add_is_trivial_trail trail_is_trivial;
    Signal.on_every A.on_lemma on_lemma;
    Env.add_generate "ind.lemmas" inf_new_lemmas;
    ()
end

let enabled_ = ref true
let depth_ = ref !Ind_cst.max_depth_
let test_depth = ref Test_prop.default_depth
let limit_to_active = ref true

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
    |> Flex_state.add k_ind_depth !depth_
    |> Flex_state.add k_test_depth !test_depth
    |> Flex_state.add k_limit_to_active !limit_to_active
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
    ; "--no-induction", Options.switch_set false enabled_, " enable induction"
    ; "--induction-depth", Arg.Set_int depth_, " maximum depth of nested induction"
    ; "--small-check-depth",
      Arg.Set_int test_depth,
      " set default depth limit for smallcheck"
    ; "--ind-only-active-pos", Arg.Set limit_to_active, " limit induction to active positions"
    ; "--no-ind-only-active-pos", Arg.Clear limit_to_active, " limit induction to active positions"
    ]
