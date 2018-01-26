
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Induction through Cut} *)

open Logtk
open Libzipperposition

module Lits = Literals
module T = Term
module Ty = Type
module Fmt = CCFormat
module RW = Rewrite

module type S = Induction_intf.S

type term = T.t
type var = T.var
type clause = Literals.t
type form = clause list

let section = Util.Section.make ~parent:Const.section "induction"

let stat_lemmas = Util.mk_stat "induction.inductive_lemmas"
let stat_trivial_lemmas = Util.mk_stat "induction.trivial_lemmas"
let stat_absurd_lemmas = Util.mk_stat "induction.absurd_lemmas"
let stat_goal_duplicate = Util.mk_stat "induction.duplicate_goal"
let stat_inductions = Util.mk_stat "induction.inductions"
let stat_split_goal = Util.mk_stat "induction.split_goals"
let stat_generalize = Util.mk_stat "induction.generalize"
let stat_generalize_vars_active_pos = Util.mk_stat "induction.generalize_vars_active_pos"
let stat_generalize_terms_active_pos = Util.mk_stat "induction.generalize_terms_active_pos"
let stat_assess_goal = Util.mk_stat "induction.assess_goal_calls"
let stat_assess_goal_ok = Util.mk_stat "induction.assess_goal_ok"

let prof_check_goal = Util.mk_profiler "induction.check_goal"

let k_enable : bool Flex_state.key = Flex_state.create_key()
let k_ind_depth : int Flex_state.key = Flex_state.create_key()
let k_limit_to_active : bool Flex_state.key = Flex_state.create_key()
let k_coverset_depth : int Flex_state.key = Flex_state.create_key()
let k_goal_assess_limit : int Flex_state.key = Flex_state.create_key()
let k_ind_on_subcst : bool Flex_state.key = Flex_state.create_key()
let k_generalize_var : bool Flex_state.key = Flex_state.create_key()
let k_generalize_term : bool Flex_state.key = Flex_state.create_key()

(** {2 Formula to be Proved Inductively *)
module Make_goal(E : Env_intf.S) : sig
  type t

  val trivial : t
  (** trivial goal *)

  val of_form : form -> t
  val of_cut_form : Cut_form.t -> t

  val form : t -> Cut_form.t
  val cs : t -> Literals.t list
  val vars : t -> T.VarSet.t

  val ind_vars : t -> var list
  (** the inductive variables *)

  val simplify : t -> t
  (** Apply rewrite rules to the goal *)

  val split : t -> t list
  (** Split the goal into independent goals to be proved separately
      (if there is a conjunction of clauses that share no variables) *)

  val pp : t CCFormat.printer

  type status =
    | S_trivial
    | S_ok
    | S_falsifiable of Subst.t

  val test : t -> status
  (** Testing using {!Test_prop} *)

  val check_not_absurd_or_trivial : t -> bool
  (** More thorough testing *)

  val is_acceptable_goal : t -> bool

  val add_lemma : Cut_form.t -> unit
  (** Signal that this cut formula is an active lemma *)

  val has_been_tried : t -> bool
  (** Is the goal already converted into a lemma? *)
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

  (* trivial clause? *)
  let trivial_c (c:Literals.t): bool = Literals.is_trivial c

  let test_ (cs:Literals.t list): status =
    (* test and save *)
    if List.for_all trivial_c cs then S_trivial
    else begin match Test_prop.check_form cs with
      | Test_prop.R_ok -> S_ok
      | Test_prop.R_fail subst -> S_falsifiable subst
    end

  let of_cut_form (f:Cut_form.t): t =
    let test_res = lazy (Cut_form.cs f |> test_) in
    {cut=f; test_res}

  let of_form cs: t = of_cut_form (Cut_form.make cs)

  let form t = t.cut
  let cs t : form = Cut_form.cs t.cut
  let vars t = Cut_form.vars t.cut
  let test (t:t): status = Lazy.force t.test_res
  let ind_vars t = Cut_form.ind_vars t.cut

  let pp out (f:t): unit = Cut_form.pp out f.cut

  let simplify (g:t): t = form g |> Cut_form.normalize |> of_cut_form

  (* union-find for sets of clauses *)
  module UF_clauses =
    UnionFind.Make(struct
      type key = T.var
      type value = clause list
      let equal = HVar.equal Type.equal
      let hash = HVar.hash
      let zero = []
      let merge = List.rev_append
    end)

  let split (g:t): t list =
    let uf =
      vars g |> T.VarSet.to_list |> UF_clauses.create
    and ground_ = ref [] in
    List.iter
      (fun c ->
         let vars = Literals.vars c in
         List.iter
           (fun v -> UF_clauses.add uf v [c])
           vars;
         begin match vars with
           | [] ->
             (* ground clause is all by itself *)
             ground_ := [c] :: !ground_;
           | v::other_vars ->
             (* merge together these variables *)
             List.iter
               (fun v' -> UF_clauses.union uf v v')
               other_vars;
         end)
      (cs g);
    let all_clusters =
      (UF_clauses.to_seq uf |> Sequence.map snd |> Sequence.to_rev_list)
      @ !ground_
    in
    let new_goals = List.rev_map of_form all_clusters in
    if List.length new_goals > 1 then (
      Util.incr_stat stat_split_goal;
      Util.debugf ~section 3
        "(@[<2>split_goal@ :goal %a@ :new_goals (@[<hv>%a@])@])"
        (fun k->k pp g (Util.pp_list pp) new_goals);
    );
    new_goals

  module C = E.C

  let test_goal_is_ok (g:t): bool =
    begin match test g with
      | S_ok -> true
      | S_trivial ->
        Util.incr_stat stat_trivial_lemmas;
        Util.debugf ~section 2 "(@[<2>lemma_trivial@ @[%a@]@@])" (fun k->k pp g);
        false
      | S_falsifiable subst ->
        Util.debugf ~section 2
          "(@[<2>lemma_absurd@ @[%a@]@ :subst %a@])"
          (fun k->k pp g Subst.pp subst);
        Util.incr_stat stat_absurd_lemmas;
        false
    end

  exception Yield_false of C.t

  (* do only a few steps of inferences for checking if a candidate lemma
     is trivial/absurd *)
  let max_steps_ = E.flex_get k_goal_assess_limit

  (* TODO: if goal passes tests, can we use the demod/sup steps to infer active
     positions? (e.g. looking at which variables were substituted with
     cstor terms) *)

  (* check that [lemma] is not obviously absurd or trivial, by making a few steps of
     superposition inferences between [lemma] and the Active Set.
     The strategy here is set of support: no inference between clauses of [lemma]
     and no inferences among active clauses, just between active clauses and
     those derived from [lemma]. Inferences with trails are dropped because
     the lemma should be inconditionally true. *)
  let check_not_absurd_or_trivial_ (g:t): bool =
    Util.debugf ~section 2 "@[<2>@{<green>assess goal@}@ :goal %a@ :max-steps %d@]"
      (fun k->k pp g max_steps_);
    let module CQ = E.ProofState.CQueue in
    let q = CQ.almost_bfs () in (* clauses waiting *)
    let push_c c = CQ.add q c in
    let n : int ref = ref 0 in (* number of steps *)
    let trivial = ref true in
    try
      (* add goal's clauses to the local saturation set *)
      List.iter
        (fun lits ->
           let c = C.create_a ~trail:Trail.empty ~penalty:0 lits Proof.Step.trivial in
           let c, _ = E.unary_simplify c in
           if E.is_trivial c then ()
           else if C.is_empty c then raise (Yield_false c)
           else (
             trivial := false;
             push_c c
           ))
        (cs g);
      (* do a few steps of saturation *)
      while not (CQ.is_empty q) && !n < max_steps_ do
        incr n;
        let c = CQ.take_first q in
        let c, _ = E.unary_simplify c in
        assert (C.trail c |> Trail.is_empty);
        (* check for empty clause *)
        if C.comes_from_goal c then () (* ignore, a valid lemma might contradict goal *)
        else if C.is_empty c then raise (Yield_false c)
        else if E.is_trivial c then ()
        else (
          trivial := false; (* at least one clause does not simplify to [true] *)
          (* now make inferences with [c] and push non-trivial clauses to [q],
             if needed *)
          if !n + 2 < max_steps_ then (
            let new_c =
              Sequence.append
                (E.do_binary_inferences c)
                (E.do_unary_inferences c)
            in
            new_c
            |> Sequence.filter_map
              (fun new_c ->
                 let new_c, _ = E.unary_simplify new_c in
                 (* discard trivial/conditional clauses, or clauses coming
                    from goals (as they might be true lemmas but contradict
                    the negated goal, which makes them even more useful);
                    also scan for empty clauses *)
                 if C.comes_from_goal new_c then None
                 else if not (Trail.is_empty (C.trail new_c)) then None
                 else if E.is_trivial new_c then None
                 else if C.is_empty new_c then raise (Yield_false new_c)
                 else Some new_c)
            |> Sequence.iter push_c
          )
        )
      done;
      Util.debugf ~section 2
        "@[<2>lemma @[%a@]@ apparently not absurd (after %d steps; trivial:%B)@]"
        (fun k->k pp g !n !trivial);
      if !trivial then Util.incr_stat stat_trivial_lemmas;
      not !trivial
    with Yield_false c ->
      assert (C.is_empty c);
      Util.debugf ~section 2
        "@[<2>lemma @[%a@] absurd:@ leads to empty clause %a (after %d steps)@]"
        (fun k->k pp g C.pp c !n);
      Util.incr_stat stat_absurd_lemmas;
      false

  let check_not_absurd_or_trivial g =
    Util.with_prof prof_check_goal check_not_absurd_or_trivial_ g

  (* some checks that [g] should be considered as a goal *)
  let is_acceptable_goal (g:t) : bool =
    Util.incr_stat stat_assess_goal;
    let res =
      test_goal_is_ok g &&
      check_not_absurd_or_trivial g
    in
    if res then Util.incr_stat stat_assess_goal_ok;
    res

  module FV = Cut_form.FV_tbl(struct
      type t = unit
      let compare ()()=0
    end)

  let add_lemma, has_been_tried =
    let tbl = FV.create () in
    let add f = FV.add tbl f ()
    and mem (g:t) = FV.mem tbl (form g) in
    add, mem
end

module T_view : sig
  type 'a t =
    | T_var of T.var
    | T_db of int
    | T_app_defined of ID.t * Rewrite.Defined_cst.t * 'a list
    | T_app_cstor of ID.t * 'a list
    | T_app_unin of ID.t * 'a list
    | T_app of 'a * 'a list
    | T_fun of Type.t * 'a
    | T_builtin of Builtin.t * 'a list

  val view : term -> term t

  val active_subterms : term -> term Sequence.t
  (** Visit all active subterms in the given term.
      A subterm is active if it's under a cstor, uninterpreted symbol,
      builtin, or under a defined function at an active position *)
end = struct
  type 'a t =
    | T_var of T.var
    | T_db of int
    | T_app_defined of ID.t * Rewrite.Defined_cst.t * 'a list
    | T_app_cstor of ID.t * 'a list
    | T_app_unin of ID.t * 'a list
    | T_app of 'a * 'a list
    | T_fun of Type.t * 'a
    | T_builtin of Builtin.t * 'a list

  let view (t:term): term t = match T.view t with
    | T.AppBuiltin (b, l) -> T_builtin (b,l)
    | T.Var v -> T_var v
    | T.Const id when Ind_ty.is_constructor id -> T_app_cstor (id, [])
    | T.Const id when Classify_cst.id_is_defined id ->
      begin match RW.as_defined_cst id with
        | Some c -> T_app_defined (id, c, [])
        | None -> T_app_unin (id, [])
      end
    | T.Const id -> T_app_unin (id, [])
    | T.Fun (arg,bod) -> T_fun (arg,bod)
    | T.App (f, l) ->
      begin match T.view f with
        | T.Const id when Ind_ty.is_constructor id -> T_app_cstor (id, l)
        | T.Const id when Classify_cst.id_is_defined id ->
          begin match RW.as_defined_cst id with
            | Some c -> T_app_defined (id, c, l)
            | None -> T_app_unin (id, l)
          end
        | T.Const id -> T_app_unin (id, l)
        | _ -> T_app (f,l)
      end
    | T.DB i -> T_db i

  let active_subterms t yield: unit =
    let rec aux t =
      yield t;
      begin match view t with
        | T_app_defined (_, c, l) ->
          let pos = RW.Defined_cst.defined_positions c in
          assert (IArray.length pos >= List.length l);
          (* only look under active positions *)
          List.iteri
            (fun i sub ->
               if IArray.get pos i = Defined_pos.P_active then aux sub)
            l
        | T_fun (_,_) -> () (* no induction under λ, we follow WHNF semantics *)
        | T_var _ | T_db _ -> ()
        | T_app (f,l) ->
          aux f;
          List.iter aux l
        | T_app_cstor (_,l) -> List.iter aux l
        | T_builtin (_,l)
        | T_app_unin (_,l) -> List.iter aux l
      end
    in aux t
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
  module Goal = Make_goal(E)

  let max_depth = Env.flex_get k_ind_depth
  let cover_set_depth = Env.flex_get k_coverset_depth

  let is_ind_conjecture_ c =
    match C.distance_to_goal c with
      | Some (0 | 1) -> true
      | Some _
      | None -> false

  let has_pos_lit_ c = CCArray.exists Literal.is_pos (C.lits c)

  (* fresh var generator *)
  let fresh_var_gen_ (): Type.t -> T.t =
    let r = ref 0 in
    fun ty ->
      let v = T.var_of_int ~ty !r in
      incr r;
      v

  (* scan terms for inductive skolems. *)
  let scan_terms ~mode (seq:term Sequence.t) : Ind_cst.ind_skolem list =
    seq
    |> Sequence.flat_map Ind_cst.find_ind_skolems
    |> Sequence.filter
      (fun (id,_) ->
         begin match Ind_cst.id_as_cst id, mode with
           | None, _ -> true
           | Some _, `All -> true
           | Some c, `No_sub_cst ->
             (* do not generalize on sub-constants,
                there are induction hypothesis on them that we will need *)
             not (Ind_cst.is_sub c)
         end)
    |> Sequence.to_rev_list
    |> CCList.sort_uniq ~cmp:Ind_cst.ind_skolem_compare

  (* scan clauses for ground terms of an inductive type,
     and perform induction on these terms.
      @return a list of ways to generalize the given clause *)
  let scan_clause (c:C.t) : Ind_cst.ind_skolem list list =
    let l1 =
      if E.flex_get k_ind_on_subcst
      then C.lits c |> Lits.Seq.terms |> scan_terms ~mode:`All
      else []
    and l2 =
      C.lits c |> Lits.Seq.terms |> scan_terms ~mode:`No_sub_cst
    in
    (* remove duplicates, empty lists, etc. *)
    begin
      [l1; l2]
      |> CCList.sort_uniq ~cmp:(CCList.compare Ind_cst.ind_skolem_compare)
      |> List.filter (fun l -> not (CCList.is_empty l))
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

  (* induction on the given variables *)
  let ind_on_vars (cut:A.cut_res)(vars:T.var list): C.t list =
    assert (vars<>[]);
    let g = A.cut_form cut in
    let depth = A.cut_depth cut in
    let cut_blit = A.cut_lit cut in
    (* proof step *)
    let proof =
      let proof_parent = A.cut_proof_parent cut in
      let infos = UntypedAST.A.(
          [app "induction"
             (List.map (fun v -> quoted (HVar.to_string_tstp v)) vars)])
      in
      Proof.Step.inference [proof_parent]
        ~infos ~rule:(Proof.Rule.mk "induction") ~tags:[Proof.Tag.T_ind]
    in
    (* other variables -> become skolems *)
    let subst_skolems: Subst.t =
      Cut_form.vars g
      |> (fun set -> T.VarSet.diff set (T.VarSet.of_list vars))
      |> T.VarSet.to_list
      |> List.map
        (fun v ->
           let ty_v = HVar.ty v in
           let id = Ind_cst.make_skolem ty_v in
           Ctx.declare id ty_v;
           (v,0), (T.const ~ty:ty_v id,1))
      |> Subst.FO.of_list' ?init:None
    in
    (* make cover-sets for the variables, for the {b skolemized} type *)
    let c_sets =
      List.map
        (fun v ->
           let ty = Subst.Ty.apply Subst.Renaming.none subst_skolems (HVar.ty v,0) in
           v, Cover_set.make ~cover_set_depth ~depth ty)
        vars
    in
    List.iter (fun (_,set) -> decl_cst_of_set set) c_sets;
    Util.debugf ~section 2
      "(@[<hv2>ind_on_vars (@[%a@])@ :form %a@ :cover_sets (@[<hv>%a@])@ :subst_skolem %a@])"
      (fun k->k (Util.pp_list HVar.pp) vars Cut_form.pp g
          (Util.pp_list (Fmt.Dump.pair HVar.pp Cover_set.pp)) c_sets
          Subst.pp subst_skolems);
    (* set of boolean literal. We will add their exclusive disjonction to
       the SAT solver. *)
    let b_lits = ref [] in
    (* build clauses for the induction on [v] *)
    let clauses =
      Util.map_product c_sets
        ~f:(fun (v,set) ->
          Cover_set.cases ~which:`All set
          |> Sequence.to_list
          |> List.map (fun case -> [v,case]))
      |> CCList.flat_map
        (fun (cases:(T.var * Cover_set.case) list) ->
           assert (cases<>[]);
           (* literal for this case *)
           let b_lit_case = BBox.inject_case (List.map snd cases) in
           CCList.Ref.push b_lits b_lit_case;
           (* clauses [goal[v := t'] <- b_lit(case), ¬cut.blit]
              for every [t'] sub-constant of [case] *)
           let pos_clauses =
             Util.seq_map_l cases
               ~f:(fun (v,case) ->
                 Cover_set.Case.sub_constants case
                 |> CCList.filter_map
                   (fun sub_cst ->
                      (* only keep sub-constants that have the same type as [v] *)
                      if Type.equal (Ind_cst.ty sub_cst) (HVar.ty v) then (
                        let t = Ind_cst.to_term sub_cst in
                        Some (v,t)
                      ) else None))
             |> Sequence.flat_map_l
               (fun v_and_t_list ->
                  let subst =
                    v_and_t_list
                    |> List.map (fun (v,t) -> (v,0),(t,1))
                    |> Subst.FO.of_list' ?init:None
                  in
                  let renaming = Subst.Renaming.create () in
                  let g' = Cut_form.apply_subst renaming subst (g,0) in
                  Cut_form.cs g'
                  |> List.map
                    (fun lits ->
                       let trail =
                         [ b_lit_case;
                           BoolLit.neg cut_blit;
                         ] |> Trail.of_list
                       in
                       C.create_a lits proof ~trail ~penalty:0))
             |> Sequence.to_list
           in
           (* clauses [CNF[¬goal[case]) <- b_lit(case), ¬cut.blit] with
              other variables being replaced by skolem symbols *)
           let neg_clauses =
             let subst =
               cases
               |> List.map (fun (v,c) -> (v,0),(Cover_set.Case.to_term c,1))
               |> Subst.FO.of_list' ~init:subst_skolems
             in
             let renaming = Subst.Renaming.create () in
             (* for each clause, apply [subst] to it and negate its
                literals, obtaining a DNF of [¬ And_i ctx_i[case]];
                then turn DNF into CNF *)
             begin
               Cut_form.apply_subst renaming subst (g,0)
               |> Cut_form.cs
               |> Util.map_product
                 ~f:(fun lits ->
                   let lits = Array.map (fun l -> [Literal.negate l]) lits in
                   Array.to_list lits)
               |> CCList.map
                 (fun l ->
                    let lits = Array.of_list l in
                    let trail =
                      [ BoolLit.neg cut_blit;
                        b_lit_case;
                      ] |> Trail.of_list
                    in
                    C.create_a lits proof ~trail ~penalty:0)
             end
           in
           (* all new clauses *)
           let res = List.rev_append pos_clauses neg_clauses in
           Util.debugf ~section 2
             "(@[<2>induction on (@[%a@])@ :form %a@ @[<2>:cases (@[%a@])@]@ \
              :depth %d@ @[<2>:res [@[<hv>%a@]]@]@])"
             (fun k-> k (Util.pp_list HVar.pp) vars Cut_form.pp g
                 (Util.pp_list Fmt.(pair ~sep:(return ":=@ ") HVar.pp Cover_set.Case.pp)) cases
                 depth (Util.pp_list C.pp) res);
           res)
    in
    (* FIXME: should do CNF here, too *)
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
    Util.debugf ~section 2 "@[<2>add boolean constraints@ @[<hv>%a@]@ :proof %a@]"
      (fun k->k (Util.pp_list BBox.pp_bclause) b_clauses Proof.Step.pp proof);
    Util.incr_stat stat_inductions;
    (* return the clauses *)
    clauses

  type defined_path =
    | P_root
    | P_under_cstor
    | P_active
    | P_inactive

  let defined_path_add (p:defined_path)(pos:Defined_pos.t): defined_path =
    begin match p, pos with
      | (P_root | P_under_cstor | P_active), Defined_pos.P_active -> P_active
      | (P_root | P_under_cstor | P_active),
        (Defined_pos.P_accumulator | Defined_pos.P_invariant) -> P_inactive
      | P_inactive, _ -> P_inactive
    end

  let subterms_with_pos
      (f:Cut_form.t)
    : (defined_path * Position.t * term) Sequence.t =
    (* true if [x] occurs in active positions somewhere in [t] *)
    let rec aux (dp:defined_path) (p:Position.t) (t:term) : _ Sequence.t = fun k ->
      k (dp,p,t);
      begin match T_view.view t with
        | T_view.T_app_defined (_, c, l) ->
          let d_pos = RW.Defined_cst.defined_positions c in
          let len = IArray.length d_pos in
          assert (len >= List.length l);
          (* only look under active positions *)
          List.iteri
            (fun i u ->
               let d = IArray.get d_pos i in
               aux
                 (defined_path_add dp d)
                 Position.(append p @@ arg (len-i-1) @@ stop)
                 u k)
            l
        | T_view.T_var _ | T_view.T_db _ -> ()
        | T_view.T_app (_,l)
        | T_view.T_app_unin (_,l) (* approx, we assume all positions are active *)
        | T_view.T_builtin (_,l) ->
          let dp = defined_path_add dp Defined_pos.P_active in
          let len = List.length l in
          List.iteri
            (fun i u -> aux dp Position.(append p @@ arg (len-i-1) @@ stop) u k)
            l
        | T_view.T_fun (_,u) ->
          let dp = defined_path_add dp Defined_pos.P_invariant in
          aux dp Position.(append p @@ body stop) u k
        | T_view.T_app_cstor (_,l) ->
          let dp = match dp with
            | P_inactive -> P_inactive | _ -> P_under_cstor
          in
          let len = List.length l in
          List.iteri
            (fun i u -> aux dp Position.(append p @@ arg (len-i-1) @@ stop) u k)
            l
      end
    in
    Cut_form.Seq.terms_with_pos ~subterms:false f
    |> Sequence.flat_map
      (fun (t, pos) -> aux P_root pos t)

  let term_is_var x t: bool = match T.view t with
    | T.Var y -> HVar.equal Type.equal x y
    | _ -> false

  (* active occurrences of [x] in [f] *)
  let var_active_pos_seq (f:Cut_form.t)(x:T.var): _ Sequence.t =
    subterms_with_pos f
    |> Sequence.filter
      (function
        | P_active, _, t -> term_is_var x t
        | _ -> false)

  (* does the variable occur in an active position in [f],
     or under some uninterpreted position? *)
  let var_occurs_under_active_pos (f:Cut_form.t)(x:T.var): bool =
    not (Sequence.is_empty @@ var_active_pos_seq f x)

  let var_invariant_pos_seq f x: _ Sequence.t =
    subterms_with_pos f
    |> Sequence.filter
      (function
        | P_inactive, _, t -> term_is_var x t
        | _ -> false)

  (* does the variable occur in a position that is invariant? *)
  let var_occurs_under_invariant_pos (f:Cut_form.t)(x:T.var): bool =
    not (Sequence.is_empty @@ var_invariant_pos_seq f x)

  (* variable appears only naked, i.e. directly under [=] *)
  let var_always_naked (f:Cut_form.t)(x:T.var): bool =
    let check_t t = T.is_var t || not (T.var_occurs ~var:x t) in
    begin
      Cut_form.cs f
      |> Sequence.of_list
      |> Sequence.flat_map Sequence.of_array
      |> Sequence.for_all
        (function
          | Literal.Equation (l,r,_) ->
            let check_t t = T.is_var t || not (T.var_occurs ~var:x t) in
            check_t l && check_t r
          | Literal.Prop (t,_) -> check_t t
          | Literal.Int _ | Literal.Rat _ -> false
          | Literal.True | Literal.False -> true)
    end

  let active_subterms_form (f:Cut_form.t): T.t Sequence.t =
    Cut_form.cs f
    |> Sequence.of_list
    |> Sequence.flat_map Sequence.of_array
    |> Sequence.flat_map Literal.Seq.terms
    |> Sequence.flat_map T_view.active_subterms

  module Generalize : sig
    type form = Cut_form.t
    type generalization = form list
    type t = form -> generalization list

    val id : t
    (** Do nothing *)

    val vars_at_active_pos : t

    val terms_at_active_pos : t

    val all : t
  end = struct
    type form = Cut_form.t
    type generalization = form list
    type t = form -> generalization list

    let id _ = []

    (* generalize on variables that occur both (several times) in active
       positions, and which also occur (several times) in passive position.
       The idea is that induction on the variable would work in active
       positions, but applying induction hypothesis would fail because
       of the occurrences in passive positions.
       This should generalize [forall x. x + (x + x) = (x + x) + x]
       into [forall x y. y + (x + x) = (y + x) + x]
    *)
    let vars_at_active_pos (f:form): generalization list =
      let vars =
        Cut_form.vars f
        |> T.VarSet.to_list
        |> List.filter
          (fun v ->
             not (Type.is_tType (HVar.ty v)) &&
             (Sequence.length @@ var_active_pos_seq f v >= 2) &&
             (Sequence.length @@ var_invariant_pos_seq f v >= 2))
      in
      begin match vars with
        | [] -> []
        | _ ->
          (* build a map to replace active occurrences of these variables by
             fresh variables *)
          let m =
            let offset =
              Cut_form.vars f
              |> T.VarSet.to_seq
              |> Sequence.map HVar.id
              |> Sequence.max |> CCOpt.get_or ~default:0 |> succ
            in
            CCList.foldi
              (fun m i v ->
                 let v' = HVar.make ~ty:(HVar.ty v) (i+offset) in
                 subterms_with_pos f
                 |> Sequence.filter_map
                   (function
                     | P_active, pos, t when term_is_var v t -> Some (pos, T.var v')
                     | _ -> None)
                 |> Position.Map.add_seq m)
              Position.Map.empty vars
          in
          let f' = Cut_form.Pos.replace_many f m in
          Util.debugf ~section 5
            "(@[<2>candidate_generalize@ :of %a@ :gen_to %a@ \
             :by vars_active_pos :on (@[%a@])@ :map {@[%a@]}@])"
            (fun k->k Cut_form.pp f Cut_form.pp f' (Util.pp_list HVar.pp) vars
                (Position.Map.pp Position.pp Term.pp) m);
          if Goal.is_acceptable_goal @@ Goal.of_cut_form f'
          then (
            Util.incr_stat stat_generalize_vars_active_pos;
            [[f']]
          )
          else []
      end

    (* generalize non-variable subterms occurring several times
       at active positions *)
    let terms_at_active_pos (f:form): generalization list =
      let relevant_subterms =
        subterms_with_pos f
        |> Sequence.filter_map
          (function
            | P_active, pos, t ->
              begin match T_view.view t with
                | T_view.T_app_unin (id,[]) when Ind_cst.id_is_sub id ->
                  None (* probably there because there are induction hyp. on it *)
                | _ when Type.is_tType (T.ty t |> Type.returns) ->
                  None (* do not generalize on type or type constructors *)
                | (T_view.T_app_unin _ | T_view.T_app_defined _)
                  when T.is_ground t ->
                  Some (pos,t)
                | _ -> None
              end
            | _ -> None)
      in
      let subterms =
        relevant_subterms |> Sequence.map snd
        |> Sequence.group_by ~hash:T.hash ~eq:T.equal
        |> Sequence.filter_map
          (function
            | t :: _ :: _ -> Some t (* at least 2 *)
            | _ -> None)
        |> Sequence.to_rev_list
      in
      begin
        subterms
        |> CCList.filter_map
          (fun t ->
             (* introduce variable for [t] *)
             let v =
               Cut_form.vars f
               |> T.VarSet.to_seq
               |> Sequence.map HVar.id
               |> Sequence.max |> CCOpt.get_or ~default:0 |> succ
               |> HVar.make ~ty:(T.ty t)
             in
             let m =
               relevant_subterms
               |> Sequence.filter_map
                 (function
                   | pos, u when T.equal t u -> Some (pos, T.var v)
                   | _ -> None)
               |> Position.Map.of_seq
             in
             let f' = Cut_form.Pos.replace_many f m in
             Util.debugf ~section 4
               "(@[<2>candidate_generalize@ :of %a@ :gen_to %a@ \
                :by terms_active_pos@ :on %a@])"
               (fun k->k Cut_form.pp f Cut_form.pp f' T.pp t);
             if Goal.is_acceptable_goal @@ Goal.of_cut_form f'
             then (
               Util.incr_stat stat_generalize_terms_active_pos;
               Some [f']
             )
             else None)
      end

    let all =
      let g1 = if Env.flex_get k_generalize_var then vars_at_active_pos else id
      and g2 = if Env.flex_get k_generalize_term then terms_at_active_pos else id
      and (<++>) o (f,x) = match o with
        | [] -> f x
        | l -> l
      in
      fun f -> g1 f <++> (g2, f)
  end

  (* should we do induction on [x] in [c]? *)
  let should_do_ind_on_var (f:Cut_form.t) (x:T.var): bool =
    not (E.flex_get k_limit_to_active) ||
    var_occurs_under_active_pos f x ||
    var_always_naked f x

  module UF_vars =
    UnionFind.Make(struct
      type key = T.var
      type value = T.var list
      let equal = HVar.equal Type.equal
      let hash = HVar.hash
      let zero = []
      let merge = List.rev_append
    end)

  let eq_var = HVar.equal Type.equal

  (* group together variables that occur at active positions under
     the same subterm *)
  let find_var_clusters (f:Cut_form.t) (vars:T.var list): T.var list list =
    let uf = UF_vars.create [] in
    (* add all variables of [f] *)
    T.VarSet.iter (fun v -> UF_vars.add uf v [v]) (Cut_form.vars f);
    (* naked variables together *)
    begin match CCList.find_pred (var_always_naked f) vars with
      | None -> ()
      | Some v ->
        assert (UF_vars.mem uf v);
        List.iter
          (fun v' ->
             assert (UF_vars.mem uf v');
             if not (HVar.equal Type.equal v v') && var_always_naked f v' then (
               UF_vars.union uf v v';
             ))
          vars;
    end;
    (* group variables naked in same (dis)equations *)
    begin
      Cut_form.cs f
      |> Sequence.of_list
      |> Sequence.flat_map Sequence.of_array
      |> Sequence.iter
        (function
          | Literal.Equation (l,r,_) ->
            begin match T.view l, T.view r with
              | T.Var x, T.Var y -> UF_vars.union uf x y
              | _ -> ()
            end
          | _ -> ())
    end;
    (* other variables grouped by occurring at active pos in same subterm *)
    begin
      active_subterms_form f
      |> Sequence.iter
        (fun t -> match T_view.view t with
           | T_view.T_app_defined (_,c,l) ->
             let pos = RW.Defined_cst.defined_positions c in
             Sequence.of_list l
             |> Util.seq_zipi
             |> Sequence.diagonal
             |> Sequence.filter_map
               (fun ((i1,t1),(i2,t2)) ->
                  match T.as_var t1, T.as_var t2 with
                    | Some x, Some y
                      when
                        i1 < i2 &&
                        IArray.get pos i1 = Defined_pos.P_active &&
                        IArray.get pos i2 = Defined_pos.P_active &&
                        not (eq_var x y) &&
                        CCList.mem ~eq:eq_var x vars &&
                        CCList.mem ~eq:eq_var y vars
                      ->
                      Some (x,y)
                    | _ -> None)
             |> Sequence.iter
               (fun (x,y) ->
                  assert (not (eq_var x y));
                  UF_vars.union uf x y)
           | _ -> ())
    end;
    let res =
      UF_vars.to_seq uf
      |> Sequence.map snd
      |> Sequence.filter_map
        (fun vars ->
           (* eliminate non-inductive variables *)
           let vars =
             List.filter (fun v -> Ind_ty.is_inductive_type @@ HVar.ty v) vars
           in
           if vars=[] then None else Some vars)
      |> Sequence.to_rev_list
    in
    Util.debugf ~section 3
      "(@[<hv2>induction_clusters@ :in %a@ :clusters (@[<hv>%a@])@])"
      (fun k->k Cut_form.pp f
          (Util.pp_list Fmt.(within "{" "}" @@ hvbox @@ Util.pp_list HVar.pp))
          res);
    res

  (* proof by direct induction *)
  let prove_cut_by_ind (cut:A.cut_res): C.t list =
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
                   "(@[<hv>ind: inactive variable `%a`@ :in %a@])"
                   (fun k->k HVar.pp v Cut_form.pp g);
               );
               ok)
            ivars
        in
        let clusters = find_var_clusters g ivars in
        (* for each variable, build a coverset of its type,
           and do a case distinction on the [top] constant of this
           coverset. *)
        CCList.flat_map (ind_on_vars cut) clusters
    end

  let new_lemma_ =
    let n = ref 0 in
    fun () ->
      let s = Printf.sprintf "zip_lemma_%d" (CCRef.incr_then_get n) in
      UntypedAST.(A_app ("name", [A_quoted s]))

  (* prove any lemma that has inductive variables. First we try
     to generalize it, otherwise we prove it by induction *)
  let inductions_on_lemma (cut:A.cut_res): C.t list =
    let g = A.cut_form cut in
    Util.debugf ~section 4 "(@[<hv>prove_lemma_by_induction@ %a@])" (fun k->k Cut_form.pp g);
    begin match Generalize.all g with
      | [] ->
        prove_cut_by_ind cut
      | new_goals_l ->
        (* try each generalization in turn *)
        List.iter
          (fun new_goals ->
             assert (new_goals <> []);
             let g0 = g in
             let new_cuts =
               List.map
                 (fun g ->
                    A.introduce_cut ~depth:(A.cut_depth cut) g
                      (Proof.Step.lemma @@ Proof.Src.internal [new_lemma_()])
                      ~reason:Fmt.(fun out ()->
                          fprintf out "generalizing %a" Cut_form.pp g0))
                 new_goals
             in
             Util.debugf ~section 4
               "(@[<2>@{<Yellow>generalize@}@ :lemma %a@ :into (@[<hv>%a@])@])"
               (fun k->k Cut_form.pp g (Util.pp_list Cut_form.pp) new_goals);
             Util.incr_stat stat_generalize;
             (* assert that the new goals imply the old one *)
             let proof = Proof.Step.trivial in
             A.add_imply new_cuts cut proof;
             (* now prove the lemmas in Avatar *)
             List.iter A.add_lemma new_cuts)
          new_goals_l;
        []
    end

  (* replace the constants by fresh variables in the given clauses,
     returning a goal. *)
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
             assert (not (Type.is_tType @@ ty));
             T.const ~ty id, T.var (HVar.make ~ty (i+offset)))
          generalize_on
        |> T.Map.of_list
      in
      Util.debugf ~section 5
        "@[<hv2>generalize_lits@ :in `@[<hv>%a@]`@ :subst (@[%a@])@]"
        (fun k->k (Util.pp_list ~sep:"∧" Lits.pp) cs
            (T.Map.pp T.pp T.pp) pairs);
      (* replace skolems by the new variables, then negate the formula
         and re-CNF the negation.
         Purification constraints are kept as hypotheses in each resulting clause. *)
      begin
        cs
        |> Util.map_product
          ~f:(fun lits ->
            let lits_l = Array.to_list lits in
            (* separate the guard (constraints) from other literals *)
            let guard, other_lits =
              List.partition Literal.is_constraint lits_l
            in
            let replace_lits =
              List.map (Literal.map (fun t -> T.replace_m t pairs))
            in
            let guard = replace_lits guard in
            let other_lits = replace_lits other_lits in
            List.map
              (fun other_lit -> Literal.negate other_lit :: guard)
              other_lits)
        |> List.map Array.of_list
        |> Goal.of_form
      end
    )

  (* try to prove theses clauses by turning the given constants into
     variables, negating the clauses, adn introducing the result
     as a lemma to be proved by induction.

      @param generalize_on the set of (skolem) constants that are replaced
       by free variables in the negation of [clauses] *)
  let prove_by_ind (clauses:C.t list) ~ignore_depth ~generalize_on : unit =
    let pp_csts = Util.pp_list Fmt.(pair ~sep:(return ":@ ") ID.pp Type.pp) in
    (* remove trivial clauses *)
    let clauses =
      List.filter (fun c -> not @@ Literals.is_trivial @@ C.lits c) clauses
    in
    Util.debugf ~section 5
      "(@[<2>consider_proving_by_induction@ \
       :clauses [@[%a@]]@ :generalize_on (@[%a@])@]"
      (fun k->k (Util.pp_list C.pp) clauses pp_csts generalize_on);
    let depth =
      Sequence.of_list generalize_on
      |> Sequence.map (fun (id,_) -> Ind_cst.ind_skolem_depth id)
      |> Sequence.max
      |> CCOpt.get_or ~default:0
    (* the trail should not contain a positive "lemma" atom.
       We accept negative lemma atoms because the induction can be used to
       prove the lemma *)
    and no_pos_lemma_in_trail () =
      Sequence.of_list clauses
      |> Sequence.map C.trail
      |> Sequence.flat_map Trail.to_seq
      |> Sequence.for_all
        (fun lit -> not (BoolLit.sign lit && BBox.is_lemma lit))
    in
    if (ignore_depth || depth < max_depth) && no_pos_lemma_in_trail () then (
      let goal =
        generalize_clauses
          (List.map C.lits clauses)
          ~generalize_on
        |> Goal.simplify
      in
      let goals = Goal.split goal in
      List.iter
        (fun goal ->
           (* check if goal is worth the effort and if it's new *)
           if Goal.has_been_tried goal then (
             Util.debugf ~section 1
               "(@[<2>goal_already_active@ %a@])"
               (fun k->k Goal.pp goal);
             Util.incr_stat stat_goal_duplicate;
             ()
           ) else if Goal.is_acceptable_goal goal then (
             Util.debugf ~section 1
               "(@[<2>@{<green>prove_by_induction@}@ :clauses (@[%a@])@ :goal %a@])"
               (fun k->k (Util.pp_list C.pp) clauses Goal.pp goal);
             let proof = Proof.Step.lemma @@ Proof.Src.internal [new_lemma_()] in
             (* new lemma has same penalty as the clauses *)
             let penalty = List.fold_left (fun n c -> n+C.penalty c) 0 clauses in
             let cut =
               A.introduce_cut ~penalty ~depth (Goal.form goal) proof
                 ~reason:Fmt.(fun out () -> fprintf out
                       "(@[prove_ind@ :clauses (@[%a@])@ :on (@[%a@])@])"
                       (Util.pp_list C.pp) clauses pp_csts generalize_on)
             in
             A.add_lemma cut
           ))
        goals
    );
    ()

  (* Try to prove the given clause by introducing an inductive lemma. *)
  let inf_prove_by_ind (c:C.t): C.t list =
    List.iter
      (fun consts ->
         assert (consts<>[]);
         prove_by_ind [c] ~ignore_depth:false ~generalize_on:consts)
      (scan_clause c);
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
               replaced by variables. But first, simplify these clauses
               because otherwise the inductive subgoals
               (which are simplified) will not match
               the inductive hypothesis.

               NOTE: do not use {!all_simplify} as it interacts badly
               with avatar splitting. *)
            let clauses =
              C.of_statement st
              |> List.map (fun c -> fst (E.basic_simplify c))
            in
            prove_by_ind clauses ~ignore_depth:true ~generalize_on:consts;
            (* "skip" in any case, because the proof is done in a cut anyway *)
            E.CR_skip
        end
      | _ -> E.cr_skip
    end

  (* checks whether the trail is trivial, that is, it contains
     two literals [i = t1] and [i = t2] with [t1], [t2] distinct cover set cases *)
  let trail_is_trivial_cases trail =
    let seq = Trail.to_seq trail in
    (* all boolean literals that express paths *)
    let relevant_cases = Sequence.filter_map BoolBox.as_case seq in
    (* are there two distinct incompatible cases in the trail? *)
    Sequence.diagonal relevant_cases
    |> Sequence.exists
      (fun (c1, c2) ->
         let res =
           not (List.length c1 = List.length c2) ||
           not (CCList.equal Cover_set.Case.equal c1 c2)
         in
         if res then (
           Util.debugf ~section 4
             "(@[<2>trail@ @[%a@]@ is trivial because of@ \
              {@[(@[%a@]),@,(@[%a@])}@]@])"
             (fun k->k C.pp_trail trail
                 (Util.pp_list Cover_set.Case.pp) c1
                 (Util.pp_list Cover_set.Case.pp )c2)
         );
         res)

  (* make trails with several lemmas in them trivial, so that we have to wait
     for a lemma to be proved before we can  use it to prove another lemma *)
  let trail_is_trivial_lemmas trail =
    let seq = Trail.to_seq trail in
    (* all boolean literals that express paths *)
    let relevant_cases =
      seq
      |> Sequence.filter_map
        (fun lit ->
           BoolBox.as_lemma lit
           |> CCOpt.map (fun lemma -> lemma, BoolLit.sign lit))
    in
    (* are there two distinct positive lemma literals in the trail? *)
    Sequence.diagonal relevant_cases
    |> Sequence.exists
      (fun ((c1,sign1), (c2,sign2)) ->
         let res = sign1 && sign2 && not (Cut_form.equal c1 c2) in
         if res then (
           Util.debugf ~section 4
             "(@[<2>trail@ @[%a@]@ is trivial because of lemmas@ \
              {@[(@[%a@]),@,(@[%a@])}@]@])"
             (fun k->k C.pp_trail trail Cut_form.pp c1 Cut_form.pp c2);
         );
         res)

  (* look whether, to prove the lemma, we need induction *)
  let prove_lemma_by_ind cut =
    let l = inductions_on_lemma cut in
    if l<>[] then (
      Util.incr_stat stat_lemmas;
      E.CR_return l
    ) else E.CR_skip

  let register () =
    Util.debug ~section 2 "register induction";
    let d = Env.flex_get k_ind_depth in
    Util.debugf ~section 2 "maximum induction depth: %d" (fun k->k d);
    Env.add_unary_inf "induction.ind" inf_prove_by_ind;
    Env.add_clause_conversion convert_statement;
    Env.add_is_trivial_trail trail_is_trivial_cases;
    if E.flex_get Avatar.k_simplify_trail then (
      Env.add_is_trivial_trail trail_is_trivial_lemmas;
    );
    (* try to prove lemmas by induction *)
    A.add_prove_lemma prove_lemma_by_ind;
    ()
end

let enabled_ = ref true
let depth_ = ref 4 (* NOTE: should be 3? *)
let limit_to_active = ref true
let coverset_depth = ref 1
let goal_assess_limit = ref 8
let ind_sub_cst = ref true
let gen_var = ref true
let gen_term = ref true

(* if induction is enabled AND there are some inductive types,
   then perform some setup after typing, including setting the key
   [k_enable].
   It will update the parameters. *)
let post_typing_hook stmts state =
  (* only enable if there are inductive types *)
  let should_enable =
    CCVector.exists
      (fun st -> match Statement.view st with
         | Statement.Data _ -> true
         | _ -> false)
      stmts
  in
  if !enabled_ && should_enable then (
    Util.debug ~section 1 "Enable induction";
    state
    |> Flex_state.add k_enable true
    |> Flex_state.add k_ind_depth !depth_
    |> Flex_state.add k_limit_to_active !limit_to_active
    |> Flex_state.add k_coverset_depth !coverset_depth
    |> Flex_state.add k_goal_assess_limit !goal_assess_limit
    |> Flex_state.add k_ind_on_subcst !ind_sub_cst
    |> Flex_state.add k_generalize_var !gen_var
    |> Flex_state.add k_generalize_term !gen_term
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
    ; "--induction-depth", Arg.Set_int depth_, " maximum depth of nested induction"
    ; "--ind-only-active-pos", Arg.Set limit_to_active, " limit induction to active positions"
    ; "--no-ind-only-active-pos", Arg.Clear limit_to_active, " limit induction to active positions"
    ; "--ind-coverset-depth", Arg.Set_int coverset_depth, " coverset depth in induction"
    ; "--ind-goal-assess", Arg.Set_int goal_assess_limit, " number of steps for assessing potential lemmas"
    ; "--ind-sub-cst", Arg.Set ind_sub_cst, " do induction on sub-constants"
    ; "--no-ind-sub-cst", Arg.Clear ind_sub_cst, " do not do induction on sub-constants"
    ; "--ind-gen-var", Arg.Set gen_var, " generalize on variables"
    ; "--ind-gen-term", Arg.Set gen_term, " generalize on terms"
    ; "--no-ind-gen-var", Arg.Clear gen_var, " do not generalize on variables"
    ; "--no-ind-gen-term", Arg.Clear gen_term, " do not generalize on terms"
    ]
