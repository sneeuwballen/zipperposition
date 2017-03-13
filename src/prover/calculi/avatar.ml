
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Basic Splitting à la Avatar} *)

open Logtk

module T = FOTerm
module Lit = Literal
module Fmt = CCFormat

type 'a printer = Format.formatter -> 'a -> unit

let section = Util.Section.make ~parent:Const.section "avatar"
(** {2 Avatar} *)

let prof_splits = Util.mk_profiler "avatar.split"
let prof_check = Util.mk_profiler "avatar.check"

let stat_splits = Util.mk_stat "avatar.splits"
let stat_trail_trivial = Util.mk_stat "avatar.trivial_trail"
let stat_trail_simplify = Util.mk_stat "avatar.simplify_trail"
let stat_backward_simp_trail = Util.mk_stat "avatar.backward_simplify_trail"

(* annotate clauses that have been introduced by lemma *)
let flag_cut_introduced = SClause.new_flag()

module type S = Avatar_intf.S

let k_avatar : (module S) Flex_state.key = Flex_state.create_key ()
let k_show_lemmas : bool Flex_state.key = Flex_state.create_key()
let k_simplify_trail : bool Flex_state.key = Flex_state.create_key()

module Make(E : Env.S)(Sat : Sat_solver.S)
= struct
  module E = E
  module Ctx = E.Ctx
  module C = E.C
  module Solver = Sat
  module BLit = BBox.Lit

  (* union-find that maps vars to list of literals, used for splitting *)
  module UF =
    UnionFind.Make(struct
      type key = T.var
      type value = Lit.t list
      let equal = HVar.equal Type.equal
      let hash = HVar.hash
      let zero = []
      let merge = List.rev_append
    end)

  module LitSet = Sequence.Set.Make(Lit)

  let infer_split_ c =
    let lits = C.lits c in
    (* maps each variable to a list of literals. Sets can be merged whenever
       two variables occur in the same literal.  *)
    let uf_vars =
      C.Seq.vars c
      |> T.VarSet.of_seq
      |> T.VarSet.to_list
      |> UF.create
    (* set of ground literals (each one is its own component) *)
    and cluster_ground = ref LitSet.empty in

    (* literals belong to either their own ground component, or to every
        sets in [uf_vars] associated to their variables *)
    Array.iter
      (fun lit ->
         let v_opt = Lit.Seq.vars lit |> Sequence.head in
         match v_opt with
           | None -> (* ground, lit has its own component *)
             cluster_ground := LitSet.add lit !cluster_ground
           | Some v ->
             (* merge other variables of the literal with [v] *)
             Lit.Seq.vars lit
             |> Sequence.iter
               (fun v' ->
                  UF.add uf_vars v' [lit];  (* lit is in the equiv class of [v'] *)
                  UF.union uf_vars v v');
      ) lits;

    (* now gather all the components as a literal list list *)
    let components = ref [] in
    LitSet.iter (fun lit -> components := [lit] :: !components) !cluster_ground;
    UF.iter uf_vars (fun _ comp -> components := comp :: !components);

    match !components with
      | [] -> assert (Array.length lits=0); None
      | [_] -> None
      | _::_ ->
        (* do a simplification! *)
        Util.incr_stat stat_splits;
        let proof =
          ProofStep.mk_esa ~rule:(ProofStep.mk_rule "split") [C.proof c] in
        let clauses_and_names =
          List.map
            (fun lits ->
               let lits = Array.of_list lits in
               let bool_name = BBox.inject_lits lits in
               (* new trail: keep some literals of [C.trail c], add the new one *)
               let trail =
                 C.trail c
                 |> Trail.filter BBox.must_be_kept
                 |> Trail.add bool_name
               in
               let c = C.create_a ~trail lits proof in
               c, bool_name)
            !components
        in
        let clauses, bool_clause = List.split clauses_and_names in
        Util.debugf ~section 4 "@[split of @[%a@]@ yields @[%a@]@]"
          (fun k->k C.pp c (Util.pp_list C.pp) clauses);
        (* add boolean constraint: trail(c) => bigor_{name in clauses} name *)
        let bool_guard =
          C.trail c
          |> Trail.to_list
          |> List.map Trail.Lit.neg in
        let bool_clause = List.append bool_clause bool_guard in
        Sat.add_clause ~proof bool_clause;
        Util.debugf ~section 4 "@[constraint clause is @[%a@]@]"
          (fun k->k BBox.pp_bclause bool_clause);
        (* return the clauses *)
        Some clauses

  (* Avatar splitting *)
  let split c =
    Util.enter_prof prof_splits;
    let res = if Array.length (C.lits c) <= 1
      then None
      else infer_split_ c
    in
    Util.exit_prof prof_splits;
    res

  let filter_absurd_trails_ = ref (fun _ -> true)
  let filter_absurd_trails f = filter_absurd_trails_ := f

  (* if c.lits = [], negate c.trail *)
  let check_empty c =
    if Array.length (C.lits c) = 0 && !filter_absurd_trails_ (C.trail c)
    then (
      assert (not (Trail.is_empty (C.trail c)));
      let b_clause =
        C.trail c
        |> Trail.to_list
        |> List.map Trail.Lit.neg
      in
      Util.debugf ~section 4 "@[negate trail of @[%a@] (id %d)@ with @[%a@]@]"
        (fun k->k C.pp c (C.id c) BBox.pp_bclause b_clause);
      Sat.add_clause ~proof:(C.proof_step c) b_clause;
    );
    [] (* never infers anything! *)

  (* check whether the trail is false and will remain so *)
  let trail_is_trivial_ (trail:Trail.t): bool =
    let res =
      Trail.exists
        (fun lit ->
           try match Sat.valuation_level lit with
             | false, 0 -> true (* false at level 0: proven false *)
             | _ -> false
           with Sat.UndecidedLit -> false)
        trail
    in
    if res then (
      Util.incr_stat stat_trail_trivial;
      Util.debugf ~section 3 "@[<2>trail @[%a@] is trivial@]" (fun k->k C.pp_trail trail);
    );
    res

  let trail_is_trivial tr =
    Sat.last_result () = Sat_solver.Sat && trail_is_trivial_ tr

  type trail_status =
    | Tr_trivial
    | Tr_simplify_into of BLit.t list * BLit.t list (* kept, removed *)
    | Tr_same

  exception Trail_is_trivial

  (* return [new_trail], [is_trivial] *)
  let simplify_opt (trail:Trail.t): trail_status =
    let n_simpl = ref 0 in
    try
      let trail, removed =
        Trail.to_list trail
        |> List.partition
          (fun lit ->
             try match Sat.valuation_level lit with
               | true, 0 ->
                 (* [lit] is proven true, it is therefore not necessary
                    to depend on it *)
                 incr n_simpl;
                 false
               | false, 0 ->
                 (* [lit] is proven false, the whole trail is trivial *)
                 raise Trail_is_trivial
               | _ -> true
             with Sat.UndecidedLit -> true)
      in
      if !n_simpl > 0
      then (
        assert (removed<>[]);
        Tr_simplify_into (trail, removed)
      ) else Tr_same
    with Trail_is_trivial ->
      Tr_trivial

  (* simplify the trail of [c] using boolean literals that have been proven *)
  let simplify_trail_ c =
    let trail = C.trail c in
    (* remove bool literals made trivial by SAT solver *)
    begin match simplify_opt trail with
      | Tr_same
      | Tr_trivial -> SimplM.return_same c (* handled by [is_trivial] *)
      | Tr_simplify_into (new_trail, removed_trail) ->
        Util.incr_stat stat_trail_simplify;
        let new_trail = Trail.of_list new_trail in
        (* use SAT resolution proofs for tracking why the trail
           has been simplified, so that the other branches that have been
           closed can appear in the proof *)
        let proof_removed = List.map Sat.get_proof_of_lit removed_trail in
        let proof =
          ProofStep.mk_simp ~rule:(ProofStep.mk_rule "simpl_trail")
            (C.proof c :: proof_removed) in
        let c' = C.create_a ~trail:new_trail (C.lits c) proof in
        Util.debugf ~section 3
          "@[<2>clause @[%a@]@ trail-simplifies into @[%a@]@]"
          (fun k->k C.pp c C.pp c');
        SimplM.return_new c'
    end

  (* only simplify if SAT *)
  let simplify_trail c =
    if Sat.last_result () = Sat_solver.Sat
    then simplify_trail_ c
    else SimplM.return_same c

  (* subset of active clauses that have a trivial trail or simplifiable
     trail *)
  let backward_simplify_trails (_:C.t): C.ClauseSet.t =
    if Sat.last_result () = Sat_solver.Sat then (
      E.ProofState.ActiveSet.clauses ()
      |> C.ClauseSet.filter
        (fun c ->
           let ok = match simplify_opt (C.trail c) with
             | Tr_trivial | Tr_simplify_into _ -> true
             | Tr_same -> false
           in
           if ok then (
             Util.incr_stat stat_backward_simp_trail;
             Util.debugf ~section 5
               "(@[<2>backward_simplify_trail@ %a@])" (fun k->k C.pp c);
           );
           ok)
    ) else C.ClauseSet.empty

  let skolem_count_ = ref 0

  (* make a new skolem symbol *)
  let skolem_ ~ty =
    let id = ID.makef "_avatar_%d" !skolem_count_  in
    incr skolem_count_;
    ID.set_payload id
      (Skolem.Attr_skolem
         (if Ind_ty.is_inductive_type ty then Skolem.K_ind else Skolem.K_normal));
    Ctx.declare id ty;
    Ordering.add_list (Ctx.ord ()) [id];
    id

  type cut_res = {
    cut_form: Cut_form.t; (** the lemma itself *)
    cut_pos: E.C.t list; (** clauses true if lemma is true *)
    cut_lit: BLit.t; (** lit that is true if lemma is true *)
    cut_depth: int; (** if the lemma is used to prove another lemma *)
    cut_proof: ProofStep.t; (** where does the lemma come from? *)
  }

  let cut_form c = c.cut_form
  let cut_pos c = c.cut_pos
  let cut_lit c = c.cut_lit
  let cut_depth c = c.cut_depth
  let cut_proof c = c.cut_proof

  let pp_cut_res out (c:cut_res): unit =
    let pp_depth out d = if d>0  then Format.fprintf out "@ :depth %d" d in
    Format.fprintf out "(@[<hv>cut@ :form @[%a@]@ :lit @[%a@]%a)"
      (Util.pp_list E.C.pp) c.cut_pos
      BLit.pp c.cut_lit pp_depth c.cut_depth

  let cut_res_clauses c = Sequence.of_list c.cut_pos

  (* generic mechanism for adding clause(s)
     and make a lemma out of them, including Skolemization, etc. *)
  let introduce_cut ?(depth=0) (f:Cut_form.t) proof : cut_res =
    let box = BBox.inject_lemma f in
    (* positive clauses *)
    let c_pos =
      List.map
        (fun lits ->
           C.create_a ~trail:(Trail.singleton box) lits proof)
        (Cut_form.cs f)
    in
    { cut_form=f; cut_pos=c_pos; cut_lit=box;
      cut_depth=depth; cut_proof=proof; }

  let on_input_lemma : cut_res Signal.t = Signal.create ()
  let on_lemma : cut_res Signal.t = Signal.create()

  module Lemma_tbl = BBox.Lit.Tbl

  (* map from [cut.cut_lit] to [cut] *)
  let all_lemmas_ : cut_res Lemma_tbl.t = Lemma_tbl.create 64

  let add_lemma (c:cut_res): unit =
    if not (Lemma_tbl.mem all_lemmas_ c.cut_lit) then (
      Util.debugf ~section 2 "(@[<2>add_lemma@ :on `[@[<hv>%a@]]`@ :lit %a@])"
        (fun k->k Cut_form.pp c.cut_form BBox.pp c.cut_lit);
      Lemma_tbl.add all_lemmas_ c.cut_lit c;
      (* actually add the clauses to passive set *)
      E.ProofState.PassiveSet.add (cut_res_clauses c);
      Signal.send on_lemma c;
    ) else (
      (* already existing lemma *)
      Util.debugf ~section 3
        "(@[<2>add_lemma [already there]@ :on `[@[<hv>%a@]]`@])"
        (fun k->k Cut_form.pp c.cut_form);
    )

  let lemma_seq : cut_res Sequence.t =
    fun yield -> Lemma_tbl.iter (fun _ c -> yield c) all_lemmas_

  let print_lemmas out () =
    let pp_lemma out c =
      let status = match Sat.proved_at_0 c.cut_lit with
        | None -> "unknown"
        | Some true -> "proved"
        | Some false -> "refuted"
      in
      Format.fprintf out "@[<hv>@{<Green>*@} %s %a@]"
        status Cut_form.pp c.cut_form
    in
    Format.fprintf out "@[<hv2>lemmas: {@ %a@,@]}"
      (Util.pp_seq pp_lemma) lemma_seq;
    ()

  let show_lemmas () = Format.printf "%a@." print_lemmas ()

  let convert_lemma st = match Statement.view st with
    | Statement.Lemma l ->
      let proof_st = ProofStep.mk_goal (Statement.src st) in
      let f =
        l
        |> List.map (List.map Ctx.Lit.of_form)
        |> List.map Array.of_list
        |> Cut_form.make
      in
      let proof =
        Cut_form.cs f
        |> List.map (fun c -> ProofStep.mk_c proof_st (SClause.make ~trail:Trail.empty c))
        |> ProofStep.mk_inference ~rule:(ProofStep.mk_rule "lemma")
      in
      let cut = introduce_cut f proof in
      let all_clauses = cut_res_clauses cut |> Sequence.to_rev_list in
      add_lemma cut;
      Signal.send on_input_lemma cut;
      (* interrupt here *)
      E.cr_return all_clauses
    | _ -> E.cr_skip

  let before_check_sat = Signal.create()
  let after_check_sat = Signal.create()

  (* Just check the solver *)
  let check_satisfiability ~full () =
    Util.enter_prof prof_check;
    Signal.send before_check_sat ();
    let res = match Sat.check ~full ()  with
      | Sat_solver.Sat ->
        Util.debug ~section 3 "SAT-solver reports \"SAT\"";
        []
      | Sat_solver.Unsat proof ->
        Util.debug ~section 1 "SAT-solver reports \"UNSAT\"";
        let proof = ProofStep.step proof in
        let c = C.create ~trail:Trail.empty [] proof in
        [c]
    in
    Signal.send after_check_sat ();
    Util.exit_prof prof_check;
    res

  let register ~split:do_split () =
    Util.debugf ~section:Const.section 2 "register extension Avatar (split: %B)"
      (fun k->k do_split);
    Sat.set_printer BBox.pp;
    if do_split then (
      E.add_multi_simpl_rule split;
    );
    E.add_unary_inf "avatar_check_empty" check_empty;
    E.add_generate "avatar_check_sat" check_satisfiability;
    E.add_clause_conversion convert_lemma;
    E.add_is_trivial_trail trail_is_trivial;
    if E.flex_get k_simplify_trail then (
      E.add_backward_simplify backward_simplify_trails;
      E.add_simplify simplify_trail;
    );
    if E.flex_get k_show_lemmas then (
      Signal.once Signals.on_exit (fun _ -> show_lemmas ());
    );
    (* be sure there is an initial valuation *)
    ignore (Sat.check ~full:true ());
    ()
end

let get_env (module E : Env.S) : (module S) = E.flex_get k_avatar

let enabled_ = ref false
let show_lemmas_ = ref false
let simplify_trail_ = ref true

let extension =
  let action env =
    let module E = (val env : Env.S) in
    Util.debug 1 "create new SAT solver";
    let module Sat = Sat_solver.Make(struct end) in
    Sat.setup();
    let module A = Make(E)(Sat) in
    E.flex_add k_avatar (module A : S);
    E.flex_add k_show_lemmas !show_lemmas_;
    E.flex_add k_simplify_trail !simplify_trail_;
    Util.debug 1 "enable Avatar";
    A.register ~split:!enabled_ ()
  in
  Extensions.({default with name="avatar"; env_actions=[action]})

let () =
  Params.add_opts
    [ "--avatar", Arg.Set enabled_, " enable Avatar splitting"
    ; "--no-avatar", Arg.Clear enabled_, " disable Avatar splitting"
    ; "--print-lemmas", Arg.Set show_lemmas_, " show status of Avatar lemmas"
    ; "--avatar-simp-trail", Arg.Set simplify_trail_, " simplify boolean trails in Avatar"
    ; "--no-avatar-simp-trail", Arg.Clear simplify_trail_, " do not simplify boolean trails in Avatar"
    ]
