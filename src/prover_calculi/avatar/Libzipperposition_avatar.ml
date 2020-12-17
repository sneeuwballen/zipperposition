
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Basic Splitting à la Avatar} *)

open Logtk
open Libzipperposition

module T = Term
module Lit = Literal
module Fmt = CCFormat

module UnionFind = UnionFind

type 'a printer = Format.formatter -> 'a -> unit

let section = Util.Section.make ~parent:Const.section "avatar"
(** {2 Avatar} *)

let prof_splits = ZProf.make "avatar.split"
let prof_check = ZProf.make "avatar.check"
let prof_simp_trail = ZProf.make "avatar.simp_trail"

let stat_splits = Util.mk_stat "avatar.splits"
let stat_trail_trivial = Util.mk_stat "avatar.trivial_trail"
let stat_trail_simplify = Util.mk_stat "avatar.simplify_trail"
let stat_backward_simp_trail = Util.mk_stat "avatar.backward_simplify_trail"

(* annotate clauses that have been introduced by lemma *)
let flag_cut_introduced = SClause.new_flag()

module type S = Avatar_intf.S

let k_avatar : (module S) Flex_state.key = Flex_state.create_key ()
let k_avatar_enabled = Flex_state.create_key ()
let k_show_lemmas : bool Flex_state.key = Flex_state.create_key()
let k_simplify_trail : bool Flex_state.key = Flex_state.create_key()
let k_back_simplify_trail : bool Flex_state.key = Flex_state.create_key()
let k_abstract_known_singletons : bool Flex_state.key = Flex_state.create_key()
let k_split_only_initial : bool Flex_state.key = Flex_state.create_key()
let k_split_only_goals : bool Flex_state.key = Flex_state.create_key()
let k_split_only_ground : bool Flex_state.key = Flex_state.create_key()
let k_max_trail_size : int Flex_state.key = Flex_state.create_key()
let k_infer_from_components : bool Flex_state.key = Flex_state.create_key()


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
      type value = Lit.Set.t
      let equal = HVar.equal Type.equal
      let hash = HVar.hash
      let zero = Lit.Set.empty
      let merge = Lit.Set.union
    end)

  let simplify_split_ (c:C.t): C.t list option =
    let lits = C.lits c in
    (* maps each variable to a list of literals. Sets can be merged whenever
       two variables occur in the same literal.  *)
    let uf_vars =
      C.Seq.vars c
      |> T.VarSet.of_iter
      |> T.VarSet.to_list
      |> UF.create
    (* set of ground literals (each one is its own component) *)
    and cluster_ground = ref Lit.Set.empty in

    (* literals belong to either their own ground component, or to every
        sets in [uf_vars] associated to their variables *)
    Array.iter
      (fun lit ->
         let v_opt = Lit.Seq.vars lit |> Iter.head in
         begin match v_opt with
           | None -> (* ground, lit has its own component *)
             cluster_ground := Lit.Set.add lit !cluster_ground
           | Some v ->
             (* merge other variables of the literal with [v] *)
             Lit.Seq.vars lit
             |> Iter.iter
               (fun v' ->
                  (* lit is in the equiv class of [v'] *)
                  UF.add uf_vars v' (Lit.Set.singleton lit);
                  UF.union uf_vars v v');
         end)
      lits;

    (* now gather all the components as a literal list list *)
    let components = ref [] in
    Lit.Set.iter (fun lit -> components := [lit] :: !components) !cluster_ground;
    UF.iter uf_vars (fun _ comp -> components := Lit.Set.to_list comp :: !components);

    let proof =
      Proof.Step.esa
        [Proof.Parent.from @@ C.proof c] in
    let bool_guard =
      C.trail c
      |> Trail.to_list
      |> List.map Trail.Lit.neg in

    begin match !components with
      | [] -> assert (Array.length lits=0); None
      | [lits] ->
        if E.flex_get k_abstract_known_singletons then (
          let lits = Array.of_list lits in
          let bool_name = BBox.find_boolean_lit lits in
          CCOpt.iter (fun bool_lit -> 
              (* asserting Trail -> bool_name *)
              if List.for_all (fun bg -> 
                  BBox.Lit.equal (BBox.Lit.neg bg) bool_lit)
                  bool_guard then (
                (* ignoring tautoligies *)
                Sat.add_clause ~proof:(proof ~rule:(Proof.Rule.mk "recognize_known")) 
                  (bool_lit :: bool_guard));
            ) bool_name
        );
        None
      | _::_ ->
        (* do a simplification! *)
        Util.incr_stat stat_splits;

        (* elements of the trail to keep *)
        let keep_trail =
          C.trail c |> Trail.filter BBox.must_be_kept
        in
        let clauses_and_names =
          List.map
            (fun lits ->
               let lits = Array.of_list lits in
               let bool_name = BBox.inject_lits lits in
               Util.debugf ~section 5 "(@[<2>inject_lits@ :lits %a@ :blit %a@])"
                 (fun k->k Literals.pp lits BBox.pp bool_name);
               (* new trail: add the new one *)
               let trail = Trail.add bool_name keep_trail in
               let c = C.create_a ~trail ~penalty:(C.penalty c) lits (proof ~rule:(Proof.Rule.mk "split")) in
               c, bool_name)
            !components
        in
        let clauses, bool_clause = List.split clauses_and_names in
        Util.debugf ~section 2 "@[split of @[%a@]@ yields @[%a@]@]"
          (fun k->k C.pp c (Util.pp_list C.pp) clauses);
        (* add boolean constraint: trail(c) => bigor_{name in clauses} name *)

        let bool_clause = List.append bool_clause bool_guard in
        Sat.add_clause ~proof:(proof ~rule:(Proof.Rule.mk "split")) bool_clause;
        Util.debugf ~section 2 "@[constraint clause is @[%a@]@]"
          (fun k->k BBox.pp_bclause bool_clause);
        (* return the clauses *)
        Some clauses
    end

  (* Avatar splitting *)
  let split c =
    ZProf.enter_prof prof_splits;

    let should_split c = 
      (not @@ Literals.is_trivial (C.lits c)) &&
      (not @@ E.flex_get k_split_only_initial || C.proof_depth c == 0) &&
      (not @@ E.flex_get k_split_only_ground || C.is_ground c) &&
      (not @@ E.flex_get k_split_only_goals || 
       CCOpt.get_or ~default:(-1) (C.distance_to_goal c) >= 0) &&
      (not @@ E.flex_get k_abstract_known_singletons ||
       Array.length (C.lits c) != 0) &&
      (E.flex_get k_abstract_known_singletons ||
       Array.length (C.lits c) > 1) &&
      (E.flex_get k_max_trail_size < 0 || 
       Trail.length (C.trail c) <= E.flex_get k_max_trail_size) in

    let res = if (should_split c) then simplify_split_ c else None in
    
    (match res with 
    | None -> Util.debugf ~section 1 "Clause @[%a@] cannot be split@." (fun k -> k C.pp c);
    | Some res ->
      Util.debugf ~section 1 "Clause @[%a@] split into:@.@[%a@]@." 
        (fun k -> k C.pp c (CCList.pp C.pp) res));


    ZProf.exit_prof prof_splits;
    res

  let filter_absurd_trails_ = ref (fun _ -> true)
  let filter_absurd_trails f = filter_absurd_trails_ := f

  (* if c.lits = [], negate c.trail *)
  let check_empty c =
    (* trail can be empty if clause is simplified into empty clause *)
    if Array.length (C.lits c) = 0 
       && not (Trail.is_empty (C.trail c))
       && !filter_absurd_trails_ (C.trail c)
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
    if Array.length (C.lits c) = 0 &&
       E.flex_get k_infer_from_components && 
       Trail.length (C.trail c) = 1 then (
      let bool_lit = List.hd (Trail.to_list (C.trail c)) in
      let negate = if BBox.Lit.sign bool_lit then Lit.negate else CCFun.id in
      match BBox.as_lits bool_lit with 
      | Some lits ->
        let skolemizer = 
          Literals.vars lits
          |> List.map (fun v -> 
              (v, 0), (snd @@ Term.mk_fresh_skolem [] (HVar.ty v), 0))
          |> Subst.FO.of_list' in
        let lits' = 
          Literals.apply_subst Subst.Renaming.none skolemizer (lits,0)
          |> CCArray.to_list in
        let proof = 
          Proof.Step.inference ~rule:(Proof.Rule.mk "ground_avatar") [C.proof_parent c] in
        List.map (fun lit ->
            C.create ~penalty:(C.penalty c) ~trail:Trail.empty 
              [negate lit] proof 
          ) lits'
      | None -> []
    ) else [] (* never infers anything -- 
                 if ground empty avatar clauses 
                 are not added to the proof state. *)

  (* check whether the trail is false and will remain so *)
  let trail_is_trivial_ (trail:Trail.t): bool =
    let res =
      Trail.to_iter trail
      |> Iter.find_map
        (fun lit ->
           try match Sat.valuation_level lit with
             | false, 0 -> Some lit (* false at level 0: proven false *)
             | _ -> None
           with Sat.UndecidedLit -> None)
    in
    begin match res with
      | None -> false
      | Some lit ->
        Util.incr_stat stat_trail_trivial;
        Util.debugf ~section 3 "(@[<hv2>trivial_trail@ :trail @[<hv>%a@]@ :lit `%a`@]"
          (fun k->k C.pp_trail trail BBox.pp lit);
        true
    end

  let trail_is_trivial tr =
    Sat.last_result () = Sat_solver.Sat && trail_is_trivial_ tr

  type trail_status =
    | Tr_trivial
    | Tr_simplify_into of BLit.t list * BLit.t list (* kept, removed *)
    | Tr_same

  exception Trail_is_trivial

  (* return [new_trail], [is_trivial] *)
  let simplify_opt_ (trail:Trail.t): trail_status =
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

  let simplify_opt trail = ZProf.with_prof prof_simp_trail simplify_opt_ trail

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
        let proof_removed =
          List.map (CCFun.compose Sat.get_proof_of_lit Proof.Parent.from) removed_trail
        in
        let proof =
          Proof.Step.simp ~rule:(Proof.Rule.mk "simpl_trail")
            (Proof.Parent.from (C.proof c) :: proof_removed) in
        let c' =
          C.create_a ~trail:new_trail ~penalty:(C.penalty c)(C.lits c) proof
        in
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

  let new_proved_lits : unit -> bool =
    let num_proved_last_ = ref 0 in
    fun () ->
      let set = Sat.all_proved () in
      let n = BLit.Set.cardinal set in
      assert (n >= !num_proved_last_);
      let yes = n > !num_proved_last_ in
      num_proved_last_ := n;
      yes


  (* subset of active clauses that have a trivial trail or simplifiable
     trail *)
  let backward_simplify_trails (_:C.t): C.ClauseSet.t =
    if Sat.last_result () = Sat_solver.Sat && new_proved_lits () then (
      E.ProofState.ActiveSet.clauses ()
      |> C.ClauseSet.to_iter
      |> Iter.filter (fun c -> not (Trail.is_empty @@ C.trail c))
      |> Iter.filter
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
      |> C.ClauseSet.of_iter
    ) else C.ClauseSet.empty

  let skolem_count_ = ref 0

  type cut_res = {
    cut_form: Cut_form.t; (** the lemma itself *)
    cut_pos: E.C.t list; (** clauses true if lemma is true *)
    cut_lit: BLit.t; (** lit that is true if lemma is true *)
    cut_depth: int; (** if the lemma is used to prove another lemma *)
    cut_proof: Proof.Step.t; (** where does the lemma come from? *)
    cut_proof_parent: Proof.Parent.t; (** how to justify sth from the lemma *)
    cut_reason: unit CCFormat.printer option; (** Informal reason why the lemma was added *)
  }

  let cut_form c = c.cut_form
  let cut_pos c = c.cut_pos
  let cut_lit c = c.cut_lit
  let cut_depth c = c.cut_depth
  let cut_proof c = c.cut_proof
  let cut_proof_parent c = c.cut_proof_parent

  let pp_cut_res out (c:cut_res): unit =
    let pp_depth out d = if d>0  then Format.fprintf out "@ :depth %d" d in
    let pp_reason out r = Format.fprintf out "@ :reason @[%a@]" r () in
    Format.fprintf out "(@[<4>@[<hv>cut@ :form @[%a@]@ :lit @[%a@]%a]%a@])"
      (Util.pp_list E.C.pp) c.cut_pos
      BLit.pp c.cut_lit pp_depth c.cut_depth
      (Fmt.some pp_reason) c.cut_reason

  let cut_res_clauses c = Iter.of_list c.cut_pos

  (* generic mechanism for adding clause(s)
     and make a lemma out of them, including Skolemization, etc. *)
  let introduce_cut ?reason ?(penalty=1) ?(depth=0) (f:Cut_form.t) proof : cut_res =
    let box = BBox.inject_lemma f in
    let cut_proof_parent =
      let form = Cut_form.to_s_form f in
      let st =
        Statement.lemma ~proof:(Proof.Step.lemma @@ Proof.Src.internal[])
          [form]
      in
      Proof.Parent.from @@ Statement.as_proof_i st
    in
    (* positive clauses *)
    let proof_pos =
      Proof.Step.esa ~rule:(Proof.Rule.mk "cut") [cut_proof_parent]
    in
    let c_pos =
      List.map
        (fun lits ->
           C.create_a ~trail:(Trail.singleton box) ~penalty lits proof_pos)
        (Cut_form.cs f)
    in
    { cut_form=f; cut_pos=c_pos; cut_lit=box;
      cut_depth=depth; cut_proof=proof; cut_reason=reason;
      cut_proof_parent; }

  let on_input_lemma : cut_res Signal.t = Signal.create ()
  let on_lemma : cut_res Signal.t = Signal.create()

  module Lemma_tbl = BBox.Lit.Tbl

  (* map from [cut.cut_lit] to [cut] *)
  let all_lemmas_ : cut_res Lemma_tbl.t = Lemma_tbl.create 64

  let prove_lemma_handlers_ : (cut_res -> C.t list E.conversion_result) list ref = ref []

  let add_prove_lemma x = CCList.Ref.push prove_lemma_handlers_ x

  (* clauses recently pushed while trying to prove lemmas *)
  let new_clauses_from_lemmas_ : C.t list ref = ref []

  (* return the list of new lemmas *)
  let inf_new_lemmas ~full:_ () =
    let l = !new_clauses_from_lemmas_ in
    new_clauses_from_lemmas_ := [];
    l

  (* try to prove a lemma, by trying handlers one by one, or just skolemizing *)
  let prove_lemma (c:cut_res): unit =
    let default () =
      let g = cut_form c in
      (* proof step *)
      let proof =
        Proof.Step.esa [cut_proof_parent c] ~rule:(Proof.Rule.mk "cut")
      in
      let vars = Cut_form.vars g |> T.VarSet.to_list in
      Util.debugf ~section 2
        "(@[<hv2>prove_lemma@ :form %a@ :vars (@[%a@])@])"
        (fun k->k Cut_form.pp g (Util.pp_list HVar.pp) vars);
      (* map variables to skolems *)
      let subst : Subst.t =
        vars
        |> List.map
          (fun v ->
             let ty_v = HVar.ty v in
             let id = Ind_cst.make_skolem ty_v in
             Ctx.declare id ty_v;
             (v,0), (T.const ~ty:ty_v id,0))
        |> Subst.FO.of_list' ?init:None
      in
      (* for each clause, apply [subst] to it and negate its
          literals, obtaining a DNF of [¬ And_i ctx_i[case]];
          then turn DNF into CNF *)
      let renaming = Subst.Renaming.create () in
      let clauses =
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
               let trail = Trail.singleton (BLit.neg @@ cut_lit c) in
               C.create_a lits proof ~trail ~penalty:1)
        end
      in
      clauses
    in
    let rec aux acc = function
      | [] -> List.rev_append (default()) acc
      | proof_handler :: tail ->
        begin match proof_handler c with
          | E.CR_drop
          | E.CR_skip -> aux acc tail
          | E.CR_return cs -> List.rev_append cs acc
          | E.CR_add cs -> aux (List.rev_append cs acc) tail
        end
    in
    (* add proof clauses to the positive clauses *)
    let cs = aux (cut_pos c) !prove_lemma_handlers_ in
    Util.debugf ~section 3
      "(@[prove_lemma@ :lemma %a@ :clauses (@[<hv>%a@])@])"
      (fun k->k Cut_form.pp (cut_form c) (Util.pp_list C.pp) cs);
    CCList.Ref.push_list new_clauses_from_lemmas_ cs

  let add_lemma (c:cut_res): unit =
    if not (Lemma_tbl.mem all_lemmas_ c.cut_lit) then (
      Util.debugf ~section 2 "(@[<2>add_lemma@ :on `[@[<hv>%a@]]`@ :lit %a@])"
        (fun k->k Cut_form.pp c.cut_form BBox.pp c.cut_lit);
      Lemma_tbl.add all_lemmas_ c.cut_lit c;
      (* start a subproof for the lemma *)
      prove_lemma c;
      Signal.send on_lemma c;
    ) else (
      (* already existing lemma *)
      Util.debugf ~section 3
        "(@[<2>add_lemma [already there]@ :on `[@[<hv>%a@]]`@])"
        (fun k->k Cut_form.pp c.cut_form);
    )

  let add_imply (l:cut_res list) (res:cut_res) (p:Proof.Step.t): unit =
    let c = res.cut_lit :: List.map (fun cut -> BLit.neg cut.cut_lit) l in
    Util.debugf ~section 3
      "(@[<2>add_imply@ :premises (@[<hv>%a@])@ :concl %a@ :proof %a@])"
      (fun k->k (Util.pp_list pp_cut_res) l pp_cut_res res Proof.Step.pp p);
    Solver.add_clause ~proof:p c;
    ()


  let lemma_seq : cut_res Iter.t =
    fun yield -> Lemma_tbl.iter (fun _ c -> yield c) all_lemmas_

  (* is this literal involved in the proof? *)
  let rec in_proof_of_ (p:Proof.t) (lit:BLit.t): bool =
    let eq_abs l1 l2 = BLit.equal (BLit.abs l1) (BLit.abs l2) in
    let in_proof_ (p:Proof.Step.t) (lit:BLit.t): bool =
      List.exists (fun parent -> in_proof_of_ (Proof.Parent.proof parent) lit) (Proof.Step.parents p)
    in
    begin match Proof.S.result p with
      | Proof.Res (_, Bool_clause.E_proof l) ->
        List.exists (eq_abs lit) l || in_proof_ (Proof.S.step p) lit
      | _ -> in_proof_ (Proof.S.step p) lit
    end

  let print_lemmas out () =
    let in_core = match Sat.get_proof_opt () with
      | None -> (fun _ -> false)
      | Some p -> in_proof_of_ p
    and pp_reason out r = Format.fprintf out "@ :reason @[%a@]" r () in
    let pp_lemma out c =
      let status = match Sat.proved_at_0 c.cut_lit with
        | _ when in_core c.cut_lit -> "in_proof"
        | None -> "unknown"
        | Some true -> "proved"
        | Some false -> "refuted"
      in
      Format.fprintf out "@[<4>@[<hv>@{<Green>*@} %s %a@]%a@]"
        status Cut_form.pp c.cut_form (Fmt.some pp_reason) c.cut_reason
    in
    let l = lemma_seq |> Iter.to_rev_list in
    Format.fprintf out "@[<v2>lemmas (%d): {@ %a@,@]}"
      (List.length l) (Util.pp_list ~sep:"" pp_lemma) l;
    ()

  let show_lemmas () = Format.printf "%a@." print_lemmas ()

  let convert_lemma st = match Statement.view st with
    | Statement.Lemma l ->
      let proof_st = Statement.proof_step st in
      let f =
        l
        |> List.map (List.map Ctx.Lit.of_form)
        |> List.map Array.of_list
        |> Cut_form.make
      in
      let proof =
        Cut_form.cs f
        |> List.map
          (fun c ->
             Proof.Parent.from @@ Proof.S.mk proof_st @@
             SClause.mk_proof_res @@ SClause.make ~trail:Trail.empty c)
        |> Proof.Step.esa ~rule:(Proof.Rule.mk "lemma")
      in
      let cut = introduce_cut ~reason:Fmt.(return "in-input") f proof in
      let all_clauses = cut_res_clauses cut |> Iter.to_rev_list in
      add_lemma cut;
      Signal.send on_input_lemma cut;
      (* interrupt here *)
      E.cr_return all_clauses
    | _ -> E.cr_skip

  let before_check_sat = Signal.create()
  let after_check_sat = Signal.create()

  (* Just check the solver *)
  let check_satisfiability ~full () =
    ZProf.enter_prof prof_check;
    Signal.send before_check_sat ();
    let res = match Sat.check ~full ()  with
      | Sat_solver.Sat ->
        Util.debug ~section 3 "SAT-solver reports \"SAT\"";
        []
      | Sat_solver.Unsat proof ->
        Util.debug ~section 1 "SAT-solver reports \"UNSAT\"";
        let proof = Proof.S.step proof in
        let c = C.create ~trail:Trail.empty ~penalty:1 [] proof in
        [c]
    in
    Signal.send after_check_sat ();
    ZProf.exit_prof prof_check;
    res

  let register ~split_kind () =
    let split_to_str = function
      | `Lazy -> "lazy"
      | `Eager -> "eager"
      | `Off -> "off" in

    Util.debugf ~section:Const.section 2 "register extension Avatar (split: %s)"
      (fun k->k (split_to_str split_kind));
    Sat.set_printer BBox.pp;
    (match split_kind with
    | `Lazy -> E.add_multi_simpl_rule ~priority:0 split
    | `Eager -> 
      E.add_cheap_multi_simpl_rule split;
      (* this rule is used to interfere with lazy clausification *)
      E.add_multi_simpl_rule ~priority:0 split
    | `Off -> ());
    
    E.add_unary_inf "avatar_check_empty" check_empty;
    E.add_generate ~priority:1000 "avatar_check_sat" check_satisfiability;
    E.add_generate ~priority:100 "avatar.lemmas" inf_new_lemmas;
    E.add_clause_conversion convert_lemma;

    if split_kind != `Off then 
      Signal.on E.on_start (fun () -> 
        E.get_passive ()
        |> Iter.iter (fun cl ->
          match split cl with 
          | None -> ()
          | Some splitted ->
            E.remove_passive (Iter.singleton cl);
            E.add_passive (Iter.of_list splitted)
        );

      Signal.ContinueListening
    );
    E.add_is_trivial_trail trail_is_trivial;
    if E.flex_get k_simplify_trail then (
      E.add_unary_simplify simplify_trail;
      if E.flex_get k_back_simplify_trail then (
        E.add_backward_simplify backward_simplify_trails;
      );
    );
    if E.flex_get k_show_lemmas then (
      Signal.once Signals.on_exit (fun _ -> show_lemmas ());
    );
    (* be sure there is an initial valuation *)
    ignore (Sat.check ~full:true ());
    ()
end

let get_env (module E : Env.S) : (module S) = E.flex_get k_avatar

let avatar_kind = ref `Lazy
let show_lemmas_ = ref false
let simplify_trail_ = ref true
let back_simplify_trail_ = ref true
let abstract_known_singletons = ref false
let split_only_initial = ref false
let split_only_goals = ref false
let split_only_ground = ref false
let max_trail_size = ref (-1)
let infer_from_components = ref false


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
    E.flex_add k_back_simplify_trail !back_simplify_trail_;
    E.flex_add k_abstract_known_singletons !abstract_known_singletons;
    E.flex_add k_split_only_initial !split_only_initial;
    E.flex_add k_split_only_goals !split_only_goals;
    E.flex_add k_split_only_ground !split_only_ground;
    E.flex_add k_max_trail_size !max_trail_size;
    E.flex_add k_infer_from_components !infer_from_components;
    E.flex_add k_avatar_enabled (!avatar_kind != `Off);

    Util.debug 1 "enable Avatar";
    A.register ~split_kind:!avatar_kind ()
  in
  Extensions.({default with name="avatar"; env_actions=[action]})

let () =
  Params.add_opts
    [ "--avatar", Arg.Symbol (["lazy";"eager";"off"],
        (fun s -> match s with 
          | "lazy" -> avatar_kind := `Lazy
          | "eager" -> avatar_kind := `Eager
          | "off" -> avatar_kind := `Off
          | _ -> assert false)), " enable Avatar splitting"
    ; "--print-lemmas", Arg.Set show_lemmas_, " show status of Avatar lemmas"
    ; "--avatar-simp-trail", Arg.Set simplify_trail_, " simplify boolean trails in Avatar"
    ; "--no-avatar-simp-trail", Arg.Clear simplify_trail_, " do not simplify boolean trails in Avatar"
    ; "--avatar-backward-simp-trail", Arg.Set back_simplify_trail_, " backward-simplify boolean trails in Avatar"
    ; "--no-avatar-backward-simp-trail", Arg.Clear back_simplify_trail_, " do not backward-simplify boolean trails in Avatar"
    ; "--abstract-known-singletons", Arg.Bool (fun b -> abstract_known_singletons :=  b), 
      " if a clause C <- [|A1, ..., An|] cannot be split, but its component is " ^
      " C is known, then add \\neg A1 \\lor ... \\lor \\neg An \\lor [| C |] to the " ^
      " set of SAT clauses."
    ; "--split-only-initial", Arg.Bool (fun b -> split_only_initial :=  b), " split only initial clauses"
    ; "--split-only-goals", Arg.Bool (fun b -> split_only_goals :=  b), " split only clauses that interacted with goal"
    ; "--split-only-ground", Arg.Bool ((:=) split_only_ground), " split only ground clauses"
    ; "--max-trail-size", Arg.Int (fun v -> max_trail_size := v), 
      " sets the limit of the trail size that a clause can have to be splittable. " ^
      " Negative value sets the limit to infinity"
    ; "--infer-from-components", Arg.Bool (fun b -> infer_from_components := b), 
      " when empty clause \bot <- A is infered, if A has only one component,
        skolemize and negate the component and add it to the proof state. "
    ];
  Params.add_to_modes 
    [ "ho-complete-basic"
    ; "ho-pragmatic"
    ; "ho-competitive"
    ; "fo-complete-basic"
    ; "lambda-free-intensional"
    ; "lambda-free-extensional"
    ; "ho-comb-complete"
    ; "lambda-free-purify-intensional"
    ; "lambda-free-purify-extensional"] 
    (fun () ->
       avatar_kind := `Off
    );