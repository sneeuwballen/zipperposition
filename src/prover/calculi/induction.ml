
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Induction through Cut} *)

open Libzipperposition

module Lits = Literals
module T = FOTerm
module Su = Substs
module Ty = Type

module type S = Induction_intf.S

let section = Util.Section.make ~parent:Const.section "induction"

let stats_lemmas = Util.mk_stat "induction.lemmas"
let stats_min = Util.mk_stat "induction.assert_min"

let k_enable : bool Flex_state.key = Flex_state.create_key()
let k_lemmas_enabled : bool Flex_state.key = Flex_state.create_key()
let k_show_lemmas : bool Flex_state.key = Flex_state.create_key()
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

(* TODO: use multiple trail literals for paths, because it allows for
   better Avatar simplifications/redundancy checks *)

(* recover the (possibly empty) path from a boolean trail *)
let path_of_trail trail : Ind_cst.path =
  Trail.to_seq trail
  |> Sequence.filter_map BBox.as_case
  |> Sequence.max ~lt:(fun a b -> Ind_cst.path_dominates b a)
  |> CCOpt.get Ind_cst.path_empty

module Debug(E : Env.S) = struct
  module C = E.C

  module CstMap =
    CCMap.Make(struct type t = Ind_cst.cst let compare = Ind_cst.cst_compare end)
  module CaseMap =
    CCMap.Make(struct type t = Ind_cst.case let compare = Ind_cst.case_compare end)
  module ClauseCtxMap =
    CCMap.Make(struct type t = ClauseContext.t list
      let compare = CCList.compare ClauseContext.compare end)

  (* tree of inductions *)
  type ind_tree = {
    ind_tree: ind_tree CaseMap.t ClauseCtxMap.t CstMap.t;
    ind_clauses: C.t list;
  }

  let empty_ind_tree = {ind_tree=CstMap.empty; ind_clauses=[];}

  let tree_of_clauses (seq:C.t Sequence.t) : ind_tree =
    let rec insert (t:ind_tree) (p:Ind_cst.path) (c:C.t) : ind_tree =
      match p with
        | [] ->
          {t with ind_clauses = c::t.ind_clauses }
        | {Ind_cst.path_case=case; path_cst=cst; path_clauses=ctxs} :: p' ->
          let m1 = CstMap.get_or cst t.ind_tree ~or_:ClauseCtxMap.empty in
          let m2= ClauseCtxMap.get_or ctxs m1 ~or_:CaseMap.empty in
          let t' = CaseMap.get_or case m2 ~or_:empty_ind_tree in
          {t with
           ind_tree=
             CstMap.add cst
               (ClauseCtxMap.add ctxs
                  (CaseMap.add case (insert t' p' c) m2)
                  m1)
               t.ind_tree;
          }
    in
    (* insert each clause' trail. Reverse it so that most nested inductions
       come last *)
    Sequence.fold
      (fun t c -> insert t (C.trail c |> path_of_trail |> List.rev) c)
      empty_ind_tree seq

  let output_ind_tree oc (t:ind_tree) : unit =
    let module B = PrintBox in
    let rec to_box t : B.t list =
      let clauses =
        List.rev_map (fun c -> B.text (C.to_string c)) t.ind_clauses
        |> B.tree (B.text "clauses")
      in
      CstMap.to_list t.ind_tree
      |> List.map
        (fun (c,sub) ->
           let sub = to_box_ctxs c sub in
           let label =
             CCFormat.sprintf "@[<hv>induction on %a :@ %a (parent=%a)@]"
               Ind_cst.pp_cst c Type.pp (Ind_cst.cst_ty c)
               (CCFormat.opt Ind_cst.pp_cst) (Ind_cst.cst_parent c)
           in
           B.tree (B.text label) sub)
      |> CCList.cons clauses
    and to_box_ctxs c ctxs_map : B.t list =
      ClauseCtxMap.to_list ctxs_map
      |> List.map
        (fun (ctxs,sub) ->
           let label =
             CCFormat.sprintf "[@[<hv>%a@]]"
               (Util.pp_list ClauseContext.pp) ctxs |> B.text in
           let sub = to_box_case c sub in
           B.tree label sub)
    and to_box_case c case_m : B.t list =
      CaseMap.to_list case_m
      |> List.map
        (fun (case,sub) ->
           let label =
             CCFormat.sprintf "@[case %a = %a@]" Ind_cst.pp_cst c Ind_cst.pp_case case in
           let sub = to_box sub in
           B.tree (B.text label) sub)
    in
    let box = B.tree B.empty (to_box t) in
    PrintBox_html.to_string_doc box |> CCIO.write_line oc;
    ()
    (* PrintBox_text.output file box *)

  (* output the set of inductive clauses as a tree, in the given file *)
  let output_ind_tree file =
    (* gather the tree of inductive clauses *)
    let tree =
      Sequence.append
        (E.ProofState.ActiveSet.clauses () |> C.ClauseSet.to_seq)
        (E.ProofState.PassiveSet.clauses () |> C.ClauseSet.to_seq)
      |> tree_of_clauses
    in
    CCIO.with_out file
      (fun oc -> output_ind_tree oc tree);
    ()
end

module Make
(E : Env.S)
(A : Avatar_intf.S with module E = E)
= struct
  module Env = E
  module Ctx = E.Ctx
  module C = E.C
  module BoolBox = BBox
  module BoolLit = BoolBox.Lit

  let lemmas_ = ref []

  let is_ind_conjecture_ c =
    match C.distance_to_goal c with
    | Some (0 | 1) -> true
    | Some _
    | None -> false

  let has_pos_lit_ c =
    CCArray.exists Literal.is_pos (C.lits c)

  let is_acceptable_lemma lits =
    (* not too deep *)
    Lits.Seq.terms lits
    |> Sequence.map T.depth
    |> Sequence.max
    |> CCOpt.map_or ~default:true (fun d -> d < 5)

  (* terms that are either inductive constants or sub-constants *)
  let constants_or_sub c =
    C.Seq.terms c
    |> Sequence.flat_map T.Seq.subterms
    |> Sequence.filter_map
      (fun t -> match T.view t with
        | T.Const id -> Ind_cst.as_cst id
        | _ -> None)
    |> Sequence.sort_uniq ~cmp:Ind_cst.cst_compare
    |> Sequence.to_rev_list

  (* sub-terms of an inductive type, that occur several times (candidate
     for "subterm generalization" *)
  let generalizable_subterms c =
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

  (* apply the list of replacements [l] to the term [t] *)
  let replace_many l t =
    List.fold_left
      (fun t (old,by) -> T.replace t ~old ~by)
      t l

  (* fresh var generator *)
  let fresh_var_gen_ () =
    let r = ref 0 in
    fun ty ->
      let v = T.var_of_int ~ty !r in
      incr r;
      v

  (* FIXME:
     - use better generalization?
     - fix typing issues, if any remains *)

  (* TODO: split this into:
   - regular (nested) induction rule
   - heuristic to guess non trivial lemmas (i.e. exclusively with
     non obvious generalization, other cases are handled by the regular
     rule); this part is guarded by --lemmas *)

  (* when a unit clause has inductive constants, take its negation
      and add it as a lemma (some restrictions apply) *)
  let inf_introduce_lemmas c =
    let ind_csts = constants_or_sub c in
    let generalize ~on lit =
      let mk_fresh_var_ = fresh_var_gen_ () in
      (* abstract w.r.t all those constants (including the term
         being generalized). The latter must occur first, as it
         might contain constants being replaced. *)
      let replacements =
        List.map
          (fun c -> Ind_cst.cst_to_term c, mk_fresh_var_ (Ind_cst.cst_ty c))
          (on @ ind_csts)
      in
      (* replace constants by variables in [lit], then
         let [c] be [forall... (not lit)] *)
      let lit =
        lit
        |> Literal.map (replace_many replacements)
        |> Literal.negate
      in
      let lits = [| lit |] in
      (* if [box lits] already exists or is too deep, no need to re-do inference *)
      if not (is_acceptable_lemma lits)
      then []
      else (
        (* introduce cut now *)
        let proof = ProofStep.mk_trivial in
        let clauses, _ = A.introduce_cut lits proof in
        Util.incr_stat stats_lemmas;
        Util.debugf ~section 2
          "@[<2>introduce cut@ from %a@ @[<hv0>%a@]@ generalizing on @[%a@]@]"
          (fun k->k C.pp c (Util.pp_list C.pp) clauses
              (Util.pp_list Ind_cst.pp_cst) on);
        lemmas_ :=
          List.rev_append (List.rev_map C.to_sclause clauses) !lemmas_;
        clauses
      )
    in
    if C.is_ground c
    && not (is_ind_conjecture_ c)
    && not (C.get_flag Avatar.flag_cut_introduced c)
    && C.is_unit_clause c
    && not (has_pos_lit_ c) (* only positive lemmas, therefore C negative *)
    && not (CCList.is_empty ind_csts) (* && not (T.Set.for_all CI.is_inductive set) *)
    then (
      assert (Array.length (C.lits c) = 1);
      let lit = (C.lits c).(0) in
      let terms = generalizable_subterms c in
      (* TODO: keep even composite terms as long as they start
         with uninterpreted symbol *)
      let consts = CCList.filter_map Ind_cst.cst_of_term terms in
      (* first, lemma without generalization;
          then, each possible way to generalize a subterm occurring multiple times *)
      List.rev_append
        (generalize ~on:[] lit)
        (CCList.flat_map (fun c -> generalize ~on:[c] lit) consts)
    ) else []

  let show_lemmas () =
    Util.debugf ~section 1 "@[<2>lemmas:@ [@[<hv>%a@]]@]"
      (fun k->k (Util.pp_list SClause.pp) !lemmas_)

  (* scan terms for inductive constants *)
  let scan_terms seq : Ind_cst.cst list =
    seq
    |> Sequence.flat_map Ind_cst.find_cst_in_term
    |> Sequence.to_rev_list
    |> CCList.sort_uniq ~cmp:Ind_cst.cst_compare

  (* scan clauses for ground terms of an inductive type,
     and declare those terms *)
  let scan_clause c : Ind_cst.cst list =
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
    mw_contexts: ClauseContext.t list;
      (* the conjunction of contexts for which [cst] is minimal
         (that is, in the model, any term smaller than [cst] makes at
         least one context false) *)
    mw_coverset : Ind_cst.cover_set;
      (* minimality should be asserted for each case of the coverset *)
    mw_path: Ind_cst.path;
      (* path leading to this *)
    mw_proof: ProofStep.t;
  }

  (* TODO: incremental strenghtening.
     - when expanding a coverset in clauses_of_min_witness, see if there
       are other constants in the path with same type, in which case
     strenghten! *)

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
                  C.create_a lits mw.mw_proof ~trail:(Trail.singleton b_lit))
               mw.mw_contexts
           in
           (* clauses [CNF(¬ And_i ctx_i[t']) <- b_lit] for
              each t' subterm of case *)
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
                      (fun lits ->
                         C.create lits mw.mw_proof ~trail:(Trail.singleton b_lit))
                  in
                  Sequence.of_list clauses)
            |> Sequence.to_rev_list
           in
           (* all new clauses *)
           let res = List.rev_append pos_clauses neg_clauses in
           Util.debugf ~section 2
             "@[<2>minimality of %a@ in case %a:@ @[<hv>%a@]@]"
             (fun k->k Ind_cst.pp_cst mw.mw_cst T.pp (Ind_cst.case_to_term case)
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
    Util.debugf ~section 2 "@[<2>add boolean constraints@ @[<hv>%a@]@]"
      (fun k->k (Util.pp_list BBox.pp_bclause) b_clauses);
    clauses, b_clauses

  (* [cst] is the minimal term for which contexts [ctxs] holds, returns
     clauses expressing that, and assert boolean constraints *)
  let assert_min ~trail ~proof ctxs (cst:Ind_cst.cst) =
    let path = path_of_trail trail in
    match Ind_cst.cst_cover_set cst with
      | Some set when not (Ind_cst.path_contains_cst path cst) ->
        let mw = {
          mw_cst=cst;
          mw_contexts=ctxs;
          mw_coverset=set;
          mw_path=path;
          mw_proof=proof;
        } in
        let clauses, b_clauses = clauses_of_min_witness ~trail mw in
        A.Solver.add_clauses ~proof b_clauses;
        Util.incr_stat stats_min;
        clauses
      | Some _ (* path already contains [cst] *)
      | None -> []  (* too deep for induction *)

  (* TODO: trail simplification that removes all path literals except
     the longest? *)

  (* checks whether the trail of [c] is trivial, that is:
     - contains two literals [i = t1] and [i = t2] with [t1], [t2]
        distinct cover set members, or
     - two literals [loop(i) minimal by a] and [loop(i) minimal by b], or
     - two literals [C in loop(i)], [D in loop(j)] if i,j do not depend
        on one another *)
  let has_trivial_trail c =
    let trail = C.trail c |> Trail.to_seq in
    (* all boolean literals that express paths *)
    let relevant_cases = Sequence.filter_map BoolBox.as_case trail in
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
             "@[<2>clause@ @[%a@]@ redundant because of@ \
              {@[@[%a@],@,@[%a@]}@] in trail@]"
             (fun k->k C.pp c Ind_cst.pp_path p1 Ind_cst.pp_path p2)
         );
         res)

  (* ensure the proper declarations are done for this constant *)
  let decl_cst_ cst =
    Ind_cst.declarations_of_cst cst
    |> Sequence.iter (fun (id,ty) -> Ctx.declare id ty)

  (* when a clause contains new inductive constants, assert minimality
     of the clause for all those constants independently *)
  let inf_assert_minimal c : C.t list =
    let consts = scan_clause c in
    let clauses =
      CCList.flat_map
        (fun cst ->
           decl_cst_ cst;
           let ctx = ClauseContext.extract_exn (C.lits c) (Ind_cst.cst_to_term cst) in
           let proof = ProofStep.mk_inference [C.proof c]
               ~rule:(ProofStep.mk_rule "min") in
           assert_min ~trail:(C.trail c) ~proof [ctx] cst)
        consts
    in
    clauses

  (* hook for converting some statements to clauses.
     It check if [Negated_goal l] contains inductive clauses, in which case
     it states their collective minimality *)
  let convert_statement st =
    match Statement.view st with
    | Statement.NegatedGoal l ->
        (* find inductive constants *)
        let consts =
          Sequence.of_list l
          |> Sequence.flat_map Sequence.of_list
          |> Sequence.flat_map SLiteral.to_seq
          |> scan_terms
        in
        if consts=[]
        then None
        else (
          (* first, get "proper" clauses *)
          let clauses = C.of_statement st in
          (* for each new inductive constant, assert minimality of
             this constant w.r.t the set of clauses that contain it *)
          CCList.flat_map
            (fun cst ->
              decl_cst_ cst;
              (* keep only clauses that depend on [cst] *)
              let ctxs =
                CCList.filter_map
                  (fun c ->
                    ClauseContext.extract (C.lits c) (Ind_cst.cst_to_term cst))
                  clauses
              in
              (* proof: one step from all the clauses above *)
              let proof =
                ProofStep.mk_inference (List.map C.proof clauses)
                  ~rule:(ProofStep.mk_rule "min")
              in
              assert_min ~trail:Trail.empty ~proof ctxs cst
            )
            consts
          |> CCOpt.return
        )
    | _ -> None

  let register () =
    Util.debug ~section 2 "register induction";
    let d = Env.flex_get k_ind_depth in
    Util.debugf ~section 2 "maximum induction depth: %d" (fun k->k d);
    Ind_cst.max_depth_ := d;
    Env.add_unary_inf "induction.ind" inf_assert_minimal;
    Env.add_clause_conversion convert_statement;
    Env.add_is_trivial has_trivial_trail;
    (* declare new constants to [Ctx] *)
    Signal.on_every Ind_cst.on_new_cst
      (fun cst ->
         Ind_cst.declarations_of_cst cst
         |> Sequence.iter (CCFun.uncurry Ctx.declare));
    (* add lemmas if option is set *)
    if Env.flex_get k_lemmas_enabled
      then Env.add_unary_inf "induction.cut" inf_introduce_lemmas;
    if Env.flex_get k_show_lemmas
      then Signal.once Signals.on_exit (fun _ -> show_lemmas ());
    ()

  module Meta = struct
    (* TODO *)
    let t : _ Plugin.t = object
      method signature = ID.Map.empty
      method clauses = []
      method owns _ = false
      method to_fact _ = assert false
      method of_fact _ = None (* TODO *)
    end

    (* TODO
    let declare_inductive p ity =
      let ity = Induction.make ity.CI.pattern ity.CI.constructors in
      Util.debugf ~section 2
        "@[<hv2>declare inductive type@ %a@]"
        (fun k->k Induction.print ity);
      let fact = Induction.t#to_fact ity in
      add_fact_ p fact

      (* declare inductive types *)
      E.Ctx.Induction.inductive_ty_seq
        (fun ity -> ignore (declare_inductive p ity));
      Signal.on E.Ctx.Induction.on_new_inductive_ty
        (fun ity ->
           ignore (declare_inductive p ity);
           Signal.ContinueListening
        );
    *)
  end
end

let enabled_ = ref true
let lemmas_enabled_ = ref false
let show_lemmas_ = ref false
let depth_ = ref !Ind_cst.max_depth_
let dot_ind_tree_ = ref ""

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
    |> Flex_state.add k_lemmas_enabled !lemmas_enabled_
    |> Flex_state.add k_show_lemmas !show_lemmas_
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
    if !dot_ind_tree_ <> ""
    then (
      let module D = Debug(E) in
      let file = !dot_ind_tree_ in
      Signal.once Signals.on_dot_output
        (fun () -> D.output_ind_tree file);
    )
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
    ; "--lemmas", Options.switch_set true lemmas_enabled_, " enable lemmas for induction"
    ; "--no-lemmas", Options.switch_set false lemmas_enabled_, " disable lemmas for induction"
    ; "--show-lemmas", Arg.Set show_lemmas_, " show inductive (candidate) lemmas"
    ; "--induction-depth", Arg.Set_int depth_, " maximum depth of nested induction"
    ; "--dot-induction-tree", Arg.Set_string dot_ind_tree_, " output induction tree in given file"
    ]
