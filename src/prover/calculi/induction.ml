
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

module Make
(E : Env.S)
(A : Avatar_intf.S with module E = E)
= struct
  module Env = E
  module Ctx = E.Ctx
  module C = E.C
  module BoolBox = Ctx.BoolBox
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
    |> Sequence.filter
      (fun t -> match T.view t with
        | T.Const id -> Ind_cst.is_cst id || Ind_cst.is_sub_cst id
        | _ -> false)
    |> Sequence.sort_uniq ~cmp:T.compare
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
     - fix typing issues *)

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
          (fun t -> t, mk_fresh_var_ (T.ty t))
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
              (Util.pp_list T.pp) on);
        lemmas_ := List.rev_append clauses !lemmas_;
        clauses
      )
    in
    if C.is_ground c
    && not (is_ind_conjecture_ c)
    && not (C.get_flag A.flag_cut_introduced c)
    && C.is_unit_clause c
    && not (has_pos_lit_ c) (* only positive lemmas, therefore C negative *)
    && not (CCList.is_empty ind_csts) (* && not (T.Set.for_all CI.is_inductive set) *)
    then (
      assert (Array.length (C.lits c) = 1);
      let lit = (C.lits c).(0) in
      let terms = generalizable_subterms c in
      (* first, lemma without generalization;
          then, each possible way to generalize a subterm occurring multiple times *)
      List.rev_append
        (generalize ~on:[] lit)
        (CCList.flat_map (fun t -> generalize ~on:[t] lit) terms)
    ) else []

  let show_lemmas () =
    Util.debugf ~section 1 "@[<2>lemmas:@ [@[<hv>%a@]]@]"
      (fun k->k (Util.pp_list C.pp) !lemmas_)

  let scan_terms seq : Ind_cst.cst list =
    seq
    |> Sequence.flat_map Ind_cst.find_cst_in_term
    |> Sequence.map
      (fun (id,_,ty) -> Ind_cst.declare_cst id ~ty)
    |> Sequence.to_rev_list
    |> CCList.sort_uniq ~cmp:Ind_cst.cst_compare

  (* scan clauses for ground terms of an inductive type,
     and declare those terms *)
  let scan_clause c : Ind_cst.cst list =
    C.lits c
    |> Lits.Seq.terms
    |> scan_terms

  let is_eq_ (t1:Ind_cst.cst) (t2:Ind_cst.case) =
    BoolBox.inject_case t1 t2

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
  }

  (* for each member [t] of the cover set:
     for each ctx in [mw.mw_contexts]:
      - add ctx[t] <- [cst=t]
      - for each [t' subterm t] of same type, add clause ~[ctx[t']] <- [cst=t]
    @param trail precondition to this minimality
  *)
  let clauses_of_min_witness ~trail ~proof mw : (C.t list * BoolBox.t list list) =
    let b_lits = ref [] in
    let clauses =
      Ind_cst.cases ~which:`All mw.mw_coverset
      |> Sequence.flat_map
        (fun (case:Ind_cst.case) ->
           let b_lit = is_eq_ mw.mw_cst case in
           CCList.Ref.push b_lits b_lit;
           (* clauses [ctx[case] <- b_lit] *)
           let pos_clauses =
             List.map
               (fun ctx ->
                  let lits = ClauseContext.apply ctx case.Ind_cst.case_term in
                  C.create_a lits proof ~trail:(C.Trail.singleton b_lit))
               mw.mw_contexts
           in
           (* clauses [CNF(¬ And_i ctx_i[t']) <- b_lit] for
              each t' subterm of case *)
           let neg_clauses =
             Ind_cst.sub_constants_case case
             |> Sequence.filter_map
               (fun sub ->
                  (* only keep sub-constants that have the same type as [cst] *)
                  let sub = Ind_cst.term_of_sub_cst sub in
                  if Type.equal (T.ty sub) mw.mw_cst.Ind_cst.cst_ty
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
                         C.create lits proof ~trail:(C.Trail.singleton b_lit))
                  in
                  Sequence.of_list clauses)
            |> Sequence.to_rev_list
           in
           (* all new clauses *)
           let res = List.rev_append pos_clauses neg_clauses in
           Util.debugf ~section 2
             "@[<2>minimality of %a@ in case %a:@ @[<hv>%a@]@]"
             (fun k->k Ind_cst.pp_cst mw.mw_cst T.pp case.Ind_cst.case_term
                 (Util.pp_list C.pp) res);
           Sequence.of_list res)
      |> Sequence.to_rev_list
    in
    (* boolean constraint(s) *)
    let b_clauses =
      (* trail => \Or b_lits *)
      let pre = trail |> C.Trail.to_list |> List.map BoolLit.neg in
      let post = !b_lits in
      [ pre @ post ]
    in
    Util.debugf ~section 2 "@[<2>add boolean constraints@ @[<hv>%a@]@]"
      (fun k->k (Util.pp_list A.pp_bclause) b_clauses);
    clauses, b_clauses

  (* [cst] is the minimal term for which contexts [ctxs] holds, returns
     clauses expressing that, and assert boolean constraints *)
  let assert_min ?id ~trail ~proof ctxs (cst:Ind_cst.cst) =
    let set = Ind_cst.cover_set cst in
    let mw = {
      mw_cst=cst;
      mw_contexts=ctxs;
      mw_coverset=set;
    } in
    let clauses, b_clauses = clauses_of_min_witness ~trail ~proof mw in
    A.Solver.add_clauses ?tag:id b_clauses;
    Util.incr_stat stats_min;
    clauses

  (* checks whether the trail of [c] is trivial, that is:
     - contains two literals [i = t1] and [i = t2] with [t1], [t2]
        distinct cover set members, or
     - two literals [loop(i) minimal by a] and [loop(i) minimal by b], or
     - two literals [C in loop(i)], [D in loop(j)] if i,j do not depend
        on one another *)
  let has_trivial_trail c =
    let trail = C.trail c |> C.Trail.to_seq in
    (* all i=t where i is inductive *)
    let relevant_cases =
      trail
      |> Sequence.filter_map
        (fun blit ->
           match BoolBox.payload (BoolBox.Lit.abs blit) with
           | BoolBox.Case (l, r) -> Some (`Case (l, r))
           | _ -> None
        )
    in
    (* is there i such that   i=t1 and i=t2 can be found in the trail? *)
    Sequence.product relevant_cases relevant_cases
    |> Sequence.exists
      (function
        | (`Case (i1, t1), `Case (i2, t2)) ->
            let res =
              not (Ind_cst.cst_equal i1 i2)
                  || (Ind_cst.cst_equal i1 i2
                      && not (Ind_cst.case_equal t1 t2)) in
            if res
            then (
              Util.debugf ~section 4
                "@[<2>clause@ @[%a@]@ redundant because of @[%a={%a,%a}@] in trail@]"
                (fun k->k C.pp c Ind_cst.pp_cst i1 Ind_cst.pp_case t1 Ind_cst.pp_case t2)
            );
            res
        | _ -> false)

  let acyclicity lit =
    (* check if [sub] occurs in [t] under a constructor, recursively. Stop
        before entering non-constructor terms *)
    let rec occurs_in_ t ~sub =  match T.view t with
      | T.App (f, l) ->
        begin match T.view f with
          | T.Const id when Ind_ty.is_constructor id ->
            List.exists
              (fun t' -> T.equal sub t' || occurs_in_ t' ~sub)
              l
          | _ -> false
        end
      | T.Const _
      | T.Var _
      | T.DB _
      | T.AppBuiltin _ -> false
    in
    match lit with
    | Literal.Equation (l, r, b) ->
        if
          ( Ind_ty.is_inductive_type (T.ty l) && occurs_in_ ~sub:l r )
          ||
          ( Ind_ty.is_inductive_type (T.ty r) && occurs_in_ ~sub:r l )
        then if b then `Absurd else `Trivial else `Neither
    | _ -> `Neither

  let acyclicity_trivial c =
    let res = C.Seq.lits c
      |> Sequence.exists
        (fun lit -> match acyclicity lit with
          | `Neither
          | `Absurd -> false
          | `Trivial -> true
        )
    in
    if res
    then Util.debugf ~section 3 "@[<2>acyclicity:@ `@[%a@]` is trivial@]" (fun k->k C.pp c);
    res

  let acyclicity_simplify c =
    let lits' = C.Seq.lits c
      |> Sequence.filter
        (fun lit -> match acyclicity lit with
          | `Neither
          | `Trivial -> true
          | `Absurd -> false (* remove lit *)
        )
      |> Sequence.to_array
    in
    if Array.length lits' = Array.length (C.lits c)
    then SimplM.return_same c
    else (
      let proof =
        ProofStep.mk_inference ~rule:(ProofStep.mk_rule "acyclicity") [C.proof c] in
      let c' = C.create_a ~trail:(C.trail c) lits' proof in
      Util.debugf ~section 3
        "@[<2>acyclicity:@ simplify `@[%a@]`@ into `@[%a@]`@]" (fun k->k C.pp c C.pp c');
      SimplM.return_new c'
    )

  exception FoundInductiveLit of int * (T.t * T.t) list

  (* if c is  f(t1,...,tn) != f(t1',...,tn') or d, with f inductive symbol, then
      replace c with    t1 != t1' or ... or tn != tn' or d *)
  let injectivity_destruct c =
    try
      let eligible = C.Eligible.(filter Literal.is_neq) in
      Lits.fold_lits ~eligible (C.lits c)
      |> Sequence.iter
        (fun (lit, i) -> match lit with
           | Literal.Equation (l, r, false) ->
               begin match T.Classic.view l, T.Classic.view r with
                 | T.Classic.App (s1, l1), T.Classic.App (s2, l2)
                   when ID.equal s1 s2
                     && Ind_ty.is_constructor s1
                   ->
                     (* destruct *)
                     assert (List.length l1 = List.length l2);
                     let pairs = List.combine l1 l2 in
                     raise (FoundInductiveLit (i, pairs))
                 | _ -> ()
               end
           | _ -> ()
        );
      SimplM.return_same c (* nothing happened *)
    with FoundInductiveLit (idx, pairs) ->
      let lits = CCArray.except_idx (C.lits c) idx in
      let new_lits = List.map (fun (t1,t2) -> Literal.mk_neq t1 t2) pairs in
      let rule = ProofStep.mk_rule ~comment:["induction"] "injectivity_destruct" in
      let proof = ProofStep.mk_inference ~rule [C.proof c] in
      let c' = C.create ~trail:(C.trail c) (new_lits @ lits) proof in
      Util.debugf ~section 3 "@[<hv2>injectivity:@ simplify @[%a@]@ into @[%a@]@]"
        (fun k->k C.pp c C.pp c');
      SimplM.return_new c'

  (* if ID is a sub-constnat, it only is relevant assuming some trail on its
     parent constant(s) *)
  let rec trail_of_cst cst =
    match Ind_cst.as_sub_cst cst.Ind_cst.cst_id with
    | None -> []
    | Some sub ->
      let case = sub.Ind_cst.sub_cst_case in
      let cst' = sub.Ind_cst.sub_cst_cst in
      (* ensure [cst=case], and add the trail of [cst] to it *)
      is_eq_ cst' case :: trail_of_cst cst'

  (* when a clause contains new inductive constants, assert minimality
     of the clause for all those constants independently *)
  let inf_assert_minimal c =
    let consts = scan_clause c in
    let clauses =
      CCList.flat_map
        (fun cst ->
           let ctx = ClauseContext.extract_exn (C.lits c) (Ind_cst.cst_to_term cst) in
           let proof = ProofStep.mk_inference [C.proof c]
               ~rule:(ProofStep.mk_rule "min") in
           let trail = C.Trail.add_list (C.trail c) (trail_of_cst cst) in
           assert_min ~id:(C.id c) ~trail ~proof [ctx] cst)
        consts
    in
    clauses

  (* hook for converting some statements to clauses.
     It check if [Negated_goal l] contains inductive clauses, in which case
     it state their collective minimality *)
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
              (* keep only clauses that depend on [cst] *)
              let ctxs =
                CCList.filter_map
                  (fun c ->
                    ClauseContext.extract (C.lits c) (Ind_cst.cst_to_term cst))
                  clauses
              in
              let proof = ProofStep.mk_goal (Statement.src st) in
              let clauses = assert_min ~trail:C.Trail.empty ~proof ctxs cst in
              clauses
            )
            consts
          |> CCOpt.return
        )
    | _ -> None

  let register () =
    Util.debug ~section 2 "register induction";
    Env.add_unary_inf "induction_lemmas.ind" inf_assert_minimal;
    Env.add_clause_conversion convert_statement;
    Env.add_is_trivial has_trivial_trail;
    Env.add_is_trivial acyclicity_trivial;
    Env.add_simplify injectivity_destruct;
    Env.add_simplify acyclicity_simplify;
    (* add lemmas if option is set *)
    if Env.flex_get k_lemmas_enabled
      then Env.add_unary_inf "induction_lemmas.cut" inf_introduce_lemmas;
    if Env.flex_get k_show_lemmas
      then Signal.once Signals.on_exit (fun _ -> show_lemmas ());
    ()

  module Meta = struct
    (* TODO *)
    let t : _ Plugin.t = object
      method signature = Signature.empty
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
let lemmas_enabled_ = ref true
let show_lemmas_ = ref false

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
    ; "--lemmas", Options.switch_set true lemmas_enabled_, " enable lemmas for induction"
    ; "--no-lemmas", Options.switch_set false lemmas_enabled_, " disable lemmas for induction"
    ; "--show-lemmas", Arg.Set show_lemmas_, " show inductive (candidate) lemmas"
    ]
