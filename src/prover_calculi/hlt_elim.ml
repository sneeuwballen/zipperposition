open Logtk
open Libzipperposition

module T = Term
module Ty = Type
module Lits = Literals
module Lit = Literal
module A = Libzipperposition_avatar

let section = Util.Section.make ~parent:Const.section "hlt-elim"

let k_enabled = Flex_state.create_key ()
let k_max_depth = Flex_state.create_key ()
let k_simpl_new = Flex_state.create_key ()
let k_track_active_only = Flex_state.create_key ()
let k_max_self_impls = Flex_state.create_key ()
let k_unit_reduction = Flex_state.create_key ()
let k_hte = Flex_state.create_key ()
let k_hle = Flex_state.create_key ()
let k_max_tracked_clauses = Flex_state.create_key ()


module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {6 Registration} *)
  val setup : unit -> unit
end

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module CS = C.ClauseSet
  module L = Literal

  (* index from literals to the map implied literal 
      -> clauses necessary for the proof *)
  module PremiseIdx = Fingerprint.Make(struct 
    type t = CS.t T.Tbl.t
    (* as we will maintain the invariant that each term is mapped to a single
       table, comparing the lengths suffices *)
    let compare (a1) (a2) = compare (T.Tbl.length a1) (T.Tbl.length a2)
  end)

  (* index from literals that appear as conclusions to all the premises
     in which they appear *)
  module ConclusionIdx = Fingerprint.Make(struct 
    type t = T.t
    let compare = Term.compare
  end)

  module UnitIdx = Fingerprint.Make(struct 
    type t = C.t
    let compare = C.compare
  end)

  let prems_ = ref (PremiseIdx.empty ())
  let concls_ = ref (ConclusionIdx.empty ())
  let units_ = ref (UnitIdx.empty ())
  (* occurrences of the clause in the premise_idx *)
  let cl_occs = ref Util.Int_map.empty
  (* clauses tracked so far *)
  let tracked_cls = ref 0 

  let get_predicate lit =
    match lit with
    | L.Equation(lhs,_,_) when L.is_predicate_lit lit ->
      Some (lhs, Lit.is_pos lit)
    | _ -> None

  let cl_is_ht_trackable cl =
    match C.lits cl with
    | [| l1; l2 |] -> 
      CCOpt.is_some (get_predicate l1) && CCOpt.is_some (get_predicate l2)
    | _ -> false

  let [@inline] rec normalize_negations lhs =
    match T.view lhs with
    | T.AppBuiltin(Builtin.Not, [t]) ->
      (match T.view t with
      | T.AppBuiltin(Builtin.Not, [s]) ->
        normalize_negations s
      | _ -> lhs)
    | _ -> lhs

  (* constants denoting the scope of index and the query, respectively *)
  let idx_sc, q_sc = 0, 1

  let lit_to_term ?(negate=false) a_lhs sign =
    let sign = if negate then not sign else sign in
    normalize_negations (if sign then a_lhs else T.Form.not_ a_lhs)

  let register_cl_term cl premise =
    let premise_set = 
      Term.Set.add premise
      (Util.Int_map.get_or ~default:Term.Set.empty (C.id cl) !cl_occs) in
    cl_occs := Util.Int_map.add (C.id cl) premise_set !cl_occs

  let generalization_present premise concl =
    PremiseIdx.retrieve_generalizations (!prems_, idx_sc) (premise, q_sc)
    |> Iter.exists (fun (_, tbl, subst) -> 
      T.Tbl.keys tbl
      |> Iter.exists (fun t ->
        try
          ignore(Unif.FO.matching ~subst ~pattern:(t, idx_sc) (concl, q_sc));
          true
        with Unif.Fail -> false))

  let remove_instances premise concl =
    PremiseIdx.retrieve_specializations (!prems_, idx_sc) (premise, q_sc)
    |> (fun i -> Iter.fold (fun tasks (t, tbl, subst) -> 
      let sets_to_remove = 
        Iter.fold (fun acc (s,cls) ->
          try
            let subst = Unif.FO.matching ~subst ~pattern:(concl,q_sc) (s,idx_sc) in
            if Subst.is_renaming subst then acc else cls :: acc
          with Unif.Fail -> acc
        ) [] (T.Tbl.to_iter tbl) in
      (t, sets_to_remove) :: tasks        
    ) [] i)
    |> CCList.iter (fun (t, sets) ->
      prems_ := PremiseIdx.update_leaf !prems_ t (fun tbl -> 
        T.Tbl.filter_map_inplace (fun concl proofset ->
          concls_ := ConclusionIdx.remove !concls_ concl t;
          if List.exists (fun set -> CS.subset set proofset) sets then None
          else Some proofset) tbl;
        T.Tbl.length tbl == 0
      );
    )

  let add_transitive_conclusions premise concl cl =
    Util.debugf ~section 5 "transitive conclusion: @[%a@] --> @[%a@]"
      (fun k -> k T.pp premise T.pp concl);
    ConclusionIdx.retrieve_specializations (!concls_,idx_sc) (premise,q_sc)
    |> Iter.iter (fun (concl',premise',subst) -> 
      (* add implication premise' -> subst (concl) *)
      Util.debugf ~section 5 "found: @[%a@] --> @[%a@]"
        (fun k -> k T.pp premise' T.pp concl');
      prems_ := PremiseIdx.update_leaf !prems_ premise' (fun tbl -> 
        (match T.Tbl.get tbl concl' with
        | Some old_proofset ->
          if CS.cardinal old_proofset < Env.flex_get k_max_depth then (
            let proofset = CS.add cl old_proofset in
            register_cl_term cl premise';
            let concl = (Subst.FO.apply Subst.Renaming.none subst (concl, q_sc)) in
            concls_ := ConclusionIdx.add !concls_ concl premise';
            T.Tbl.add tbl concl proofset
          )
        | None -> assert false;);
        true
      );
    )

  let triggered_conclusions tbl premise' concl cl =
    let aux concl =
      T.Tbl.add tbl concl (CS.singleton cl);
      concls_ := ConclusionIdx.add !concls_ concl premise';
      let max_proof_size = Env.flex_get k_max_depth in
      PremiseIdx.retrieve_generalizations (!prems_, idx_sc) (concl, q_sc)
      |> Iter.iter (fun (_,tbl',subst) -> 
        T.Tbl.to_iter tbl'
        |> Iter.iter (fun (t,proof_set) ->
          if C.ClauseSet.cardinal proof_set < max_proof_size then (
            let new_cls = CS.add cl proof_set in
            CS.iter (fun cl -> register_cl_term cl premise') new_cls;
            let concl = (Subst.FO.apply Subst.Renaming.none subst (t, idx_sc)) in
            concls_ := ConclusionIdx.add !concls_ concl premise';
            T.Tbl.add tbl concl (new_cls)
          ))
      ) in
    
    try
      let subst = Unif.FO.matching ~pattern:(premise',0) (concl,1) in
      let i = ref 0 in
      let concl' = Subst.FO.apply Subst.Renaming.none subst (concl,0) in
      (* if the terms are the same -- variable does not get bound to a term containing
         that variable we cannot pump the term -- give up *)
      if T.equal concl concl' then raise Unif.Fail;
      let concl = ref concl in
      while !i <= Env.flex_get k_max_self_impls do
        aux !concl;
        (* PUTTING concl IN THE SCOPE OF premise' -- intentional!!! *)
        concl := Subst.FO.apply Subst.Renaming.none subst (!concl,0);
        if T.depth !concl > 3 then (
          (* breaking out of the loop for the very deep terms *)
          i := Env.flex_get k_max_self_impls + 1
        );
        i := !i + 1;
      done;
    with Unif.Fail -> 
      aux concl

  let get_unit_predicate cl =
    match C.lits cl with
    | [| (L.Equation(lhs, _, _) as l) |] when L.is_predicate_lit l ->
      Some (lit_to_term lhs (L.is_pos l))
    | _ -> None

  let add_new_premise premise concl cl =
    let alpha_renaming = 
      PremiseIdx.retrieve_specializations (!prems_, idx_sc) (premise, q_sc)
      |> Iter.find (fun (premise', tbl, subst) ->  
        if Subst.is_renaming subst then Some (premise', subst)
        else None
      ) in
    (match alpha_renaming with
    | Some (premise', subst) ->
      let concl = Subst.FO.apply Subst.Renaming.none subst (concl, q_sc) in
      prems_ := PremiseIdx.update_leaf !prems_ premise' (fun tbl -> 
        if not (T.Tbl.mem tbl concl) then (
          triggered_conclusions tbl premise' concl cl
        );
        true
      );
    | _ ->
      let tbl = T.Tbl.create 64 in
      triggered_conclusions tbl premise concl cl;
      prems_ := PremiseIdx.add !prems_ premise tbl
    )

  let insert_implication premise concl cl =
    if not (generalization_present premise concl) &&
       not (T.equal premise concl) then (
      remove_instances premise concl;
      add_transitive_conclusions premise concl cl;
      add_new_premise premise concl cl;
    )

  let insert_into_indices cl =
    match CCArray.map get_predicate (C.lits cl) with
    | [| Some (a_lhs, a_sign); Some (b_lhs, b_sign) |] ->
      (* read clause a | b as either ~a -> b or ~b -> a *)
      insert_implication (lit_to_term ~negate:true a_lhs a_sign)
                         (lit_to_term b_lhs b_sign) cl;
      insert_implication (lit_to_term ~negate:true b_lhs b_sign)
                         (lit_to_term a_lhs a_sign) cl
    | _ -> ()
  
  let limit_not_reached () =
    let tracked = !tracked_cls in
    let tracked_max = Env.flex_get k_max_tracked_clauses in
    tracked_max == -1 || tracked <= tracked_max

  let track_clause cl =
    if cl_is_ht_trackable cl && limit_not_reached () then (
      Util.debugf ~section 3 "tracking @[%a@]" (fun k -> k C.pp cl);
      
      insert_into_indices cl;
      incr tracked_cls;
      
      Util.debugf ~section 2 "idx_size: @[%d@]" (fun k -> k (PremiseIdx.size !prems_));
      Util.debugf ~section 3 "premises:" CCFun.id;
      PremiseIdx.iter !prems_ (fun t tbl -> 
        Util.debugf ~section 2 "@[%a@] --> @[%a@]" (fun k -> k T.pp t (Iter.pp_seq T.pp) (T.Tbl.keys tbl))
      );
    ) else (
      match get_unit_predicate cl with
      | Some unit ->
        units_ := UnitIdx.add !units_ unit cl
      | None -> ()
    )

  let find_implication cl premise concl =
    PremiseIdx.retrieve_generalizations (!prems_, idx_sc) (premise, q_sc)
    |> Iter.find (fun (premise', tbl, subst) -> 
      T.Tbl.to_iter tbl
      |> Iter.find (fun (concl', proofset) ->
        try
          if CS.mem cl proofset then None
          else (
            let subst = Unif.FO.matching ~subst ~pattern:(concl', idx_sc) (concl, q_sc) in
            Some(premise', concl', proofset, subst))
        with Unif.Fail -> None)) 

  let do_unit_reduction cl = 
    let pred_lits, eq_lits = 
      List.partition (Lit.is_predicate_lit) (CCArray.to_list (C.lits cl))
    in
    let bv = CCBV.create ~size:(List.length pred_lits) true in
    let proofset = ref (CS.empty) in
    List.iteri (fun i lit -> 
      let lhs, sign =  CCOpt.get_exn (L.View.get_lhs lit), L.is_pos lit in
      let lhs_neg = lit_to_term ~negate:true lhs sign in 
      let unit_sc = (max idx_sc q_sc) + 1 in

      (* for the literal l we are looking for implications p -> c such that
         c\sigma = ~l. Then we see if there is an unit clause p' such that
         p\sigma\rho = p' *)

      ConclusionIdx.retrieve_generalizations (!concls_, idx_sc) (lhs_neg, q_sc)
      |> Iter.find_map (fun (concl, premise, subst) ->
        let orig_premise = premise in
        let premise = Subst.FO.apply Subst.Renaming.none subst (premise, idx_sc) in
        UnitIdx.retrieve_generalizations (!units_, unit_sc) (premise, idx_sc)
        |> Iter.head
        |> CCOpt.map (fun (_, unit_cl, _) -> 
          prems_ := PremiseIdx.update_leaf !prems_ orig_premise (fun tbl -> 
            let proofset' = T.Tbl.find tbl concl in
            if not (CS.mem cl proofset') then (
              proofset := CS.union (CS.add unit_cl (proofset')) !proofset;
              CCBV.reset bv i);
            true
          );
        ))
      (* if nothing is found stiffle compiler warning *)
      |> CCOpt.get_or ~default:()
    ) pred_lits;
    if CCBV.is_empty (CCBV.negate bv) then None
    else (
      let pred_cls_l = CCBV.select bv (CCArray.of_list pred_lits) in
      let lit_l = pred_cls_l @ eq_lits in
      let proof = 
        Proof.Step.simp ~rule:(Proof.Rule.mk "unit_hidden_literal_elimination")
        (List.map C.proof_parent (cl :: CS.to_list !proofset))
      in
      Some (C.create ~penalty:(C.penalty cl) ~trail:(C.trail cl) lit_l proof)
    )

  let do_simplify cl =
    let exception HiddenTauto of T.t * T.t * CS.t in

    let pred_lits, eq_lits = 
      List.partition (Lit.is_predicate_lit) (CCArray.to_list (C.lits cl))
    in
    let n = List.length pred_lits in
    let lhs lit = CCOpt.get_exn @@ L.View.get_lhs lit in
    if n >= 2 then (
      try 
        let bv = CCBV.create ~size:n true in
        let proofset = ref CS.empty in
        List.iteri (fun i i_lit ->
          if CCBV.get bv i then (
            let i_neg_t = lit_to_term ~negate:true (lhs i_lit) (L.is_pos i_lit) in
            List.iteri (fun j j_lit ->
              if CCBV.get bv j && i!=j then (
                let j_t = lit_to_term (lhs j_lit) (L.is_pos j_lit) in
                let j_neg_t = lit_to_term ~negate:true (lhs j_lit) (L.is_pos j_lit) in
                if Env.flex_get k_hte then (
                  (match find_implication cl i_neg_t j_t with
                  | Some (lit_a, lit_b, proofset, subst) 
                      when (C.length cl != 2 || not (Subst.is_renaming subst)) ->
                    (* stopping further search *)
                    raise (HiddenTauto (lit_a, lit_b, proofset))
                  | _ -> ())
                );
                if Env.flex_get k_hle then (
                  (match find_implication cl i_neg_t j_neg_t with
                    | Some (_, _, proofset',_) ->
                      CCBV.reset bv j;

                      Util.debugf ~section 1 "@[%a@] --> @[%a@]" 
                        (fun k -> k T.pp i_neg_t T.pp j_neg_t);
                      Util.debugf ~section 1 "used(%d): @[%a@]" 
                        (fun k -> k j (CS.pp C.pp) proofset');

                      proofset := CS.union proofset' !proofset
                    | None -> () )
                )

                )
            ) pred_lits)
        ) pred_lits;
        
        if CCBV.is_empty (CCBV.negate bv) then None
        else (        
          let pred_cls_l = CCBV.select bv (CCArray.of_list pred_lits) in
          let lit_l = pred_cls_l @ eq_lits in
          let proof = 
            Proof.Step.simp ~rule:(Proof.Rule.mk "hidden_literal_elimination")
            (List.map C.proof_parent (cl :: CS.to_list !proofset))
          in

          Some (C.create ~penalty:(C.penalty cl) ~trail:(C.trail cl) lit_l proof))
      with HiddenTauto(lit_a,lit_b,proofset) ->
        let lit_l = [L.mk_prop lit_a false; L.mk_prop lit_b true] in
        let proof = 
          Proof.Step.simp ~rule:(Proof.Rule.mk "hidden_tautology_elimination")
          (List.map C.proof_parent (cl :: CS.to_list proofset))
        in
        let repl = C.create ~penalty:(C.penalty cl + 1) ~trail:(C.trail cl) lit_l proof in
        let tauto = C.create ~penalty:(C.penalty cl) ~trail:(C.trail cl) [L.mk_tauto] proof in

        E.add_passive (Iter.singleton repl);
        Some (tauto)
    ) else None

  let simplify_cl cl =
    match do_simplify cl with
    | Some cl' ->
      Util.debugf ~section 1 "simplified: @[@[%a@] --> @[%a@]@]" (fun k -> k C.pp cl C.pp cl');
      SimplM.return_new cl'
    | None -> 
      SimplM.return_same cl

  let unit_reduction cl =
    match do_unit_reduction cl with
    | Some cl' ->
      Util.debugf ~section 3 "simplified: @[@[%a@] --> @[%a@]@]" (fun k -> k C.pp cl C.pp cl');
      SimplM.return_new cl'
    | None -> 
      SimplM.return_same cl
  
  let untrack_clause cl =
    (match Util.Int_map.get (C.id cl) !cl_occs with
    | Some premises ->
      Term.Set.iter (fun premise ->
        prems_ := PremiseIdx.update_leaf !prems_ premise (fun tbl -> 
          T.Tbl.filter_map_inplace (fun concl proofset -> 
            if CS.mem cl proofset then (
              concls_ := ConclusionIdx.remove !concls_ concl premise;
              None
            ) else Some proofset) tbl;
          T.Tbl.length tbl != 0)) premises;
    | _ -> ());
    if Util.Int_map.mem (C.id cl) !cl_occs then (
      Util.debugf ~section 3 "removed: @[%a@]." (fun k -> k C.pp cl);
      decr tracked_cls;
      ConclusionIdx.iter !concls_ (fun premise concl -> 
        Util.debugf ~section 3 "@[%a@] -> @[%a@]" (fun k -> k T.pp premise T.pp concl)));
    cl_occs := Util.Int_map.remove (C.id cl) !cl_occs;
    match get_unit_predicate cl with
    | Some unit ->
      units_ := UnitIdx.remove !units_ unit cl 
    | None -> ()
    

  let initialize () =
    if Env.flex_get k_track_active_only then (
      Signal.on_every Env.ProofState.ActiveSet.on_add_clause track_clause;
      Signal.on_every Env.ProofState.ActiveSet.on_remove_clause untrack_clause
    ) else (
      assert (Iter.is_empty @@ E.get_active ());
      Iter.iter track_clause (E.get_passive ());

      Util.debugf ~section 5 "discovered implications:" CCFun.id;
      PremiseIdx.iter !prems_ (fun premise tbl -> 
        Util.debugf ~section 5 "@[%a@] --> @[%a@]" (fun k -> k T.pp premise (Iter.pp_seq T.pp) (T.Tbl.keys tbl))
      );

      Signal.on_every Env.ProofState.PassiveSet.on_add_clause track_clause;
      Signal.on_every Env.ProofState.ActiveSet.on_remove_clause untrack_clause;
      Signal.on_every Env.on_forward_simplified (fun (c, new_state) -> 
        match new_state with
        | Some c' ->
          if not (C.equal c c') then (
            untrack_clause c; 
            track_clause c'
          )
        | _ -> untrack_clause c; (* c is redundant *)));
    
    Signal.StopListening


  let setup () =
    if E.flex_get k_enabled then (
      Signal.on Env.on_start initialize;

      if not (Env.flex_get A.k_avatar_enabled) then (
        let add_simpl = 
          if Env.flex_get k_simpl_new 
          then Env.add_basic_simplify 
          else Env.add_active_simplify
        in  

        add_simpl simplify_cl;
        if Env.flex_get k_unit_reduction then (
          add_simpl unit_reduction
        );
      ) else (
        CCFormat.printf "AVATAR is not yet compatible with HLT@."
      )
    )
end

let max_depth_ = ref 3
let enabled_ = ref false
let simpl_new_ = ref false
let track_active_only_ = ref true
let max_self_impls_ = ref 1
let max_tracked_clauses = ref (-1)
let unit_reduction_ = ref true
let hte_ = ref true
let hle_ = ref true


let extension =
  let register env =
    let module E = (val env : Env.S) in
    let module HLT = Make(E) in
    E.flex_add k_enabled !enabled_;
    E.flex_add k_max_depth !max_depth_;
    E.flex_add k_simpl_new !simpl_new_;
    E.flex_add k_track_active_only !track_active_only_;
    E.flex_add k_max_self_impls !max_self_impls_;
    E.flex_add k_unit_reduction !unit_reduction_;
    E.flex_add k_max_tracked_clauses !max_tracked_clauses;
    E.flex_add k_hle !hle_;
    E.flex_add k_hte !hte_;
    HLT.setup ()
  in
  { Extensions.default with
      Extensions.name = "hidden literal elimination";
      env_actions=[register]
  }

let () =
  Options.add_opts [
    "--hidden-lt-elim", Arg.Bool ((:=) enabled_), " enable/disable hidden literal and tautology elimination";
    "--hidden-lt-elim-max-tracked", Arg.Int ((:=) max_tracked_clauses), " negative value for disabling the limit";
    "--hidden-lt-elim-hle", Arg.Bool ((:=) hle_), " enable/disable hidden literal elimination (hidden-lt-elim must be on)";
    "--hidden-lt-elim-hte", Arg.Bool ((:=) hte_), " enable/disable hidden literal tautology elimination (hidden-lt-elim must be on)";
    "--hidden-lt-max-depth", Arg.Set_int max_depth_, " max depth of binary implication graph precomputation";
    "--hidden-lt-simplify-new", Arg.Bool ((:=) simpl_new_), " apply HLTe also when moving a clause from fresh to passive";
    "--hidden-lt-track-only-active", Arg.Bool ((:=) track_active_only_), 
      " HLTe tracks only active clauses (by default tracks passive clauses)";
    "--hidden-lt-max-self-implications", Arg.Int ((:=) max_self_impls_), 
      " how many times do we loop implications of the kind p(X) -> p(f(X)) ";
    "--hidden-lt-unit-reduction", Arg.Bool ((:=) unit_reduction_), 
      " do unit-triggered removal of literals "

  ];
  Extensions.register extension
