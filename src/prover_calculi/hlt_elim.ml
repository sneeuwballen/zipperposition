open Logtk
open Libzipperposition

module T = Term
module Ty = Type
module Lits = Literals
module Lit = Literal

let k_enabled = Flex_state.create_key ()
let k_max_depth = Flex_state.create_key ()


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
    let compare (a1) (a2) = Int.compare (T.Tbl.length a1) (T.Tbl.length a2)
  end)

  (* index from literals that appear as conclusions to all the premises
     in which they appear *)
  module ConclusionIdx = Fingerprint.Make(struct 
    type t = T.t
    let compare = Term.compare
  end)

  let prems_ = ref (PremiseIdx.empty ())
  let concls_ = ref (ConclusionIdx.empty ())
  (* occurrences of the clause in the premise_idx *)
  let cl_occs = ref Util.Int_map.empty

  let get_predicate lit =
    match lit with
    | L.Equation(lhs,_,_) when L.is_predicate_lit lit ->
      Some (lhs, Lit.is_pos lit)
    | _ -> None

  let cl_is_trackable cl =
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
        T.Tbl.filter_map_inplace (fun _ proofset ->
          (* todo this might be a possible place to remove the link from the clause index *)
          if List.exists (fun set -> CS.subset set proofset) sets then None
          else Some proofset) tbl;
        T.Tbl.length tbl == 0
      );
    )

  let add_transitive_conclusions premise concl cl =
    ConclusionIdx.retrieve_specializations (!concls_,idx_sc) (premise,q_sc)
    |> Iter.iter (fun (concl',premise',subst) -> 
      (* add implication premise' -> subst (concl) *)
      prems_ := PremiseIdx.update_leaf !prems_ premise' (fun tbl -> 
        (match T.Tbl.get tbl concl' with
        | Some old_proofset ->
          let proofset = CS.add cl old_proofset in
          register_cl_term cl premise';
          T.Tbl.add tbl (Subst.FO.apply Subst.Renaming.none subst (concl, q_sc)) proofset;
        | None -> assert false;);
        true
      );
    )

  let triggered_conclusions tbl premise' concl cl =
    T.Tbl.add tbl concl (CS.singleton cl);
    let max_proof_size = Env.flex_get k_max_depth in
    PremiseIdx.retrieve_generalizations (!prems_, idx_sc) (concl, q_sc)
    |> Iter.iter (fun (_,tbl',subst) -> 
      T.Tbl.to_iter tbl'
      |> Iter.iter (fun (t,proof_set) ->
        if C.ClauseSet.cardinal proof_set < max_proof_size then (
          let new_cls = CS.add cl proof_set in
          CS.iter (fun cl -> register_cl_term cl premise') new_cls;
          T.Tbl.add tbl 
            (Subst.FO.apply Subst.Renaming.none subst (t, idx_sc))
            (new_cls)
        ))
    )

  let add_new_premise premise concl cl =
    let alpha_renaming = 
      PremiseIdx.retrieve_specializations (!prems_, idx_sc) (premise, q_sc)
      |> Iter.find (fun (premise', tbl, subst) ->  
        if Subst.is_renaming subst then Some (premise', subst)
        else None
      ) in
    (match alpha_renaming with
    | Some (premise', subst) ->
      assert(T.equal (Subst.FO.apply Subst.Renaming.none subst (premise, q_sc))
                     premise');
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
    if not (generalization_present premise concl) then (
      (* it will be inserted, as premise -> conclusion is not implied
         by the set *)
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

  let track_clause cl =
    if cl_is_trackable cl then (
      insert_into_indices cl
    )

  let find_implication cl premise concl =
    PremiseIdx.retrieve_specializations (!prems_, idx_sc) (premise, q_sc)
    |> Iter.find (fun (premise', tbl, subst) -> 
      T.Tbl.to_iter tbl
      |> Iter.find (fun (concl', proofset) ->
        try
          ignore (Unif.FO.matching ~subst ~pattern:(concl', idx_sc) (concl, q_sc));
          Some(premise', concl', proofset)
        with Unif.Fail -> None)) 

  let do_simplify cl =
    let exception HiddenTauto of T.t * T.t * CS.t in

    let pred_cls, eq_cls = 
      List.partition (Lit.is_predicate_lit) (CCArray.to_list (C.lits cl))
    in
    let n = List.length pred_cls in
    let lhs lit = CCOpt.get_exn @@ L.View.get_lhs lit in
    if n >= 2 then (
      try 
        let bv = CCBV.create ~size:n true in
        let proofset = ref CS.empty in
        List.iteri (fun i i_lit ->
          if CCBV.get bv i then (
            let i_neg_t = lit_to_term ~negate:true (lhs i_lit) (L.is_pos i_lit) in
            List.iteri (fun j j_lit ->
              if CCBV.get bv j then (
                let j_t = lit_to_term (lhs j_lit) (L.is_pos j_lit) in
                let j_neg_t = lit_to_term ~negate:true (lhs j_lit) (L.is_pos j_lit) in
                (match find_implication cl i_neg_t j_t with
                | Some (lit_a, lit_b, proofset) ->
                  raise (HiddenTauto (lit_a, lit_b, proofset))
                | None -> 
                  (match find_implication cl i_neg_t j_neg_t with
                  | Some (_, _, proofset') ->
                    CCBV.reset bv j;
                    proofset := CS.union proofset' !proofset
                  | None -> () )
                ))
            ) pred_cls)
        ) pred_cls;
        let pred_cls_l = CCBV.select bv (CCArray.of_list pred_cls) in
        let lit_l = pred_cls_l @ eq_cls in
        let proof = 
          Proof.Step.inference ~rule:(Proof.Rule.mk "hidden_literal_elimination")
          (List.map C.proof_parent (CS.to_list !proofset))
        in

        Some (C.create ~penalty:(C.penalty cl) ~trail:(C.trail cl) lit_l proof)
      with HiddenTauto(lit_a,lit_b,proofset) ->
        let lit_l = CCList.map (fun t -> L.mk_prop t true) [lit_a; lit_b] in
        let proof = 
          Proof.Step.inference ~rule:(Proof.Rule.mk "hidden_tautology_elimination")
          (List.map C.proof_parent (CS.to_list proofset))
        in

        Some (C.create ~penalty:(C.penalty cl) ~trail:(C.trail cl) lit_l proof)
    ) else None

  let simplify_cl cl =
    match do_simplify cl with
    | Some cl' -> SimplM.return_new cl'
    | None -> SimplM.return_same cl
  
  let untrack_clause cl =
    (match Util.Int_map.get (C.id cl) !cl_occs with
    | Some premises ->
      Term.Set.iter (fun premise -> 
        prems_ := PremiseIdx.update_leaf !prems_ premise (fun tbl -> 
          T.Tbl.filter_map_inplace (fun _ proofset -> 
            let proofset = CS.remove cl proofset in
            if CS.is_empty proofset then None
            else Some proofset) tbl;
          T.Tbl.length tbl != 0)) premises;
    | _ -> ());
    cl_occs := Util.Int_map.remove (C.id cl) !cl_occs
  
  let setup () = ()
end

let max_depth_ = ref 5
let enabled_ = ref false

let extension =
  let lam2combs seq = seq in

  let register env =
    let module E = (val env : Env.S) in
    let module HLT = Make(E) in
    E.flex_add k_enabled !enabled_;
    E.flex_add k_max_depth !max_depth_;
    ()
  in
  { Extensions.default with
      Extensions.name = "combinators";
      env_actions=[register];
      post_cnf_modifiers=[lam2combs];
  }

let () =
  Options.add_opts [
    "--hidden-lt-elim", Arg.Bool ((:=) enabled_), " enable/disable hidden literal and tautology elimination";
    "--hidden-lit-max-depth", Arg.Set_int max_depth_, " max depth of binary implication graph precomputation"
  ];
  Extensions.register extension
