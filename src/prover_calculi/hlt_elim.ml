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
let k_clauses_to_track = Flex_state.create_key ()
let k_max_self_impls = Flex_state.create_key ()
let k_unit_propagated_hle = Flex_state.create_key ()
let k_reduce_tautologies = Flex_state.create_key ()
let k_delete_lits = Flex_state.create_key ()
let k_max_tracked_clauses = Flex_state.create_key ()
let k_track_eq = Flex_state.create_key ()
let k_insert_only_ordered = Flex_state.create_key ()
let k_heartbeat_steps = Flex_state.create_key ()
let k_heartbeat_disabled_hlbe = Flex_state.create_key ()
let k_max_imp_entries = Flex_state.create_key ()
let k_basic_rules = Flex_state.create_key ()
let k_penalize_tautologies = Flex_state.create_key ()

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {5 Registration} *)
  val setup : unit -> unit
end

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module CS = C.ClauseSet
  module L = Literal

  (* index from literals to the map implied literal
      -> clauses necessary for the proof
     with an additional field storing whether there are literals
     l, \neg l in the set of implied literals*)
  module PremiseIdx = NPDtree.MakeTerm(struct 
    type t = (CS.t T.Tbl.t) * bool
    (* as we will maintain the invariant that each term is mapped to a single
       table, comparing the lengths suffices *)
    let compare (a1,_) (a2,_) = compare (T.Tbl.length a1) (T.Tbl.length a2)
  end)

  (* index from literals that appear as conclusions to all the premises
     in which they appear *)
  module ConclusionIdx = NPDtree.MakeTerm(struct 
    type t = T.t
    let compare = Term.compare
  end)

  module UnitIdx = NPDtree.MakeTerm(struct 
    type t = C.t
    let compare = C.compare
  end)

  type propagation_kind = Failed | UnitPropagated 
  module PropagatedLitsIdx = NPDtree.MakeTerm(struct 
    type t = CS.t * propagation_kind
    let compare (a,_) (b,_) = CS.compare a b
  end)

  exception RuleNotApplicable

  let prems_ = ref (PremiseIdx.empty ())
  let concls_ = ref (ConclusionIdx.empty ())
  let units_ = ref (UnitIdx.empty ())
  let propagated_ = ref (PropagatedLitsIdx.empty ())
  let propagated_size_ = ref 0
  (* occurrences of the clause in the premise_idx *)
  let cl_occs = ref Util.Int_map.empty
  (* binary clauses tracked so far *)
  let tracked_binary = ref 0
  (* binary clauses tracked so far *)
  let tracked_unary = ref 0
  (* HLBE heartbeat will be set as soons as one rule modifies a clause *)
  let heartbeat_ = ref false

  let is_tauto c =
    Literals.is_trivial (C.lits c) || Trail.is_trivial (C.trail c)

  let [@inline] tracking_eq () =
    Env.flex_get k_track_eq

  (* constants denoting the scope of index and the query, respectively *)
  let idx_sc, q_sc = 0, 1

  let should_update_propagated () =
    Env.flex_get k_unit_propagated_hle &&
    !propagated_size_ <= (Env.flex_get k_max_tracked_clauses)

  let app_subst ?(sc=idx_sc) ~subst t =
    Subst.FO.apply Subst.Renaming.none subst (t,sc)

  let register_conclusion ~tbl ~premise concl proofset =
    if not @@ T.Tbl.mem tbl concl && T.depth concl <= 4 then (
      T.Tbl.replace tbl concl proofset;
      concls_ := ConclusionIdx.add !concls_ concl premise) 

  (* iterate over the parts of the common context of the term --
     EXCLUDING THE EMPTY CONTEXT *)
  let iter_ctx a b k =
    let common_arg xs ys =
      let rec aux acc xs ys =
        match xs with 
        | x :: xs' ->
          begin match ys with
          | y :: ys' ->
            if (not (T.equal x y)) && CCOpt.is_none acc then (
              aux (Some (x,y)) xs' ys'
            ) else if T.equal x y then aux acc xs' ys'
            else None
          | _ -> assert false
          end
        | _ -> 
          (assert (CCList.is_empty ys)); 
          acc
      in
      aux None xs ys
    in

    let rec aux a b = 
      match T.view a, T.view b with
      | T.App(hda, argsa), T.App(hdb, argsb) 
        when Type.equal (T.ty a) (T.ty b) ->
        if T.is_const hda && T.equal hda hdb then (
          match common_arg argsa argsb with 
          | Some (a, b) -> 
            k (a, b);
            aux a b
          | None -> ()
        )
      | _ -> ()
    in
    aux a b

  let retrieve_idx ~getter (premise, q_sc) =
    match T.view premise with
    | T.AppBuiltin(Builtin.Eq, ([_;a;b]|[a;b])) when tracking_eq () ->
      Iter.append (getter (premise, q_sc)) (getter ((T.Form.eq b a), q_sc))
    | T.AppBuiltin(Builtin.Neq, ([_;a;b]|[a;b])) when tracking_eq () ->
      Iter.append (getter (premise, q_sc)) (getter ((T.Form.neq b a), q_sc))
    | _ ->  getter (premise, q_sc)
  
  let retrieve_gen_prem_idx ()  =
    retrieve_idx ~getter:(PremiseIdx.retrieve_generalizations (!prems_, idx_sc))
  
  let retrieve_spec_prem_idx ()  =
    retrieve_idx ~getter:(PremiseIdx.retrieve_specializations (!prems_, idx_sc))
  
  let retrieve_gen_concl_idx ()  =
    retrieve_idx ~getter:(ConclusionIdx.retrieve_generalizations (!concls_,idx_sc))

  let retrieve_spec_concl_idx ()  =
    retrieve_idx ~getter:(ConclusionIdx.retrieve_specializations (!concls_,idx_sc))
  
  let retrieve_gen_unit_idx unit_sc  =
    retrieve_idx ~getter:(UnitIdx.retrieve_generalizations (!units_, unit_sc))
  
  let [@inline] get_predicate lit =
    match lit with
    | L.Equation(lhs,_,_) when L.is_predicate_lit lit ->
      Some (lhs, Lit.is_positivoid lit)
    | L.Equation(lhs,rhs,sign) when tracking_eq () ->
      Some (T.Form.eq lhs rhs, sign)
    | _ -> None

  let [@inline] matching_eq ?(decompose=false) ~subst ~pattern (t, sc) =
    let try_decompositions ?(cons=T.Form.eq) a b =
      iter_ctx a b
      |> Iter.find (fun (a, b) -> 
        try
          Some (Unif.FO.matching ~subst ~pattern (cons a b, sc))
        with Unif.Fail ->
          begin 
            try
              Some (Unif.FO.matching ~subst ~pattern (cons b a, sc))
            with Unif.Fail -> None
          end)
      |> (function | Some unif -> unif | _ -> raise Unif.Fail)    
    in

    try
      Unif.FO.matching ~subst ~pattern (t, sc)
    with Unif.Fail ->
      match T.view t with 
      | T.AppBuiltin(Builtin.Eq, ([_;a;b]|[a;b])) when tracking_eq () ->
        begin try 
          Unif.FO.matching ~subst ~pattern (T.Form.eq b a, sc)
        with Unif.Fail when decompose ->
          try_decompositions a b end
      | T.AppBuiltin(Builtin.Neq, ([_;a;b]|[a;b])) when tracking_eq () ->
        Unif.FO.matching ~subst ~pattern (T.Form.neq b a, sc)
      | _ ->  raise Unif.Fail

  let [@inline] flip_eq t =
    match T.view t with
    | T.AppBuiltin(Builtin.Eq, ([a;b]|[_;a;b])) when tracking_eq () ->
      T.Form.eq b a
    | T.AppBuiltin(Builtin.Neq, ([a;b]|[_;a;b])) when tracking_eq () ->
      T.Form.neq b a
    | _ -> t

  let [@inline] cl_is_ht_trackable cl =
    Trail.is_empty (C.trail cl) &&
    (match C.lits cl with
    | [| l1; l2 |] -> 
      CCOpt.is_some (get_predicate l1) && CCOpt.is_some (get_predicate l2)
    | _ -> false)

  let [@inline] rec normalize_negations lhs =
    match T.view lhs with
    | T.AppBuiltin(Builtin.Not, [t]) ->
      (match T.view t with
      | T.AppBuiltin(Builtin.Not, [s]) ->
        normalize_negations s
      | T.AppBuiltin(Builtin.Eq, ([_;a;b]|[a;b])) ->
        T.Form.neq a b
      | T.AppBuiltin(Builtin.Neq, ([_;a;b]|[a;b])) ->
        T.Form.eq a b
      | _ -> lhs)
    | _ -> lhs

  let [@inline] lit_to_term ?(negate=false) a_lhs sign =
    let sign = if negate then not sign else sign in
    normalize_negations (if sign then a_lhs else T.Form.not_ a_lhs)

  let register_cl_term cl premise =
    let premises, propagated = 
      Util.Int_map.get_or ~default:(Term.Set.empty, Term.Set.empty) (C.id cl) !cl_occs
    in
    cl_occs := Util.Int_map.add (C.id cl) ((Term.Set.add premise premises), propagated) !cl_occs
  
  let register_cl_propagated cl premise =
    let premises, propagated = 
      Util.Int_map.get_or ~default:(Term.Set.empty, Term.Set.empty) (C.id cl) !cl_occs
    in
    cl_occs := Util.Int_map.add (C.id cl) (premises, Term.Set.add premise propagated) !cl_occs

  let register_propagated_lit ~prop_kind lit_t cl cs =
    let has_renaming = ref false in
    let to_remove = ref Term.Set.empty in

    
    retrieve_idx ~getter:(PropagatedLitsIdx.retrieve_specializations (!propagated_, idx_sc)) (lit_t,q_sc)
    |> Iter.iter (fun (t, _, subst) ->
      if Subst.is_renaming subst then has_renaming := true
      else (to_remove := Term.Set.add t !to_remove;)
    );
    if not @@ Term.Set.is_empty !to_remove then (
      Util.debugf ~section 2 "removing: @[%a@]" (fun k -> k (Term.Set.pp T.pp) !to_remove));
    Term.Set.iter (fun t -> 
      decr propagated_size_;
      propagated_ := 
        PropagatedLitsIdx.update_leaf !propagated_ t (fun _ -> false)) 
    !to_remove;

    if not (!propagated_size_ >= 0) then (
      CCFormat.printf "prop size: %d@." !propagated_size_;
      assert false
    );
    if not !has_renaming then (
      let proofset = CS.add cl cs in
      CS.iter (fun c -> register_cl_propagated c lit_t) proofset;
      propagated_ := PropagatedLitsIdx.add !propagated_ lit_t (proofset, prop_kind);
      incr propagated_size_
    )
  
  let react_unit_added unit_cl unit_term =
    if should_update_propagated () then (
      retrieve_idx ~getter:(PremiseIdx.retrieve_unifiables (!prems_, idx_sc)) (unit_term, q_sc)
      |> Iter.iter (fun (_, (concls,_), subst) ->
        let subst = Unif_subst.subst subst in
        T.Tbl.to_iter concls
        |> Iter.iter (fun (concl,cs) -> 
          let concl = Subst.FO.apply Subst.Renaming.none subst (concl, idx_sc) in
          register_propagated_lit ~prop_kind:UnitPropagated concl unit_cl cs)))

  let generalization_present premise concl =
    retrieve_gen_prem_idx () (premise, q_sc)
    |> Iter.exists (fun (_, (tbl, _), subst) -> 
      T.Tbl.keys tbl
      |> Iter.exists (fun t ->
        try
          ignore(matching_eq ~subst ~pattern:(t, idx_sc) (concl, q_sc));
          true
        with Unif.Fail -> false))

  let remove_instances premise concl =
    retrieve_spec_prem_idx () (premise, q_sc)
    |> (fun i -> Iter.fold (fun tasks (t, (tbl, _), subst) -> 
      let sets_to_remove =
        Iter.fold (fun acc (s,cls) ->
          try
            let subst = matching_eq ~subst ~pattern:(concl,q_sc) (s,idx_sc) in
            if Subst.is_renaming subst then acc else cls :: acc
          with Unif.Fail -> acc
        ) [] (T.Tbl.to_iter tbl) in
      if CCList.is_empty sets_to_remove then tasks
      else (t, sets_to_remove) :: tasks        
    ) [] i)
    |> CCList.iter (fun (t, sets) ->
      prems_ := PremiseIdx.update_leaf !prems_ t (fun (tbl, _) -> 
        T.Tbl.filter_map_inplace (fun concl proofset ->
          if List.exists (fun set -> CS.subset set proofset) sets 
          then (concls_ := ConclusionIdx.remove !concls_ concl t; None)
          else Some proofset) tbl;
        T.Tbl.length tbl != 0
      );
    )

  let compute_is_unit tbl concl cl =
    let neg_concl = normalize_negations (T.Form.not_ concl) in
    let neg_concl_flip = normalize_negations (T.Form.not_ (flip_eq concl)) in
    T.Tbl.find_opt tbl neg_concl
    |> CCOpt.(<+>) (T.Tbl.find_opt tbl neg_concl_flip)
    |> CCOpt.(<$>) (CS.add cl)

  (* find already stored implications a -> b such that premise\sigma = b.
     Then, store the implication a -> concl\sigma   *)
  let extend_concl premise concl cl =
    Util.debugf ~section 3 "transitive conclusion: @[%a@] --> @[%a@]"
      (fun k -> k T.pp premise T.pp concl);
    let to_add_concl = ref [] in
    let max_imps = Env.flex_get k_max_imp_entries in
    retrieve_spec_concl_idx () (premise,q_sc)
    |> Iter.iter (fun (concl',premise',subst) ->
      (* add implication premise' -> subst (concl) *)
      prems_ := PremiseIdx.update_leaf !prems_ premise' (fun (tbl, is_unit) -> 
        (match T.Tbl.get tbl concl' with
        | Some old_proofset ->
          if CS.cardinal old_proofset < Env.flex_get k_max_depth then (
            let concl = (Subst.FO.apply Subst.Renaming.none subst (concl, q_sc)) in
            if not @@ T.Tbl.mem tbl concl && T.depth concl <= 4 && T.Tbl.length tbl <= max_imps then(
              let proofset = CS.add cl old_proofset in
              register_cl_term cl premise';
              to_add_concl := (concl, premise', proofset, tbl) :: !to_add_concl))
        | None -> assert false;);
        (* if by adding concl something became unit we remove
           the leaf as the new one with updated unit status will be added *)
        true
      ));
    
    CCList.iter (fun (c,premise,ps,tbl) -> 
      register_conclusion ~tbl ~premise c ps) !to_add_concl;
    (* checking if the literal became unit *)
    CCList.iter (fun (c,premise,ps,tbl) -> 
      match compute_is_unit tbl c cl with
      | Some ps ->
        prems_  := PremiseIdx.add !prems_ premise (tbl, true);
        let neg_prem = normalize_negations (T.Form.not_ premise) in
        register_propagated_lit ~prop_kind:Failed neg_prem cl ps
      | _ -> ()
    ) !to_add_concl

  let extend_premise tbl premise' concl cl =
    let aux concl =
      register_conclusion ~tbl ~premise:premise' concl (CS.singleton cl);
      (match T.view concl with
      | T.AppBuiltin(Builtin.Neq, ([_;a;b] | [a;b])) ->
        iter_ctx a b
        |> Iter.iter (fun (a,b) ->
          let new_neq = T.Form.neq a b in
          register_conclusion ~tbl ~premise:premise' new_neq (CS.singleton cl));
      | _ -> ());
      register_cl_term cl premise';

      let max_proof_size = Env.flex_get k_max_depth in
      retrieve_gen_prem_idx () (concl, q_sc)
      |> Iter.iter (fun (_,(tbl', _),subst) -> 
        let to_add = ref [] in
        T.Tbl.to_iter tbl'
        |> Iter.iter (fun (t,proof_set) ->
          if C.ClauseSet.cardinal proof_set < max_proof_size then (
            let concl = (Subst.FO.apply Subst.Renaming.none subst (t, idx_sc)) in
            if T.depth concl <= 4 then (
              let new_cls = CS.add cl proof_set in
              CS.iter (fun cl -> register_cl_term cl premise') new_cls;
              to_add := (concl, new_cls) :: !to_add);
          ));
        CCList.iter (fun (c,n) -> 
          register_conclusion ~tbl ~premise:premise' c n) !to_add
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
    if Trail.is_empty (C.trail cl) then (
      match C.lits cl with
      | [| (L.Equation(lhs, _, _) as l) |] when L.is_predicate_lit l ->
        Some (lit_to_term lhs (L.is_positivoid l))
      | [| L.Equation(lhs, rhs, sign) |] when tracking_eq () ->
        Some (lit_to_term (T.Form.eq lhs rhs) (sign))
      | _ -> None)
    else None

  let add_new_premise premise concl cl =
    let alpha_renaming = 
      retrieve_spec_prem_idx () (premise, q_sc)
      |> Iter.find (fun (premise', tbl, subst) ->  
        if Subst.is_renaming subst then Some (premise', subst)
        else None
      ) in

    match alpha_renaming with
    | Some (premise', subst) ->
      let concl = Subst.FO.apply Subst.Renaming.none subst (concl, q_sc) in
      let became_unit = ref None in
      prems_ := PremiseIdx.update_leaf !prems_ premise' (fun (tbl,is_unit) -> 
        if not (T.Tbl.mem tbl concl) && not (T.Tbl.mem tbl (flip_eq concl)) &&
           T.Tbl.length tbl <= Env.flex_get k_max_imp_entries  then (
          extend_premise tbl premise' concl cl;
          if not is_unit then (
            (match compute_is_unit tbl concl cl with
            | Some ps -> became_unit := Some (ps, tbl)
            | None -> ());
          )
        );
        (* if by adding concl something became unit we remove
           the leaf as the new one with updated unit status will be added *)
        CCOpt.is_none !became_unit
      );

      (match !became_unit with 
      | Some (ps,tbl) ->
        ignore(PremiseIdx.update_leaf !prems_ premise' (fun (tbl, is_unit) -> assert false));
        prems_ := PremiseIdx.add !prems_ premise' (tbl,true);
        let neg_prem = normalize_negations (T.Form.not_ premise') in
        register_propagated_lit ~prop_kind:Failed neg_prem cl ps
      | None -> ())
    | _ ->
      let tbl = T.Tbl.create 64 in
      extend_premise tbl premise concl cl;
      match compute_is_unit tbl concl cl with 
      | Some ps ->
        prems_ := PremiseIdx.add !prems_ premise (tbl,true);
        let neg_prem = normalize_negations (T.Form.not_ premise) in
        register_propagated_lit ~prop_kind:Failed neg_prem cl ps 
      | None -> prems_ := PremiseIdx.add !prems_ premise (tbl,false)
      
    

  let normalize_variables premise concl = 
    let to_rename = T.VarSet.diff (T.vars concl) (T.vars premise) in
    if T.VarSet.is_empty to_rename then premise,concl
    else (
      let renamer = T.VarSet.fold (fun var subst ->
        let ty = HVar.ty var in
        Subst.FO.bind' subst (var, 0) (T.var (HVar.fresh ~ty ()), 0)
      ) to_rename Subst.empty in
      premise, Subst.FO.apply Subst.Renaming.none renamer (concl,0)
    )

  let max_t_depth = 3 
  let insert_implication premise concl cl =
    if T.depth premise <= max_t_depth && T.depth concl <= max_t_depth &&
       not (T.equal premise concl) &&
       not (T.equal premise (flip_eq concl) &&
       not (generalization_present premise concl)) then (
      Util.debugf ~section 3 "inserting @[%a@] -> @[%a@]" (fun k -> k T.pp premise T.pp concl);
      let premise, concl = normalize_variables premise concl in
      remove_instances premise concl;
      extend_concl premise concl cl;
      add_new_premise premise concl cl;
    )

  let insert_into_indices cl =
    match CCArray.map get_predicate (C.lits cl) with
    | [| Some (a_lhs, a_sign); Some (b_lhs, b_sign) |] ->
      let elig = 
        if Env.flex_get k_insert_only_ordered 
        then C.eligible_param (cl,0) Subst.empty
        else CCBV.create ~size:2 true in
      if (CCBV.get elig 0) then ( 
        insert_implication (lit_to_term ~negate:true a_lhs a_sign)
                           (lit_to_term b_lhs b_sign) cl);
      if (CCBV.get elig 1) then ( 
        insert_implication (lit_to_term ~negate:true b_lhs b_sign)
                           (lit_to_term a_lhs a_sign) cl)
    | _ -> ()
  
  let can_track_bin_cl cl =
    cl_is_ht_trackable cl &&
    (Env.flex_get k_max_tracked_clauses == -1 || 
     !tracked_binary <= Env.flex_get k_max_tracked_clauses)
  
  let can_track_unary_cl cl =
    Env.flex_get k_unit_propagated_hle &&
    (Env.flex_get k_max_tracked_clauses == -1 || 
     !tracked_unary <= 4*Env.flex_get k_max_tracked_clauses)


  let steps = ref 0
  let track_clause cl =
    try
      if Env.flex_get k_heartbeat_disabled_hlbe then raise RuleNotApplicable;
      (match Env.flex_get k_heartbeat_steps with
      | Some h_steps when !steps!=0 && !steps mod h_steps = 0 ->
        if !heartbeat_ then heartbeat_ := false
        else (
          CCFormat.printf "disabling heartbeat %d@." !steps;
          Env.flex_add k_heartbeat_disabled_hlbe true;
          raise RuleNotApplicable;
        )
      | _ -> ());
      if can_track_bin_cl cl then (
        Util.debugf ~section 2 "tracking @[%a@]" (fun k -> k C.pp cl);
        
        insert_into_indices cl;
        incr tracked_binary;
      ) else if can_track_unary_cl cl then (
        match get_unit_predicate cl with
        | Some unit ->
          react_unit_added cl unit;
          incr tracked_unary
        | None -> ());
    with RuleNotApplicable -> ()

  let make_tauto ~proof =
    C.create ~penalty:1 ~trail:Trail.empty [Literal.mk_tauto] proof

  let penalize_hidden_tautology cl = 
    if Env.flex_get k_penalize_tautologies &&
       not @@ ID.Set.exists (fun id -> Signature.sym_in_conj id (Env.signature ())) 
      (C.symbols (Iter.singleton cl)) 
    then (C.inc_penalty cl (C.length cl - 1))

  let find_implication cl premise concl =
    retrieve_gen_prem_idx () (premise, q_sc)
    |> Iter.find (fun (premise', (tbl,_), subst) -> 
      T.Tbl.to_iter tbl
      |> Iter.find (fun (concl', proofset) ->
        try
          if CS.mem cl proofset then None
          else (
            let subst = matching_eq ~decompose:true ~subst ~pattern:(concl', idx_sc) (concl, q_sc) in
            Some(premise', concl', proofset, subst))
        with Unif.Fail -> None)) 

  let do_unit_hle_htr cl =
    let n = C.length cl in
    let bv = CCBV.create ~size:n true in
    let exception UnitHTR of int * (CS.t * propagation_kind) in
    let proofset = ref CS.empty in
    try
      if Env.flex_get k_heartbeat_disabled_hlbe then raise RuleNotApplicable;
      if n>7 then raise RuleNotApplicable;
      CCArray.iteri (fun i lit -> 
        match get_predicate lit with
        | Some (i_lhs, i_sign) ->
          let i_t = lit_to_term (i_lhs) (i_sign) in
          let i_neg_t = lit_to_term ~negate:true (i_lhs) (i_sign) in
          
          if n!=1 && Env.flex_get k_reduce_tautologies then (
            retrieve_idx ~getter:(PropagatedLitsIdx.retrieve_generalizations (!propagated_, idx_sc)) 
              (i_t, q_sc)
            |> Iter.head
            |> CCOpt.iter (fun (_,ps,_) -> raise (UnitHTR(i,ps))));

          if Env.flex_get k_delete_lits then (
            retrieve_idx ~getter:(PropagatedLitsIdx.retrieve_generalizations (!propagated_, idx_sc)) (i_neg_t, q_sc)
            |> Iter.head
            |> CCOpt.iter (fun (_,(ps, _),_) -> 
              proofset := CS.union ps !proofset;
              CCBV.reset bv i))
        | None -> ()
      ) (C.lits cl);

      if CCBV.is_empty (CCBV.negate bv) then None
      else (
        let lit_l = List.rev @@ CCBV.select bv (C.lits cl) in
        let proof = 
          Proof.Step.simp ~rule:(Proof.Rule.mk "unit_hle/fle")
          (List.map C.proof_parent (cl :: CS.to_list !proofset))
        in
        let res = C.create ~penalty:(C.penalty cl) ~trail:(C.trail cl) lit_l proof in

        Util.debugf ~section 2 "simplified[fle]: @[%a@] --> @[%a@]" 
          (fun k -> k C.pp cl C.pp res);
        Util.debugf ~section 2 "used: @[%a@]" (fun k -> k (CS.pp C.pp) !proofset);

        Some (res))

    with UnitHTR(idx, (ps, prop_kind)) -> 
      let lit_l = [CCArray.get (C.lits cl) idx] in
      let proof =
        Proof.Step.simp ~rule:(Proof.Rule.mk (if prop_kind = Failed then "ftr" else "unit_htr"))
        (List.map C.proof_parent (cl :: CS.to_list ps))
      in
      let repl = C.create ~penalty:(C.penalty cl) ~trail:(C.trail cl) lit_l proof in
      penalize_hidden_tautology repl;

      Util.debugf ~section 2 "simplified[unit_htr]: @[@[%a@] --> @[%a@]@]" 
        (fun k -> k C.pp cl C.pp repl);

      Some (repl)
    | RuleNotApplicable -> None

  let do_hte_hle cl =
    let exception HiddenTauto of int * int * CS.t in

    let n = C.length cl in
    try
      if Env.flex_get k_heartbeat_disabled_hlbe then raise RuleNotApplicable;
      if n > 7 then raise RuleNotApplicable; 
      let bv = CCBV.create ~size:n true in
      let (<+>) = CCOpt.(<+>) in
      let proofset = ref CS.empty in
      CCArray.iteri (fun i i_lit ->
        match get_predicate i_lit with
        | Some(i_lhs, i_sign) when CCBV.get bv i -> 
          let i_t = lit_to_term (i_lhs) (i_sign) in
          let i_neg_t = lit_to_term ~negate:true (i_lhs) (i_sign) in
          CCArray.iteri (fun j j_lit ->
            begin match get_predicate j_lit with
            | Some (j_lhs, j_sign) when CCBV.get bv j && i!=j ->
              let j_t = lit_to_term (j_lhs) (j_sign) in
              let j_neg_t = lit_to_term ~negate:true (j_lhs) (j_sign) in
              if Env.flex_get k_reduce_tautologies && C.length cl != 2 then (
                (match find_implication cl i_neg_t j_t
                       <+> find_implication cl j_neg_t i_t with
                | Some (lit_a, lit_b, proofset, subst) 
                    when (not (CS.mem cl proofset)) && 
                        (C.length cl != 2 || not (Subst.is_renaming subst)) ->
                  (* stopping further search *)
                  raise (HiddenTauto (i, j, proofset))
                | _ -> ())
              );
              if Env.flex_get k_delete_lits then (
                (match find_implication cl i_neg_t j_neg_t
                        <+> find_implication cl j_t i_t with
                  | Some (_, _, proofset',subst) ->
                    CCBV.reset bv j;

                    Util.debugf ~section 3 "@[%a@] --> @[%a@]" 
                      (fun k -> k T.pp i_neg_t T.pp j_neg_t);
                    Util.debugf ~section 3 "used(%d): @[%a@]" 
                      (fun k -> k j (CS.pp C.pp) proofset');

                    proofset := CS.union proofset' !proofset
                  | _ -> () )
              )
            | _ -> () end
          ) (C.lits cl)
        | _ -> ()
      ) (C.lits cl);
      
      if CCBV.is_empty (CCBV.negate bv) then None
      else (
        let lit_l = List.rev @@ CCBV.select bv (C.lits cl) in
        let proof = 
          Proof.Step.simp ~rule:(Proof.Rule.mk "hidden_literal_elimination")
          (List.map C.proof_parent (cl :: CS.to_list !proofset))
        in
        let res = C.create ~penalty:(C.penalty cl) ~trail:(C.trail cl) lit_l proof in

        Util.debugf ~section 2 "simplified[hle]: @[%a@] --> @[%a@]" 
          (fun k -> k C.pp cl C.pp res);
        Util.debugf ~section 2 "used: @[%a@]" (fun k -> k (CS.pp C.pp) !proofset);

        Some (res))
    with HiddenTauto(i,j,proofset) ->
      let lit_l = [CCArray.get (C.lits cl) i; CCArray.get (C.lits cl) j] in
      let proof = 
        Proof.Step.simp ~rule:(Proof.Rule.mk "hidden_tautology_elimination")
        (List.map C.proof_parent (cl :: CS.to_list proofset))
      in
      let repl = C.create ~penalty:(C.penalty cl) ~trail:(C.trail cl) lit_l proof in 
      penalize_hidden_tautology repl;
      Some repl 
      |> CCFun.tap (function
        | Some res ->
          Util.debugf ~section 2 "HTR(@[%a@])=@[%a@]@. > @[%a@]" 
            (fun k -> k C.pp cl C.pp res (CS.pp C.pp) proofset);
        | _ -> ())
    | RuleNotApplicable -> None


  let do_context_simplification cl =
    let lits_to_keep = CCBV.create ~size:(C.length cl) true in
    let exception StopIteration in
    CCArray.iteri (fun i lit ->
      if CCBV.get lits_to_keep i && not (Lit.is_predicate_lit lit) then (
        try 
          CCArray.iteri (fun j lit' -> 
            if i!=j && CCBV.get lits_to_keep j && not (Lit.is_predicate_lit lit')
                && Lit.is_positivoid lit = Lit.is_positivoid lit' then (
              match Lit.View.as_eqn lit, Lit.View.as_eqn lit' with
              | Some(s,t,sign), Some(ctx_s, ctx_t, _) ->
                iter_ctx ctx_s ctx_t (fun (s',t') -> 
                  if (T.equal s s' && T.equal t t') || 
                    (T.equal s t' && T.equal t s') then (
                    if sign then  (
                      (* if we found a pair of literals i:s=t and j:u[s]=u[t] 
                        then we remove the first one *)
                      CCBV.reset lits_to_keep i;
                      raise StopIteration;
                    ) else (CCBV.reset lits_to_keep j)
                    (* else the pair is i:s!=t and j:u[s]!=u[t] and the literal j
                      can be removed *)
                  ))
              | _ -> ()
            )) (C.lits cl);
        with StopIteration -> ()
      )) (C.lits cl);
    if CCBV.is_empty (CCBV.negate lits_to_keep) then None
    else (
      let lit_l = List.rev @@ CCBV.select lits_to_keep (C.lits cl) in
      let proof = 
        Proof.Step.simp 
          ~rule:(Proof.Rule.mk "eq_context_simplification") 
          ([C.proof_parent cl])
      in
      let res = C.create ~penalty:(C.penalty cl) ~trail:(C.trail cl) lit_l proof in
      Some (res))

  let simplify_opt ~f cl =
    (* other rules will take care of this *)
    if is_tauto cl then (SimplM.return_same cl)
    else (match f cl with
    | Some cl' -> SimplM.return_new cl'
    | None -> SimplM.return_same cl)

  let [@inline] check_heartbeat arg =
    if CCOpt.is_some arg then heartbeat_ := true;
    arg 

  let hle_htr = simplify_opt ~f:(fun a -> check_heartbeat @@ do_hte_hle a)

  let unit_hle_htr = simplify_opt ~f:(fun a -> check_heartbeat @@ do_unit_hle_htr a)

  let ctx_simpl = simplify_opt ~f:do_context_simplification

  let untrack_clause cl =
    (match Util.Int_map.get (C.id cl) !cl_occs with
    | Some (premises, propagated) ->
      Term.Set.iter (fun premise ->
        prems_ := PremiseIdx.update_leaf !prems_ premise (fun (tbl,_) -> 
          T.Tbl.filter_map_inplace (fun concl proofset -> 
            if CS.mem cl proofset then (
              concls_ := ConclusionIdx.remove !concls_ concl premise;
              None
            ) else Some proofset) tbl;
          T.Tbl.length tbl != 0)) premises;
      Term.Set.iter (fun prop_lit -> 
        propagated_ := PropagatedLitsIdx.update_leaf !propagated_ prop_lit (fun (cs, _) ->
        if (not @@ CS.mem cl cs) then (decr propagated_size_; false) else true
        )
      ) propagated
    | _ -> ());
    if Util.Int_map.mem (C.id cl) !cl_occs then (
      Util.debugf ~section 3 "removed: @[%a@]." (fun k -> k C.pp cl);
      decr tracked_binary;
    );
    cl_occs := Util.Int_map.remove (C.id cl) !cl_occs;
    match get_unit_predicate cl with
    | Some unit ->
      units_ := UnitIdx.remove !units_ unit cl;
      decr tracked_unary;
    | None -> ()
    

  let initialize () =
    let track_active () =
      Signal.on_every Env.ProofState.ActiveSet.on_add_clause track_clause;
      Signal.on_every Env.ProofState.ActiveSet.on_remove_clause untrack_clause;
      Signal.on_every Env.on_forward_simplified (fun (_, _) -> incr steps);
    in
    let track_passive () =
      Signal.on_every Env.ProofState.PassiveSet.on_add_clause track_clause;
      Signal.on_every Env.ProofState.PassiveSet.on_remove_clause untrack_clause;
      Signal.on_every Env.on_forward_simplified (fun (_, _) -> incr steps)
    in
    let track_all () =
      Signal.on_every Env.ProofState.PassiveSet.on_add_clause track_clause;
      Signal.on_every Env.ProofState.ActiveSet.on_remove_clause untrack_clause;
      Signal.on_every Env.on_forward_simplified (fun (c, new_state) -> 
        incr steps;
        match new_state with
        | Some c' ->
          if not (C.equal c c') then (
            untrack_clause c; 
            track_clause c'
          )
        | _ -> untrack_clause c (* c is redundant *))
    in

    let initialize_with_passive () =
      Iter.iter track_clause (E.get_passive ());

      Util.debugf ~section 3 "discovered implications:" CCFun.id;
      PremiseIdx.iter !prems_ (fun premise (tbl,_) -> 
        Util.debugf ~section 3 "@[%a@] --> @[%a@]" (fun k -> k T.pp premise (Iter.pp_seq T.pp) (T.Tbl.keys tbl))
      )
    in
      
    begin match Env.flex_get k_clauses_to_track with
    | `Passive ->
      initialize_with_passive ();
      track_passive ()
    | `Active ->
      track_active ()
    | `All -> 
      initialize_with_passive ();
      track_all ()
    end;
    Signal.StopListening
  


  let setup () =
    if E.flex_get k_enabled then (
      Signal.on Env.on_start initialize;
      let add_simpl = 
        if Env.flex_get k_simpl_new 
        then Env.add_basic_simplify 
        else Env.add_active_simplify
      in

      if Env.flex_get k_basic_rules then add_simpl hle_htr;
      add_simpl ctx_simpl;
      if Env.flex_get k_unit_propagated_hle then (add_simpl unit_hle_htr);
    )
end

let max_depth_ = ref 3
let enabled_ = ref false
let simpl_new_ = ref false
let clauses_to_track_ = ref `Active
let max_self_impls_ = ref 1
let max_tracked_clauses = ref (-1)
let propagated_hle = ref true
let hte_ = ref true
let hle_ = ref true
let track_eq_ = ref false
let insert_ordered_ = ref false
let heartbeat_steps = ref None
let max_imp_ = ref 48
let basic_rules_ = ref true
let penalize_tautologies_ = ref true

let extension =
  let register env =
    let module E = (val env : Env.S) in
    let module HLT = Make(E) in
    E.flex_add k_enabled !enabled_;
    E.flex_add k_max_depth !max_depth_;
    E.flex_add k_simpl_new !simpl_new_;
    E.flex_add k_clauses_to_track !clauses_to_track_;
    E.flex_add k_max_self_impls !max_self_impls_;
    E.flex_add k_unit_propagated_hle !propagated_hle;
    E.flex_add k_max_tracked_clauses !max_tracked_clauses;
    E.flex_add k_track_eq !track_eq_;
    E.flex_add k_delete_lits !hle_;
    E.flex_add k_reduce_tautologies !hte_;
    E.flex_add k_insert_only_ordered !insert_ordered_;
    E.flex_add k_heartbeat_steps !heartbeat_steps;
    E.flex_add k_heartbeat_disabled_hlbe false;
    E.flex_add k_max_imp_entries !max_imp_;
    E.flex_add k_basic_rules !basic_rules_;
    E.flex_add k_penalize_tautologies !penalize_tautologies_;
    HLT.setup ()
  in
  { Extensions.default with
      Extensions.name = "hidden literal elimination";
      prio = 45;
      env_actions=[register]
  }

let () =
  Options.add_opts [
    "--hlbe-elim", Arg.Bool ((:=) enabled_), " enable/disable hidden literal and tautology elimination";
    "--hlbe-elim-max-tracked", Arg.Int ((:=) max_tracked_clauses), " negative value for disabling the limit";
    "--hlbe-elim-lits", Arg.Bool ((:=) hle_), " remove literals using HLBE (hidden-lt-elim must be on)";
    "--hlbe-reduce-tautologies", Arg.Bool ((:=) hte_), " reduce tautologies using HLBE (hidden-lt-elim must be on)";
    "--hlbe-max-depth", Arg.Set_int max_depth_, " max depth of binary implication graph precomputation";
    "--hlbe-simplify-new", Arg.Bool ((:=) simpl_new_), " apply HLTe also when moving a clause from fresh to passive";
    "--hlbe-track-eq", Arg.Bool ((:=) track_eq_), " enable/disable tracking and simplifying equality literals";
    "--hlbe-heartbeat", Arg.Int (fun v -> heartbeat_steps := Some v), 
      " when set to n, every n steps it will be checked if any HLBE simplification is performed." ^
      " If not, any HLBE will be disabled.";
    "--hlbe-clauses-to-track", Arg.Symbol(["all";"passive";"active"], 
      (function 
        | "all" ->
          clauses_to_track_ := `All;
        | "passive" ->
          clauses_to_track_ := `Passive;
        | "active" ->
          clauses_to_track_ := `Active;
        | _ -> ())), 
      " what clauses to use for simplification";
    "--hlbe-max-self-implications", Arg.Int ((:=) max_self_impls_), 
      " how many times do we loop implications of the kind p(X) -> p(f(X)) ";
    "--hlbe-unit-rules", Arg.Bool ((:=) propagated_hle), 
      " do unit-triggered removal of literals ";
    "--hlbe-insert-ordered", Arg.Bool ((:=) insert_ordered_), 
      " for clauses of the form l|r where l > r then insert only ~l -> r ";
    "--hlbe-max-entries", Arg.Int ((:=) max_imp_), 
      " maximal number of entries stored for each element mapped by implication map ";
    "--hlbe-basic-rules", Arg.Bool ((:=) basic_rules_), " enable/disable basic (non unit) rules HLE and HTR";
    "--hlbe-penalize-tautologies", Arg.Bool ((:=) penalize_tautologies_), " penalize hidden tautologies"
  ];
  Extensions.register extension
