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
let k_unit_htr = Flex_state.create_key ()
let k_hte = Flex_state.create_key ()
let k_hle = Flex_state.create_key ()
let k_max_tracked_clauses = Flex_state.create_key ()
let k_track_eq = Flex_state.create_key ()
let k_insert_only_ordered = Flex_state.create_key ()


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
      -> clauses necessary for the proof
     with an additional field storing whether there are literals
     l, \neg l in the set of implied literals*)
  module PremiseIdx = NPDtree.MakeTerm(struct 
    type t = (CS.t T.Tbl.t) * (CS.t option)
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

  let prems_ = ref (PremiseIdx.empty ())
  let concls_ = ref (ConclusionIdx.empty ())
  let units_ = ref (UnitIdx.empty ())
  (* occurrences of the clause in the premise_idx *)
  let cl_occs = ref Util.Int_map.empty
  (* binary clauses tracked so far *)
  let tracked_binary = ref 0
  (* binary clauses tracked so far *)
  let tracked_unary = ref 0

  let [@inline] tracking_eq () =
    Env.flex_get k_track_eq

  (* constants denoting the scope of index and the query, respectively *)
  let idx_sc, q_sc = 0, 1

  let app_subst ?(sc=idx_sc) ~subst t =
    Subst.FO.apply Subst.Renaming.none subst (t,sc)

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
      Some (lhs, Lit.is_pos lit)
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
    let premise_set = 
      Term.Set.add premise
      (Util.Int_map.get_or ~default:Term.Set.empty (C.id cl) !cl_occs) in
    cl_occs := Util.Int_map.add (C.id cl) premise_set !cl_occs

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
      (t, sets_to_remove) :: tasks        
    ) [] i)
    |> CCList.iter (fun (t, sets) ->
      prems_ := PremiseIdx.update_leaf !prems_ t (fun (tbl, _) -> 
        T.Tbl.filter_map_inplace (fun concl proofset ->
          concls_ := ConclusionIdx.remove !concls_ concl t;
          if List.exists (fun set -> CS.subset set proofset) sets then None
          else Some proofset) tbl;
        T.Tbl.length tbl == 0
      );
    )

  let compute_is_unit tbl concl cl =
    if Env.flex_get k_unit_htr then (
      let neg_concl = normalize_negations (T.Form.not_ concl) in
      let neg_concl_flip = normalize_negations (T.Form.not_ (flip_eq concl)) in
      T.Tbl.find_opt tbl neg_concl
      |> CCOpt.(<+>) (T.Tbl.find_opt tbl neg_concl_flip)
      |> CCOpt.(<$>) (CS.add cl)
    ) else None


  (* find already stored implications a -> b such that premise\sigma = b.
     Then, store the implication a -> concl\sigma   *)
  let extend_concl premise concl cl =
    Util.debugf ~section 3 "transitive conclusion: @[%a@] --> @[%a@]"
      (fun k -> k T.pp premise T.pp concl);
    let to_add_concl = ref [] in
    retrieve_spec_concl_idx () (premise,q_sc)
    |> Iter.iter (fun (concl',premise',subst) ->
      (* add implication premise' -> subst (concl) *)
      Util.debugf ~section 3 "found: @[%a@] --> @[%a@]"
        (fun k -> k T.pp premise' T.pp concl');
      let became_unit = ref None in
      prems_ := PremiseIdx.update_leaf !prems_ premise' (fun (tbl, is_unit) -> 
        (match T.Tbl.get tbl concl' with
        | Some old_proofset ->
          if CS.cardinal old_proofset < Env.flex_get k_max_depth then (
            let proofset = CS.add cl old_proofset in
            register_cl_term cl premise';
            let concl = (Subst.FO.apply Subst.Renaming.none subst (concl, q_sc)) in
            (* concls_ := ConclusionIdx.add !concls_ concl premise'; 
               To avoid adding to the same object we are iterating over *)
            to_add_concl := (concl, premise') :: !to_add_concl;
            (* ConclusionIdx.pp_keys !concls_; *)
            T.Tbl.add tbl concl proofset;
            if CCOpt.is_none is_unit then (
              match compute_is_unit tbl concl cl with
              | Some proofset -> became_unit := Some(proofset,tbl)
              | None -> ()
            )
          )
        | None -> assert false;);
        (* if by adding concl something became unit we remove
           the leaf as the new one with updated unit status will be added *)
        CCOpt.is_none !became_unit
      );
      (match !became_unit with
      | Some (ps, tbl) ->
        ignore(PremiseIdx.update_leaf !prems_ premise' (fun (tbl, is_unit) -> assert false));
        prems_  := PremiseIdx.add !prems_ premise' (tbl, Some ps)
      | _ ->  ());
    );
    concls_ :=  ConclusionIdx.add_list !concls_ !to_add_concl

  (*  *)
  let extend_premise tbl premise' concl cl =
    let aux concl =
      T.Tbl.add tbl concl (CS.singleton cl);
      concls_ := ConclusionIdx.add !concls_ concl premise';
      (match T.view concl with
      | T.AppBuiltin(Builtin.Neq, ([_;a;b] | [a;b])) ->
        iter_ctx a b
        |> Iter.iter (fun (a,b) ->
          let new_neq = T.Form.neq a b in
          T.Tbl.add tbl new_neq (CS.singleton cl);
          concls_ := ConclusionIdx.add !concls_ new_neq premise';
        );
      | _ -> ());
      register_cl_term cl premise';

      let max_proof_size = Env.flex_get k_max_depth in
      retrieve_gen_prem_idx () (concl, q_sc)
      |> Iter.iter (fun (_,(tbl', _),subst) -> 
        let to_add = ref [] in
        T.Tbl.to_iter tbl'
        |> Iter.iter (fun (t,proof_set) ->
          if C.ClauseSet.cardinal proof_set < max_proof_size then (
            let new_cls = CS.add cl proof_set in
            CS.iter (fun cl -> register_cl_term cl premise') new_cls;
            let concl = (Subst.FO.apply Subst.Renaming.none subst (t, idx_sc)) in
            concls_ := ConclusionIdx.add !concls_ concl premise';
            to_add := (concl, new_cls) :: !to_add;
          ));
        CCList.iter (fun (c,n) -> T.Tbl.add tbl c n) !to_add
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
        Some (lit_to_term lhs (L.is_pos l))
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
        if not (T.Tbl.mem tbl concl) && not (T.Tbl.mem tbl (flip_eq concl)) then (
          extend_premise tbl premise' concl cl;
          if CCOpt.is_none is_unit then (
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
        prems_ := PremiseIdx.add !prems_ premise (tbl,Some ps)
      | None -> ())
    | _ ->
      let tbl = T.Tbl.create 64 in
      extend_premise tbl premise concl cl;
      prems_ := PremiseIdx.add !prems_ premise (tbl,compute_is_unit tbl concl cl)
    

  let insert_implication premise concl cl =
    if not (generalization_present premise concl) &&
       not (T.equal premise concl) &&
       not (T.equal premise (flip_eq concl)) then (
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
    let tracked = !tracked_binary in
    let tracked_max = Env.flex_get k_max_tracked_clauses in
    cl_is_ht_trackable cl &&
    ((Env.flex_get k_clauses_to_track == `All && (Env.is_active cl)) ||
    tracked_max == -1 || tracked <= tracked_max)
  
  let can_track_unary_cl cl =
    let tracked = !tracked_unary in
    let tracked_max = 2*Env.flex_get k_max_tracked_clauses in
    Env.flex_get k_unit_propagated_hle &&
    ((Env.flex_get k_clauses_to_track == `All && (Env.is_active cl)) ||
    tracked_max == -1 || tracked <= tracked_max)


  let track_clause cl =
    if can_track_bin_cl cl then (
      Util.debugf ~section 3 "tracking @[%a@]" (fun k -> k C.pp cl);
      
      insert_into_indices cl;
      if not (Env.flex_get k_clauses_to_track == `All && Env.is_active cl )
      then incr tracked_binary;
      
      Util.debugf ~section 2 "idx_size: @[%d@]" (fun k -> k (PremiseIdx.size !prems_));
      Util.debugf ~section 3 "premises:" CCFun.id;
      PremiseIdx.iter !prems_ (fun t (tbl,_) -> 
        Util.debugf ~section 3 "@[%a@] --> @[%a@]" (fun k -> k T.pp t (Iter.pp_seq T.pp) (T.Tbl.keys tbl))
      );
    ) else if can_track_unary_cl cl then (
      match get_unit_predicate cl with
      | Some unit ->
        units_ := UnitIdx.add !units_ unit cl;
        if not (Env.flex_get k_clauses_to_track == `All && Env.is_active cl) 
        then incr tracked_unary
      | None -> ()
    )

  let make_tauto ~proof =
    C.create ~penalty:1 ~trail:Trail.empty [Literal.mk_tauto] proof

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

  exception RuleNotApplicable
  let do_propagated_simpl cl = 
    let bv = CCBV.create ~size:( C.length cl ) true in
    let proofset = ref (CS.empty) in
    let exception PropagatedHTE of int * CS.t in
    let is_unit = C.length cl == 1 in
    try
      if Lits.num_equational (C.lits cl) > 3 || Array.length (C.lits cl) > 7 
      then raise RuleNotApplicable;
      CCArray.iteri (fun i lit -> 
        match get_predicate lit with 
        | Some (lhs, sign) ->
          let lhs_neg = lit_to_term ~negate:true lhs sign in 
          let lhs = lit_to_term lhs sign in
          let unit_sc = (max idx_sc q_sc) + 1 in

          (* checking if we can replace the clause with the literal i  *)
          if is_unit then ()
          else (
            CCOpt.get_or ~default:()
            (
              (* for the literal l we are looking for implications p -> c such that
                  c\sigma = l. Then we see if there is an unit clause p' such that
                  p\sigma = p'\rho *)
              retrieve_gen_concl_idx () (lhs, q_sc)
              |> Iter.find_map (fun (concl, premise, subst) ->
                let orig_premise = premise in
                let premise = Subst.FO.apply Subst.Renaming.none subst (premise, idx_sc) in
                retrieve_gen_unit_idx unit_sc (premise, idx_sc)
                |> Iter.head
                |> CCOpt.map (fun (_, unit_cl, _) -> 
                  prems_ := PremiseIdx.update_leaf !prems_ orig_premise (fun (tbl,_) -> 
                    let proofset' = CS.add unit_cl (T.Tbl.find tbl concl) in
                    if not (CS.mem cl proofset') then (
                      raise (PropagatedHTE(i, proofset'));
                    );
                    true
                  );
                ))));

          (* checking if we can kill the literal i *)
          CCOpt.get_or ~default:()
          (
            (* for the literal l we are looking for implications p -> c such that
                c\sigma = ~l. Then we see if there is an unit clause p' such that
                p\sigma = p'\rho *)
            retrieve_gen_concl_idx () (lhs_neg, q_sc)
            |> Iter.find_map (fun (concl, premise, subst) ->
              let orig_premise = premise in
              let premise = Subst.FO.apply Subst.Renaming.none subst (premise, idx_sc) in
              retrieve_gen_unit_idx unit_sc (premise, idx_sc)
              |> Iter.head
              |> CCOpt.map (fun (_, unit_cl, _) -> 
                prems_ := PremiseIdx.update_leaf !prems_ orig_premise (fun (tbl,_) -> 
                  let proofset' = T.Tbl.find tbl concl in
                  if not (CS.mem cl proofset') then (
                    proofset := CS.union (CS.add unit_cl (proofset')) !proofset;
                    CCBV.reset bv i);
                  true
                ))))
        | None -> ()
      ) (C.lits cl);
      if CCBV.is_empty (CCBV.negate bv) then None
      else (
        let lit_l = List.rev (CCBV.select bv (C.lits cl)) in
        let proof = 
          Proof.Step.simp ~rule:(Proof.Rule.mk "propagated_hle")
          (List.map C.proof_parent (cl :: CS.to_list !proofset))
        in
        Some (C.create ~penalty:(C.penalty cl) ~trail:(C.trail cl) lit_l proof)
      )
    with PropagatedHTE(i, proofset) ->
      let lit_l = [CCArray.get (C.lits cl) i] in
      let proof = 
        Proof.Step.simp ~rule:(Proof.Rule.mk "propagated_htr")
        (List.map C.proof_parent (cl :: CS.to_list proofset))
      in
      let repl = C.create ~penalty:(C.penalty cl) ~trail:(C.trail cl) lit_l proof in

      Util.debugf ~section 1 "simplified[unit_htr]: @[@[%a@] --> @[%a@]@]@. using @[%a@]"
        (fun k -> k C.pp cl C.pp repl (CS.pp C.pp) proofset);

      (* E.add_passive (Iter.singleton repl); *)
      Some (repl)
    | RuleNotApplicable -> None
  
  let unit_simplify cl =
    let exception UnitHTR of int * CS.t in
    let n = C.length cl in
    let bv = CCBV.create ~size:n true in
    let proofset = ref CS.empty in
    try
      if Lits.num_equational (C.lits cl) > 3 || Array.length (C.lits cl) > 7 
      then raise RuleNotApplicable;

      CCArray.iteri (fun i i_lit ->
        match get_predicate i_lit with
        | Some(i_lhs, i_sign) ->
          let i_t = lit_to_term (i_lhs) (i_sign) in
          let i_neg_t = lit_to_term ~negate:true (i_lhs) (i_sign) in
          let unit_htr () = 
            retrieve_gen_prem_idx () (i_neg_t, q_sc)
            |> Iter.find_map (fun (_, (_,is_unit), subst) -> 
              if Subst.is_renaming subst then None else is_unit)
          in
          let unit_hle () = 
            retrieve_gen_prem_idx () (i_t, q_sc)
            |> Iter.find_map (fun (_, (_,is_unit), _) -> is_unit)
          in
          (match unit_htr () with
          | Some cs -> raise (UnitHTR(i, cs))
          | None -> (
              match unit_hle () with
              | Some cs ->
                CCBV.reset bv i;
                proofset := CS.union cs !proofset
              | None -> ()
          ))
        | None -> ()
      ) (C.lits cl);
      if CCBV.is_empty (CCBV.negate bv) then None
      else (
        let lit_l = List.rev @@ CCBV.select bv (C.lits cl) in
        let proof = 
          Proof.Step.simp ~rule:(Proof.Rule.mk "unit_hle")
          (List.map C.proof_parent (cl :: CS.to_list !proofset))
        in
        let res = C.create ~penalty:(C.penalty cl) ~trail:(C.trail cl) lit_l proof in

        Util.debugf ~section 2 "simplified[hle]: @[%a@] --> @[%a@]" 
          (fun k -> k C.pp cl C.pp res);
        Util.debugf ~section 2 "used: @[%a@]" (fun k -> k (CS.pp C.pp) !proofset);

        Some (res))
    with UnitHTR(i, proofset) when n!=1 ->
      let lit_l = [CCArray.get (C.lits cl) i] in
      let proof = 
        Proof.Step.simp ~rule:(Proof.Rule.mk "unit_htr")
        (List.map C.proof_parent (cl :: CS.to_list proofset))
      in
      let repl = C.create ~penalty:(C.penalty cl) ~trail:(C.trail cl) lit_l proof in

      Util.debugf ~section 1 "simplified[unit_htr]: @[@[%a@] --> @[%a@]@]" 
        (fun k -> k C.pp cl C.pp repl);

      Some (repl)
    | _ -> None

  let do_hte_hle cl =
    let exception HiddenTauto of int * int * CS.t in

    let n = C.length cl in
    if Lits.num_equational (C.lits cl) <= 3 && n <= 7 then (
      try 
        let bv = CCBV.create ~size:n true in
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
                if Env.flex_get k_hte && C.length cl != 2 then (
                  (match find_implication cl i_neg_t j_t with
                  | Some (lit_a, lit_b, proofset, subst) 
                      when (not (CS.mem cl proofset)) && 
                          (C.length cl != 2 || not (Subst.is_renaming subst)) ->
                    (* stopping further search *)
                    raise (HiddenTauto (i, j, proofset))
                  | _ -> ())
                );
                if Env.flex_get k_hle then (
                  let (<+>) = CCOpt.(<+>) in
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
        Some (C.create ~penalty:(C.penalty cl) ~trail:(C.trail cl) lit_l proof)
        |> CCFun.tap (function
          | Some res ->
            Util.debugf ~section 1 "HTR(@[%a@])=@[%a@]@. > @[%a@]" 
              (fun k -> k C.pp cl C.pp res (CS.pp C.pp) proofset);
          | _ -> ())
    ) else None

  let do_context_simplification cl =
    let lits_to_keep = CCBV.create ~size:(C.length cl) true in
    let exception StopIteration in
    CCArray.iteri (fun i lit ->
      if CCBV.get lits_to_keep i && not (Lit.is_predicate_lit lit) then (
        try 
          CCArray.iteri (fun j lit' -> 
            if i!=j && CCBV.get lits_to_keep j && not (Lit.is_predicate_lit lit')
                && Lit.is_pos lit = Lit.is_pos lit' then (
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
    match f cl with
    | Some cl' -> SimplM.return_new cl'
    | None -> SimplM.return_same cl

  let simplify_cl = simplify_opt ~f:do_hte_hle

  let propagated_hle_hte = simplify_opt ~f:do_propagated_simpl

  let unit_htr = simplify_opt ~f:unit_simplify

  let ctx_simpl = simplify_opt ~f:do_context_simplification

  let untrack_clause cl =
    (match Util.Int_map.get (C.id cl) !cl_occs with
    | Some premises ->
      Term.Set.iter (fun premise ->
        prems_ := PremiseIdx.update_leaf !prems_ premise (fun (tbl,_) -> 
          T.Tbl.filter_map_inplace (fun concl proofset -> 
            if CS.mem cl proofset then (
              concls_ := ConclusionIdx.remove !concls_ concl premise;
              None
            ) else Some proofset) tbl;
          T.Tbl.length tbl != 0)) premises;
    | _ -> ());
    if Util.Int_map.mem (C.id cl) !cl_occs then (
      Util.debugf ~section 3 "removed: @[%a@]." (fun k -> k C.pp cl);
      if not (Env.flex_get k_clauses_to_track == `All && Env.is_active cl) 
      then decr tracked_binary;
    );
    cl_occs := Util.Int_map.remove (C.id cl) !cl_occs;
    match get_unit_predicate cl with
    | Some unit ->
      units_ := UnitIdx.remove !units_ unit cl;
      if not (Env.flex_get k_clauses_to_track == `All && Env.is_active cl)
      then decr tracked_unary;
    | None -> ()
    

  let initialize () =
    let track_active () =
      Signal.on_every Env.ProofState.ActiveSet.on_add_clause track_clause;
      Signal.on_every Env.ProofState.ActiveSet.on_remove_clause untrack_clause
    in
    let track_passive () =
      Signal.on_every Env.ProofState.PassiveSet.on_add_clause track_clause;
      Signal.on_every Env.ProofState.PassiveSet.on_remove_clause untrack_clause
    in
    let track_all () =
      Signal.on_every Env.ProofState.PassiveSet.on_add_clause track_clause;
      Signal.on_every Env.ProofState.ActiveSet.on_remove_clause untrack_clause;
      Signal.on_every Env.on_forward_simplified (fun (c, new_state) -> 
        match new_state with
        | Some c' ->
          if not (C.equal c c') then (
            untrack_clause c; 
            track_clause c'
          )
        | _ -> untrack_clause c (* c is redundant *))
    in

    let initialize_with_passive () =
      assert (Iter.is_empty @@ E.get_active ());
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

      add_simpl simplify_cl;
      if Env.flex_get k_track_eq then (add_simpl ctx_simpl);
      if Env.flex_get k_unit_propagated_hle then (add_simpl propagated_hle_hte);
      if Env.flex_get k_unit_htr then (add_simpl unit_htr)
    )
end

let max_depth_ = ref 3
let enabled_ = ref false
let simpl_new_ = ref false
let clauses_to_track_ = ref `Active
let max_self_impls_ = ref 1
let max_tracked_clauses = ref (-1)
let propagated_hle = ref true
let unit_htr_ = ref true
let hte_ = ref true
let hle_ = ref true
let track_eq_ = ref false
let insert_ordered_ = ref false


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
    E.flex_add k_unit_htr !unit_htr_;
    E.flex_add k_max_tracked_clauses !max_tracked_clauses;
    E.flex_add k_track_eq !track_eq_;
    E.flex_add k_hle !hle_;
    E.flex_add k_hte !hte_;
    E.flex_add k_insert_only_ordered !insert_ordered_;
    HLT.setup ()
  in
  { Extensions.default with
      Extensions.name = "hidden literal elimination";
      prio = 100;
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
    "--hidden-lt-track-eq", Arg.Bool ((:=) track_eq_), " enable/disable tracking and simplifying equality literals";
    "--hidden-lt-clauses-to-track", Arg.Symbol(["all";"passive";"active"], 
      (function 
        | "all" ->
          clauses_to_track_ := `All;
        | "passive" ->
          clauses_to_track_ := `Passive;
        | "active" ->
          clauses_to_track_ := `Active;
        | _ -> ())), 
      " what clauses to use for simplification";
    "--hidden-lt-max-self-implications", Arg.Int ((:=) max_self_impls_), 
      " how many times do we loop implications of the kind p(X) -> p(f(X)) ";
    "--hidden-lt-propagated-hle", Arg.Bool ((:=) propagated_hle), 
      " do unit-triggered removal of literals ";
    "--hidden-lt-unit-htr", Arg.Bool ((:=) unit_htr_), 
      " do unit hidden tautology removal ";
    "--hidden-lt-insert-ordered", Arg.Bool ((:=) insert_ordered_), 
      " for clauses of the form l|r where l > r then insert only ~l -> r ";
  ];
  Extensions.register extension
