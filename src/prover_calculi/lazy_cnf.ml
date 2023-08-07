open Logtk
open Libzipperposition

module L = Literal
module Ls = Literals
module T = Term

let enabled = ref false
let _eager_lcnf = ref false

let k_lazy_cnf_kind = Flex_state.create_key ()
let k_lazy_cnf_eager = Flex_state.create_key ()
let k_renaming_threshold = Flex_state.create_key ()
let k_rename_eq = Flex_state.create_key ()
let k_scoping = Flex_state.create_key ()
let k_skolem_mode = Flex_state.create_key ()
let k_enum_bool_funs = Flex_state.create_key ()
let k_replace_bool_fun_quants = Flex_state.create_key ()
let k_pa_renaming = Flex_state.create_key ()
let k_only_eligible = Flex_state.create_key ()
let k_penalize_eq_cnf = Flex_state.create_key ()
let k_clausify_eq_max_nonint = Flex_state.create_key ()
let k_clausify_implications = Flex_state.create_key ()
let k_simp_limit = Flex_state.create_key ()
let k_simplify_quant = Flex_state.create_key ()
let k_inf_quant = Flex_state.create_key ()

let section = Util.Section.make ~parent:Const.section "lazy_cnf"

module type S = sig
  module Env : Env.S
  module C : module type of Env.C with type t = Env.C.t

  (** {5 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)

  val update_form_counter: action:[< `Decrease | `Increase ] -> C.t -> unit
end

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module Combs = Combinators.Make(Env)
  module FR = Env.FormRename
  
  let _form_counter = Term.Tbl.create 256
  let _counted_clauses = ref Util.Int_set.empty

  let update_form_counter ~action c =
    let should_update c =
      not (FR.is_renaming_clause c) &&
      (* we make sure than we do not add or remove the clause twice *)
      (match action with
      | `Increase ->
        if Util.Int_set.mem (C.id c) !_counted_clauses then false
        else (
          _counted_clauses := Util.Int_set.add (C.id c) !_counted_clauses;
          true
        )
      | `Decrease -> 
        if not (Util.Int_set.mem (C.id c) !_counted_clauses) then false
        else (
          _counted_clauses := Util.Int_set.remove (C.id c) !_counted_clauses;
          true;
        ))
    in
    if should_update c then (
      Ls.fold_eqn 
        ~both:false ~ord:(E.Ctx.ord ()) ~eligible:(C.Eligible.always) 
        (C.lits c)
      |> Iter.iter (fun (lhs,rhs,_,pos) ->
        let i,_ = Ls.Pos.cut pos in
        let lit = (C.lits c).(i) in
        let terms = 
          if L.is_predicate_lit lit && T.is_appbuiltin lhs  then [lhs]
          else if Type.is_prop (T.ty lhs) && not (L.is_predicate_lit lit) &&
                  (Term.is_appbuiltin lhs || T.is_appbuiltin rhs) then
            [T.Form.equiv lhs rhs]
          else [] in
        List.iter (fun t -> 
          match action with
            | `Increase -> Term.Tbl.incr _form_counter t
            | `Decrease -> Term.Tbl.decr _form_counter t
        ) terms))
    else (
      (* CCFormat.printf "not updating @[%a@]@." C.pp c; *)
    )

  let fold_lits c =
    let lits = Ls.fold_eqn_simple (C.lits c) in
    if Env.flex_get k_only_eligible then (
      let eligible = C.eligible_res_no_subst c in
      Iter.filter (fun (_,_,_,p) -> CCBV.get eligible (Ls.Pos.idx p)) lits
    ) else lits

  (* if k_clausify_eq_max_nonint is disabled, then we will not clausify
       if the max side is non-interpreted *)
  let check_eq_cnf_ordering_conditions lhs rhs =
    let is_noninterpeted t = CCOpt.is_some (Term.head t) in
    let ord = E.Ctx.ord () in
    (E.flex_get k_clausify_eq_max_nonint) ||
    (match Ordering.compare ord lhs rhs with
    | Comparison.Lt -> is_noninterpeted rhs
    | Comparison.Gt -> is_noninterpeted lhs
    | _ -> is_noninterpeted lhs || is_noninterpeted rhs)

  let _ty_map = Type.Tbl.create 16

  let init_ty_map () =
    Type.Tbl.add _ty_map Type.prop [T.true_; T.false_]

  let rec enum_bool_funs ~ty =
    assert(Iter.for_all (fun ty -> Type.is_fun ty || Type.is_prop ty) (Type.Seq.sub ty));

    let insert_defining_clauses id sym arg_combs table = 
      List.iteri (fun i arg_comb ->
        let lhs = T.app sym arg_comb in
        let rhs = table.(i) in
        let lits = [Literal.mk_eq lhs rhs] in
        let proof = Proof.Step.define_internal id [] in
        let res = C.create ~penalty:1 ~trail:Trail.empty lits proof in
        Util.debugf ~section 2 "defining: @[%a@]@." (fun k -> k C.pp res);
        Env.add_passive (Iter.singleton res)
      ) (arg_combs)
    in

    let create_members arg_ms ret_m =
      assert(not (CCList.is_empty ret_m));

      let arg_combinations = CCList.cartesian_product arg_ms in
      let arg_comb_num = List.fold_left (fun acc a -> acc * (List.length a)) 1 arg_ms in
      let fun_table = CCArray.create arg_comb_num (List.hd ret_m) in

      let iter_funs () = 
        let rec aux i k =
          if i == arg_comb_num then k fun_table
          else (
            List.iter (fun x ->
              fun_table.(i) <- x;
              aux (i+1) k
            ) ret_m )
        in
        aux 0
      in

      let prefix = "finite_domain_fun" in

      iter_funs ()
      |> Iter.map (fun table -> 
        let ((id, id_ty), fresh_sym) = Term.mk_fresh_skolem ~prefix [] ty in
        insert_defining_clauses id fresh_sym arg_combinations table;
        (* let stm = definition_stream id fresh_sym arg_combinations table in
        let stm_res = Env.Stm.make ~penalty:(1) ~parents:[] (stm) in
        Env.StmQ.add (Env.get_stm_queue ()) stm_res;  *)
        Util.debugf ~section 2 "declaring %a" (fun k -> k ID.pp id);
        fresh_sym
      )
      |> Iter.to_list
    in

    if Type.Tbl.length _ty_map == 0 then init_ty_map ();

    try
      Type.Tbl.find _ty_map ty
    with Not_found ->
      let args, ret = Type.open_fun ty in
      let arg_members = List.map (fun ty -> enum_bool_funs ~ty) args in
      let ret_members = enum_bool_funs ~ty:ret in
      let new_members = create_members arg_members ret_members in

      Type.Tbl.add _ty_map ty new_members;

      Env.Ctx.declare_syms (List.map (fun t -> T.head_exn t, T.ty t) new_members);
      Util.debugf ~section 2 "members of type @[%a@]@. >@[%a@]" 
        (fun k -> k Type.pp ty (CCList.pp T.pp) new_members);
      new_members
  
  let estimate_num_clauses sign limit f =
    let exception TooManyClauses in
    let check_cl_size size = 
      if size>limit then raise TooManyClauses;
      size
    in
    let rec aux sign f =
      match T.view f with
      | T.AppBuiltin(Not, [f]) ->
        aux (not sign)  f
      | T.AppBuiltin(And, l) ->
        if sign then sum_l sign l else prod_l sign l
      | T.AppBuiltin(Or, l) ->
        if sign then prod_l sign l else sum_l sign l
      | T.AppBuiltin(Imply, [a;b]) ->
        if sign then prod_l true [T.Form.not_ a; b] 
        else sum_l true [a; T.Form.not_ b]
      | T.AppBuiltin((Eq|Equiv|Neq|Xor) as hd, ([_;a;b]|[a;b])) when Type.is_prop (T.ty a) ->
        if Builtin.equal Eq hd || Builtin.equal Equiv hd then (
          aux true (T.Form.and_ (T.Form.or_ (T.Form.not_ a) b) 
                                (T.Form.or_ (T.Form.not_ b) a))
        ) else (
          aux true (T.Form.and_ (T.Form.or_ (T.Form.not_ a) (T.Form.not_ b)) 
                                (T.Form.or_ a b))
        )
      | T.AppBuiltin((ForallConst|ExistsConst),[_;body]) ->
        let _,body = T.open_fun body in
        aux sign body
      | _ -> 1
    and sum_l sign xs =
      List.fold_left (fun acc f -> 
        check_cl_size @@ acc + aux sign f
      ) 0 xs
    and prod_l sign xs =
      List.fold_left (fun acc f -> 
        check_cl_size @@ (acc * (aux sign f))
      ) 1 xs
    in
    try 
      let estimate = aux sign f in
      Util.debugf ~section 1 "estimate(%b, @[%a@])=@[%d@]@." (fun k -> k sign T.pp f estimate);
      true
    with TooManyClauses -> 
      Util.debugf ~section 1 "estimate(%b, @[%a@])=too many@." (fun k -> k sign T.pp f);
      false
  
  let check_size_limits sign f =
    let limit = Env.flex_get k_simp_limit in
    Util.debugf ~section 1 "checking limit: %d@." (fun k -> k limit);
    Env.flex_get k_lazy_cnf_kind != `Simp ||
    limit < 0 || 
    estimate_num_clauses sign limit f
    

  let proof ~constructor ~name ~parents c =
    constructor ~rule:(Proof.Rule.mk name)
      (List.map C.proof_parent parents)

  let rename_eq ~c ~should_rename lhs rhs sign =
    assert(Type.equal (T.ty lhs) (T.ty rhs));
    assert(Type.is_prop (T.ty lhs));
    assert(T.is_appbuiltin lhs || T.is_appbuiltin rhs);
    assert(not (T.is_true_or_false lhs) || (T.is_true_or_false rhs));

    let yields_clauses f = 
      match T.view f with
      | T.AppBuiltin((Eq|Neq), [ty;_;_]) ->
        Type.is_prop (Type.of_term_unsafe (ty :> InnerTerm.t))
      | T.AppBuiltin(_, _) -> true
      | _ -> false
    in    

    let polarity_aware = Env.flex_get k_pa_renaming in
    let app_sign = if sign then CCFun.id else T.Form.not_ in
    if Env.flex_get k_rename_eq 
    then (
      if (yields_clauses lhs && yields_clauses rhs) then
        (CCOpt.map (fun (r,d,p) -> app_sign r, d, p)
          (FR.rename_form ~should_rename ~polarity_aware ~c (T.Form.equiv lhs rhs) sign))
      else if yields_clauses lhs then
        (CCOpt.map (fun (r,d,p) -> app_sign (T.Form.eq r rhs), d, p) 
          (FR.rename_form ~should_rename ~polarity_aware:false ~c lhs sign))
      else if yields_clauses rhs then
        (CCOpt.map (fun (r,d,p) -> app_sign (T.Form.eq lhs r), d, p) 
          (FR.rename_form ~should_rename ~polarity_aware:false ~c rhs sign))
      else None
    ) else None

  let mk_and ~proof_cons ~rule_name and_args c ?(parents=[c]) lit_idx =
    let lits = CCArray.except_idx (C.lits c) lit_idx in
    let proof = proof ~constructor:proof_cons ~parents ~name:rule_name c  in
    List.map (fun t -> 
      C.create ~penalty:(C.penalty c) ~trail:(C.trail c) 
        (L.mk_true t :: lits) proof) and_args
  
  let mk_or ~proof_cons ~rule_name or_args c ?(parents=[c]) lit_idx =
    let lits = 
      (List.map L.mk_true or_args) @
      (CCArray.except_idx (C.lits c) lit_idx) in
    let proof = proof ~constructor:proof_cons ~parents ~name:rule_name c in
    [C.create ~penalty:(C.penalty c) ~trail:(C.trail c) lits proof]

  let cnf_scope_form form =
    let kind = Env.flex_get k_scoping in
    let open CCOpt in

    let rec maxiscoping_eligible l =
      let get_quant t = 
        let t = Combs.expand t in
        match T.view t with
        | T.AppBuiltin((ForallConst|ExistsConst) as b, [_;x]) ->
          let ty, body = T.open_fun x in
          assert(List.length ty = 1);
          Some (b, List.hd ty, [body])
        | _ -> None in

      match l with
        | [] -> assert false;
        | [x] -> get_quant x
        | x :: xs -> 
          get_quant x
          >>= (fun (b,ty,body) -> 
            maxiscoping_eligible xs
            >>= (fun (b', ty', bodies) ->
              if Builtin.equal b b' && Type.equal ty ty' then (
                Some(b, ty, (List.hd body)::bodies)
              ) else None)) in

    let miniscope hd f =
      let distribute_quant hd ty bodies =
        let quant_hd = 
          if Builtin.equal hd Or then T.Form.exists else T.Form.forall in
        let outer_hd =
          if Builtin.equal hd Or then T.Form.or_l else T.Form.and_l in
        
        outer_hd (List.map (fun t -> quant_hd (T.fun_ ty t) ) bodies) in 

      let f = Combs.expand f in
      if T.is_fun f then (
        let ty, body = T.open_fun f in
        assert(List.length ty = 1);
        match T.view body with
        | T.AppBuiltin(Or, l) when Builtin.equal hd ExistsConst ->
          Some (distribute_quant Or (List.hd ty) l)
        | T.AppBuiltin(And, l) when Builtin.equal hd ForallConst ->
          Some (distribute_quant And (List.hd ty) l)
        | _ -> None
      ) else None in

    match T.view form with 
    | T.AppBuiltin(And, ((_ :: _) as l)) when kind = `Maxi ->
      begin match maxiscoping_eligible l with
      | Some (ForallConst, ty, bodies) ->
        Some (T.Form.forall (T.fun_ ty (T.Form.and_l bodies)))
      | _ -> None end
    | T.AppBuiltin(Or, ((_ :: _) as l)) when kind = `Maxi ->
      begin match maxiscoping_eligible l with
      | Some (ExistsConst, ty, bodies) ->
        Some (T.Form.exists (T.fun_ ty (T.Form.or_l bodies)))
      | _ -> None end
    | T.AppBuiltin(((ExistsConst|ForallConst) as b), [_;f]) when kind = `Mini ->
      miniscope b f
    | _ -> None

  let clausify_quant ~parent ~var_offset ~sign ~quant_body ~(quant_hd:Builtin.t) =
    let f = Combs.expand quant_body in
    let var_tys, body =  T.open_fun f in
    assert(List.length var_tys = 1);
    let var_ty = List.hd var_tys in
    let hd, f =
      if sign then quant_hd,f
      else ((if quant_hd=ForallConst then ExistsConst else ForallConst),
            T.fun_ var_ty (T.Form.not_ body)) in
    let rule_name = 
      CCFormat.sprintf "lazy_cnf_%s" 
        (if hd = ForallConst then "forall" else "exists") 
    in
    let subst_term =
      if hd = ForallConst then (
        T.var @@ HVar.make ~ty:var_ty (var_offset + 1)
      ) else (
        FR.get_skolem ~parent ~mode:(Env.flex_get k_skolem_mode) f
        |> CCFun.tap (fun t ->
          CCOpt.iter (fun hd_id -> 
          ID.set_payload (hd_id) (ID.Attr_skolem ID.K_lazy_cnf)) (T.head t))) 
    in
    let expand_quant = not @@ Env.flex_get Combinators.k_enable_combinators in
    let res_t = Lambda.eta_reduce ~expand_quant @@ Lambda.snf @@ T.app f [subst_term] in
    res_t, hd, subst_term, rule_name


  let lazy_clausify_driver ?(ignore_eq=false) ?(force_clausification=false) ~proof_cons c =
    let return acc l =
      Iter.append acc (Iter.of_list l), `Stop in
    
    let continue acc =
      acc, `Continue in

    let eligible_to_ignore_eq ~ignore_eq lhs rhs = 
      (not (check_eq_cnf_ordering_conditions lhs rhs)) ||
      (ignore_eq && not (T.is_true_or_false lhs) && not (T.is_true_or_false rhs)) in

    Util.debugf ~section 3 "lazy_cnf(@[%a@])@." (fun k -> k C.pp c);

    let init = 
      Iter.empty
    in

    let should_clausify sign f = 
      force_clausification || 
      (Env.flex_get k_lazy_cnf_kind != `Ignore &&
      check_size_limits sign f) in

    fold_lits c
    |> Iter.fold_while ( fun acc (lhs, rhs, sign, pos) ->
      let i,_ = Ls.Pos.cut pos in
      let lit = (C.lits c).(i) in
      if L.is_predicate_lit lit then (
        Util.debugf ~section 3 "  subformula:%d:@[%a@]" (fun k -> k i L.pp lit );
        begin match T.view lhs with 
        | T.AppBuiltin(And, l) when List.length l >= 2 && should_clausify sign lhs->
          let rule_name = "lazy_cnf_and" in
          if sign then return acc @@ mk_and ~proof_cons l c i ~rule_name
          else return acc @@ mk_or ~proof_cons (List.map T.Form.not_ l) c i ~rule_name
        | T.AppBuiltin(Or, l) when List.length l >= 2 && should_clausify sign lhs ->
          let rule_name = "lazy_cnf_or" in
          if sign then return acc @@ mk_or ~proof_cons l c i ~rule_name
          else return acc @@ mk_and ~proof_cons (List.map T.Form.not_ l) c i ~rule_name
        | T.AppBuiltin(Imply, [a;b]) when should_clausify sign lhs ->
          let rule_name = "lazy_cnf_imply" in
          if sign then (
            if force_clausification || 
               Env.flex_get k_lazy_cnf_kind != `Simp ||
               Env.flex_get k_clausify_implications
            then return acc @@ mk_or ~proof_cons [T.Form.not_ a; b] c i ~rule_name
            else continue acc )
          else return acc @@ mk_and ~proof_cons [a; T.Form.not_ b] c i ~rule_name
        | T.AppBuiltin((Equiv|Xor) as hd, [a;b]) when should_clausify sign lhs ->
          let hd = if sign then hd else (if hd = Equiv then Xor else Equiv) in
          if eligible_to_ignore_eq ~ignore_eq a b then continue acc
          else (
            let rule_name = 
              CCFormat.sprintf "lazy_cnf_%s" 
                (if hd = Equiv then "equiv" else "xor") in
            if hd = Equiv then (
              return acc @@ (
                mk_or ~proof_cons ~rule_name [T.Form.not_ a; b] c i 
                  @ mk_or ~proof_cons ~rule_name [a; T.Form.not_ b] c i)
            ) else (
              return acc @@ (
                mk_or ~proof_cons ~rule_name [T.Form.not_ a; T.Form.not_ b] c i 
                @ mk_or ~proof_cons ~rule_name [a; b] c i
                )))
        | T.AppBuiltin((ForallConst|ExistsConst) as hd, [_; f]) 
            when Env.flex_get k_lazy_cnf_kind != `Simp || 
                 not (Env.flex_get k_inf_quant) ->
          let var_offset = T.Seq.max_var (C.Seq.vars c) + 1 in 
          let res, hd, subst_term, rule_name = clausify_quant ~parent:c ~var_offset ~sign ~quant_body:f ~quant_hd:hd in
          assert(Type.is_prop (T.ty res));
          let res_cl = mk_or ~proof_cons ~rule_name [res] c i in
          if Type.returns_prop (T.ty subst_term) && hd == ForallConst then (
            assert (List.length res_cl == 1);
            assert (T.is_var subst_term);
            Signal.send Env.on_pred_var_elimination (List.hd res_cl, subst_term)
          );

          let sub_ty = T.ty subst_term in
          if Env.flex_get k_enum_bool_funs && Type.Seq.has_bools_only sub_ty then (
            let repls = enum_bool_funs ~ty:sub_ty in
            if Env.flex_get k_replace_bool_fun_quants then (
              let bodies = List.map (fun r -> 
                Lambda.eta_reduce @@ Lambda.whnf (T.app f [r])) repls in
              return acc @@
                 (if hd == ForallConst then mk_and ~proof_cons ~rule_name:"_inst_quant" bodies c i
                  else mk_or ~proof_cons ~rule_name:"_inst_quant" bodies c i)
            ) else return acc res_cl
          ) else return acc res_cl
        | _ -> continue acc end
      ) else if Type.is_prop (T.ty lhs) && not (L.is_predicate_lit lit) then (
          let rule_name = 
            CCFormat.sprintf "lazy_cnf_%s" (if sign then "equiv" else "xor") in
          
          Util.debugf ~section 3 "  subeq:%d:@[%a %s= %a@]" (fun k -> k i T.pp lhs (if sign then "" else "~") T.pp rhs );
          if eligible_to_ignore_eq ~ignore_eq lhs rhs
             || not (check_size_limits sign (T.Form.equiv lhs rhs)) 
             then continue acc
          else if sign then (
            return acc @@ (
              mk_or ~proof_cons ~rule_name [T.Form.not_ lhs; rhs] c i 
              @ mk_or ~proof_cons ~rule_name [lhs; T.Form.not_ rhs] c i)
          ) else (
            return acc @@ (
                mk_or ~proof_cons ~rule_name [T.Form.not_ lhs; T.Form.not_ rhs] c i 
                @ mk_or ~proof_cons ~rule_name [lhs; rhs] c i ))
      ) else continue acc) (init)

  let clausify_quants ~proof_cons c =
    C.lits c
    |> CCArray.find_map_i (fun i -> function 
      | Literal.Equation(lhs, _, _) as lit 
          when Literal.is_predicate_lit lit  ->
        begin match T.view lhs with
        | T.AppBuiltin((ForallConst|ExistsConst) as hd, [_; body]) ->
          let var_offset = T.Seq.max_var (C.Seq.vars c) + 1 in 
          let sign = Literal.is_positivoid lit in
          let res, hd, subst_term, rule_name = 
            clausify_quant ~parent:c ~var_offset ~sign ~quant_body:body ~quant_hd:hd in
          assert(Type.is_prop (T.ty res));
          let res_cl = List.hd @@ mk_or ~proof_cons ~rule_name [res] c i in
          if Type.returns_prop (T.ty subst_term) && hd == ForallConst then (
            assert (T.is_var subst_term);
            Signal.send Env.on_pred_var_elimination (res_cl, subst_term)
          );
          Some (res_cl)
        | _ -> None end
      | _-> None)

  let reduce_quantifiers c =
    let proof_cons = 
      Proof.Step.simp ~infos:[] 
                      ~tags:[Proof.Tag.T_live_cnf;
                             Proof.Tag.T_dont_increase_depth] in
    clausify_quants ~proof_cons c
    |> CCOpt.map_or ~default:(SimplM.return_same c) (SimplM.return_new)


  let rename_subformulas c =
    Util.debugf ~section 3 "lazy-cnf-rename(@[%a@])@." (fun k -> k C.pp c);
    let proof_cons = 
      Proof.Step.simp ~infos:[] 
                      ~tags:[Proof.Tag.T_live_cnf;
                             Proof.Tag.T_dont_increase_depth] in
    
    let clausify_defs new_defs =
      List.fold_left (fun acc c -> 
        lazy_clausify_driver ~ignore_eq:false ~force_clausification:true ~proof_cons c
        |> Iter.to_list
        |> (fun l -> if CCList.is_empty l then c :: acc else l @ acc)
      ) [] new_defs
    in

    let should_rename sign f =
      let will_yield_claues f =
        let rec aux ~sign f =
          match T.view f with
          | T.AppBuiltin(Builtin.And, l) ->
            (sign && (List.length l >= 2)) || aux_l sign l
          | T.AppBuiltin(Builtin.Or, l) ->
            ((not sign) && (List.length l >= 2)) || aux_l sign l
          | T.AppBuiltin(Builtin.(ForallConst|ExistsConst), [_; body]) ->
            aux ~sign (snd (T.open_fun body))
          | T.AppBuiltin(Builtin.Not, [body]) -> aux ~sign:(not sign) body
          | T.AppBuiltin(Builtin.Imply, [a; b]) ->
            not sign || aux ~sign:(not sign) a || aux ~sign b
          | T.AppBuiltin(Builtin.(Eq|Equiv|Neq|Xor), ([a;b]|[_;a;b])) ->
            (* if it is either an equivalence(xor) which yields at
               least two clauses, or a complicated higher-order
               disequation (between app-vars or lambdas) in which case
               it is good to hide it under renamed formula and 
               delay reasoning about it *)
            Type.is_prop (T.ty a) ||
              T.Seq.subterms ~include_builtin:true f
              |> Iter.exists (fun x -> T.is_app_var x || T.is_fun x)
          | _ -> false
        and aux_l sign = function 
        | [] -> false
        | x :: xs -> aux ~sign x || aux_l sign xs in
      let ans = aux ~sign f in
      Util.debugf ~section 3 "@[%a@] will%s yield clauses@." 
        (fun k -> k T.pp f (if ans then "" else " not"));
      ans in

      let num_occurences = Term.Tbl.get_or _form_counter f ~default:0 in
      (* CCFormat.printf "|@[%a@]| = %d@." T.pp f num_occurences; *)
      num_occurences >= Env.flex_get k_renaming_threshold &&
      (not (FR.is_renaming_clause c)) &&
      will_yield_claues f
    in

    Ls.fold_eqn_simple (C.lits c)
    |> Iter.fold_while (fun _ (lhs,rhs,sign,pos) -> 
      let i,_ = Ls.Pos.cut pos in
      let lit = (C.lits c).(i) in
      let proof_cons =
        Proof.Step.simp ~infos:[]
        ~tags:[Proof.Tag.T_live_cnf; Proof.Tag.T_dont_increase_depth] in
      let polarity_aware = Env.flex_get k_pa_renaming in
      let should_rename = should_rename sign in
      if L.is_predicate_lit lit && T.is_appbuiltin lhs then (
        match FR.rename_form ~should_rename ~polarity_aware ~c lhs sign with
        | Some (renamer, new_defs, parents) ->
          Term.Tbl.remove _form_counter lhs;
          let rule_name = "renaming" in
          let new_defs = clausify_defs new_defs in
          let renamer = (if sign then CCFun.id else T.Form.not_) renamer in
          let renamed = mk_or ~proof_cons ~rule_name [renamer] c ~parents:(c :: parents) i in
          let res = renamed @ new_defs in
          Util.debugf ~section 3 "  @[renamed subformula %d:(@[%a@])=@. @[%a@]@]@." 
            (fun k -> k i C.pp c (CCList.pp C.pp) renamed);
          Util.debugf ~section 3 "  new defs:@[%a@]@." 
            (fun k -> k (CCList.pp C.pp) new_defs);
          Some res, `Stop
        | None -> None, `Continue
      ) else if Type.is_prop (T.ty lhs) && not (L.is_predicate_lit lit) &&
              (T.is_appbuiltin lhs || T.is_appbuiltin rhs) then (
          match rename_eq ~should_rename ~c lhs rhs sign with
          | Some (renamer, new_defs, parents) ->
            let rule_name = "renaming" in
            let new_defs = clausify_defs new_defs in
            let renamed = mk_or ~proof_cons ~rule_name [renamer] c ~parents:(c :: parents) i in
            let res = renamed @ new_defs in
            Util.debugf ~section 3 "  @[renamed eq %d(@[%a@]) into @[%a@]@]@." 
              (fun k -> k i L.pp (C.lits c).(i) (CCList.pp C.pp) renamed);
            Util.debugf ~section 3 "  new defs:@[%a@]@." 
              (fun k -> k (CCList.pp C.pp) new_defs);
              Some res, `Stop
          | None -> None, `Continue)
        else None, `Continue) None   
  
  let inf_quantifiers c =
    let proof_cons = 
      Proof.Step.inference ~infos:[] 
                      ~tags:[Proof.Tag.T_live_cnf;
                             Proof.Tag.T_dont_increase_depth] in
    CCOpt.map_or ~default:[] (fun c -> [c]) @@ clausify_quants ~proof_cons c

  let clausify_eq c =
    let rule_name = "eq_elim" in
    fold_lits c
    |> Iter.fold (fun acc (lhs,rhs,sign,pos) -> 
        let i,_ = Ls.Pos.cut pos in
        let lit = (C.lits c).(i) in
        let proof_cons = Proof.Step.inference ~infos:[] ~tags:[Proof.Tag.T_live_cnf; Proof.Tag.T_dont_increase_depth] in
        if not (L.is_predicate_lit lit) && Type.is_prop (T.ty lhs) 
           && check_eq_cnf_ordering_conditions lhs rhs
           && check_size_limits sign (T.Form.equiv lhs rhs) then (
          let new_cls =
            if sign then (
              mk_or ~proof_cons ~rule_name [T.Form.not_ lhs; rhs] c i 
              @ mk_or ~proof_cons ~rule_name [lhs; T.Form.not_ rhs] c i
            ) else (
              (mk_or ~proof_cons ~rule_name [T.Form.not_ lhs; T.Form.not_ rhs] c i)
              @ (mk_or ~proof_cons ~rule_name [lhs; rhs] c i)) in
          let pen_inc = 
            if (T.is_ground lhs && T.is_ground rhs) then 0 
            else (if T.is_app_var lhs || T.is_app_var rhs then 3 else 1) 
          in
          if Env.flex_get k_penalize_eq_cnf then (
            List.iter (fun c -> C.inc_penalty c pen_inc) new_cls
          );
          new_cls @ acc
        ) else acc) []
    |> CCFun.tap (fun res -> 
      Util.debugf ~section 3 "eq_elim(@[%a@])" (fun k -> k C.pp c);
      if CCList.is_empty res then (
        Util.debugf ~section 3 "=∅" CCFun.id;
      ) else (
        Util.debugf ~section 3 "=@[%a@]" (fun k -> k (CCList.pp C.pp) res);
      ) 
    )

  let clausify_imp c =
    let rule_name = "imp_elim" in
    fold_lits c
    |> Iter.fold (fun acc (lhs,rhs,sign,pos) -> 
        let i,_ = Ls.Pos.cut pos in
        let lit = (C.lits c).(i) in
        let proof_cons = Proof.Step.inference ~infos:[] ~tags:[Proof.Tag.T_live_cnf; Proof.Tag.T_dont_increase_depth] in
        if (L.is_predicate_lit lit) && Type.is_prop (T.ty lhs) &&
            L.is_positivoid lit  then (
          match lit with
          | L.Equation(lhs, rhs, true) when T.equal T.true_ rhs ->
            begin match T.view lhs with
            | T.AppBuiltin(Imply, [prem;concl]) ->
              (mk_or ~proof_cons ~rule_name [T.Form.not_ prem; concl] c i) @ acc
            | _ -> acc
            end
          | _ -> acc
        ) else acc) []
  
  let cnf_scope c =
    fold_lits c
    |> Iter.fold_while (fun _ (lhs,rhs,sign,pos) -> 
      let i,_ = Ls.Pos.cut pos in
      let lit = (C.lits c).(i) in
      let proof_cons = Proof.Step.simp ~infos:[] ~tags:[Proof.Tag.T_live_cnf; Proof.Tag.T_dont_increase_depth] in
      if L.is_predicate_lit lit && T.is_appbuiltin lhs then (
        match cnf_scope_form lhs with 
        | Some f ->
          let rule_name = CCFormat.sprintf "lazy_cnf_%sscoping" 
            (if Env.flex_get k_scoping == `Maxi then "maxi" else "mini") in
          let app_sign = if sign then CCFun.id else T.Form.not_ in
          Some (mk_or ~proof_cons [app_sign f] c i ~rule_name), `Stop
        | None -> None, `Continue
      ) else None, `Continue) None

  let lazy_clausify_simpl c =
    update_form_counter ~action:`Increase c;

    let proof_cons = Proof.Step.simp ~infos:[] ~tags:[Proof.Tag.T_live_cnf; Proof.Tag.T_dont_increase_depth] in
    let res = Iter.to_list @@ lazy_clausify_driver ~ignore_eq:true ~proof_cons c  in
    if not @@ CCList.is_empty res then (
      Util.debugf ~section 3 "lazy_cnf_simp(@[%a@])=" (fun k -> k C.pp c);
      Util.debugf ~section 3 "@[%a@]@." (fun k -> k (CCList.pp C.pp) res);
      Util.debugf ~section 3 "proof:@[%a@]@." (fun k -> k (CCList.pp (Proof.S.pp_tstp)) (List.map C.proof res));
      update_form_counter ~action:`Decrease c;
      CCList.iter (update_form_counter ~action:`Increase) res;
    ) else Util.debugf ~section 3 "lazy_cnf_simp(@[%a@])=Ø" (fun k -> k C.pp c);
    if CCList.is_empty res then None
    else (Some res)

  let lazy_clausify_inf c =
    let proof_cons = Proof.Step.inference ~infos:[] ~tags:[Proof.Tag.T_live_cnf; Proof.Tag.T_dont_increase_depth] in
    let res = Iter.to_list (lazy_clausify_driver ~ignore_eq:false ~proof_cons c) in
    if not @@ CCList.is_empty res then (
      Util.debugf ~section 3 "lazy_cnf_inf(@[%a@])=" (fun k -> k C.pp c);
      Util.debugf ~section 3 "@[%a@]@." (fun k -> k (CCList.pp C.pp) res);
    ) else Util.debugf ~section 3 "lazy_cnf_simp(@[%a@])=Ø" (fun k -> k C.pp c);
    res

  let setup () =
    if !enabled then (
      let handler f c =
      f c;
      Signal.ContinueListening in

  
      Signal.on E.ProofState.PassiveSet.on_add_clause 
        (handler (update_form_counter ~action:`Increase));
      Signal.on E.ProofState.ActiveSet.on_add_clause
        (handler (update_form_counter ~action:`Increase));
      Signal.on E.ProofState.PassiveSet.on_remove_clause
        (handler (update_form_counter ~action:`Decrease));
      Signal.on E.ProofState.ActiveSet.on_remove_clause
        (handler (update_form_counter ~action:`Decrease));


      (* Env.Ctx.lost_completeness (); *)
      begin match Env.flex_get k_lazy_cnf_kind with 
      | `Inf | `Ignore -> 
        Env.add_unary_inf "lazy_cnf" lazy_clausify_inf;
        if (Env.flex_get k_simplify_quant) then (
          Env.add_basic_simplify reduce_quantifiers;
        )
      | `Simp -> 
          Env.add_unary_inf "elim eq" clausify_eq;
          if not (Env.flex_get k_clausify_implications) then (
            Env.add_unary_inf "inf_imp" clausify_imp
          );
          if Env.flex_get k_inf_quant then (
            Env.add_unary_inf "inf_quant" inf_quantifiers;
          );
          Env.add_multi_simpl_rule ~priority:5 lazy_clausify_simpl;
          if Env.flex_get k_lazy_cnf_eager then (
            Env.add_cheap_multi_simpl_rule lazy_clausify_simpl;
          )
      end;

      (* ** IMPORTANT **
         Due to correctly set priorioty, renaming will run before simplification *)
      if Env.flex_get k_renaming_threshold > 0 then (
        Env.add_multi_simpl_rule ~priority:4 rename_subformulas;
        if Env.flex_get k_lazy_cnf_eager then (
          Env.add_cheap_multi_simpl_rule rename_subformulas;
        )
      );
      if Env.flex_get k_scoping != `Off then (
        Env.add_multi_simpl_rule ~priority:3 cnf_scope;
      )
    )
end

let _lazy_cnf_kind = ref `Simp
let _renaming_threshold = ref 8
let _rename_eq = ref true
let _enum_bool_funs = ref false
let _replace_bool_fun_quant = ref false
let _scoping = ref `Off
let _skolem_mode = ref `SkolemRecycle
let _pa_renaming = ref true
let _only_eligible = ref false
let _clausify_eq_pen = ref false
let _clausify_eq_max_noninterpreted = ref true
let _clausify_impls = ref true
let _simp_limit = ref (-1)
let _simp_quant = ref false
let _inf_quant = ref false

let extension =
  let register env =
    let module E = (val env : Env.S) in
    let module ET = Make(E) in
    E.flex_add k_lazy_cnf_kind !_lazy_cnf_kind;
    E.flex_add k_lazy_cnf_eager !_eager_lcnf;
    E.flex_add k_enum_bool_funs !_enum_bool_funs;
    E.flex_add k_replace_bool_fun_quants !_replace_bool_fun_quant;
    E.flex_add k_renaming_threshold !_renaming_threshold;
    E.flex_add k_rename_eq !_rename_eq;
    E.flex_add k_scoping !_scoping;
    E.flex_add k_skolem_mode !_skolem_mode;
    E.flex_add k_pa_renaming !_pa_renaming;
    E.flex_add k_only_eligible !_only_eligible;
    E.flex_add k_penalize_eq_cnf !_clausify_eq_pen;
    E.flex_add k_clausify_eq_max_nonint !_clausify_eq_max_noninterpreted;
    E.flex_add k_simp_limit !_simp_limit;
    E.flex_add k_clausify_implications !_clausify_impls;
    E.flex_add k_simplify_quant !_simp_quant;
    E.flex_add k_inf_quant !_inf_quant;

    ET.setup ()
  in
  { Extensions.default with
    Extensions.name = "lazy_cnf";
    env_actions=[register];
  }

let () =
  Options.add_opts [
    "--lazy-cnf", Arg.Bool ((:=) enabled), " turn on lazy clausification";
    "--lazy-cnf-eager", Arg.Bool ((:=) _eager_lcnf), " apply the rule to every newly created clause";
    "--lazy-cnf-only-eligible-lits", Arg.Bool ((:=) _only_eligible), " apply lazy clausification only on eligible literals";
    "--lazy-cnf-clausify-max-eq", Arg.Bool ((:=) _clausify_eq_max_noninterpreted),
      " enable/disable clausification of an EQ literal if max side is non-interpreted ";
    "--lazy-cnf-clausify-implications", Arg.Bool ((:=) _clausify_impls),
       " apply simplifying clausification to implications";
    "--lazy-cnf-simp-depth-limit", Arg.Int ((:=) _simp_limit),
       " apply simplifying clausification only to formulas that would yield less than N" ^
       " clauses; negative value is interpreted as infinity" ;
    "--lazy-cnf-scoping", Arg.Symbol (["off"; "mini"; "maxi"], (fun str -> 
      match str with 
      | "mini" -> _scoping := `Mini
      | "maxi" -> _scoping := `Maxi
      | "off" -> _scoping := `Off
      | _ -> assert false)), 
    " use mini/maxi scoping rules for lazy cnf";
    "--lazy-cnf-renaming-threshold", Arg.Int ((:=) _renaming_threshold), 
      " set the subformula renaming threshold -- negative value turns renaming off";
    "--polarity-aware-renaming"
    , Arg.Bool (fun v -> _pa_renaming := v)
    , " enable/disable polarity aware renaming (introducing clause with only one definition polarity)";    
    "--lazy-cnf-skolem-mode", Arg.Symbol (["skolem-recycle"; "skolem-fresh"; "choice"], (fun str -> 
      match str with 
      | "skolem-recycle" -> _skolem_mode := `SkolemRecycle
      | "skolem-fresh" -> _skolem_mode := `SkolemAlwaysFresh
      | "choice" -> _skolem_mode := `Choice
      | _ -> assert false)), " use lazy cnf as either simplification or inference";
    "--lazy-cnf-kind", Arg.Symbol (["inf"; "simp"; "ignore"], (fun str -> 
      match str with 
      | "inf" -> _lazy_cnf_kind := `Inf
      | "simp" -> _lazy_cnf_kind := `Simp
      | "ignore" -> _lazy_cnf_kind := `Ignore
      | _ -> assert false)), " use lazy cnf as either simplification, inference, or let calculus clausify";
    "--lazy-cnf-rename-eq", Arg.Bool ((:=) _rename_eq), " turn on/of renaming of boolean equalities";
    "--lazy-cnf-simplify-quant", Arg.Bool ((:=) _simp_quant), " when non-simplifying clausification is used, clausify quantifiers in a simplifying manner";
    "--lazy-cnf-inf-quant", Arg.Bool ((:=) _inf_quant), " when simplifying clausification is used, clausify quantifiers in a non-simplifying manner";
    "--enum-bool-funs", Arg.Bool ((:=) _enum_bool_funs), " enumerate all functions over Boolean domain (up to the order of the problem)";
    "--replace-bool-quant-fun", Arg.Bool ((:=) _replace_bool_fun_quant), " replace Boolean fun quantifier with all possible values";
    "--lazy-cnf-clausify-eq-penalty", Arg.Bool ((:=) _clausify_eq_pen), " turn on/of penalizing clausification of equivalences"];

  Params.add_to_modes ["ho-complete-basic";
                       "ho-pragmatic";
                       "lambda-free-intensional";
                       "lambda-free-purify-intensional";
                       "lambda-free-extensional";
                       "ho-comb-complete";
                       "lambda-free-purify-extensional";
                       "fo-complete-basic"] (fun () ->
      enabled := false;
  );
  Extensions.register extension
