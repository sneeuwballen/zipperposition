
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk
open Libzipperposition

module T = Term
module Pos = Position
module US = Unif_subst
module L = Literal
module Ls = Literals

type reasoning_kind    = 
    BoolReasoningDisabled 
  | BoolSimplificationsOnly
  | BoolHoist 
  | BoolCasesPreprocess

let section = Util.Section.make ~parent:Const.section "booleans"


let k_bool_reasoning = Flex_state.create_key ()
let k_cased_term_selection = Flex_state.create_key ()
let k_quant_rename = Flex_state.create_key ()
let k_interpret_bool_funs = Flex_state.create_key ()
let k_cnf_non_simpl = Flex_state.create_key ()
let k_norm_bools = Flex_state.create_key () 
let k_filter_literals = Flex_state.create_key ()
let k_nnf = Flex_state.create_key ()
let k_simplify_bools = Flex_state.create_key ()
let k_trigger_bool_inst = Flex_state.create_key ()
let k_trigger_bool_ind = Flex_state.create_key ()
let k_include_quants = Flex_state.create_key ()
let k_bool_hoist_simpl = Flex_state.create_key ()
let k_rename_nested_bools = Flex_state.create_key ()
let k_bool_app_var_repl = Flex_state.create_key ()
let k_fluid_hoist = Flex_state.create_key ()
let k_fluid_log_hoist = Flex_state.create_key ()
let k_solve_formulas = Flex_state.create_key ()
let k_replace_unsupported_quants = Flex_state.create_key ()
let k_disable_ho_bool_unif = Flex_state.create_key ()
let k_generalize_trigger = Flex_state.create_key ()
let k_bool_triggers_only = Flex_state.create_key ()


module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {5 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)
end


module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module PS = E.ProofState
  module C = Env.C
  module Ctx = Env.Ctx
  module Fool = Fool.Make(Env)
  module Combs = Combinators.Make(Env)
  module HO = Higher_order.Make(Env)
  module LazyCNF = Lazy_cnf.Make(Env)
  module FR = Env.FormRename
  module Stm = Env.Stm
  module StmQ = Env.StmQ

  let (=~),(/~) = Literal.mk_eq, Literal.mk_neq
  let (@:) = T.app_builtin ~ty:Type.prop
  let no a = a =~ T.false_
  let yes a = a =~ T.true_

  let _trigger_bools   = ref (Type.Map.empty) (* type --> boolean trigger *)
  let _cls_w_pred_vars = ref (Type.Map.empty) (* type --> (clause,var) *)
  
  let get_unif_alg () =
    if Env.flex_get k_disable_ho_bool_unif
    then (fun _ _ -> OSeq.empty)
    else Env.flex_get Superposition.k_unif_alg

  let get_unif_alg_l () =
    if Env.flex_get k_disable_ho_bool_unif
    then (fun _ _ -> OSeq.empty)
    else (
      let (module U) = Superposition.get_unif_module (module E) in
      U.unify_scoped_l
    )

  let get_triggers c =
    let ord = Ctx.ord () in
    let trivial_trigger t =
      let body = snd @@ T.open_fun t in
      T.is_var body || T.is_true_or_false body in
    
    let rec get_terms t k =
      if T.DB.is_closed t then k t;

      match T.view t with
      | T.App(hd, args) -> List.iter (fun t -> get_terms t k) (args)
      | T.AppBuiltin((And|Or|Not|Imply|Eq|Neq|Equiv|Xor), args) ->
        List.iter (fun t -> get_terms t k) (args)
      | T.AppBuiltin((ForallConst|ExistsConst), [_; body]) -> 
        if Env.flex_get k_include_quants then k body
      | _ -> ()
    in

    Literals.fold_terms ~ord ~subterms:false ~eligible:C.Eligible.always 
      ~which:`All (C.lits c) ~fun_bodies:false
    |> Iter.flat_map (fun (t, p) -> get_terms t)
    |> Iter.filter_map (fun t -> 
        let ty = Term.ty t and hd = Term.head_term t in
        let cached_t = Subst.FO.canonize_all_vars t in
        if not (Term.Set.mem cached_t !Higher_order.prim_enum_terms) &&
          Type.is_fun ty && Type.returns_prop ty && not (Term.is_var hd) &&
          not (trivial_trigger t) then (
          Some t
        ) else None
      )

  let instantiate_w_bool ~clause ~var ~trigger =
    assert(Type.equal (T.ty var) (T.ty trigger));

    let cl_sc, trig_sc = 0, 1 in
    let subst = Subst.FO.bind' Subst.empty (T.as_var_exn var, cl_sc) (trigger, trig_sc) in
    let renaming = Subst.Renaming.create () in
    let expand_quant = not @@ Env.flex_get Combinators.k_enable_combinators in
    let lits = Literals.apply_subst renaming subst (C.lits clause, cl_sc) in
    let lits = Literals.map (fun t -> Lambda.eta_reduce ~expand_quant @@ Lambda.snf t) lits in
      let proof =
        Proof.Step.inference 
          ~rule:(Proof.Rule.mk "triggered_bool_instantiation") 
          ~tags:[Proof.Tag.T_ho; Proof.Tag.T_cannot_orphan]
          [C.proof_parent_subst renaming (clause, cl_sc) subst] in
    let res = C.create_a lits proof ~penalty:(C.penalty clause) ~trail:(C.trail clause) in
    (* CCFormat.printf "instatiate:@.c:@[%a@]@.subst:@[%a@]@.res:@[%a@]@." C.pp clause Subst.pp subst C.pp res; *)
    res

  let inst_clauses_w_trigger t =
    let triggers = Type.Map.get_or ~default:[] (T.ty t) !_trigger_bools in
    if not (CCList.mem ~eq:T.equal t triggers) &&
       (not (Env.flex_get k_bool_triggers_only) || Type.returns_prop (T.ty t)) then (
      _trigger_bools := Type.Map.update (T.ty t) (function 
        | None -> Some [t]
        | Some res -> Some (t :: res)
      ) !_trigger_bools;

      Type.Map.get_or ~default:[] (T.ty t) !_cls_w_pred_vars
      |> CCList.map (fun (clause,var) -> instantiate_w_bool ~clause ~var ~trigger:t)
    ) else []

  let insert_new_trigger t =
    let do_insert t = 
      inst_clauses_w_trigger t
      |> CCList.to_iter
      |> Env.add_passive
    in
    do_insert t;
    if (Type.returns_prop (T.ty t)) then (
      let t = Lambda.eta_expand t in
      match Env.flex_get k_generalize_trigger with 
      | `Neg ->
        let vars, body = T.open_fun t in
        if not (Type.is_prop (T.ty body)) then (
          CCFormat.printf "%a:%a@." T.pp body Type.pp (T.ty body);        
          assert (false);
        );
        do_insert (T.fun_l vars (T.Form.not_ body))
      | `Var ->
        let vars, body = T.open_fun t in
        assert(Type.is_prop (T.ty body));
        let n = List.length vars in
        let dbs = List.mapi (fun i ty -> T.bvar ~ty (n-i-1)) vars in
        let var_ty = Type.(==>) (vars @ [Type.prop]) Type.prop in
        let var = T.var (HVar.fresh ~ty:var_ty ()) in
        do_insert (T.fun_l vars (T.app var (dbs@[body])))
      | _ -> ()
    )
    

  let update_triggers cl =
    (* if triggered boolean instantiation is off
       k_trigger_bool_inst is -1 *)
    if C.proof_depth cl < Env.flex_get k_trigger_bool_inst then (
      let new_triggers = (get_triggers cl) in
      if not (Iter.is_empty new_triggers) then (
        Iter.iter insert_new_trigger new_triggers
    ));
    Signal.ContinueListening

    
  let handle_new_pred_var_clause (clause,var) =
    assert(T.is_var var);
    assert(Type.returns_prop (T.ty var));
    let ty = T.ty var in
    Type.Map.get_or ~default:[] ty !_trigger_bools
    |> CCList.map (fun trigger -> instantiate_w_bool ~clause ~var ~trigger)
    |> CCList.to_iter
    |> Env.add_passive;

    _cls_w_pred_vars := Type.Map.update ty (function 
      | None -> Some [(clause, var)]
      | Some res -> Some ((clause, var) :: res)
    ) !_cls_w_pred_vars;

    Signal.ContinueListening

  let handle_new_skolem_sym (c,trigger) =
    let trig_hd = T.head_term trigger in
    assert(T.is_const trig_hd);
    assert(ID.is_postcnf_skolem (T.as_const_exn trig_hd));

    if C.proof_depth c <  Env.flex_get k_trigger_bool_inst 
    then insert_new_trigger trigger;
    
    Signal.ContinueListening

    let trigger_induction cl =
      (* abstracts away closed subterm from the term t
        by replacing it with (accordingly shifted) DB variable 0 *)
      let abstract ~subterm t =
        assert(T.DB.is_closed subterm);

        let rec aux ~depth t =
          if T.equal subterm t then (
            T.bvar ~ty:(T.ty subterm) depth
          ) else (
            match T.view t with
            | T.App(hd, args) ->
              let hd' = aux ~depth hd in
              let args' = List.map (aux ~depth) args in
              if T.equal hd hd' && T.same_l args args' then t
              else T.app hd' args'
            | T.AppBuiltin(hd, args) ->
              let args' = List.map (aux ~depth) args in
              if T.same_l args args' then t
              else T.app_builtin ~ty:(T.ty t) hd args'
            | T.Fun _ ->
              let pref, body = T.open_fun t in
              let body' = aux ~depth:(depth+(List.length pref)) body in
              if T.equal body body' then t else T.fun_l pref body'
            | _ -> t
          ) in
      
        let res = aux ~depth:0 t in
        assert (Type.equal (T.ty res) (T.ty t));
        if T.equal res t then None else (Some res) in
    
      let make_triggers lhs rhs sign =
        let lhs_body, rhs_body = snd (Term.open_fun lhs), snd (Term.open_fun rhs) in
        let immediate_args =
          if (T.is_const (T.head_term lhs_body)) && 
            (T.is_const (T.head_term rhs_body)) then (
            List.sort_uniq T.compare (T.args lhs_body @ T.args rhs_body)
          ) else [] in
        CCList.filter_map (fun arg -> 
          if T.DB.is_closed arg && not (Type.is_tType (T.ty arg)) then (
            match abstract ~subterm:arg lhs, abstract ~subterm:arg rhs with
            | Some(lhs'), Some(rhs') ->
              assert (Type.equal (T.ty lhs') (T.ty rhs'));
              (* Flipping the sign that is present in conjecture,
                to prove the negated conjecture using induction *)
              let build_body sign = if sign then T.Form.neq else T.Form.eq in
              let res = T.fun_ (T.ty arg) (build_body sign lhs' rhs') in
              assert (T.DB.is_closed res);
              Some res
            | _ -> None
          ) else None
        ) immediate_args in

    if C.proof_depth cl < Env.flex_get k_trigger_bool_ind &&
       CCOpt.is_some (C.distance_to_goal cl) then (
      match C.lits cl with
      | [| Literal.Equation(lhs, rhs, _) as lit |] ->
        let res = 
          CCList.flat_map inst_clauses_w_trigger (make_triggers lhs rhs (Literal.is_positivoid lit))
        in
        res
      | _ -> []
    ) else []

  let () =
    Signal.on PS.ActiveSet.on_add_clause (fun c ->
      update_triggers c)
  
  let mk_res ~proof ~old ~repl new_lit c =
    C.create ~trail:(C.trail c) ~penalty:(C.penalty c)
      (new_lit :: Array.to_list( C.lits c |> Literals.map (T.replace ~old ~by:repl)))
    proof

  let get_green_eligible c =
    let ord = C.Ctx.ord () in
    Ls.fold_terms ~vars:false ~var_args:false ~fun_bodies:false 
                  ~ty_args:false ~ord ~which:`Max ~subterms:true  
                  ~eligible:(C.Eligible.res c) (C.lits c)
  
  let get_bool_eligible c =
    Iter.append 
      (SClause.TPSet.to_iter (C.eligible_subterms_of_bool c))
      (get_green_eligible c)

  let get_bool_hoist_eligible c =
    get_bool_eligible c
    |> Iter.filter (fun (t,p) ->
      let module P = Position in
      match p with
      | P.Arg(idx, P.Left P.Stop)
      | P.Arg(idx, P.Right P.Stop) ->
        (match (C.lits c).(idx) with 
          | L.Equation(_,_,false) -> true
          | _ -> false)
      | _ -> true
    ) |> Iter.filter (fun (t,_) -> 
        let ty = T.ty t in
        (Type.is_prop ty || Type.is_var ty) &&
        not (T.is_true_or_false t) &&
        not (T.is_var t) && 
        match T.view t with
        | T.AppBuiltin(hd,_) ->
          (* check that the term has no interpreted sym on top *)
          not (List.mem hd Builtin.([Eq; Neq; ForallConst; ExistsConst; Not]) || 
              Builtin.is_logical_binop hd)
        | T.App(hd, _) -> not @@ T.is_var hd
        | _ -> true)
    |> Iter.to_list
    (* since we are doing simultaneous version -- we take only unique terms *)
    |> CCList.sort_uniq ~cmp:(fun (t1,_) (t2,_) -> T.compare t1 t2)

  let handle_poly_bool_hoist t c =
    let ty = T.ty t in
    assert(Type.is_prop ty || Type.is_var ty);
    
    if Type.is_prop ty then (
      (t, c)
    ) else (
      let sub = 
        Subst.Ty.bind Subst.empty 
          (* absolutely horrible castings of OCaml *)
          ((Type.as_var_exn ty, 0) :> InnerTerm.t HVar.t Scoped.t) 
          (Type.prop, 0) 
      in
      let renaming = Subst.Renaming.create () in
      let t' = Subst.FO.apply renaming sub (t,0) in
      let c' = C.apply_subst ~renaming (c,0) sub in
      (t',c')
    )


  let bool_hoist (c:C.t) : C.t list = 
    let proof = Proof.Step.inference [C.proof_parent c]
                ~rule:(Proof.Rule.mk "bool_hoist") ~tags:[Proof.Tag.T_ho] in

    List.map (fun (t, _) ->
      let t,c = handle_poly_bool_hoist t c in
      mk_res ~proof ~old:t ~repl:T.false_ (yes t) c)
    (get_bool_hoist_eligible c)
    |> CCFun.tap (fun res -> 
      Util.debugf ~section 3 "hoist(@[%a@])" (fun k -> k C.pp c);
      if CCList.is_empty res then (
        Util.debugf ~section 3 " = ∅ (%d)(%a)(%d)" 
          (fun k -> 
            k (Iter.length (get_green_eligible c)) 
              (Iter.pp_seq Term.pp) (Iter.map fst (get_bool_eligible c))
              (List.length (get_bool_hoist_eligible c)));
      ) else (Util.debugf ~section 3  " = @[%a@]" (fun k -> k (CCList.pp C.pp) res))
    )

  let bool_hoist_simpl (c:C.t) : C.t list option = 
    let proof = Proof.Step.simp [C.proof_parent c]
                ~rule:(Proof.Rule.mk "bool_hoist") ~tags:[Proof.Tag.T_ho] in

    let bool_subterms  = get_bool_hoist_eligible c in
    CCOpt.return_if (not @@ CCList.is_empty bool_subterms) (
      CCList.fold_left (fun acc (t,_) ->
        let t,c = handle_poly_bool_hoist t c in
        let neg_lit, repl_neg = no t, T.true_ in
        let pos_lit, repl_pos = yes t, T.false_ in
        (mk_res ~proof ~old:t ~repl:repl_neg neg_lit c) ::
        (mk_res ~proof ~old:t ~repl:repl_pos pos_lit c) :: acc ) 
      [] bool_subterms
    )
    |> CCFun.tap (function 
      | Some res ->
        Util.debugf ~section 2 "bool_hoist_simpl(@[%a@])=@. @[%a@]@." (fun k -> k C.pp c (CCList.pp C.pp) res);
      | None -> 
        Util.debugf ~section 2 "bool_hoist_simpl(@[%a@])= None@." (fun k -> k C.pp c)
    )

  let eq_hoist (c:C.t) : C.t list =
    let proof ~prefix = 
      Proof.Step.inference [C.proof_parent c]
        ~rule:(Proof.Rule.mk (prefix^"_hoist")) ~tags:[Proof.Tag.T_ho] in

    get_bool_eligible c
    |> Iter.filter (fun (t,_) -> 
        match T.view t with
        | T.AppBuiltin(hd,_) ->
          List.mem hd [Builtin.Eq;Neq;Xor;Equiv;ForallConst;ExistsConst] &&
          Type.is_prop (T.ty t)
        | _ -> false)
    |> Iter.to_list
    (* since we are doing simultaneous version -- we take only unique terms *)
    |> CCList.sort_uniq ~cmp:(fun (t1,_) (t2,_) -> T.compare t1 t2)
    |> List.map (fun (t, _) ->
        match T.view t with 
        | T.AppBuiltin(Builtin.(Eq|Equiv), ([a;b]|[_;a;b])) ->
          let new_lit = Literal.mk_eq a b in
          mk_res ~proof:(proof ~prefix:"eq") ~old:t ~repl:T.false_ new_lit c
        | T.AppBuiltin(Builtin.(Neq|Xor), ([a;b]|[_;a;b])) ->
          let new_lit = Literal.mk_eq a b in
          mk_res ~proof:(proof ~prefix:"neq") ~old:t ~repl:T.true_ new_lit c
        | _ -> assert false
    )
    |> CCFun.tap (fun res -> 
      Util.debugf ~section 3 "eq-hoist(@[%a@])" (fun k -> k C.pp c);
      if CCList.is_empty res then (
        Util.debugf ~section 3 " = ∅ (%d)(%d)(%a)" 
          (fun k -> 
            k (Iter.length (get_green_eligible c))
              (Iter.length (get_bool_eligible c))
              (Iter.pp_seq Term. pp) (Iter.map fst (get_bool_eligible c)) );
      ) else (Util.debugf ~section 3 " = @[%a@]" (fun k -> k (CCList.pp C.pp) res))
    )

  let fluid_hoist (c:C.t) =
    let tyvar = Type.var (HVar.fresh ~ty:Type.tType ()) in
    let z = T.var (HVar.fresh ~ty:(Type.arrow [Type.prop] tyvar) ()) in
    let x = T.var (HVar.fresh ~ty:Type.prop ()) in
    let zx = T.app z [x] in
    let z_false, z_true = T.app z [T.false_], T.app z [T.true_] in

    let sc_zx, sc_cl = 0, 1 in

    let mk_res sign renaming sub at =
      let c' = C.apply_subst ~renaming (c, sc_cl) sub in
      let still_at_eligible = 
        get_bool_eligible c'
        |> Iter.exists (fun (_, p) -> Position.equal p at) 
      in

      if still_at_eligible then (
        let new_lit = 
          let x_sub = Subst.FO.apply renaming sub (x, sc_zx) in
          Literal.mk_eq x_sub (if sign then T.true_ else T.false_) in
        let by = 
          Subst.FO.apply renaming sub ((if sign then z_false else z_true), sc_zx) in
        Literals.Pos.replace (C.lits c') ~at ~by;
        let rule = 
          Proof.Rule.mk ("fluid_" ^ (if sign then "bool_" else "loob_") ^ "hoist") in
        let proof = Proof.Step.inference ~rule [C.proof_parent_subst renaming (c,sc_cl) sub] in
        let res = 
          C.create ~penalty:(C.penalty c + 
                             (C.proof_depth c) + 
                             (if Proof.Step.has_ho_step (C.proof_step c) then 3 else 1))
                   ~trail:(C.trail c)
            (new_lit :: CCArray.to_list (C.lits c')) proof in
        Some res
      ) else None
    in

    get_bool_eligible c
    |> (fun iter -> 
      Iter.fold (fun acc (u,p) ->
        if T.is_app_var u || T.is_fun u && (not (T.is_ground u)) then (
          let unif_seq =
            get_unif_alg () (zx, sc_zx) (u, sc_cl)
            |> OSeq.flat_map (fun us_opt -> 
              CCOpt.map_or ~default:OSeq.empty (fun us ->
                assert(not @@ US.has_constr us);
                let sub = US.subst us in
                let renaming = Subst.Renaming.create () in
                let z_false_sub = 
                  Lambda.snf @@ Subst.FO.apply renaming sub (z_false, sc_zx) in
                let z_true_sub = 
                  Lambda.snf @@ Subst.FO.apply renaming sub (z_true, sc_zx) in
                let x_sub = 
                  Lambda.snf @@ Subst.FO.apply renaming sub (x, sc_zx) in
                let zx_sub =
                  Lambda.snf @@ Subst.FO.apply renaming sub (zx, sc_zx) in
                if T.is_true_or_false x_sub then OSeq.empty
                else (
                  let bool_res =
                    if T.equal z_false_sub zx_sub then []
                    else [(mk_res true renaming sub p)] in
                  let loob_res = 
                    if T.equal z_true_sub zx_sub then []
                    else [(mk_res false renaming sub p)] in
                  OSeq.of_list (bool_res @ loob_res)
                )
              ) us_opt
            )
          in
          if Env.should_force_stream_eval () then (
            (Env.get_finite_infs [unif_seq]) @ acc 
          ) else (
            let stm_res = Env.Stm.make ~penalty:(C.penalty c + 2) ~parents:[c] (unif_seq) in
            Env.StmQ.add (Env.get_stm_queue ()) stm_res;
            acc)
        ) else acc
    ) [] iter)

  (* Record holding info for constructing Eq/Neq/Forall/ExistsHoist inference*)
  type fluid_log_partner_info =
    {
      unif_partner : Term.t; (* what is u unified with *)
      repl : Term.t; (* what is u replaced with *)
      new_lit : Literal.t option (* possibly a new literal *)
    }
  
  (* Fluid hoisting of logical symbols -- 
     rules EqHoist, NeqHoist, ForallHoist, ExistsHoist 
     and BoolRw where head is a variable*)
  let fluid_log_hoist (c:C.t) =
    (* It is assumed Boolean selection will 
       not select any top-level terms *)

    let a = Type.var (HVar.fresh ~ty:(Type.tType) ()) in
    let x_a = T.var (HVar.fresh ~ty:a ()) in
    let y_a = T.var (HVar.fresh ~ty:a ()) in
    let y_quant = 
      Lambda.eta_expand @@
      T.var (HVar.fresh ~ty:(Type.arrow [a] Type.prop) ()) in
    let yx = Lambda.whnf @@ T.app y_quant [y_a] in
    
    let module F = T.Form in
    let module PB = Position.Build in

    let partners = 
    [ {unif_partner=F.eq x_a y_a; repl= T.false_; new_lit=Some (L.mk_eq x_a y_a)};
      {unif_partner=F.neq x_a y_a; repl= T.true_; new_lit=Some (L.mk_eq x_a y_a)};
      {unif_partner=F.forall y_quant; repl= T.false_; new_lit=Some (L.mk_eq yx T.true_)};
      {unif_partner=F.exists y_quant; repl= T.true_; new_lit=Some (L.mk_eq yx T.false_)};
      {unif_partner=(F.not_ T.false_); repl= T.true_; new_lit=None};
      {unif_partner=(F.not_ T.true_); repl= T.false_; new_lit=None};
      {unif_partner=(F.eq x_a x_a); repl= T.true_; new_lit=None};
      {unif_partner=(F.neq x_a x_a); repl= T.false_; new_lit=None};
      {unif_partner=(F.and_ T.false_ T.false_); repl= T.false_; new_lit=None};
      {unif_partner=(F.and_ T.true_ T.false_); repl= T.false_; new_lit=None};
      {unif_partner=(F.and_ T.false_ T.true_); repl= T.false_; new_lit=None};
      {unif_partner=(F.and_ T.true_ T.true_); repl= T.true_; new_lit=None};
      {unif_partner=(F.or_ T.false_ T.false_); repl= T.false_; new_lit=None};
      {unif_partner=(F.or_ T.true_ T.false_); repl= T.true_; new_lit=None};
      {unif_partner=(F.or_ T.false_ T.true_); repl= T.true_; new_lit=None};
      {unif_partner=(F.or_ T.true_ T.true_); repl= T.true_; new_lit=None};
      {unif_partner=(F.imply T.false_ T.false_); repl= T.true_; new_lit=None};
      {unif_partner=(F.imply T.true_ T.false_); repl= T.false_; new_lit=None};
      {unif_partner=(F.imply T.false_ T.true_); repl= T.true_; new_lit=None};
      {unif_partner=(F.imply T.true_ T.true_); repl= T.true_; new_lit=None};]
    in

    let sc_partner, sc_cl = 0, 1 in
    let mk_res sub at partner= 
      let renaming = Subst.Renaming.create () in
      let lits = Literals.apply_subst renaming sub ((C.lits c), sc_cl) in
      Literals.Pos.replace lits ~at ~by:partner.repl;
      let new_lits =
        (CCOpt.map_or ~default:[] (fun x -> [L.apply_subst renaming sub (x,sc_partner)]) partner.new_lit)
        @ (Array.to_list (lits))
      in
      let rule = Proof.Rule.mk "fluid_log_symbol_hoist" in
      let step = Proof.Step.inference ~rule [C.proof_parent_subst renaming (c,sc_cl) sub] in
      C.create ~penalty:(C.penalty c + 
                         (C.proof_depth c) + 
                         (if Proof.Step.has_ho_step (C.proof_step c) then 2 else 0)) ~trail:(C.trail c) new_lits step
    in

    let eligible = C.Eligible.res c in
    Literals.fold_lits ~eligible (C.lits c)
    |> Iter.fold (fun acc (lit, idx) ->
      let lit_pos = PB.arg idx PB.empty in
      match lit with
      | Literal.Equation(u, v, true) 
        when (Type.is_prop (T.ty u) || Type.is_var (T.ty u)) 
             && T.is_app_var u ->
        let app_vars = 
          if Literal.is_predicate_lit lit then [(u, PB.to_pos (PB.left lit_pos))]
          else if Term.is_app_var v then [(u, PB.to_pos (PB.left lit_pos));
                                          (v, PB.to_pos (PB.right lit_pos))]
          else []
        in
        List.fold_left (fun acc (var, pos) -> 
          List.fold_left (fun acc p -> 
            let seq = 
              get_unif_alg () (p.unif_partner, sc_partner) (var, sc_cl)
              |> OSeq.filter_map (
                CCOpt.map (fun us -> 
                  assert(not (Unif_subst.has_constr us));
                  let sub = Unif_subst.subst us in
                  let eligible' = C.eligible_res (c, sc_cl) sub in
                  (* not eligible under substitution *)
                  if not (CCBV.get eligible' idx) then None
                  else (
                    Some (mk_res sub pos p)        
                )))
            in
            
            if Env.should_force_stream_eval () then (
              Env.get_finite_infs [seq] @ acc
            ) else (
              let stm_res = Env.Stm.make ~penalty:(C.penalty c + 2) ~parents:[c] (seq) in
              Env.StmQ.add (Env.get_stm_queue ()) stm_res;
              acc)
          ) acc partners;
        ) acc app_vars
      | _ -> acc
    ) []

  (* Fluid version of (Forall|Exists)RW *)
  let fluid_quant_rw  (c:C.t) =    
    let module PB = Position.Build in
    let module F = T.Form in

    let sc_partner, sc_cl = 0, 1 in
    let a = Type.var (HVar.fresh ~ty:(Type.tType) ()) in
    let y_quant = 
      Lambda.eta_expand @@
      T.var (HVar.fresh ~ty:(Type.arrow [a] Type.prop) ()) in
    let partners = 
    [ {unif_partner=F.forall y_quant; 
       repl= T.fun_ a (F.not_ (Lambda.whnf @@ T.app y_quant [T.bvar ~ty:a 0])); 
       new_lit=None};
      {unif_partner=F.exists y_quant;
       repl= y_quant (*already eta-expanded *); 
       new_lit=None};]
    in
    
    let mk_res sub at partner= 
      let lits = Array.copy @@ C.lits c in
      (* Literals.Pos.replace lits ~at ~by:partner.repl; *)
      let renaming = Subst.Renaming.create () in
      let expand_quant = not @@ Env.flex_get Combinators.k_enable_combinators in
      let repl_sub =
        Lambda.eta_reduce ~expand_quant @@ Lambda.snf @@
          Subst.FO.apply renaming sub (partner.repl, sc_partner) in
      let sk = FR.get_skolem ~parent:c ~mode:`SkolemRecycle repl_sub in
      let y_sk = 
        T.app (Subst.FO.apply renaming sub (y_quant, sc_partner)) [sk]
      in
      let lits = Literals.apply_subst renaming sub (lits, sc_cl) in
      Literals.Pos.replace lits ~at ~by:y_sk;

      let rule = Proof.Rule.mk "fluid_quant_rw" in
      let step = Proof.Step.inference ~rule [C.proof_parent_subst renaming (c,sc_cl) sub] in
      let res = C.create_a ~penalty:(C.penalty c + 2) ~trail:(C.trail c) lits step in

      Util.debugf ~section 5 "fluid_quant_rw:@.@[%a@] -> @.@[%a@]@." 
        (fun k -> k C.pp c C.pp res);

      res
    in

    let eligible = C.Eligible.res c in
    Literals.fold_lits ~eligible (C.lits c)
    |> Iter.fold (fun acc (lit, idx) ->
      let lit_pos = PB.arg idx PB.empty in
      match lit with
      | Literal.Equation(u, v, true) 
        when (Type.is_var (T.ty u) || Type.is_prop (T.ty u)) 
             && T.is_app_var u ->
        let app_vars = 
          if Literal.is_predicate_lit lit then [(u, PB.to_pos (PB.left lit_pos))]
          else if Term.is_app_var v then [(u, PB.to_pos (PB.left lit_pos));
                                          (v, PB.to_pos (PB.right lit_pos))]
          else []
        in
        List.fold_left (fun acc (var, pos) -> 
          List.fold_left (fun acc p -> 
            let seq = 
              get_unif_alg () (p.unif_partner, sc_partner) (var, sc_cl)
              |> OSeq.filter_map (
                CCOpt.map (fun us -> 
                  assert(not (Unif_subst.has_constr us));
                  let sub = Unif_subst.subst us in
                  let eligible' = C.eligible_res (c, sc_cl) sub in
                  (* not eligible under substitution *)
                  if not (CCBV.get eligible' idx) then None
                  else (
                    Some (mk_res sub pos p)
                )))
            in
            if Env.should_force_stream_eval () then (
              Env.get_finite_infs [seq] @ acc
            ) else (
              let stm_res = Env.Stm.make ~penalty:(C.penalty c + 2) ~parents:[c] ( seq) in
              Env.StmQ.add (Env.get_stm_queue ()) stm_res;
              acc)
          ) acc partners;          
        ) acc app_vars
      | _ -> acc
    ) []

  let false_elim c =
    let module S = Subst.FO in

    let p sub renaming =
      Proof.Step.inference [C.proof_parent_subst renaming (c,0) sub]
        ~rule:(Proof.Rule.mk ("false_elim")) 
        ~tags:[Proof.Tag.T_ho] 
    in

    let add_immediate acc sub idx =
      let renaming = Subst.Renaming.create () in
      let res = 
        C.apply_subst ~renaming ~proof:(Some (p sub renaming)) (c,0) sub 
      in
      res :: acc
    in
      
    let schedule app_var target idx =     
      assert(T.is_ground target);
      let seq =
        get_unif_alg () (app_var, 0) (target, 0)
        |> OSeq.filter_map (
            CCOpt.map (fun us -> 
              assert(not (Unif_subst.has_constr us));
              let sub = Unif_subst.subst us in
              let renaming = Subst.Renaming.create () in
              let res = 
                C.apply_subst ~penalty_inc:(Some 1) ~renaming 
                              ~proof:(Some (p sub renaming)) (c,0) sub 
              in
              (* not eligible under substitution *)
              if not @@ CCBV.get (C.eligible_res_no_subst res) idx then None
              else (
                Some (res)
            )))
        in
      if Env.should_force_stream_eval () then (
        Env.get_finite_infs [seq]
      ) else (
        let stm_res = Env.Stm.make ~penalty:(C.penalty c) ~parents:[c] (seq) in
        Env.StmQ.add (Env.get_stm_queue ()) stm_res;
        []
      )
    in

    let eligible = C.eligible_res (c, 0) Subst.empty in
    let get_var = T.as_var_exn in
    
    Ls.fold_eqn_simple (C.lits c)
    |> Iter.filter (fun (_,_,_,p) ->
      let idx = Ls.Pos.idx p in
      CCBV.get eligible idx )
    |> Iter.fold (fun acc (lhs,rhs,sign,p) -> 
      let idx = Ls.Pos.idx p in
      if T.equal T.true_ rhs then (
        if T.is_var lhs then (
          let sub = 
            S.bind' Subst.empty (get_var lhs, 0) (T.false_, 0) in
          add_immediate acc sub idx
        ) else if T.is_app_var lhs then (
          schedule lhs T.false_ idx @ acc
        ) else acc
      ) else if T.equal T.false_ rhs then (
        if T.is_var lhs then (
          let sub = S.bind' Subst.empty (get_var lhs, 0) (T.true_, 0) in
          add_immediate acc sub idx
        ) else if T.is_app_var lhs then (
          schedule lhs T.true_ idx @ acc
        ) else acc
      ) else if T.is_var (T.head_term lhs) 
                && T.is_var (T.head_term rhs)
                && sign then (
          if T.is_var lhs && T.is_var rhs && Type.is_prop (T.ty lhs) then (
            let sub_t_f = 
              S.bind' 
                (S.bind' Subst.empty (get_var lhs, 0) (T.true_, 0))
                (get_var rhs,0) (T.false_, 0)
            in
            let sub_f_t = 
              S.bind' 
                (S.bind' Subst.empty (get_var lhs, 0) (T.false_, 0))
                (get_var rhs,0) (T.true_, 0)
            in
            add_immediate (add_immediate acc sub_t_f idx) sub_f_t idx
          ) else (
            let l_eq_r = T.Form.eq lhs rhs in
            let t_eq_f = T.Form.eq T.true_ T.false_ in
            let f_eq_t = T.Form.eq T.false_ T.true_ in
          
            schedule l_eq_r t_eq_f idx @ schedule l_eq_r f_eq_t idx @ acc
          )
      ) else acc
    ) []

  let replace_bool_vars (c:C.t) =
    let p =
      Proof.Step.simp [C.proof_parent c]
        ~rule:(Proof.Rule.mk ("replace_bool_vars")) 
        ~tags:[Proof.Tag.T_ho] in

    let all_bool_substs vars =
      let sc = 0 in

      assert (not (CCList.is_empty vars));
      let rec aux = function
        | [] -> assert false
        | [v] ->
          [Subst.FO.bind' Subst.empty (v,sc) (T.true_, sc);
           Subst.FO.bind' Subst.empty (v,sc) (T.false_, sc)]
        | v :: vs ->
          CCList.flat_map (fun subst -> 
            [Subst.FO.bind' subst (v,sc) (T.true_, sc);
             Subst.FO.bind' subst (v,sc) (T.false_, sc)]
          ) (aux vs)
      in
      aux vars
    in


    get_bool_eligible c
    |> Iter.find_pred (fun (t,_) -> 
        Type.is_prop (T.ty t) 
          &&
        (match T.view t with
        | T.AppBuiltin(Builtin.(Eq|Neq), [_;a;b]) when Type.is_prop (T.ty a) ->
          (* tyarg does not have to be a variable *)
          T.is_var a && T.is_var b
        | T.AppBuiltin(hd, args) ->
          (Builtin.is_logical_binop hd || hd = Builtin.Not)
          && List.for_all T.is_var args
        | _ -> false)) 
    |> CCOpt.map (fun (t,_) -> 
        let vars = T.VarSet.to_list (T.vars t) in
        assert (List.for_all (fun t -> Type.is_prop (HVar.ty t)) vars);
        all_bool_substs vars
        |> List.map (C.apply_subst ~proof:(Some p) (c,0)))
    
  let replace_bool_app_vars (c:C.t) =
    let p sub renaming =
      Proof.Step.simp [C.proof_parent_subst renaming (c,0) sub]
        ~rule:(Proof.Rule.mk ("replace_bool_app_vars")) 
        ~tags:[Proof.Tag.T_ho] 
      in

    let is_var_headed t = T.is_var (T.head_term t) in
    let is_eligible xs =
      CCOpt.return_if ((List.for_all is_var_headed xs) && (List.exists T.is_app_var xs)) xs
    in

    let create_targets n =
      let rec aux = function 
        | 0 -> []
        | 1 -> [[T.true_]; [T.false_]]
        | n -> 
          let rest = aux (n-1) in
          List.rev_append (List.rev_map (List.cons T.true_) rest)
                          (List.rev_map (List.cons T.false_) rest)
      in
      aux n
    in 

    let stms = 
      get_bool_eligible c
      |> Iter.filter_map (fun (t,_) ->
          (
          match T.view t with
          | T.AppBuiltin((Eq|Neq), [_;a;b]) when Type.is_prop (T.ty a) ->
            (* tyarg does not have to be a variable *)
            is_eligible [a;b]
          | T.AppBuiltin(hd, args)
            when (Builtin.is_logical_binop hd || hd = Builtin.Not) && Type.is_prop (T.ty t) ->
            is_eligible args
          | _ -> None))
      |> Iter.flat_map_l (fun (args) -> 
        List.map (fun target -> 
          get_unif_alg_l () (args, 0) (target, 0)
          |> OSeq.filter_map (
              CCOpt.map (fun us -> 
                assert(not (Unif_subst.has_constr us));
                let sub = Unif_subst.subst us in
                let renaming = Subst.Renaming.create () in
                let res = 
                  C.apply_subst ~penalty_inc:(Some 1) ~renaming 
                                ~proof:(Some (p sub renaming)) (c,0) sub 
                in
                (* not eligible under substitution *)
                Some (res)
              ))) (create_targets (List.length args)))
        |> Iter.to_list
    in
    if Env.should_force_stream_eval () then (
      Env.get_finite_infs stms 
    ) else (
      Env.StmQ.add_lst (Env.get_stm_queue ()) (
        List.map (fun seq -> 
          Env.Stm.make ~penalty:(C.penalty c) ~parents:[c] (seq)) stms
      );
      []
    )

  let quantifier_rw_and_hoist (c:C.t) =
    let quant_rw ~at b body =
      let quant_rw_unapplicable =
        let module P = Pos in
        match at with
        | P.Arg(i, P.Left P.Stop)
        | P.Arg(i, P.Right P.Stop) when L.is_predicate_lit (C.lits c).(i) ->
          (* CAUTION: Using is_pos to really mean is the RHS of the
             term true or false? *)
          let sign = L.is_positivoid ((C.lits c).(i)) in
          (Builtin.equal b Builtin.ForallConst && sign)
          || (Builtin.equal b Builtin.ExistsConst && not sign)
        | _ -> false
      in

      if quant_rw_unapplicable then None
      else (
        let proof =
        Proof.Step.simp [C.proof_parent c]
          ~rule:(Proof.Rule.mk ("quantifier_rw")) 
          ~tags:[Proof.Tag.T_ho] 
        in
        let body = Combs.expand body in
        let form_for_skolem = 
          (if b = Builtin.ForallConst then T.Form.not_ else CCFun.id) 
          (snd @@ T.open_fun body) in
        let sk = 
          FR.get_skolem ~parent:c ~mode:`SkolemRecycle
          (T.fun_l (fst @@ T.open_fun body) form_for_skolem) in
        let repl = T.app body [sk] in
        let new_lits = CCArray.copy (C.lits c) in
        Literals.Pos.replace ~at ~by:repl new_lits;
        Some (C.create ~trail:(C.trail c) ~penalty:(C.penalty c) 
          (Array.to_list new_lits) proof)
    )
    in
    
    let quant_hoist ~old b body =
      let proof ~prefix = 
      Proof.Step.inference [C.proof_parent c]
        ~rule:(Proof.Rule.mk (prefix^"_hoist")) ~tags:[Proof.Tag.T_ho] in

      let fresh_var ~body = 
        assert(Type.is_fun (T.ty body));
        let pref,_ = Type.open_fun (T.ty body) in
        assert(List.length pref == 1);
        let var_ty = List.hd pref in
        let fresh_var  = HVar.fresh ~ty:var_ty () in
        T.var fresh_var
      in
      let subst_t = fresh_var ~body in
      match b with 
      | Builtin.ForallConst ->
        let new_lit = yes (T.app body [subst_t]) in
        let res = mk_res ~proof:(proof ~prefix:"forall") ~old ~repl:T.false_ new_lit c in
        if Type.returns_prop (T.ty subst_t) then (
          Signal.send Env.on_pred_var_elimination (res, subst_t));
        res
      | Builtin.ExistsConst ->
        let new_lit = no (T.app body [subst_t]) in
        let res = mk_res ~proof:(proof ~prefix:"exists") ~old ~repl:T.true_ new_lit c in
        if Type.returns_prop (T.ty subst_t) then (
          Signal.send Env.on_pred_var_elimination (res, subst_t));
        res
      | _ -> assert false
    in
        
    
    get_bool_eligible c
    |> Iter.fold (fun acc (t,p) -> 
      match T.view t with
      | T.AppBuiltin(Builtin.(ForallConst|ExistsConst) as b, [_;body]) ->
        let hoisted = quant_hoist ~old:t b body in
        let rest = if CCArray.exists Literal.is_trivial (C.lits hoisted) then acc else hoisted :: acc in
        CCList.cons_maybe (quant_rw ~at:p b body) (rest)
      | _ -> acc) []
    |> (fun res -> CCOpt.return_if (not @@ CCList.is_empty res) res)

   let nested_eq_rw c =
    (* TODO(BOOL): currently incompatible with combiantors *)
    let sc = 0 in
    let mk_sc t = (t,sc) in 
    let parents r s = [C.proof_parent_subst r (mk_sc c) s ] in
    get_bool_eligible c
    |> Iter.filter_map (fun (t,p) -> 
      match T.view t with
      | T.AppBuiltin((Builtin.(Eq|Neq|Equiv|Xor) as hd), ([a;b]|[_;a;b])) ->
        Some (
          get_unif_alg () (mk_sc a) (mk_sc b)
          |> OSeq.map (fun unif_subst_opt ->
              CCOpt.map (fun unif_subst -> 
                assert (not @@ US.has_constr unif_subst);
                let subst = US.subst unif_subst in
                let repl = 
                  if hd = Builtin.Eq || hd = Builtin.Equiv
                  then T.true_ else T.false_ in
                let new_lits = Array.copy (C.lits c) in
                Literals.Pos.replace ~at:p ~by:repl new_lits;
                let renaming = Subst.Renaming.create () in
                let new_lits = 
                  Literals.apply_subst renaming subst (mk_sc new_lits)
                  |> CCArray.to_list 
                in
                let rule = Proof.Rule.mk ((if T.equal repl T.true_ then "eq" else "neq") ^ "_rw")  in
                let proof = Proof.Step.inference ~tags:[Proof.Tag.T_ho] ~rule (parents renaming subst) in
                C.create ~penalty:(C.penalty c) ~trail:(C.trail c) new_lits proof
              ) unif_subst_opt))
      | _ -> None)
    |> Iter.flat_map_l (fun clause_seq ->
      if Env.should_force_stream_eval () then (
        Env.get_finite_infs [clause_seq]
      ) else (
      let stm_res = Env.Stm.make ~penalty:(C.penalty c) ~parents:[c] (clause_seq) in
      Env.StmQ.add (Env.get_stm_queue ()) stm_res;
      []
    ))
    |> Iter.to_rev_list

  let rename_nested_booleans c =
    let module L = Literal in

    (* Introduce a new simple name of the form P(vars) for the
       given literal -- renaming clauses will stop combinatorial explosion
       when hoisting boolean subterms *)
    let rename_lit lit =
      let sign = L.is_positivoid lit in
      if L.is_predicate_lit lit then (
        match lit with
        | L.Equation(lhs,_,_) ->
          CCOpt.get_exn (FR.rename_form ~polarity_aware:false ~c lhs sign)
        | _ -> assert false
      ) else (
        let mk_form =
          (if sign then T.Form.eq else T.Form.neq) 
        in
        match lit with
        | L.Equation(lhs,rhs,_) ->
          CCOpt.get_exn (FR.rename_form ~polarity_aware:false ~c (mk_form lhs rhs) sign)
        | _ -> assert false
      )
    in

    (* literal needs to have at least 2 nested booleans *)
    let threshold = 2 in

    C.bool_selected c
    |> List.map (fun (_, lit_pos) -> 
      Literals.Pos.idx lit_pos)
    |> CCList.group_by ~hash:CCInt.hash ~eq:CCInt.equal 
    |> List.map (fun idx_list -> (List.hd idx_list, List.length idx_list))
    |> List.filter (fun (_, cnt) -> cnt >= threshold)
    |> (function 
       | x :: xs when not (CCList.is_empty xs) ->
         (* there need to be at least two literals with deep booleans.
            we will rename all but one (for example the first one) *)
          let new_lits = CCArray.copy (C.lits c) in
          let (new_defs, new_parents) = 
            CCList.fold_left (fun (defs, parents) (idx, _) -> 
              let lit = (C.lits c).(idx) in
              let renamer, new_defs, new_parents = rename_lit lit in
              let new_lit = Literal.mk_prop renamer (Literal.is_positivoid lit) in
              new_lits.(idx) <- new_lit;
              (new_defs @ defs, new_parents @ parents)
            ) ([], []) xs
          in
          let rule = Proof.Rule.mk "rename_nested_bools" in
          let parents = List.map C.proof_parent (c :: new_parents) in
          let proof = Proof.Step.simp  ~rule parents in
          let renamed =
            C.create_a ~penalty:(C.penalty c) ~trail:(C.trail c) new_lits proof in

          Util.debugf ~section 1 "renamed @[%a@] into@. @[%a@]" 
            (fun k -> k C.pp c C.pp renamed);
          Util.debugf ~section 1 "new_defs @[%a@]" (fun k -> k (CCList.pp C.pp) new_defs);
          Some (renamed :: new_defs)
       | _ -> None)

  let simplify_bools t =
    let negate t =
      match T.view t with
      | T.AppBuiltin(((Builtin.Eq|Builtin.Neq) as b), l) ->
        let hd = if b = Builtin.Eq then Builtin.Neq else Builtin.Eq in
        T.app_builtin ~ty:(T.ty t) hd l
      | T.AppBuiltin(Builtin.Not, [s]) -> s
      | _ -> T.Form.not_ t in

    let simplify_and_or t b l =
      let open Term in
      let compl_in_l l =
        let pos, neg = 
          CCList.partition_map (fun t ->
              match view t with
              | AppBuiltin(Builtin.Not, [s]) -> `Right s
              | _ -> `Left t) l
          |> CCPair.map_same Set.of_list in
        not (Set.is_empty (Set.inter pos neg)) in
      
      let res = 
        assert(b = Builtin.And || b = Builtin.Or);
        let netural_el, absorbing_el = 
          if b = Builtin.And then true_,false_ else (false_,true_) in

        let l' = CCList.sort_uniq ~cmp:compare l in

        if compl_in_l l || List.exists (equal absorbing_el) l then absorbing_el
        else (
          let l' = List.filter (fun s -> not (equal s netural_el)) l' in
          if List.length l = List.length l' then t
          else (
            if CCList.is_empty l' then netural_el
            else (if List.length l' = 1 then List.hd l'
                  else app_builtin ~ty:(Type.prop) b l')
          ))
      in
      res 
    in

  let rec aux t =
    let ty_is_prop t = Type.is_prop (T.ty t) in
    match T.view t with 
    | DB _ | Const _ | Var _ -> t
    | Fun(ty, body) ->
      let body' = aux body in
      assert(Type.equal (T.ty body) (T.ty body'));
      if T.equal body body' then t
      else T.fun_ ty body'
    | App(hd, args) ->
      let hd' = aux hd and args' = List.map aux args in
      if T.equal hd hd' && T.same_l args args' then t
      else T.app hd' args'
    | AppBuiltin (Builtin.And, [x]) 
        when T.is_true_or_false x
             && ty_is_prop t
             && List.length (Type.expected_args (T.ty t)) = 1 ->
      if T.equal x T.true_ then (
        T.fun_ Type.prop (T.bvar ~ty:Type.prop 0)
      ) else (
        assert (T.equal x T.false_);
        T.fun_ Type.prop T.false_
      )
    | AppBuiltin(Builtin.Or, [x]) 
        when T.is_true_or_false x 
             && ty_is_prop t
             && List.length (Type.expected_args (T.ty t)) = 1 ->
      let prop = Type.prop in
      if T.equal x T.true_ then (
        T.fun_ prop T.true_
      ) else (
        assert (T.equal x T.false_);
        T.fun_ prop (T.bvar ~ty:prop 0)
      )
    | AppBuiltin(Builtin.And, l)
      when  ty_is_prop t &&
            List.length l > 1 ->
      let l' = List.map aux l in
      let t = 
        if T.same_l l l' then t 
        else T.app_builtin ~ty:(Type.prop) Builtin.And l' in
      simplify_and_or t Builtin.And l'
    | AppBuiltin(Builtin.Or, l)
      when ty_is_prop t &&
           List.length l > 1 ->
      let l' = List.map aux l in
      let t = 
        if T.same_l l l' then t 
        else T.app_builtin ~ty:(Type.prop) Builtin.Or l' in
      simplify_and_or t Builtin.Or l'
    | AppBuiltin(Builtin.Not, [s]) ->
      let s' = aux s in
      begin match T.view s' with 
      | AppBuiltin(Builtin.Not, [s'']) -> s''
      | _ ->  
        if T.equal s' T.true_ then T.false_
        else if T.equal s' T.false_ then T.true_
        else if T.equal s s' then t 
        else T.app_builtin ~ty:(Type.prop) Builtin.Not [s'] end
    | AppBuiltin(Builtin.Imply, [p;c]) ->
      let unroll_and p = match T.view p with 
        | AppBuiltin(And, l) -> T.Set.of_list l
        | _ -> T.Set.singleton p in
      let unroll_or p = match T.view p with 
        | AppBuiltin(Or, l) -> T.Set.of_list l
        | _ -> T.Set.singleton p in
      let is_impl p = match T.view p with 
        | AppBuiltin(Imply, [l;r]) -> true
        | _ -> false in
      
      (* Take a term of the form p11 /\ ... /\ p1n1 -> p21 /\ ... /\ p2n2 -> ... -> q1 \/ ... \/ qn  
         and return the set of premises {p11,...} and conclusions {q1, ... }
      *)
      let unroll_impl p =
        assert(is_impl p);
        let rec aux acc p =
          match T.view p with
          | AppBuiltin(Imply, [l;r]) ->
            let unrolled_l = unroll_and l in
            let acc' = Term.Set.union unrolled_l acc in
            if is_impl r then aux acc' r
            else (acc', unroll_or r)
          | _ -> assert false in
        aux Term.Set.empty p
      in
      
      let p' = aux p and c' = aux c in
      let (premises,conclusions) = unroll_impl (T.Form.imply p' c') in

      if not (T.Set.is_empty (T.Set.inter premises conclusions)) then (
        T.true_
      ) else if T.equal p' c' then T.true_
      else if T.equal c' (negate p') then c'
      else if T.equal p' (negate c') then c'
      else if T.equal p' T.true_ then c'
      else if T.equal p' T.false_ then T.true_
      else if T.equal c' T.false_ then aux (T.Form.not_ p')
      else if T.equal c' T.true_ then T.true_
      else (
        if T.equal p p' && T.equal c c' then t
        else T.app_builtin ~ty:(T.ty t) Builtin.Imply [p';c']
      )
    | AppBuiltin((Builtin.Eq | Builtin.Equiv) as hd, ([a;b]|[_;a;b])) when Type.is_prop (T.ty t)->
      let cons = if hd = Builtin.Eq then T.Form.eq else T.Form.equiv in
      let a',b' = aux a, aux b in
      if T.equal a' b' then T.true_ 
      else if T.equal a' T.true_ then b'
      else if T.equal b' T.true_ then a'
      else if T.equal a' T.false_ then aux (T.Form.not_ b')
      else if T.equal b' T.false_ then aux (T.Form.not_ a')
      else (
        if T.equal a a' && T.equal b b' then t 
        else cons a' b'
      )
    | AppBuiltin((Builtin.Neq | Builtin.Xor) as hd, ([a;b]|[_;a;b])) when Type.is_prop (T.ty t) ->
      let cons = if hd = Builtin.Neq then T.Form.neq else T.Form.xor in
      let a',b' = aux a, aux b in
      if T.equal a' b' then T.false_ 
      else if T.equal a' T.true_ then aux (T.Form.not_ b')
      else if T.equal b' T.true_ then aux (T.Form.not_ a')
      else if T.equal a' T.false_ then b'
      else if T.equal b' T.false_ then a'
      else (
        if T.equal a a' && T.equal b b' then t 
        else cons a' b'
      )
    | AppBuiltin((ExistsConst|ForallConst) as b, [tyarg;g]) ->
      let g' = aux g in
      let exp_g = Combs.expand g' in
      let _, body = T.open_fun exp_g in
      assert(Type.is_prop (T.ty body));
      if (T.Seq.subterms ~include_builtin:true body
          |> Iter.exists T.is_bvar) then (
        if T.equal g g' then t
        else T.app_builtin ~ty:(T.ty t) b [tyarg; g']
      ) else body
    | AppBuiltin(hd, args) ->
      let args' = List.map aux args in
      if T.same_l args args' then t
      else T.app_builtin ~ty:(T.ty t) hd args' in  
  
  let res = aux t in
  assert (T.DB.is_closed res);
  res

  (* Look at the HOSup paper for the definition of unsupported quant *)
  let fix_unsupported_quant t =
    (* for the time being var_ty is not used.. if the definition
       changes it is useful to have it around *)
    let quant_normal _ q_body =
      let has_loosely_bound_0 t =
        List.mem 0 (T.DB.unbound t)
      in
      let rec aux t =
        match T.view t with
        | Fun(_, body) -> not (has_loosely_bound_0 t)
        | App(hd, args) ->
          if T.is_const hd then List.for_all aux args
          else (
            not (List.exists has_loosely_bound_0 (hd::args))
          )
        | AppBuiltin(hd, args) -> List.for_all aux args
        | _ -> true
      in
      let res = aux q_body in
      res
    in

    let rec aux t =
      match T.view t with 
      | Fun(ty,body) ->
        let body' = aux body in
        assert(Type.equal (T.ty body) (T.ty body'));
        if T.equal body body' then t
        else T.fun_ ty body'
      | App(hd,args) ->
        let hd' = aux hd in
        let args' = List.map aux args in
        if T.equal hd hd' && T.same_l args args' then t
        else T.app hd' args'
      | AppBuiltin((ExistsConst|ForallConst) as hd, [alpha]) ->
        let alpha = Type.of_term_unsafe (alpha :> InnerTerm.t) in
        let alpha2prop = Type.arrow [alpha] Type.prop in
        let inner_quant = 
          let body = 
            if Builtin.equal hd ExistsConst then T.false_
            else T.true_
          in
          T.fun_ alpha body 
        in
        let var = T.bvar ~ty:alpha2prop 0 in
        let body = 
          if Builtin.equal hd ExistsConst then T.Form.neq var inner_quant
          else T.Form.eq var inner_quant
        in
        T.fun_ alpha2prop body
      | AppBuiltin((ExistsConst|ForallConst), ([])) ->
        invalid_arg "type argument must be present"
      | AppBuiltin(hd,args) ->
        let args' = List.map aux args in
        (* fully applied quantifier *)
        if Builtin.is_quantifier hd && List.length args' == 2 then (
          let q_pref, q_body = T.open_fun @@ List.nth args' 1 in
          let var_ty = List.hd q_pref in
          if not (quant_normal var_ty q_body) then (
            if Builtin.equal hd Builtin.ExistsConst then (
              T.Form.neq (List.nth args' 1) (T.fun_ var_ty T.false_)
            ) else (
              T.Form.eq (List.nth args' 1) (T.fun_ var_ty T.true_)
            )
          ) else T.app_builtin ~ty:(T.ty t) hd args'
        ) else (
          if T.same_l args args' then t
          else T.app_builtin ~ty:(T.ty t) hd args'
        )
      | DB _ | Const _ | Var _ -> t
    in
    if Env.flex_get Combinators.k_enable_combinators then t 
    else aux (Lambda.eta_reduce @@ Lambda.snf t)

  let replace_unsupported_quants c =
    let new_lits = Literals.map fix_unsupported_quant (C.lits c) in
    if Literals.equal (C.lits c) new_lits then (
      None
    ) else (
      let proof = Proof.Step.simp [C.proof_parent c] 
          ~rule:(Proof.Rule.mk "replace unsupported quants") in
      let new_ = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) 
          (Array.to_list new_lits) proof in
      Some new_
    )

  let simpl_bool_subterms c =
    try
      let new_lits = Literals.map simplify_bools (C.lits c) in
      if Literals.equal (C.lits c) new_lits then (
        SimplM.return_same c
      ) else (
        let proof = Proof.Step.simp [C.proof_parent c] 
            ~rule:(Proof.Rule.mk "simplify boolean subterms") in
        let new_ = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) 
            (Array.to_list new_lits) proof in
        SimplM.return_new new_
      )
    with Type.ApplyError err ->
      CCFormat.printf "error(%s):@[%a@]@." err C.pp c;
      CCFormat.printf "@[%a@]@." Proof.S.pp_tstp (C.proof c);
      assert false

  let nnf_bools t =
    let module F = T.Form in
    let expand_quant = not @@ Env.flex_get Combinators.k_enable_combinators in
    let rec aux t =
      match T.view t with 
      | Const _ | DB _ | Var _ -> t
      | Fun _ ->
        let tyargs, body = T.open_fun t in
        let body' = aux body in
        if T.equal body body' then t
        else T.fun_l tyargs body'
      | App(hd, l) ->
        let hd' = aux hd and l' = List.map aux l in
        if T.equal hd hd' && T.same_l l l' then t
        else T.app hd' l'
      | AppBuiltin (Builtin.Not, [f]) ->
        begin match T.view f with 
        | AppBuiltin(Not, [g]) -> aux g
        | AppBuiltin( ((And|Or) as b), l) when List.length l >= 2 ->
          let flipped = if b = Builtin.And then F.or_l else F.and_l in
          flipped (List.map (fun t -> aux (F.not_ t))  l)
        | AppBuiltin( ((ForallConst|ExistsConst) as b), ([g]|[_;g]) ) ->
          let flipped = 
            if b = Builtin.ForallConst then Builtin.ExistsConst
            else Builtin.ForallConst in
          let g_ty_args, g_body = T.open_fun (Combs.expand g)  in
          let g_body' = aux @@ F.not_ g_body in
          let g' = Lambda.eta_reduce ~expand_quant (T.fun_l g_ty_args g_body') in
          T.app_builtin ~ty:(T.ty t) flipped [g']
        | AppBuiltin( Imply, [g;h] ) ->
          F.and_ (aux g) (aux @@ F.not_ h)
        | AppBuiltin( ((Equiv|Xor) as b), [g;h] ) ->
          let flipped = if b = Equiv then Builtin.Xor else Builtin.Equiv in
          aux (T.app_builtin ~ty:(T.ty t) flipped [g;h])
        | AppBuiltin(((Eq|Neq) as b), ([_;s;t]|[s;t])) ->
          let flipped = if b = Eq then F.neq else F.eq in
          flipped (aux s) (aux t)
        | _ -> F.not_ (aux f)
        end
      | AppBuiltin(Imply, [f;g]) -> aux (F.or_ (F.not_ f) g)
      | AppBuiltin(Equiv, [f;g]) ->
        aux (F.and_ (F.imply f g) (F.imply g f))
      | AppBuiltin(Xor, [f;g]) ->
        aux (F.and_ (F.or_ f g) (F.or_ (F.not_ f)  (F.not_ g)))
      | AppBuiltin(b, l) ->
        let l' = List.map aux l in
        if T.same_l l l' then t
        else T.app_builtin ~ty:(T.ty t) b l' in
    aux t


  let nnf_bool_subters c =
    let new_lits = Literals.map nnf_bools (C.lits c) in
    if Literals.equal (C.lits c) new_lits then (
      SimplM.return_same c
    ) else (
      let proof = Proof.Step.simp [C.proof_parent c] 
          ~rule:(Proof.Rule.mk "nnf boolean subterms") in
      let new_ = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) 
          (Array.to_list new_lits) proof in
      SimplM.return_new new_
    )

  let normalize_bool_terms c =
    let new_lits = Literals.map T.normalize_bools (C.lits c) in
    if Literals.equal (C.lits c) new_lits then (
      SimplM.return_same c
    ) else (
      let proof = Proof.Step.simp [C.proof_parent c] 
          ~rule:(Proof.Rule.mk "normalize subterms") in
      let new_ = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) 
          (Array.to_list new_lits) proof in
      SimplM.return_new new_
    )
  
  (* Find resolvable boolean literals and resolve them.
     If which is `OnlyPositive then _only_ literals of the form s = t
     are rewritten into s != ~t and then unified. Else, both negative
     and positive literals are unified  *)
  let solve_bool_formulas ~which c =    
    let normalize_not t =
      let rec aux t = 
        match T.view t with
        | T.AppBuiltin(Not, [f]) ->
          begin match T.view f with
          | T.AppBuiltin(Not, [g]) -> aux g
          | T.AppBuiltin( ((Eq|Equiv) as b), l ) ->
            let flipped = 
              if b = Builtin.Eq then Builtin.Neq else Builtin.Xor in
            T.app_builtin flipped l ~ty:(T.ty f)
          | T.AppBuiltin( ((Neq|Xor) as b), l ) ->
            let flipped = 
              if b = Builtin.Neq then Builtin.Eq else Builtin.Equiv in
            T.app_builtin flipped l ~ty:(T.ty f)
          | _ -> t end
        | _ -> t in
      aux t in

    let find_resolvable_form lit =
      let is_var_headed t = T.is_var (T.head_term t) in

      (* Rewrite positive equations of the form F s = t, where t is not
         var-headed into F s != (not t).  *)
      let find_pos_var_headed_eq l r = 
        if is_var_headed l && not (is_var_headed r) then (Some(l, T.Form.not_ r)) 
        else if is_var_headed r && not (is_var_headed l) then (Some(r, T.Form.not_ l))
        else None in

      match (Literal.View.as_eqn lit) with 
      | Some (l,r,sign) ->
        if not (T.is_true_or_false r) && Type.is_prop (T.ty l) then (
          if not sign && which = `All then Some (l,r)
          else if sign then find_pos_var_headed_eq l r
          else None)
        else if T.is_true_or_false r then (
          let apply_sign = if sign then CCFun.id else T.Form.not_ in
          match T.view (normalize_not (apply_sign l)) with 
          | T.AppBuiltin((Neq|Xor), ([f;g]|[_;f;g])) 
            when Type.is_prop (T.ty f) && which == `All ->
            assert(Type.equal (T.ty f) (T.ty g));
            Some (f,g)
          | T.AppBuiltin((Eq|Equiv), ([f;g]|[_;f;g])) when Type.is_prop (T.ty f) ->
            assert(Type.equal (T.ty f) (T.ty g));
            find_pos_var_headed_eq f g
          | _ -> None
        ) else None
      | None -> None in
    
    let unif_alg l r =
      if not (Env.flex_get Combinators.k_enable_combinators) then (
        Env.flex_get Superposition.k_unif_alg (l,0) (r,0)
      ) else OSeq.return (Some (Unif.FO.unify_full (l,0) (r,0))) in
    
    Util.debugf ~section 5 "bool solving @[%a@]@."(fun k -> k C.pp c);

    C.lits c
    |> CCArray.mapi (fun i lit ->
      match find_resolvable_form lit with 
      | None ->
        Util.debugf ~section 5 "for lit %d(@[%a@]) of @[%a@] no resolvable lits found@." 
          (fun k -> k i Literal.pp lit C.pp c);
        None
      | Some (l,r) ->
        let module US = Unif_subst in
        try
          Util.debugf ~section 5 "trying lit @[%d:%a@]@."(fun k -> k i Literal.pp lit);
          Util.debugf ~section 5 "unif problem: @[%a=?=%a@]@."(fun k -> k T.pp l T.pp r);
          let stm = 
            unif_alg l r
            |> OSeq.map (CCOpt.map (fun subst -> 
              let renaming = Subst.Renaming.create () in
              let new_lits =
                CCArray.except_idx (C.lits c) i
                |> CCArray.of_list
                |> (fun l -> 
                    Literals.apply_subst renaming (US.subst subst) (l,0))
                |> CCArray.to_list in
              let proof = 
                Proof.Step.simp ~tags:[Proof.Tag.T_ho]
                  ~rule:(Proof.Rule.mk "solve_formulas")
                  [C.proof_parent_subst renaming (c,0) (US.subst subst) ] in
              let res = C.create ~penalty:(C.penalty c) ~trail:(C.trail c) new_lits proof in
              Util.debugf ~section 5 "solved by @[%a@]@."(fun k ->  k C.pp res);
              res  
            ))
          in
          match stm () with
          | OSeq.Cons(hd, rest) ->
            let stm = Stm.make ~penalty:(C.penalty c) ~parents:[c] rest in
            StmQ.add (Env.get_stm_queue ()) stm;
            hd
          | OSeq.Nil -> None
        with _ -> 
          Util.debugf ~section 5 "failed @." (fun k -> k);
          None)
      |> CCArray.filter_map CCFun.id
      |> CCArray.to_list
      |> (fun l -> if CCList.is_empty l then None else Some l)

  let cnf_otf c : C.t list option =
    let idx = CCArray.find_idx (fun l -> 
        let eq = Literal.View.as_eqn l in
        match eq with 
        | Some (l,r,_) -> 
          Type.is_prop (T.ty l) &&
          not (T.equal l r) &&
          ((not (T.equal r T.true_) && not (T.equal r T.false_))
           || T.is_formula l || T.is_formula r)
        | None            -> false 
      ) (C.lits c) in

    let renaming_weight = 40 in
    let max_formula_weight = 
      C.Seq.terms c 
      |> Iter.filter T.is_formula
      |> Iter.map T.size
      |> Iter.max in
    let opts = 
      match max_formula_weight with
      | None -> [Cnf.DisableRenaming]
      | Some m -> if m < renaming_weight then [Cnf.DisableRenaming] else [] in

    match idx with 
    | Some _ ->
      let f = Literals.Conv.to_tst (C.lits c) in
      let proof = Proof.Step.simp ~rule:(Proof.Rule.mk "cnf_otf") ~tags:[Proof.Tag.T_ho] [C.proof_parent c] in
      let trail = C.trail c and penalty = C.penalty c in
      let stmt = Statement.assert_ ~proof f in
      let cnf_vec = Cnf.convert @@ CCVector.to_iter @@ Cnf.cnf_of ~opts ~ctx:(Ctx.sk_ctx ()) stmt in
      CCVector.iter (fun cl -> 
          Statement.Seq.ty_decls cl
          |> Iter.iter (fun (id,ty) -> 
            Ctx.declare id ty; 
            ID.set_payload id (ID.Attr_skolem ID.K_after_cnf)
          )) cnf_vec;
      let solved = 
        if Env.flex_get k_solve_formulas then (
          CCOpt.get_or ~default:[] (solve_bool_formulas ~which:`All c))
        else [] in

      let clauses = CCVector.map (C.of_statement ~convert_defs:true) cnf_vec
                    |> CCVector.to_list 
                    |> CCList.flatten
                    |> List.map (fun c -> 
                        C.create ~penalty  ~trail (CCArray.to_list (C.lits c)) proof) in
      Util.debugf ~section 5 "cl:@[%a@]@." (fun k-> k C.pp c);
      Util.debugf ~section 5 " @[%a@]@." (fun k-> k (CCList.pp C.pp) clauses);
      List.iteri (fun i new_c -> 
          assert((C.proof_depth c) <= C.proof_depth new_c);) clauses;
      Some (solved @clauses)
    | None -> None

  let cnf_infer cl = 
    CCOpt.get_or ~default:[] (cnf_otf cl)

  let interpret_boolean_functions c =
    (* Collects boolean functions only at top level, 
       and not the ones that are already a part of the quantifier *)
    let collect_tl_bool_funs t k = 
      let rec aux t =
        let ty_args, ret_ty = Type.open_fun (T.ty t) in
        if not (CCList.is_empty ty_args) 
           && Type.is_prop ret_ty
           && not (T.is_var t) 
           && not (T.is_app_var t) then k t else(
          match T.view t with
          | App (f, l) ->
            (* head positions are not taken into account *)
            List.iter aux l
          | AppBuiltin (b,l) when not (Builtin.is_quantifier b) ->
            List.iter aux l
          | _ -> ())
      in
      aux t in
    let interpret t i = 
      let ty_args, body = T.open_fun t in
      assert(Type.is_prop (Term.ty body));
      T.fun_l ty_args i 
    in
    let negate_bool_fun bool_fun =
      let ty_args, body = T.open_fun bool_fun in
      assert(Type.is_prop (Term.ty body));
      T.fun_l ty_args (T.Form.not_ body)
    in

    let forall_close t = 
      let ty_args, body = T.open_fun t in
      assert(Type.is_prop (T.ty body));

      List.fold_right (fun ty acc -> 
        T.Form.forall (T.fun_ ty acc)
      ) ty_args body in 

    Iter.flat_map collect_tl_bool_funs 
      (C.Seq.terms c
       (* If the term is a top-level function, then apply extensionality,
          not this rule on it. *)
       |> Iter.filter (fun t -> not @@ Type.is_fun (T.ty t)))
    (* avoiding terms introduced by primitive enumeration *)
    |> Iter.filter (fun t ->
        let cached_t = Subst.FO.canonize_all_vars t in
        not (Term.Set.mem cached_t !Higher_order.prim_enum_terms))
    |> Iter.sort_uniq ~cmp:Term.compare
    |> Iter.fold (fun res t -> 
        assert(T.DB.is_closed t);
        let proof = Proof.Step.inference [C.proof_parent c]
            ~rule:(Proof.Rule.mk"interpret boolean function") ~tags:[Proof.Tag.T_ho]
        in

        let t' = Combs.expand t in
        let _,t'_body = T.open_fun t' in

        if not (T.is_true_or_false t'_body) then (
          let as_forall = Literal.mk_prop (forall_close t') false in
          let as_neg_forall = Literal.mk_prop (forall_close (negate_bool_fun t')) false in
          let forall_cl =
            C.create ~trail:(C.trail c) ~penalty:(C.penalty c)
              (as_forall :: Array.to_list(C.lits c |> Literals.map(T.replace ~old:t ~by:(interpret t' T.true_))))
              proof in
          let forall_neg_cl = 
            C.create ~trail:(C.trail c) ~penalty:(C.penalty c)
              (as_neg_forall :: Array.to_list(C.lits c |> Literals.map(T.replace ~old:t ~by:(interpret t' T.false_))))
              proof in

          Util.debugf ~section  5 "interpret bool(@[%a@]):@.@[%a@] !!> @. @[%a@]@."  
            (fun k -> k T.pp t Literals.pp (C.lits c) Literals.pp (C.lits forall_cl));
          Util.debugf ~section  5 "interpret bool(@[%a@]):@.@[%a@] !!> @. @[%a@]@." 
            (fun k -> k T.pp t Literals.pp (C.lits c) Literals.pp (C.lits forall_neg_cl));

          forall_cl :: forall_neg_cl :: res
        ) else res) 
      []

  let setup () =
    (* Env.add_basic_simplify normalize_equalities; put into superposition right now *)
    if Env.flex_get k_replace_unsupported_quants then (
      Signal.once Env.on_start (fun () -> 
        Env.ProofState.PassiveSet.clauses ()
          |> C.ClauseSet.iter (fun cl -> 
          match replace_unsupported_quants cl with
          | None -> ()
          | Some new_ -> 
            Env.remove_passive (Iter.singleton cl); 
            Env.add_passive (Iter.singleton new_);
        )););
    match Env.flex_get k_bool_reasoning with 
    | BoolReasoningDisabled -> ()
    | BoolCasesPreprocess ->
      Env.add_unary_inf "false_elim" false_elim;
    | _ ->
      if Env.flex_get k_solve_formulas then (
        Env.add_unary_inf "solve formulas" (
          fun c -> 
            CCOpt.get_or ~default:[] @@
            solve_bool_formulas ~which:`OnlyPositive c
        ));
      if Env.flex_get k_trigger_bool_inst > 0 || Env.flex_get k_trigger_bool_ind > 0 then (
        Signal.on Env.on_pred_var_elimination handle_new_pred_var_clause;
        Signal.on Env.FormRename.on_pred_skolem_introduction handle_new_skolem_sym;
      );
      if Env.flex_get k_trigger_bool_ind > 0 then (
        Env.add_unary_inf "trigger bool ind" trigger_induction
      );
      if Env.flex_get k_simplify_bools then (
        Env.add_basic_simplify simpl_bool_subterms
      );
      if Env.flex_get k_nnf then (
        E.add_basic_simplify nnf_bool_subters;
      );
      if Env.flex_get k_norm_bools then (
        Env.add_basic_simplify normalize_bool_terms
      );
      if not !Lazy_cnf.enabled then (
        Env.add_multi_simpl_rule ~priority:2 Fool.rw_bool_lits;
        if Env.flex_get k_cnf_non_simpl then (
          Env.add_unary_inf "cnf otf inf" cnf_infer;
        ) else  Env.add_multi_simpl_rule ~priority:2 cnf_otf);
      if (Env.flex_get k_interpret_bool_funs) then (
        Env.add_unary_inf "interpret boolean functions" interpret_boolean_functions;
      );

      Env.add_unary_inf "false_elim" false_elim;
      if Env.flex_get k_bool_reasoning = BoolHoist then (
        if Env.flex_get k_bool_hoist_simpl
        then Env.add_multi_simpl_rule ~priority:1000 bool_hoist_simpl;
        Env.add_unary_inf "bool_hoist" bool_hoist;

        if Env.flex_get k_rename_nested_bools then (
          Env.add_multi_simpl_rule ~priority:500 rename_nested_booleans
        );

        Env.add_unary_inf "formula_hoist" eq_hoist;
        Env.add_multi_simpl_rule ~priority:100 replace_bool_vars;
        Env.add_multi_simpl_rule ~priority:90 quantifier_rw_and_hoist;
        Env.add_unary_inf "eq_rw" nested_eq_rw;

        if Env.flex_get Superposition.k_ho_basic_rules then (
          if Env.flex_get k_bool_app_var_repl then (
            Env.add_unary_inf "replace_bool_app_vars" replace_bool_app_vars
          );
          if Env.flex_get k_fluid_hoist then (
            Env.add_unary_inf "fluid_hoist" fluid_hoist
          );
          if Env.flex_get k_fluid_log_hoist then (
            Env.add_unary_inf "fluid_log_hoist" fluid_log_hoist;
            Env.add_unary_inf "fluid_quant_rw" fluid_quant_rw;
          );
        );
      )
end


open CCFun
open Builtin
open Statement
open TypedSTerm
open CCList


let if_changed proof (mk: ?attrs:Logtk.Statement.attrs -> 'r) s f p =
  let fp = f s p in
  if fp == [p] then [s] else map(fun x -> mk ~proof:(proof s) x) fp
(* match fp with 
   | [ x ] when TypedSTerm.equal x p -> [s]
   | _ -> map(fun x -> mk ~proof:(proof s) x) fp *)

let map_propositions ~proof f =
  CCVector.flat_map_list(fun s -> match Statement.view s with
      | Assert p	-> if_changed proof assert_ s f p
      | Lemma ps	-> if_changed proof lemma s (map%f) ps
      | Goal p	-> if_changed proof goal s f p
      | NegatedGoal(ts, ps)	-> if_changed proof (neg_goal ~skolems:ts) s (map%f) ps
      | _ -> [s]
    )


let is_bool t = CCOpt.equal Ty.equal (Some prop) (ty t)
let is_T_F t = match view t with AppBuiltin((True|False),[]) -> true | _ -> false

(* Modify every subterm of t by f except those at the "top". Here top is true if subterm occurs under a quantifier Æ in a context where it could participate to the clausification if the surrounding context of Æ was ignored. *)
let rec replaceTST f top t =
  let re = replaceTST f in
  let ty = ty_exn t in
  let transformer = if top then id else f in
  transformer 
    (match view t with
     | App(t,ts) -> 
       app_whnf ~ty (re false t) (map (re false) ts)
     | Ite(c,x,y) -> 
       ite (re false c) (re false x) (re false y)
     | Match(t, cases) -> 
       match_ (re false t) (map (fun (c,vs,e) -> (c,vs, re false e)) cases)
     | Let(binds, expr) -> 
       let_ (map(CCPair.map_snd (re false)) binds) (re false expr)
     | Bind(b,x,t) -> 
       let top = Binder.equal b Binder.Forall || Binder.equal b Binder.Exists in
       bind ~ty b x (re top t)
     | AppBuiltin(b,ts) ->
       let logical = for_all is_bool ts in
       app_builtin ~ty b (map (re(top && logical)) ts)
     | Multiset ts -> 
       multiset ~ty (map (re false) ts)
     | _ -> t)


let name_quantifiers stmts =
  let proof s = Proof.Step.esa [Proof.Parent.from(Statement.as_proof_i s)]
      ~rule:(Proof.Rule.mk "Quantifier naming")
  in
  let new_stmts = CCVector.create() in
  let changed = ref false in
  let if_changed (mk: ?attrs:Logtk.Statement.attrs -> 'r) s r = 
    if !changed then (changed := false; mk ~proof:(proof s) r) else s in
  let if_changed_list (mk: ?attrs:Logtk.Statement.attrs -> 'l) s r = 
    if !changed then (changed := false; mk ~proof:(proof s) r) else s in
  let name_prop_Qs s = replaceTST(fun t -> match TypedSTerm.view t with
      | Bind(Binder.Forall,_,_) | Bind(Binder.Exists, _, _) ->
        changed := true;
        let vars = Var.Set.of_iter (TypedSTerm.Seq.free_vars t) |> Var.Set.to_list in
        let qid = ID.gensym() in
        let ty = app_builtin ~ty:tType Arrow (prop :: map Var.ty vars) in
        let q = const ~ty qid in
        let q_vars = app ~ty:prop q (map var vars) in
        let proof = Proof.Step.define_internal qid [Proof.Parent.from(Statement.as_proof_i s)] in
        let q_typedecl = ty_decl ~proof qid ty in
        let definition = 
          (* ∀ vars: q[vars] ⇔ t, where t is a quantifier formula and q is a new name for it. *)
          bind_list ~ty:prop Binder.Forall vars 
            (app_builtin ~ty:prop Builtin.Equiv [q_vars; t]) 
        in
        CCVector.push new_stmts q_typedecl;
        CCVector.push new_stmts (assert_ ~proof definition);
        q_vars
      | _ -> t) true
  in
  stmts |> CCVector.map(fun s ->
      match Statement.view s with
      | TyDecl(id,t)	-> s
      | Data ts	-> s
      | Def defs	-> s
      | Rewrite _	-> s
      | Assert p	-> if_changed assert_ s (name_prop_Qs s p)
      | Lemma ps	-> if_changed_list lemma s (map (name_prop_Qs s) ps)
      | Goal p	-> if_changed goal s (name_prop_Qs s p)
      | NegatedGoal(ts, ps)	-> if_changed_list (neg_goal ~skolems:ts) s (map (name_prop_Qs s) ps)
    ) |> CCVector.append new_stmts;
  CCVector.freeze new_stmts


let rec replace old by t =
  let r = replace old by in
  let ty = ty_exn t in
  if TypedSTerm.equal t old then by
  else match view t with
    | App(f,ps) -> app_whnf ~ty (r f) (map r ps)
    | AppBuiltin(f,ps) -> app_builtin ~ty f (map r ps)
    | Ite(c,x,y) -> ite (r c) (r x) (r y)
    | Let(bs,e) -> let_ (map (CCPair.map_snd r) bs) (r e)
    | Bind(b,v,e) -> bind ~ty b v (r e)
    | _ -> t


exception Return of TypedSTerm.t
(* If f _ s = Some r for a subterm s of t, then r else t. *)
let with_subterm_or_id t f = try
    (Seq.subterms_with_bound t (fun(s, var_ctx) ->
         match f var_ctx s with
         | None -> ()
         | Some r -> raise(Return r)));
    t
  with Return r -> r


(* If p is non-constant subproposition closed wrt variables vs, then (p ⇒ c[p:=⊤]) ∧ (p ∨ c[p:=⊥]) or else c unmodified. *)
let case_bool vs c p =
  if is_bool p && not(is_T_F p) && not (TypedSTerm.equal p c) && Var.Set.is_empty(Var.Set.diff (free_vars_set p) vs) then
    let ty = prop in
    app_builtin ~ty And [
      app_builtin ~ty Imply [p; replace p Form.true_ c];
      app_builtin ~ty Or [p; replace p Form.false_ c];
    ]
  else c


(* Apply repeatedly the transformation t[p] ↦ (p ⇒ t[⊤]) ∧ (¬p ⇒ t[⊥]) for each boolean parameter p≠⊤,⊥ that is closed in context where variables vs are bound. *)
let rec case_bools_wrt vs t =
  with_subterm_or_id t (fun _ s ->
      match view s with
      | App(f,ps) ->
        let t' = fold_left (case_bool vs) t ps in
        if TypedSTerm.equal t t' then None else Some(case_bools_wrt vs t')
      | _ -> None
    )

let eager_cases_far stms =
  let proof s = Proof.Step.esa [Proof.Parent.from(Statement.as_proof_i s)]
      ~rule:(Proof.Rule.mk "eager_cases_far")
  in
  map_propositions ~proof (fun _ t ->
      [with_subterm_or_id t (fun vs s -> 
        match view s with
           | Bind((Forall|Exists) as q, v, b) ->
             let b' = case_bools_wrt (Var.Set.add vs v) b in
             if TypedSTerm.equal b b' then None else Some(replace s (bind ~ty:prop q v b') t)
           | _ -> None)
       |> case_bools_wrt Var.Set.empty]) stms


let eager_cases_near stms =
  let proof s = Proof.Step.esa [Proof.Parent.from(Statement.as_proof_i s)]
      ~rule:(Proof.Rule.mk "eager_cases_near")
  in

  let module T = TypedSTerm in

  let find_fool_subterm ?(free_vars=Var.Set.empty) p =
    let rec aux ~top p =
      let p_ty = T.ty_exn p in

      let no_leaky_variables t = 
        Var.Set.intersection_empty (T.free_vars_set t) free_vars
      in

      let return p =
        assert(T.Ty.is_prop (T.ty_exn p));
        assert(no_leaky_variables p);
        Some (T.Form.true_, T.Form.false_, p) in

      match T.view p with
      | AppBuiltin(hd, args)
          when not top && no_leaky_variables p && T.Ty.is_prop (T.ty_exn p) &&
            (* making sure it is not T or F *)
            (Builtin.is_logical_op hd ||
            Builtin.equal hd Builtin.Eq ||
            Builtin.equal hd Builtin.Neq) -> 
        CCFormat.printf "found OK eq@.";
        return p
      | Bind((Binder.Exists | Binder.Forall), var, body)
          when not top && no_leaky_variables p ->
        return p
      | Bind(Binder.Lambda, var, body) ->
        CCOpt.map (fun (body_t, body_f, s) -> 
          assert(no_leaky_variables s);
          (T.fun_l [var] body_t, T.fun_l [var] body_f, s)
        ) (aux ~top:false body)
      | App(hd, args) when not top && T.Ty.is_prop p_ty && no_leaky_variables p  ->
        return p
      | Const _ when not top && T.Ty.is_prop p_ty  ->
        return p
      | AppBuiltin(b, args) ->
        CCOpt.map (fun (args_t,args_f, s) -> 
          (T.app_builtin ~ty:p_ty b args_t, T.app_builtin ~ty:p_ty b args_f, s)
        ) (aux_l args)
      | App(hd,args) ->
        CCOpt.map (fun (args_t,args_f, s) -> 
          (T.app ~ty:p_ty hd args_t, T.app ~ty:p_ty hd args_f, s)
        ) (aux_l args)
      | _ -> None
    and aux_l = function 
    | [] -> None
    | x :: xs ->
      begin match aux ~top:false x with
      | Some (x_t, x_f, s) -> Some(x_t::xs, x_f::xs, s)
      | None -> 
        begin match aux_l xs with 
        | Some (xs_t, xs_f, s) -> Some (x::xs_t, x::xs_f, s)
        | None -> None end
      end in
  let res = aux ~top:true p in
  res in
  

  
  let unroll_fool p =
    let rec aux ~vars p = 
      let p_ty = T.ty_exn p in
      match T.view p with 
      | AppBuiltin(((Builtin.Neq|Builtin.Eq) as hd), ([_;a;b]|[a;b])) when not (T.Ty.is_prop (T.ty_exn a)) ->
        let cons = if hd = Neq then T.Form.neq else T.Form.eq in
        begin match find_fool_subterm a with 
        | None ->
          begin match find_fool_subterm b with 
          | None -> p
          | Some(b_t, b_f, subterm) ->
            let subterm' = aux ~vars subterm in
            let if_true = T.Form.or_ [T.Form.not_ (subterm'); aux ~vars @@ cons a b_t] in
            let if_false = T.Form.or_ [subterm'; aux ~vars @@ cons a b_f] in
            T.Form.and_ [if_true; if_false]
          end
        | Some(a_t, a_f, subterm) ->
          let subterm' = aux ~vars subterm in
          let if_true = T.Form.or_ [T.Form.not_ (subterm'); aux ~vars @@ cons a_t b] in
          let if_false = T.Form.or_ [subterm'; aux ~vars @@ cons a_f b] in
          T.Form.and_ [if_true; if_false]
        end
      | AppBuiltin(hd, args) -> 
        T.app_builtin ~ty:p_ty hd (List.map (aux ~vars) args)
      | App(hd, args) ->
        begin match find_fool_subterm p with
        | Some(p_t, p_f, subterm) ->
          let subterm' = aux ~vars subterm in
          let if_true = T.Form.or_ [T.Form.not_ (subterm'); aux ~vars p_t] in
          let if_false = T.Form.or_ [subterm'; aux ~vars p_f] in
          T.Form.and_ [if_true; if_false]
        | None -> p end
      | Bind((Binder.Exists | Binder.Forall) as b, var , body) ->
        let body' = aux ~vars:(Var.Set.add vars var) body in
        T.bind ~ty:p_ty b var body'
      | _ -> p in
    let res = aux ~vars:Var.Set.empty  p in
    res in
  map_propositions ~proof (fun _ p -> [unroll_fool p]) stms



open Term

let post_eager_cases =
  let proof s = Proof.Step.esa [Proof.Parent.from(Statement.as_proof_c s)]
      ~rule:(Proof.Rule.mk "post_eager_cases")
  in
  map_propositions ~proof (fun _ c ->
      let cased = ref Set.empty in
      fold_left(SLiteral.fold(fun res -> (* Loop over subterms of terms of literals of a clause. *)
          Seq.subterms_depth %> Iter.fold(fun res (s,d) ->
              if d = 0 || not(Type.is_prop(ty s)) || is_true_or_false s || is_var s || Set.mem s !cased
                       || not (T.DB.is_closed s)
              then
                res
              else(
                cased := Set.add s !cased;
                let replace_s_by by = map(SLiteral.map ~f:(replace ~old:s ~by)) in
                flatten(map(fun c -> [
                      SLiteral.atom_true s :: replace_s_by false_ c; 
                      SLiteral.atom_false s :: replace_s_by true_ c
                    ]) res))
            ) res
        )) [c] c)

let _bool_reasoning = ref BoolReasoningDisabled
let _quant_rename = ref false


(* These two options run before CNF, 
   so (for now it is impossible to move them to Env
   since it is not even made at the moment) *)
let preprocess_booleans stmts = (match !_bool_reasoning with
    | BoolCasesPreprocess -> eager_cases_near
    | _ -> id
  ) (if !_quant_rename then name_quantifiers stmts else stmts)

let preprocess_cnf_booleans stmts = match !_bool_reasoning with
  | BoolCasesPreprocess -> 
    let res = post_eager_cases stmts in
    res
  | _ -> stmts

let _interpret_bool_funs = ref false
let _cnf_non_simpl = ref false
let _norm_bools = ref false 
let _filter_literals = ref `Max
let _nnf = ref false
let _simplify_bools = ref true
let _trigger_bool_inst = ref (-1)
let _trigger_bool_ind = ref (-1)
let _generalize_trigger = ref (`Off)
let _include_quants = ref true
let _bool_hoist_simpl = ref false
let _rename_nested_bools = ref false
let _fluid_hoist = ref false
let _bool_app_var_repl = ref false
let _fluid_log_hoist = ref false
let _solve_formulas = ref false
let _replace_quants = ref false
let _disable_ho_unif = ref false
let _bool_triggers_only = ref (false)



let extension =
  let register env =
    let module E = (val env : Env.S) in
    let module ET = Make(E) in
    E.flex_add k_bool_reasoning !_bool_reasoning;
    E.flex_add k_quant_rename !_quant_rename;
    E.flex_add k_interpret_bool_funs !_interpret_bool_funs;
    E.flex_add k_cnf_non_simpl !_cnf_non_simpl;
    E.flex_add k_norm_bools !_norm_bools;
    E.flex_add k_filter_literals !_filter_literals;
    E.flex_add k_nnf !_nnf;
    E.flex_add k_simplify_bools !_simplify_bools;
    E.flex_add k_trigger_bool_inst !_trigger_bool_inst;
    E.flex_add k_trigger_bool_ind !_trigger_bool_ind;
    E.flex_add k_include_quants !_include_quants;
    E.flex_add k_bool_hoist_simpl !_bool_hoist_simpl;
    E.flex_add k_rename_nested_bools !_rename_nested_bools;
    E.flex_add k_fluid_hoist !_fluid_hoist;
    E.flex_add k_bool_app_var_repl !_bool_app_var_repl;
    E.flex_add k_fluid_log_hoist !_fluid_log_hoist;
    E.flex_add k_solve_formulas !_solve_formulas;
    E.flex_add k_replace_unsupported_quants !_replace_quants;
    E.flex_add k_disable_ho_bool_unif !_disable_ho_unif;
    E.flex_add k_generalize_trigger !_generalize_trigger;
    E.flex_add k_bool_triggers_only !_bool_triggers_only;

    ET.setup ()
  in
  { Extensions.default with
    Extensions.name = "bool";
    env_actions=[register];
  }

let () =
  Options.add_opts
    [ "--boolean-reasoning", Arg.Symbol (["off"; "simpl-only"; "bool-hoist"; "cases-preprocess"], 
        (fun s -> 
        _bool_reasoning := 
            (match s with 
            | "off" -> BoolReasoningDisabled
            | "simpl-only" -> BoolSimplificationsOnly
            | "bool-hoist" -> BoolHoist
            | "cases-preprocess" -> BoolCasesPreprocess
            | _ -> assert false);
        if !_bool_reasoning == BoolHoist then (
          (* setting default Boolean selection if BoolHoist is on *)
          Params.bool_select := "smallest";
        );)), 
      " enable/disable boolean axioms";
      "--quantifier-renaming"
      , Arg.Bool (fun v -> _quant_rename := v)
      , " turn the quantifier renaming on or off";
      "--replace-quants"
      , Arg.Bool (fun v -> _replace_quants := v)
      , " replace unsupported quantifiers";
      "--replace-bool-app-vars"
      , Arg.Bool (fun v -> _bool_app_var_repl := v)
      , " unify applied variables with combinations of T and F";
      "--rename-nested-bools"
      , Arg.Bool (fun v -> _rename_nested_bools := v)
      , " rename deeply nested bool subterms";
      "--trigger-bool-ind", Arg.Set_int _trigger_bool_ind
      , " abstract away constants from the goal and use them to trigger axioms of induction";
      "--trigger-bool-inst", Arg.Set_int _trigger_bool_inst
        , " instantiate predicate variables with boolean terms already in the proof state. Argument is the maximal proof depth of predicate variable";
      "--trigger-bool-inst-prop-only", Arg.Bool ((:=) _bool_triggers_only)
        , " make sure that lambdas are REALLY only of Boolean type";
      "--trigger-bool-include-quants", Arg.Bool ((:=) _include_quants)
        , " include lambdas directly under a quant in consdieration";
      "--trigger-bool-generalize", Arg.Symbol (["off"; "neg"; "var" ], (fun s -> 
          _generalize_trigger := (match s with 
          | "off" -> `Off
          | "neg" -> `Neg
          | "var" -> `Var
          | _ -> invalid_arg "off, neg or var are the only options")  
      )), " generalize the trigger: neg adds the negation before the trigger body, " ^
          " and var applies the body to a fresh variable";
      "--disable-simplifying-cnf",
        Arg.Set _cnf_non_simpl,
        " implement cnf on-the-fly as an inference rule";
      "--interpret-bool-funs"
      , Arg.Bool (fun v -> _interpret_bool_funs := v)
      , " turn interpretation of boolean functions as forall or negation of forall on or off";
      "--bool-hoist-simpl"
      , Arg.Bool (fun v -> _bool_hoist_simpl := v; _rename_nested_bools := true)
      , " use BoolHoistSimpl instead of BoolHoist; NOTE: Setting this option triggers nested booleans renaming";
        "--normalize-bool-terms", Arg.Bool((fun v -> _norm_bools := v)),
        " normalize boolean subterms using their weight.";
      "--nnf-nested-formulas"
      , Arg.Bool (fun v -> _nnf := v)
      , " convert nested formulas into negation normal form";
      "--simplify-bools"
      , Arg.Bool (fun v -> _simplify_bools := v)
      , " simplify boolean subterms";
      "--fluid-hoist"
      , Arg.Bool (fun v -> _fluid_hoist := v)
      , " enable/disable Fluid(Bool|Loob)Hoist rules";
      "--fluid-log-hoist"
      , Arg.Bool (fun v -> _fluid_log_hoist := v)
      , " enable/disable fluid version of BoolRW, (Forall|Exists)RW, (Eq|Neq|Forall|Exists)Hoist rules";
      "--solve-formulas"
      , Arg.Bool (fun v -> _solve_formulas := v)
      , " solve phi = psi eagerly by unifying phi != ~psi, where phi and psi are formulas";
      "--boolean-reasoning-filter-literals"
      , Arg.Symbol(["all"; "max"], (fun v ->
          match v with 
          | "all" -> _filter_literals:=`All
          | "max" -> _filter_literals:= `Max
          | _ -> assert false;))
      , " select on which literals to apply bool reasoning rules"
    ];
  Params.add_to_modes ["ho-pragmatic";
                       "lambda-free-intensional";
                       "lambda-free-purify-intensional";
                       "lambda-free-extensional";
                       "ho-comb-complete";
                       "lambda-free-purify-extensional";
                       "fo-complete-basic"] (fun () ->
      _bool_reasoning := BoolReasoningDisabled
  );
  Params.add_to_mode "ho-complete-basic" (fun () -> 
    _bool_reasoning := BoolHoist;
    _fluid_hoist := true;
    _bool_app_var_repl := true;
    _fluid_log_hoist := true;
    _replace_quants := true);
  Params.add_to_modes ["ho-pragmatic";
                       "lambda-free-intensional";
                       "lambda-free-purify-intensional";
                       "lambda-free-extensional";
                       "ho-comb-complete";
                       "ho-competititve";
                       "lambda-free-purify-extensional";
                       "fo-complete-basic"] (fun () ->
      _replace_quants := false;
  );
  Params.add_to_modes ["lambda-free-intensional";
                       "lambda-free-purify-intensional";
                       "lambda-free-extensional";
                       "ho-comb-complete";
                       "lambda-free-purify-extensional"] (fun () -> 
    _disable_ho_unif := true
  );
  Extensions.register extension
