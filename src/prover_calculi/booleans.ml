
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk
open Libzipperposition

module T = Term
module Pos = Position
module US = Unif_subst

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
let k_elim_bvars = Flex_state.create_key ()
let k_simplify_bools = Flex_state.create_key ()
let k_trigger_bool_inst = Flex_state.create_key ()
let k_trigger_bool_ind = Flex_state.create_key ()
let k_bool_hoist_simpl = Flex_state.create_key ()

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {6 Registration} *)

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

  let (=~),(/~) = Literal.mk_eq, Literal.mk_neq
  let (@:) = T.app_builtin ~ty:Type.prop
  let no a = a =~ T.false_
  let yes a = a =~ T.true_

  let _trigger_bools   = ref (Type.Map.empty) (* type --> boolean trigger *)
  let _cls_w_pred_vars = ref (Type.Map.empty) (* type --> (clause,var) *)

  let get_triggers c =
    let ord = Ctx.ord () in
    let trivial_trigger t =
      let body = snd @@ T.open_fun t in
      T.is_var body || T.is_true_or_false body in

    Literals.fold_terms ~ord ~subterms:true ~eligible:C.Eligible.always 
      ~which:`All (C.lits c) ~fun_bodies:false 
    |> Iter.filter_map (fun (t,p) -> 
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
    let lits = Literals.apply_subst renaming subst (C.lits clause, cl_sc) in
    let lits = Literals.map (fun t -> Lambda.eta_reduce @@ Lambda.snf t) lits in
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
    if not (CCList.mem ~eq:T.equal t triggers) then (
      _trigger_bools := Type.Map.update (T.ty t) (function 
        | None -> Some [t]
        | Some res -> Some (t :: res)
      ) !_trigger_bools;

      Type.Map.get_or ~default:[] (T.ty t) !_cls_w_pred_vars
      |> CCList.map (fun (clause,var) -> instantiate_w_bool ~clause ~var ~trigger:t)
    ) else []

  let insert_new_trigger t =
    inst_clauses_w_trigger t
    |> CCList.to_iter
    |> Env.add_passive

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
          CCList.flat_map inst_clauses_w_trigger (make_triggers lhs rhs (Literal.is_pos lit))
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

  let get_bool_subterms c =
    C.eligible_for_bool_infs c
    |> SClause.TPSet.to_list
    |> List.filter (fun (t,_) -> 
        match T.view t with
        | T.AppBuiltin(hd,_) ->
          (* check that the term has no interpreted sym on top *)
          not (List.mem hd [Builtin.Eq;Builtin.Neq] || 
              Builtin.is_logical_binop hd)
        | _ -> true)
    (* since we are doing simultaneous version -- we take only unique terms *)
    |> CCList.sort_uniq ~cmp:(fun (t1,_) (t2,_) -> T.compare t1 t2)

  let bool_hoist (c:C.t) : C.t list = 
    let proof = Proof.Step.inference [C.proof_parent c]
                ~rule:(Proof.Rule.mk "bool_hoist") ~tags:[Proof.Tag.T_ho] in

    List.map (fun (t, _) ->
      mk_res ~proof ~old:t ~repl:T.false_ (yes t) c)
    (get_bool_subterms c)

  let bool_hoist_simpl (c:C.t) : C.t list option = 
    let proof = Proof.Step.inference [C.proof_parent c]
                ~rule:(Proof.Rule.mk "bool_hoist") ~tags:[Proof.Tag.T_ho] in

    let bool_subterms  = get_bool_subterms c in
    CCOpt.return_if (not @@ CCList.is_empty bool_subterms) (
      CCList.fold_left (fun acc (t,_) -> 
        let neg_lit, repl_neg = no t, T.true_ in
        let pos_lit, repl_pos = yes t, T.false_ in
        (mk_res ~proof ~old:t ~repl:repl_neg neg_lit c) ::
        (mk_res ~proof ~old:t ~repl:repl_pos pos_lit c) :: acc ) 
      [] bool_subterms
    )

  let formula_hoist (c:C.t) : C.t list =
    let proof ~prefix = 
      Proof.Step.inference [C.proof_parent c]
        ~rule:(Proof.Rule.mk (prefix^"_hoist")) ~tags:[Proof.Tag.T_ho] in

    C.eligible_for_bool_infs c
    |> SClause.TPSet.to_list
    |> List.filter (fun (t,_) -> 
        match T.view t with
        | T.AppBuiltin(hd,_) ->
          List.mem hd [Builtin.Eq;Neq;Xor;Equiv;ForallConst;ExistsConst] &&
          Type.is_prop (T.ty t)
        | _ -> false)
    (* since we are doing simultaneous version -- we take only unique terms *)
    |> CCList.sort_uniq ~cmp:(fun (t1,_) (t2,_) -> T.compare t1 t2)
    |> List.map (fun (t, _) ->
        let fresh_var ~body = 
          assert(Type.is_fun (T.ty body));
          let pref,_ = Type.open_fun (T.ty body) in
          assert(List.length pref == 1);
          let var_ty = List.hd pref in
          let fresh_var  = HVar.fresh ~ty:var_ty () in
          T.var fresh_var
        in

        match T.view t with 
        | T.AppBuiltin(Builtin.(Eq|Equiv), ([a;b]|[_;a;b])) ->
          let new_lit = Literal.mk_eq a b in
          mk_res ~proof:(proof ~prefix:"eq") ~old:t ~repl:T.false_ new_lit c
        | T.AppBuiltin(Builtin.(Neq|Xor), ([a;b]|[_;a;b])) ->
          let new_lit = Literal.mk_eq a b in
          mk_res ~proof:(proof ~prefix:"neq") ~old:t ~repl:T.true_ new_lit c
        | T.AppBuiltin(Builtin.ForallConst, [_;body]) ->
          let new_lit = yes (T.app body [fresh_var ~body]) in
          mk_res ~proof:(proof ~prefix:"forall") ~old:t ~repl:T.false_ new_lit c
        | T.AppBuiltin(Builtin.ExistsConst, [_;body]) ->
          let new_lit = no (T.app body [fresh_var ~body]) in
          mk_res ~proof:(proof ~prefix:"exists") ~old:t ~repl:T.true_ new_lit c
        | _ -> assert false
    )

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


    C.eligible_for_bool_infs c
    |> SClause.TPSet.to_list
    |> List.find_opt (fun (t,_) -> 
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

  let quantifier_rw (c:C.t) =
    let proof =
      Proof.Step.simp [C.proof_parent c]
        ~rule:(Proof.Rule.mk ("quantifier_rw")) 
        ~tags:[Proof.Tag.T_ho] in

    C.eligible_for_bool_infs c
    |> SClause.TPSet.to_list
    |> CCList.filter_map (fun (t,p) -> 
      (* TODO(BOOL): Implement skolem reusing *)
      match T.view t with
      | T.AppBuiltin(Builtin.(ForallConst|ExistsConst), [_;body]) ->
        let free_vars_l = T.VarSet.to_list (T.vars body) in
        let ret_ty = List.hd (fst (Type.open_fun (T.ty body))) in
        let (id,ty), sk = T.mk_fresh_skolem ~prefix:"sk" free_vars_l ret_ty in
        E.Ctx.declare id ty;
        Signal.send Env.on_pred_skolem_introduction (c, t);
        let repl = T.app body [sk] in
        let new_lits = CCArray.copy (C.lits c) in
        Literals.Pos.replace ~at:p ~by:repl new_lits;
        let new_cl = 
          C.create ~trail:(C.trail c) ~penalty:(C.penalty c) 
            (Array.to_list new_lits) proof in
        Some new_cl
      | _ -> None
    )

   let nested_eq_rw c =
    (* TODO(BOOL): currently incompatible with combiantors *)
    let unif_alg = 
      if Env.flex_get Combinators.k_enable_combinators 
      then (fun _ _ -> OSeq.empty)
      else Env.flex_get Superposition.k_unif_alg in
    let sc = 0 in
    let mk_sc t = (t,sc) in 
    let parents r s = [C.proof_parent_subst r (mk_sc c) s ] in
    C.eligible_for_bool_infs c
    |> SClause.TPSet.to_list
    |> CCList.filter_map (fun (t,p) -> 
      match T.view t with
      | T.AppBuiltin((Builtin.(Eq|Neq|Equiv|Xor) as hd), ([a;b]|[_;a;b])) ->
        Some (
          unif_alg (mk_sc a) (mk_sc b)
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
    |> List.iter (fun clause_seq -> 
      let stm_res = Env.Stm.make ~penalty:(C.penalty c) ~parents:[c] clause_seq in
      Env.StmQ.add (Env.get_stm_queue ()) stm_res);
    []

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
    | AppBuiltin((Builtin.Eq | Builtin.Equiv) as hd, ([a;b]|[_;a;b])) ->
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
    | AppBuiltin((Builtin.Neq | Builtin.Xor) as hd, ([a;b]|[_;a;b])) ->
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
          let g' = Lambda.eta_reduce (T.fun_l g_ty_args g_body') in
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
      let cnf_vec = Cnf.convert @@ CCVector.to_seq @@ Cnf.cnf_of ~opts ~ctx:(Ctx.sk_ctx ()) stmt in
      CCVector.iter (fun cl -> 
          Statement.Seq.ty_decls cl
          |> Iter.iter (fun (id,ty) -> Ctx.declare id ty)) cnf_vec;
      let solved = 
        if Env.flex_get Lazy_cnf.k_solve_formulas then (
          CCOpt.get_or ~default:[] (LazyCNF.solve_bool_formulas c))
        else [] in

      let clauses = CCVector.map (C.of_statement ~convert_defs:true) cnf_vec
                    |> CCVector.to_list 
                    |> CCList.flatten
                    |> List.map (fun c -> 
                        C.create ~penalty  ~trail (CCArray.to_list (C.lits c)) proof) in
      Util.debugf ~section 1 "cl:@[%a@]@." (fun k-> k C.pp c);
      Util.debugf ~section 1 " @[%a@]@." (fun k-> k (CCList.pp C.pp) clauses);
      List.iteri (fun i new_c -> 
          assert((C.proof_depth c) <= C.proof_depth new_c);) clauses;
      Some (solved @clauses)
    | None -> None

  let cnf_infer cl = 
    CCOpt.get_or ~default:[] (cnf_otf cl)

  let elim_bvars c =
    C.Seq.terms c
    |> Iter.filter_map (fun v -> 
        if T.is_var v && Type.is_prop (T.ty v) then (
          Some (T.as_var_exn v) 
        ) else None)
    |> Iter.sort_uniq ~cmp:(HVar.compare Type.compare)
    |> Iter.flat_map_l (fun v ->
        let subst_true = 
          Subst.FO.bind' Subst.empty (v, 0) (T.true_, 0) in
        let subst_false = 
          Subst.FO.bind' Subst.empty (v, 0) (T.false_, 0) in
        [subst_true; subst_false])
    |> Iter.map (fun subst ->
        let proof =
          Some (Proof.Step.simp 
            ~tags:[Proof.Tag.T_ho] ~rule:(Proof.Rule.mk "elim_bool_vars")
            [C.proof_parent c]) in
        C.apply_subst ~proof ~penalty_inc:(Some (-1)) (c,0) subst)
    |> Iter.to_list

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

          Util.debugf ~section  1 "interpret bool(@[%a@]):@.@[%a@] !!> @. @[%a@]@."  
            (fun k -> k T.pp t Literals.pp (C.lits c) Literals.pp (C.lits forall_cl));
          Util.debugf ~section  1 "interpret bool(@[%a@]):@.@[%a@] !!> @. @[%a@]@." 
            (fun k -> k T.pp t Literals.pp (C.lits c) Literals.pp (C.lits forall_neg_cl));

          forall_cl :: forall_neg_cl :: res
        ) else res) 
      []

  let normalize_equalities c =
    let lits = Array.to_list (C.lits c) in
    let normalized = List.map Literal.normalize_eq lits in
    if List.exists CCOpt.is_some normalized then (
      let new_lits = List.mapi (fun i l_opt -> 
          CCOpt.get_or ~default:(Array.get (C.lits c) i) l_opt) normalized in
      let proof = Proof.Step.simp [C.proof_parent c] 
          ~rule:(Proof.Rule.mk "simplify nested equalities")  in
      let new_c = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) new_lits proof in
      SimplM.return_new new_c
    ) 
    else (
      SimplM.return_same c 
    )


  let setup () =
    match Env.flex_get k_bool_reasoning with 
    | BoolReasoningDisabled -> ()
    | _ ->
      Env.add_basic_simplify normalize_equalities;
      if Env.flex_get k_trigger_bool_inst > 0 || Env.flex_get k_trigger_bool_ind > 0 then (
        Signal.on Env.on_pred_var_elimination handle_new_pred_var_clause;
        Signal.on Env.on_pred_skolem_introduction handle_new_skolem_sym;
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
      if Env.flex_get k_elim_bvars then (
        Env.add_unary_inf "elim_bvars" elim_bvars;
      );
      if not !Lazy_cnf.enabled then (
        Env.add_multi_simpl_rule ~priority:2 Fool.rw_bool_lits;
        if Env.flex_get k_cnf_non_simpl then (
          Env.add_unary_inf "cnf otf inf" cnf_infer;
        ) else  Env.add_multi_simpl_rule ~priority:2 cnf_otf);
      if (Env.flex_get k_interpret_bool_funs) then (
        Env.add_unary_inf "interpret boolean functions" interpret_boolean_functions;
      );

      if Env.flex_get k_bool_reasoning = BoolHoist then (
        if Env.flex_get k_bool_hoist_simpl 
        then Env.add_multi_simpl_rule ~priority:1000 bool_hoist_simpl
        else Env.add_unary_inf "bool_hoist" bool_hoist;
        Env.add_unary_inf "formula_hoist" formula_hoist;
        Env.add_unary_inf "quant_rw" quantifier_rw;
        Env.add_multi_simpl_rule ~priority:100 replace_bool_vars;
        Env.add_unary_inf "eq_rw" nested_eq_rw;
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

(* Modify every subterm of t by f except those at the "top". Here top is true if subterm occures under a quantifier Æ in a context where it could participate to the clausification if the surrounding context of Æ was ignored. *)
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
       let_ (map(CCPair.map2 (re false)) binds) (re false expr)
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
        let vars = Var.Set.of_seq (TypedSTerm.Seq.free_vars t) |> Var.Set.to_list in
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
    | Let(bs,e) -> let_ (map (CCPair.map2 r) bs) (r e)
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

  let find_fool_subterm p =
    let rec aux ~top p =
      let p_ty = T.ty_exn p in

      let return p =
        assert(T.Ty.is_prop (T.ty_exn p));
        assert(T.closed p);
        Some (T.Form.true_, T.Form.false_, p) in

      match T.view p with
      | AppBuiltin(hd, args)
          when not top && T.closed p && T.Ty.is_prop (T.ty_exn p) &&
            (* making sure it is not T or F *)
            (Builtin.is_logical_op hd ||
            Builtin.equal hd Builtin.Eq ||
            Builtin.equal hd Builtin.Neq) -> 
        return p
      | Bind((Binder.Exists | Binder.Forall), var, body)
          when not top && T.closed p ->
        return p
      | Bind(Binder.Lambda, var, body) ->
        CCOpt.map (fun (body_t, body_f, s) -> 
          assert(T.closed s);
          (T.fun_l [var] body_t, T.fun_l [var] body_f, s)
        ) (aux ~top:false body)
      | App(hd, args) when not top && T.Ty.is_prop p_ty && T.closed p  ->
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
    let rec aux p = 
      let p_ty = T.ty_exn p in
      match T.view p with 
      | AppBuiltin(((Builtin.Neq|Builtin.Eq) as hd), ([_;a;b]|[a;b])) when not (T.Ty.is_prop (T.ty_exn a)) ->
        let cons = if hd = Neq then T.Form.neq else T.Form.eq in
        begin match find_fool_subterm a with 
        | None ->
          begin match find_fool_subterm b with 
          | None -> p
          | Some(b_t, b_f, subterm) ->
            let subterm' = aux subterm in
            let if_true = T.Form.or_ [T.Form.not_ (subterm'); aux @@ cons a b_t] in
            let if_false = T.Form.or_ [subterm'; aux @@ cons a b_f] in
            T.Form.and_ [if_true; if_false]
          end
        | Some(a_t, a_f, subterm) ->
          let subterm' = aux subterm in
          let if_true = T.Form.or_ [T.Form.not_ (subterm'); aux @@ cons a_t b] in
          let if_false = T.Form.or_ [subterm'; aux @@ cons a_f b] in
          T.Form.and_ [if_true; if_false]
        end
      | AppBuiltin(hd, args) -> 
        T.app_builtin ~ty:p_ty hd (List.map aux args)
      | App(hd, args) ->
        begin match find_fool_subterm p with
        | Some(p_t, p_f, subterm) ->
          let subterm' = aux subterm in
          let if_true = T.Form.or_ [T.Form.not_ (subterm'); aux p_t] in
          let if_false = T.Form.or_ [subterm'; aux p_f] in
          T.Form.and_ [if_true; if_false]
        | None -> p end
      | Bind((Binder.Exists | Binder.Forall) as b, var , body) ->
        let body' = aux body in
        T.bind ~ty:p_ty b var body'
      | _ -> p in
    let res = aux p in
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
let _elim_bvars = ref true
let _trigger_bool_inst = ref (-1)
let _trigger_bool_ind = ref (-1)
let _bool_hoist_simpl = ref false


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
    E.flex_add k_elim_bvars !_elim_bvars;
    E.flex_add k_simplify_bools !_simplify_bools;
    E.flex_add k_trigger_bool_inst !_trigger_bool_inst;
    E.flex_add k_trigger_bool_ind !_trigger_bool_ind;
    E.flex_add k_bool_hoist_simpl !_bool_hoist_simpl;

    ET.setup ()
  in
  { Extensions.default with
    Extensions.name = "bool";
    env_actions=[register];
  }

let () =
  Options.add_opts
    [ "--boolean-reasoning", Arg.Symbol (["off"; "simpl-only"; "bool-hoist"; "cases-preprocess"], 
                                         fun s -> _bool_reasoning := 
                                             match s with 
                                             | "off" -> BoolReasoningDisabled
                                             | "simpl-only" -> BoolSimplificationsOnly
                                             | "bool-hoist" -> BoolHoist
                                             | "cases-preprocess" -> BoolCasesPreprocess
                                             | _ -> assert false), 
      " enable/disable boolean axioms";
      "--quantifier-renaming"
      , Arg.Bool (fun v -> _quant_rename := v)
      , " turn the quantifier renaming on or off";
      "--trigger-bool-ind", Arg.Set_int _trigger_bool_ind
      , " abstract away constants from the goal and use them to trigger axioms of induction";
      "--trigger-bool-inst", Arg.Set_int _trigger_bool_inst
        , " instantiate predicate variables with boolean terms already in the proof state. Argument is the maximal proof depth of predicate variable";
      "--disable-simplifying-cnf",
        Arg.Set _cnf_non_simpl,
        " implement cnf on-the-fly as an inference rule";
      "--interpret-bool-funs"
      , Arg.Bool (fun v -> _interpret_bool_funs := v)
      , " turn interpretation of boolean functions as forall or negation of forall on or off";
      "--bool-hoist-simpl"
      , Arg.Bool (fun v -> _bool_hoist_simpl := v)
      , " use BoolHoistSimpl instead of BoolHoist";
        "--normalize-bool-terms", Arg.Bool((fun v -> _norm_bools := v)),
        " normalize boolean subterms using their weight.";
      "--nnf-nested-formulas"
      , Arg.Bool (fun v -> _nnf := v)
      , " convert nested formulas into negation normal form";
      "--simplify-bools"
      , Arg.Bool (fun v -> _simplify_bools := v)
      , " simplify boolean subterms";
      "--elim-bvars"
      , Arg.Bool ((:=) _elim_bvars)
      , " replace boolean variables by T and F";
      "--boolean-reasoning-filter-literals"
      , Arg.Symbol(["all"; "max"], (fun v ->
          match v with 
          | "all" -> _filter_literals:=`All
          | "max" -> _filter_literals:= `Max
          | _ -> assert false;))
      , " select on which literals to apply bool reasoning rules"
    ];
  Params.add_to_modes ["ho-complete-basic";
                       "ho-pragmatic";
                       "lambda-free-intensional";
                       "lambda-free-purify-intensional";
                       "lambda-free-extensional";
                       "ho-comb-complete";
                       "lambda-free-purify-extensional";
                       "fo-complete-basic"] (fun () ->
      _bool_reasoning := BoolReasoningDisabled
  );
  Extensions.register extension
