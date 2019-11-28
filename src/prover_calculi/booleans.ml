
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk
open Libzipperposition

module T = Term

type selection_setting = Any | Minimal | Large
type reasoning_kind    = 
    BoolReasoningDisabled | BoolCasesInference | BoolCasesSimplification | BoolCasesKeepParent
  | BoolCasesEagerFar | BoolCasesEagerNear

let section = Util.Section.make ~parent:Const.section "booleans"


let k_bool_reasoning = Flex_state.create_key ()
let k_cased_term_selection = Flex_state.create_key ()
let k_quant_rename = Flex_state.create_key ()
let k_interpret_bool_funs = Flex_state.create_key ()
let k_cnf_non_simpl = Flex_state.create_key ()
let k_norm_bools = Flex_state.create_key () 
let k_solve_formulas = Flex_state.create_key ()

module type S = sig
  module Env : Env.S
  module C : module type of Env.C

  (** {6 Registration} *)

  val setup : unit -> unit
  (** Register rules in the environment *)
end


module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module Ctx = Env.Ctx
  module Fool = Fool.Make(Env)

  let (=~),(/~) = Literal.mk_eq, Literal.mk_neq
  let (@:) = T.app_builtin ~ty:Type.prop
  let no a = a =~ T.false_
  let yes a = a =~ T.true_
  let imply a b = Builtin.Imply @: [a;b]
  let const_true p = T.fun_ (List.hd @@ fst @@ Type.open_fun (T.ty p)) T.true_

  let true_not_false = [T.true_ /~ T.false_]
  let true_or_false a = [yes a; a =~ T.false_]
  let imp_true1 a b = [yes a; yes(imply a b)]
  let imp_true2 a b = [no b; yes(imply a b)]
  let imp_false a b = [no(imply a b); no a; yes b]
  let all_true p = [p /~ const_true p; yes(Builtin.ForallConst@:[p])]
  let all_false p = [no(Builtin.ForallConst@:[p]); p =~ const_true p]
  let eq_true x y = [x/~y; yes(Builtin.Eq@:[x;y])]
  let eq_false x y = [no(Builtin.Eq@:[x;y]); x=~y]
  let and_ a b = [Builtin.And @: [a;b] 
                  =~  imply (imply a (imply b T.false_)) T.false_]
  let or_ a b = [Builtin.Or @: [a;b] 
                 =~  imply (imply a T.false_) b] 

  let and_true a  = [Builtin.And @: [T.true_; a] =~ a]
  let and_false a  = [Builtin.And @: [T.false_; a] =~ T.false_]

  let exists t = 
    let t2bool = Type.arrow [t] Type.prop in
    [T.app_builtin ~ty:(Type.arrow [t2bool] Type.prop) Builtin.ExistsConst [] =~ T.fun_ t2bool
       (Builtin.Not @:[Builtin.ForallConst @:[T.fun_ t (Builtin.Not @:[T.app (T.bvar t2bool 1) [T.bvar t 0]])]])]

  let as_clause c = Env.C.create ~penalty:1 ~trail:Trail.empty c Proof.Step.trivial

  let create_clauses () = 
    let a = T.var (HVar.make ~ty:Type.prop 0) in
    [ [Builtin.And @:[T.true_; a] =~ a];
      [Builtin.And @:[T.false_; a] =~ T.false_];
      [Builtin.Or @:[T.true_; a] =~ T.true_];
      [Builtin.Or @:[T.false_; a] =~ a];
      [Builtin.Imply @:[T.true_; a] =~ a];
      [Builtin.Imply @:[T.false_; a] =~ T.true_];
      [Builtin.Not @:[T.true_] =~ T.false_];
      [Builtin.Not @:[T.false_] =~ T.true_]; ] 
    |> List.map as_clause |> Iter.of_list

  let bool_cases(c: C.t) : C.t list =
    let term_as_true = Term.Tbl.create 8 in
    let term_as_false = Term.Tbl.create 4 in
    let cased_term_selection = Env.flex_get k_cased_term_selection in
    let rec find_bools top t =
      let can_be_cased = Type.is_prop(T.ty t) && T.DB.is_closed t && (not top ||
                                                                      (* It is useful to case top level equality like 𝘵𝘦𝘳𝘮𝘴 
                                                                         because these are simplified into 𝘭𝘪𝘵𝘦𝘳𝘢𝘭𝘴. *)
                                                                      match T.view t with AppBuiltin((Eq|Neq|Equiv|Xor),_) -> true | _ -> false) in
      let is_quant = match T.view t with 
        | AppBuiltin(b,_) -> 
          Builtin.equal b Builtin.ForallConst || Builtin.equal b Builtin.ExistsConst
        | _ -> false in
      (* Add only propositions. *)
      let add = if can_be_cased then Term.Tbl.add term_as_true else fun _ _ -> () in
      let yes = if can_be_cased then yes else fun _ -> yes T.true_ in
      (* Stop recursion in combination of certain settings. *)
      let inner f x = 
        if is_quant || can_be_cased && cased_term_selection = Large 
        then () 
        else List.iter(f false) x in
      match T.view t with
      | DB _ | Var _ -> ()
      | Const _ -> add t (yes t)
      | Fun(_,b) -> find_bools false b
      | App(f,ps) -> add t (yes t); inner find_bools (f::ps)
      | AppBuiltin(f,ps) ->
        inner find_bools ps;
        match f with
        | Builtin.True | Builtin.False -> ()
        | Builtin.Eq | Builtin.Neq | Builtin.Equiv | Builtin.Xor ->
          begin match ps with 
            | [x;y] when (cased_term_selection != Minimal || Type.is_prop(T.ty x)) ->
              if f = Builtin.Neq || f = Builtin.Xor then(
                if can_be_cased then Term.Tbl.add term_as_false t (x =~ y);
                add t (x /~ y))
              else add t (x =~ y)
            | _ -> () end
        | Builtin.And | Builtin.Or | Builtin.Imply | Builtin.Not ->
          if cased_term_selection != Minimal then add t (yes t) else()
        | _ -> add t (yes t) 
    in
    Literals.Seq.terms(C.lits c) |> Iter.iter(find_bools true);
    let case polarity b b_lit clauses =
      let proof = Proof.Step.inference[C.proof_parent c]
          ~rule:(Proof.Rule.mk"bool_cases") ~tags:[Proof.Tag.T_ho]
      in
      C.create ~trail:(C.trail c) ~penalty:(C.penalty c)
        (b_lit :: Array.to_list(C.lits c |> Literals.map(T.replace ~old:b ~by:polarity)))
        proof :: clauses
    in
    Term.Tbl.fold(case T.false_) term_as_true [] @
    Term.Tbl.fold(case T.true_) term_as_false []


  let bool_case_simp(c: C.t) : C.t list option =
    let term_to_equations = Term.Tbl.create 8 in
    let cased_term_selection = Env.flex_get k_cased_term_selection in
    let rec find_bools top t =
      let can_be_cased = Type.is_prop(T.ty t) && T.DB.is_closed t && (not top ||
                                                                      (* It is useful to case top level equality like 𝘵𝘦𝘳𝘮𝘴 because these are simplified into 𝘭𝘪𝘵𝘦𝘳𝘢𝘭𝘴. *)
                                                                      match T.view t with AppBuiltin((Eq|Neq|Equiv|Xor),_) -> true | _ -> false) in
      let is_quant = match T.view t with 
        | AppBuiltin(b,_) -> 
          Builtin.equal b Builtin.ForallConst || Builtin.equal b Builtin.ExistsConst
        | _ -> false in
      (* Add only propositions. *)
      let add t x y = if can_be_cased then Term.Tbl.add term_to_equations t (x=~y, x/~y) in
      (* Stop recursion in combination of certain settings. *)
      let inner f x = 
        if is_quant || (can_be_cased && cased_term_selection = Large) 
        then () 
        else List.iter(f false) x in
      match T.view t with
      | DB _ | Var _ -> ()
      | Const _ -> add t t T.true_
      | Fun(_,b) -> find_bools false b
      | App(f,ps) -> add t t T.true_; inner find_bools (f::ps)
      | AppBuiltin(f,ps) ->
        inner find_bools ps;
        match f with
        | Builtin.True | Builtin.False -> ()
        | Builtin.Eq | Builtin.Neq | Builtin.Equiv | Builtin.Xor ->
          (match ps with 
           | [_;x;y]
           | [x;y] when (cased_term_selection != Minimal || Type.is_prop(T.ty x)) ->
             add t x y;
             if (f = Builtin.Neq || f = Builtin.Xor) && can_be_cased then
               Term.Tbl.replace term_to_equations t (Term.Tbl.find term_to_equations t |> CCPair.swap)
           | _ -> ())
        | Builtin.And | Builtin.Or | Builtin.Imply | Builtin.Not ->
          if cased_term_selection != Minimal then add t t T.true_ else()
        | _ -> add t t T.true_
    in
    if not @@ Iter.exists T.is_formula (C.Seq.terms c) then (
      (* first clausify, then get bool subterms *)
      Literals.Seq.terms(C.lits c) 
      |> Iter.iter(find_bools true));
    let res = 
      Term.Tbl.fold(fun b (b_true, b_false) clauses ->
          if cased_term_selection != Minimal ||
             Term.Seq.subterms b |> 
             Iter.for_all (fun st -> T.equal b st || 
                                     not (Type.is_prop (T.ty st))) then (
            let proof = Proof.Step.simp[C.proof_parent c]
                ~rule:(Proof.Rule.mk"bool_case_simp") ~tags:[Proof.Tag.T_ho]
            in
            C.create ~trail:(C.trail c) ~penalty:(C.penalty c)
              (b_true :: Array.to_list(C.lits c |> Literals.map(T.replace ~old:b ~by:T.false_)))
              proof ::
            C.create ~trail:(C.trail c) ~penalty:(C.penalty c)
              (b_false :: Array.to_list(C.lits c |> Literals.map(T.replace ~old:b ~by:T.true_)))
              proof ::
            clauses)
          else clauses) term_to_equations [] in
    if CCList.is_empty res then None
    else (
      (* CCFormat.printf "bool case simp: %a.\n" C.pp c; *)
      (* CCList.iteri (fun i nc -> CCFormat.printf "@[%d: @[%a@]@].\n" i C.pp nc) res; *)
      Some res)

  let simpl_bool_subterms c =
    let new_lits = Literals.map T.simplify_bools (C.lits c) in
    if Literals.equal (C.lits c) new_lits then (
      SimplM.return_same c
    ) else (
      let proof = Proof.Step.simp [C.proof_parent c] 
          ~rule:(Proof.Rule.mk "simplify boolean subterms") in
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

  let normalize_equalities c =
    let lits = Array.to_list (C.lits c) in
    let normalized = List.map Literal.normalize_eq lits in
    if List.exists CCOpt.is_some normalized then (
      let new_lits = List.mapi (fun i l_opt -> 
          CCOpt.get_or ~default:(Array.get (C.lits c) i) l_opt) normalized in
      let proof = Proof.Step.inference [C.proof_parent c] 
          ~rule:(Proof.Rule.mk "simplify nested equalities")  in
      let new_c = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) new_lits proof in
      SimplM.return_new new_c
    ) 
    else (
      SimplM.return_same c 
    )

  let cnf_otf c : C.t list option =   
    let idx = CCArray.find_idx (fun l -> 
        let eq = Literal.View.as_eqn l in
        match eq with 
        | Some (l,r,sign) -> 
          Type.is_prop (T.ty l) && 
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
      let cnf_vec = Cnf.convert @@ CCVector.to_seq @@ 
        Cnf.cnf_of ~opts ~ctx:(Ctx.sk_ctx ()) stmt in
      CCVector.iter (fun cl -> 
          Statement.Seq.ty_decls cl
          |> Iter.iter (fun (id,ty) -> Ctx.declare id ty)) cnf_vec;

      let clauses = CCVector.map (C.of_statement ~convert_defs:true) cnf_vec
                    |> CCVector.to_list 
                    |> CCList.flatten
                    |> List.map (fun c -> 
                        C.create ~penalty  ~trail (CCArray.to_list (C.lits c)) proof) in
      List.iteri (fun i new_c -> 
          assert((C.proof_depth c) <= C.proof_depth new_c);) clauses;
      Some clauses
    | None -> None

  let cnf_infer cl = 
    CCOpt.get_or ~default:[] (cnf_otf cl)

  let interpret_boolean_functions c =
    (* Collects boolean functions only at top level, 
       and not the ones that are already a part of the quantifier *)
    let collect_tl_bool_funcs t k = 
      let rec aux t =
        match T.view t with
        | Var _  | Const _  | DB _ -> ()
        | Fun _ -> if Type.is_prop (Term.ty (snd @@ Term.open_fun t)) then k t
        | App (f, l) ->
          aux f;
          List.iter aux l
        | AppBuiltin (b,l) -> 
          if not @@ Builtin.is_quantifier b then List.iter aux l 
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

    Iter.flat_map collect_tl_bool_funcs 
      (C.Seq.terms c
       |> Iter.filter (fun t -> not @@ T.is_fun t))
    |> Iter.sort_uniq ~cmp:Term.compare
    |> Iter.filter (fun t ->  
        let cached_t = Subst.FO.canonize_all_vars t in
        not (Term.Set.mem cached_t !Higher_order.prim_enum_terms))
    |> Iter.fold (fun res t -> 
        assert(T.DB.is_closed t);
        let proof = Proof.Step.inference[C.proof_parent c]
            ~rule:(Proof.Rule.mk"interpret boolean function") ~tags:[Proof.Tag.T_ho]
        in
        let as_forall = Literal.mk_prop (T.Form.forall t) false in
        let as_neg_forall = Literal.mk_prop (T.Form.forall (negate_bool_fun t)) false in
        let forall_cl = 
          C.create ~trail:(C.trail c) ~penalty:(C.penalty c)
            (as_forall :: Array.to_list(C.lits c |> Literals.map(T.replace ~old:t ~by:(interpret t T.true_))))
            proof in
        let forall_neg_cl = 
          C.create ~trail:(C.trail c) ~penalty:(C.penalty c)
            (as_neg_forall :: Array.to_list(C.lits c |> Literals.map(T.replace ~old:t ~by:(interpret t T.false_))))
            proof in

        Util.debugf ~section  1 "interpret bool: %a !!> %a.\n"  (fun k -> k C.pp c C.pp forall_cl);
        Util.debugf ~section  1 "interpret bool: %a !!~> %a.\n" (fun k -> k C.pp c C.pp forall_neg_cl);

        forall_cl :: forall_neg_cl :: res
      ) []

  let solve_bool_formulas cl =
    let module PUnif = PUnif.Make(struct let st = Env.flex_state () end) in
    let unifiers = CCList.flat_map (fun literal -> 
        match literal with 
        | Literal.Equation(lhs, rhs, false) when Type.is_prop (Term.ty lhs) ->
          PUnif.unify_scoped (lhs,0) (rhs,0)
          |> OSeq.filter_map CCFun.id
          |> OSeq.to_list
        | _ -> []
      ) (CCArray.to_list (C.lits cl)) in
    if CCList.is_empty unifiers then None
    else Some (List.map (fun subst -> 
        let subst = Unif_subst.subst subst in
        C.apply_subst (cl,0) subst) unifiers)

  let setup () =
    match Env.flex_get k_bool_reasoning with 
    | BoolReasoningDisabled -> ()
    | _ -> 
      (* Env.ProofState.PassiveSet.add (create_clauses ()); *)
      Env.add_basic_simplify simpl_bool_subterms;
      Env.add_basic_simplify normalize_equalities;
      if Env.flex_get k_norm_bools then (
        Env.add_basic_simplify normalize_bool_terms
      );
      Env.add_multi_simpl_rule Fool.rw_bool_lits;
      if Env.flex_get k_cnf_non_simpl then (
        Env.add_unary_inf "cnf otf inf" cnf_infer;
      ) else  Env.add_multi_simpl_rule cnf_otf;
      if Env.flex_get k_solve_formulas then (
        Env.add_multi_simpl_rule solve_bool_formulas
      );
      if (Env.flex_get k_interpret_bool_funs) then (
        Env.add_unary_inf "interpret boolean functions" interpret_boolean_functions;
      );

      if Env.flex_get k_bool_reasoning = BoolCasesInference then (
        Env.add_unary_inf "bool_cases" bool_cases;
      )
      else if Env.flex_get k_bool_reasoning = BoolCasesSimplification then (
        Env.set_single_step_multi_simpl_rule bool_case_simp;
      ) else if Env.flex_get k_bool_reasoning = BoolCasesKeepParent then (
        let keep_parent c  = CCOpt.get_or ~default:[] (bool_case_simp c) in
        Env.add_unary_inf "bool_cases_keep_parent" keep_parent;
      )
end


open CCFun
open Builtin
open Statement
open TypedSTerm
open CCList


let if_changed proof (mk: ?attrs:Logtk.Statement.attrs -> 'r) s f p =
  let fp = f s p in
  if fp = [p] then [s] else map(fun x -> mk ~proof:(proof s) x) fp

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
  if is_bool p && not(is_T_F p) && p!=c && Var.Set.is_empty(Var.Set.diff (free_vars_set p) vs) then
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
        if t==t' then None else Some(case_bools_wrt vs t')
      | _ -> None
    )

let eager_cases_far =
  let proof s = Proof.Step.esa [Proof.Parent.from(Statement.as_proof_i s)]
      ~rule:(Proof.Rule.mk "eager_cases_far")
  in
  map_propositions ~proof (fun _ t ->
      [with_subterm_or_id t (fun vs s -> match view s with
           | Bind((Forall|Exists) as q, v, b) ->
             let b' = case_bools_wrt (Var.Set.add vs v) b in
             if b==b' then None else Some(replace s (bind ~ty:prop q v b') t)
           | _ -> None)
       |> case_bools_wrt Var.Set.empty])


let eager_cases_near =
  let proof s = Proof.Step.esa [Proof.Parent.from(Statement.as_proof_i s)]
      ~rule:(Proof.Rule.mk "eager_cases_near")
  in
  let rec case_near t =
    with_subterm_or_id t (fun vs s ->
        match view s with
        | AppBuiltin((And|Or|Imply|Not|Equiv|Xor|ForallConst|ExistsConst),_)
        | Bind((Forall|Exists),_,_) -> None
        | AppBuiltin((Eq|Neq), [x;y]) when is_bool x -> None
        | _ when is_bool s ->
          let s' = case_bool vs s (with_subterm_or_id s (fun _ -> CCOpt.if_(fun x -> x!=s && is_bool x && not(is_T_F x)))) in
          if s==s' then None else Some(case_near(replace s s' t))
        | _ -> None)
  in
  map_propositions ~proof (fun _ p -> [case_near p])


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
    | BoolCasesEagerFar -> eager_cases_far
    | BoolCasesEagerNear -> eager_cases_near
    | _ -> id
  ) (if !_quant_rename then name_quantifiers stmts else stmts)

let preprocess_cnf_booleans stmts = match !_bool_reasoning with
  | BoolCasesEagerFar | BoolCasesEagerNear -> post_eager_cases stmts
  | _ -> stmts


let _cased_term_selection = ref Large
let _interpret_bool_funs = ref false
let _cnf_non_simpl = ref false
let _norm_bools = ref false 
let _solve_formulas = ref false


let extension =
  let register env =
    let module E = (val env : Env.S) in
    let module ET = Make(E) in
    E.flex_add k_bool_reasoning !_bool_reasoning;
    E.flex_add k_cased_term_selection !_cased_term_selection;
    E.flex_add k_quant_rename !_quant_rename;
    E.flex_add k_interpret_bool_funs !_interpret_bool_funs;
    E.flex_add k_cnf_non_simpl !_cnf_non_simpl;
    E.flex_add k_norm_bools !_norm_bools;
    E.flex_add k_solve_formulas !_solve_formulas;

    ET.setup ()
  in
  { Extensions.default with
    Extensions.name = "bool";
    env_actions=[register];
  }

let () =
  Options.add_opts
    [ "--boolean-reasoning", Arg.Symbol (["off"; "cases-inf"; "cases-simpl"; "cases-simpl-kp"; "cases-eager"; "cases-eager-near"], 
                                         fun s -> _bool_reasoning := 
                                             match s with 
                                             | "off" -> BoolReasoningDisabled
                                             | "cases-inf" -> BoolCasesInference
                                             | "cases-simpl" -> BoolCasesSimplification
                                             | "cases-simpl-kp" -> BoolCasesKeepParent
                                             | "cases-eager" -> BoolCasesEagerFar
                                             | "cases-eager-near" -> BoolCasesEagerNear
                                             | _ -> assert false), 
      " enable/disable boolean axioms";
      "--bool-subterm-selection", 
      Arg.Symbol(["A"; "M"; "L"], (fun opt -> _cased_term_selection := 
                                      match opt with "A"->Any | "M"->Minimal | "L"->Large
                                                   | _ -> assert false)), 
      " select boolean subterm selection criterion: A for any, M for minimal and L for large";
      "--quantifier-renaming"
    , Arg.Bool (fun v -> _quant_rename := v)
    , " turn the quantifier renaming on or off";
      "--disable-simplifying-cnf",
      Arg.Set _cnf_non_simpl,
      "implement cnf on-the-fly as an inference rule";
      "--interpret-bool-funs"
    , Arg.Bool (fun v -> _interpret_bool_funs := v)
    , " turn interpretation of boolean functions as forall or negation of forall on or off";
      "--normalize-bool-terms", Arg.Bool((fun v -> _norm_bools := v)),
      " normalize boolean subterms using their weight.";
      "--solve-formulas"
    , Arg.Bool (fun v -> _solve_formulas := v)
    , " solve phi != psi eagerly using unification, where phi and psi are formulas"
    ];
  Params.add_to_mode "ho-complete-basic" (fun () ->
      _bool_reasoning := BoolReasoningDisabled
    );
  Params.add_to_mode "fo-complete-basic" (fun () ->
      _bool_reasoning := BoolReasoningDisabled
    );
  Extensions.register extension