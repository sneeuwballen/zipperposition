
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 boolean subterms} *)

open Logtk
open Libzipperposition

module T = Term

type selection_setting = Any | Minimal | Large
type reasoning_kind    = BoolReasoningDisabled | BoolCasesInference | BoolCasesSimplification | BoolCasesEager

let _bool_reasoning = ref BoolReasoningDisabled
let cased_term_selection = ref Any
let quant_rename = ref false

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
    (*let alpha_var = HVar.make ~ty:Type.tType 0 in
    let alpha = Type.var alpha_var in
    let b = T.var (HVar.make ~ty:Type.prop 1) in
    let p = T.var (HVar.make ~ty:(Type.arrow [alpha] Type.prop) 1) in
    let x = T.var (HVar.make ~ty:alpha 1) in
    let y = T.var (HVar.make ~ty:alpha 2) in*)
    let a = T.var (HVar.make ~ty:Type.prop 0) in
    [
      [Builtin.And @:[T.true_; a] =~ a];
	  [Builtin.And @:[T.false_; a] =~ T.false_];
	  [Builtin.Or @:[T.true_; a] =~ T.true_];
	  [Builtin.Or @:[T.false_; a] =~ a];
	  [Builtin.Imply @:[T.true_; a] =~ a];
	  [Builtin.Imply @:[T.false_; a] =~ T.true_];
	  [Builtin.Not @:[T.true_] =~ T.false_];
	  [Builtin.Not @:[T.false_] =~ T.true_];
	  (*
	  imp_true1 a b; imp_true2 a b; imp_false a b; 
      and_ a b;
      all_true p; all_false p;
	  eq_true x y; eq_false x y; 
      (*not; exists alpha;*)
      or_ a b;
      (*and_false a; and_true a;*)
	  *)
    ] |> List.map as_clause |> Iter.of_list

  let bool_cases(c: C.t) : C.t list =
  let term_as_true = Hashtbl.create 8 in
  let term_as_false = Hashtbl.create 4 in
	let rec find_bools top t =
		let can_be_cased = Type.is_prop(T.ty t) && T.DB.is_closed t && not top in
    let is_quant = match T.view t with 
      | AppBuiltin(b,_) -> 
        Builtin.equal b Builtin.ForallConst || Builtin.equal b Builtin.ExistsConst
      | _ -> false in
		(* Add only propositions. *)
		let add = if can_be_cased then Hashtbl.add term_as_true else fun _ _ -> () in
		let yes = if can_be_cased then yes else fun _ -> yes T.true_ in
		(* Stop recursion in combination of certain settings. *)
		let inner f x = 
      if is_quant || can_be_cased && !cased_term_selection = Large 
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
						(match ps with 
              | [x;y] when (!cased_term_selection != Minimal || Type.is_prop(T.ty x)) ->
                if f = Builtin.Neq || f = Builtin.Xor then(
                  if can_be_cased then Hashtbl.add term_as_false t (x =~ y);
                  add t (x /~ y)
                )else
                  add t (x =~ y)
              | _ -> ())
					| Builtin.And | Builtin.Or | Builtin.Imply | Builtin.Not ->
						if !cased_term_selection != Minimal then add t (yes t) else()
					| _ -> add t (yes t)
	in
	Literals.Seq.terms(C.lits c) |> Iter.iter(find_bools true);
	let case polarity b b_lit clauses =
		let proof = Proof.Step.inference[C.proof_parent c]
			~rule:(Proof.Rule.mk"bool_cases")
		in
		C.create ~trail:(C.trail c) ~penalty:(C.penalty c)
			(b_lit :: Array.to_list(C.lits c |> Literals.map(T.replace ~old:b ~by:polarity)))
		proof :: clauses
	in
	Hashtbl.fold(case T.false_) term_as_true [] @
	Hashtbl.fold(case T.true_) term_as_false []


  let bool_case_simp(c: C.t) : C.t list option =
    let term_to_equations = Hashtbl.create 8 in
    let rec find_bools top t =
      let can_be_cased = Type.is_prop(T.ty t) && T.DB.is_closed t && (not top ||
        (* It is useful to case top level equality like 𝘵𝘦𝘳𝘮𝘴 because these are simplified into 𝘭𝘪𝘵𝘦𝘳𝘢𝘭𝘴. *)
        match T.view t with AppBuiltin((Eq|Neq|Equiv|Xor),_) -> true | _ -> false) in
      let is_quant = match T.view t with 
        | AppBuiltin(b,_) -> 
          Builtin.equal b Builtin.ForallConst || Builtin.equal b Builtin.ExistsConst
        | _ -> false in
      (* Add only propositions. *)
      let add t x y = if can_be_cased then Hashtbl.add term_to_equations t (x=~y, x/~y) in
      (* Stop recursion in combination of certain settings. *)
      let inner f x = 
        if is_quant || (can_be_cased && !cased_term_selection = Large) 
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
              (* Format.printf "Equality like: %b %b (%a) %a\n" (!cased_term_selection != Minimal) (Type.is_prop(T.ty(List.hd ps))) (CCList.pp T.pp) ps C.pp c; *)
              (match ps with 
                | [x;y] when (!cased_term_selection != Minimal || Type.is_prop(T.ty x)) ->
                  add t x y;
                  if (f = Builtin.Neq || f = Builtin.Xor) && can_be_cased then
                    Hashtbl.replace term_to_equations t (Hashtbl.find term_to_equations t |> CCPair.swap)
                | _ -> ())
            | Builtin.And | Builtin.Or | Builtin.Imply | Builtin.Not ->
              if !cased_term_selection != Minimal then add t t T.true_ else()
            | _ -> add t t T.true_
      in
      Literals.Seq.terms(C.lits c) |> Iter.iter(find_bools true);
      let res = 
        Hashtbl.fold(fun b (b_true, b_false) clauses ->
          if !cased_term_selection != Minimal ||
            Term.Seq.subterms b |> 
            Iter.for_all (fun st -> T.equal b st || 
                                    not (Type.is_prop (T.ty st))) then (
            let proof = Proof.Step.inference[C.proof_parent c]
              ~rule:(Proof.Rule.mk"bool_case_simp")
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
      else (Some res)

  let simpl_eq_subterms c =
    let simplified = ref false in
    let new_lits = 
      C.Seq.terms c |>
      Iter.flat_map T.Seq.subterms
      |> Iter.fold (fun acc t -> 
        match T.view t with
        | T.AppBuiltin(hd, [lhs;rhs]) when T.equal lhs rhs -> 
            if Builtin.equal hd Builtin.Eq  || Builtin.equal hd Builtin.Equiv then (
              simplified := true;
              Literals.map (T.replace ~old:t ~by:T.true_) acc
            ) else if Builtin.equal hd Builtin.Neq  || Builtin.equal hd Builtin.Xor then (
              simplified := true;
              Literals.map (T.replace ~old:t ~by:T.false_) acc
            ) else  acc
        | _ -> acc) (C.lits c)
       in
    if not (!simplified) then (
      SimplM.return_same c
    ) else (
      let proof = Proof.Step.inference [C.proof_parent c] 
                    ~rule:(Proof.Rule.mk "simplify trivial (in)equalities") in
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
                    ~rule:(Proof.Rule.mk "simplify nested equalities") in
      let new_c = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) new_lits proof in
      SimplM.return_new new_c
    ) 
    else (
      SimplM.return_same c 
    )
  
  let canonize_variables c =
    let all_vars = Literals.vars (C.lits c) |> T.VarSet.of_list in
    let max_id   = T.VarSet.max_elt_opt all_vars in
    match max_id with 
    | Some id ->
      let max_id = ref (HVar.id id) in
      let neg_vars_renaming = T.VarSet.fold (fun v subst -> 
        let v_id = HVar.id v in
          if v_id < 0 then (
            match Subst.FO.get_var subst ((v :> InnerTerm.t HVar.t),0) with
            | Some _ -> subst 
            | _ -> (
              incr max_id;
              let renamed_var = T.var (HVar.make ~ty:(HVar.ty v) !max_id) in
              Subst.FO.bind subst ((v :> InnerTerm.t HVar.t), 0) (renamed_var, 0)))
          else subst) 
        all_vars Subst.empty 
      in
      if Subst.is_empty neg_vars_renaming then SimplM.return_same c
      else (
        let new_lits = Literals.apply_subst Subst.Renaming.none neg_vars_renaming (C.lits c, 0)
                      |> CCArray.to_list in
        let proof = Proof.Step.inference [C.proof_parent c] ~rule:(Proof.Rule.mk "cannonize vars") in
        let new_c = C.create ~trail:(C.trail c) ~penalty:(C.penalty c) new_lits proof in
        SimplM.return_new new_c)
    | None -> SimplM.return_same c

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
    match idx with 
    | Some _ ->
      let f = Literals.Conv.to_tst (C.lits c) in
      let proof = Proof.Step.esa ~rule:(Proof.Rule.mk "cnf_otf") [C.proof_parent c] in
      let stmt = Statement.assert_ ~proof f in
      let cnf_vec = Cnf.convert @@ CCVector.to_seq @@ Cnf.cnf_of ~ctx:(Ctx.sk_ctx ()) stmt in
      let sets = Env.convert_input_statements cnf_vec in
      let clauses = sets.Clause.c_set |> CCVector.to_list in
      Some clauses
    | None       -> None

  let setup () =
	(* if !_bool_reasoning then(
		Env.ProofState.ActiveSet.add (create_clauses () );
		Env.add_unary_inf "bool_cases" bool_cases;
    Env.add_basic_simplify simpl_eq_subterms;
	) *)
  match !_bool_reasoning with 
  | BoolReasoningDisabled -> ()
  | _ -> 
    (* Env.ProofState.PassiveSet.add (create_clauses ()); *)
    Env.add_basic_simplify simpl_eq_subterms;
    Env.add_basic_simplify canonize_variables;
    Env.add_basic_simplify normalize_equalities;
    Env.add_multi_simpl_rule Fool.rw_bool_lits;
    Env.add_multi_simpl_rule cnf_otf;
    if !_bool_reasoning = BoolCasesInference then (
      Env.add_unary_inf "bool_cases" bool_cases;
    )
    else if !_bool_reasoning = BoolCasesSimplification then (
      Env.set_single_step_multi_simpl_rule bool_case_simp;
    )
end


open Builtin
open Statement
open TypedSTerm
open List


let if_changed proof (mk: ?attrs:Logtk.Statement.attrs -> 'r) s f' p =
  let fp = f' p in
  if fp = p then s else mk ~proof:(proof s) fp

let map_propositions ~proof f =
  CCVector.map(fun s -> match Statement.view s with
    | TyDecl(id,t)	-> s
    | Data ts	-> s
    | Def defs	-> s
    | Rewrite _	-> s
    | Assert p	-> if_changed proof assert_ s (f s) p
    | Lemma ps	-> if_changed proof lemma s (map(f s)) ps
    | Goal p	-> if_changed proof goal s (f s) p
    | NegatedGoal(ts, ps)	-> if_changed proof (neg_goal ~skolems:ts) s (map(f s)) ps
  )


let is_bool t = Ty.equal prop (ty_exn t)
let is_T_F t = match view t with AppBuiltin((True|False),[]) -> true | _ -> false

(* Modify every subterm of t by f except those at the "top". Here top is true if subterm occures under a quantifier Æ in a context where it could participate to the clausification if the surrounding context of Æ was ignored. *)
let rec replaceTST f top t =
  let re = replaceTST f in
  let ty = match ty t with
    | Some ty -> ty
    | None -> assert false (* These are typed terms so they must have a type. *)
  in
  let transformer = if top then CCFun.id else f in
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
          let logical = List.for_all is_bool ts in
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
        let ty = app_builtin ~ty:tType Builtin.Arrow (prop :: List.map Var.ty vars) in
        let q = const ~ty qid in
        let q_vars = app ~ty:prop q (List.map var vars) in
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
    | Lemma ps	-> if_changed_list lemma s (List.map (name_prop_Qs s) ps)
    | Goal p	-> if_changed goal s (name_prop_Qs s p)
    | NegatedGoal(ts, ps)	-> if_changed_list (neg_goal ~skolems:ts) s (List.map (name_prop_Qs s) ps)
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


(* Apply repeatedly the transformation t[p] ↦ (p ⇒ t[⊤]) ∧ (¬p ⇒ t[⊥]) for each boolean parameter p≠⊤,⊥ that is closed in context where variables vs are bound. *)
let rec case_bools_wrt vs t =
  let ty = prop in
  with_subterm_or_id t (fun _ s -> 
  match view s with
    | App(f,ps) ->
        let t' = List.fold_left(fun t' p ->
          if is_bool p && not(is_T_F p) && Var.Set.is_empty(Var.Set.diff (free_vars_set s) vs)
          then
            app_builtin ~ty And [
              app_builtin ~ty Imply [p; replace p Form.true_ t'];
              app_builtin ~ty Or [p; replace p Form.false_ t'];
            ]
          else t') t ps
        in
        if t==t' then None else Some(case_bools_wrt vs t')
    | _ -> None
  )

let eager_bool_cases x =
  let proof s = Proof.Step.esa [Proof.Parent.from(Statement.as_proof_i s)]
          ~rule:(Proof.Rule.mk "eager_bool_cases")
  in
  map_propositions ~proof (fun _ t ->
    with_subterm_or_id t (fun vs s -> match view s with
      | Bind((Forall|Exists) as q, v, b) ->
          let b' = case_bools_wrt (Var.Set.add vs v) b in
          if b==b' then None else Some(replace s (bind ~ty:prop q v b') t)
      | _ -> None)
    |> case_bools_wrt Var.Set.empty)x


let preprocess_booleans stmts = name_quantifiers(match !_bool_reasoning with
  | BoolReasoningDisabled -> stmts
  | BoolCasesEager -> eager_bool_cases stmts
  | _ -> stmts)

let extension =
  let register env =
    let module E = (val env : Env.S) in
    let module ET = Make(E) in
    ET.setup ()
  in
  { Extensions.default with
      Extensions.name = "bool";
      env_actions=[register];
  }

let () =
  Options.add_opts
    [ "--boolean-reasoning", Arg.Symbol (["off"; "cases-inf"; "cases-simpl"; "cases-eager"], 
        fun s -> _bool_reasoning := 
                    match s with 
                    | "off" -> BoolReasoningDisabled
                    | "cases-inf" -> BoolCasesInference
                    | "cases-simpl" -> BoolCasesSimplification
                    | "cases-eager" -> BoolCasesEager
                    | _ -> assert false), 
      " enable/disable boolean axioms";
      "--bool-subterm-selection", 
      Arg.Symbol(["A"; "M"; "L"], (fun opt -> if opt = "A" then cased_term_selection := Any
                                            else if opt = "N" then cased_term_selection := Minimal
                                            else cased_term_selection := Large)), 
      " select boolean subterm selection criterion: A for any, M for minimal and L for large";
      "--quantifier-renaming"
      , Arg.Bool (fun v -> quant_rename := v)
      , " turn the quantifier renaming on or off"];
  Params.add_to_mode "ho-complete-basic" (fun () ->
    _bool_reasoning := BoolReasoningDisabled
  );
  Params.add_to_mode "fo-complete-basic" (fun () ->
    _bool_reasoning := BoolReasoningDisabled
  );
  Extensions.register extension