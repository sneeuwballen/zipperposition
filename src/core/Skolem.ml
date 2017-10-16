
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Skolem symbols} *)

module T = TypedSTerm
module Stmt = Statement
module Fmt = CCFormat

type type_ = TypedSTerm.t
type term = TypedSTerm.t
type form = TypedSTerm.t

let section = Util.Section.(make "skolem")

type polarity =
  [ `Pos
  | `Neg
  | `Both
  ]

let pp_polarity out = function
  | `Pos -> CCFormat.string out "+"
  | `Neg -> CCFormat.string out "-"
  | `Both -> CCFormat.string out "+/-"

type form_definition = {
  form: form;
  proxy_id: ID.t; (* name *)
  (* the defined object *)
  proxy : term;
  (* atom/term standing for the defined object *)
  proxy_ty : type_;
  (* type of [proxy_id] *)
  rw_rules: bool;
  (* do we add the add rules
     [proxy -> true if form]
     [proxy -> false if not form] (depending on polarity) *)
  polarity : polarity;
  proof: Proof.step;
  (* source for this definition *)
  as_stmt: Statement.input_t list lazy_t;
}

type term_definition = {
  td_id: ID.t;
  td_ty: type_;
  td_rules: (form, term, type_) Statement.def_rule list;
  td_as_def: (form,term,type_) Statement.def;
  td_proof: Proof.step;
  td_stmt: Statement.input_t list lazy_t;
}

type definition =
  | Def_form of form_definition
  | Def_term of term_definition

type ctx = {
  sc_prefix : string;
  sc_prop_prefix : string;
  mutable sc_counter: int;
  mutable sc_gensym: (string,int) Hashtbl.t; (* prefix -> count *)
  mutable sc_new_defs : definition list; (* "new" definitions *)
  mutable sc_new_ids: (ID.t * type_) list; (* "new" symbols *)
  sc_on_new : ID.t -> type_ -> unit;
}

let create
    ?(prefix="zip_sk_") ?(prop_prefix="zip_prop") ?(on_new=fun _ _->()) () =
  let ctx = {
    sc_prefix=prefix;
    sc_prop_prefix=prop_prefix;
    sc_counter=0;
    sc_new_defs = [];
    sc_gensym = Hashtbl.create 16;
    sc_new_ids = [];
    sc_on_new = on_new;
  } in
  ctx

let incr_counter ctx = ctx.sc_counter <- ctx.sc_counter + 1

let fresh_id ?(start0=false) ~ctx prefix =
  let n = CCHashtbl.get_or ~default:0 ctx.sc_gensym prefix in
  Hashtbl.replace ctx.sc_gensym prefix (n+1);
  let name = if n=0 && not start0 then prefix else prefix ^ string_of_int n in
  ID.make name

let fresh_skolem_prefix ~ctx ~ty ~vars_count prefix =
  incr_counter ctx;
  let s = fresh_id ~ctx prefix in
  let kind =
    if Ind_ty.is_inductive_simple_type ty then ID.K_ind else ID.K_normal
  in
  ID.set_payload s (ID.Attr_skolem (kind, vars_count));
  ctx.sc_new_ids <- (s,ty) :: ctx.sc_new_ids;
  ctx.sc_on_new s ty;
  Util.debugf ~section 3 "@[<2>new skolem symbol `%a`@ with type `@[%a@]`@]"
    (fun k->k ID.pp s T.pp ty);
  s

let fresh_skolem ~ctx ~ty = fresh_skolem_prefix ~ctx ~ty ctx.sc_prefix

let collect_vars subst f =
  (* traverse [t] and return free variables, dereferencing on the fly *)
  let rec vars_seq t =
    T.Seq.free_vars t
    |> Sequence.flat_map
      (fun v -> match Var.Subst.find subst v with
         | None -> Sequence.return (Var.update_ty ~f:(T.Subst.eval subst) v)
         | Some t' -> vars_seq t')
  in
  let is_ty_var v = T.Ty.is_tType (Var.ty v) in
  vars_seq f
  |> Var.Set.of_seq
  |> Var.Set.to_list
  |> List.partition is_ty_var

let ty_forall ?loc v ty =
  if T.Ty.is_tType (Var.ty v) && T.Ty.returns_tType ty
  then T.Ty.fun_ ?loc [T.Ty.tType] ty (* [forall v:type. t] becomes [type -> t] *)
  else T.Ty.forall ?loc v ty

let ty_forall_l = List.fold_right ty_forall

let skolem_form ~ctx subst var form =
  incr_counter ctx;
  let tyvars, vars = collect_vars subst form in
  Util.debugf ~section 5
    "@[<2>creating skolem for@ `@[%a@]`@ with tyvars=[@[%a@]],@ vars=[@[%a@]],@ subst={@[%a@]}@]"
    (fun k->k T.pp form (Util.pp_list Var.pp_full) tyvars
        (Util.pp_list Var.pp_full) vars (Var.Subst.pp T.pp) subst);
  let tyvars_t = List.map (fun v->T.Ty.var v) tyvars in
  let vars_t = List.map (fun v->T.var v |> T.Subst.eval subst) vars in
  (* type of the symbol: quantify over type vars, apply to vars' types *)
  let ty_var = T.Subst.eval subst (Var.ty var) in
  let ty = ty_forall_l tyvars (T.Ty.fun_ (List.map Var.ty vars) ty_var) in
  let prefix = "sk_" ^ Var.to_string var in
  let vars_count = List.length vars in
  let f = fresh_skolem_prefix ~ctx ~ty ~vars_count prefix in
  let skolem_t = T.app ~ty:T.Ty.prop (T.const ~ty f) (tyvars_t @ vars_t) in
  T.Subst.eval subst skolem_t

let pop_new_skolem_symbols ~ctx =
  let l = ctx.sc_new_ids in
  ctx.sc_new_ids <- [];
  l

let counter ctx = ctx.sc_counter

(** {2 Definitions} *)

let pp_form_definition out def =
  Format.fprintf out "(@[<hv>def %a@ for: %a@ rw_rules: %B@ polarity: %a@])"
    T.pp def.proxy T.pp def.form def.rw_rules pp_polarity def.polarity

let pp_term_definition out def =
  let pp_rule out r = Stmt.pp_def_rule T.pp T.pp T.pp out r in
  Format.fprintf out "(@[<hv>def_term `%a : %a`@ rules: (@[<hv>%a@])@])"
    ID.pp def.td_id T.pp def.td_ty (Util.pp_list pp_rule) def.td_rules

let pp_definition out = function
  | Def_form f -> pp_form_definition out f
  | Def_term t -> pp_term_definition out t

let stmt_of_form rw_rules polarity proxy proxy_id proxy_ty form proof =
  let module F = T.Form in
  if rw_rules then (
    (* introduce the required definition as an axiom, with polarity as needed *)
    let rule : _ Stmt.def_rule =
      let vars = T.vars proxy in
      let lhs, polarity, rhs = match polarity with
        | `Neg -> SLiteral.atom_false proxy, `Imply, F.not_ form
        | `Pos -> SLiteral.atom_true proxy, `Imply, form
        | `Both -> SLiteral.atom_true proxy, `Equiv, form
      in
      Stmt.Def_form {vars;lhs;rhs=[rhs];polarity;as_form=[form]}
    in
    let proof = proof in
    [Stmt.def ~proof [Stmt.mk_def ~rewrite:true proxy_id proxy_ty [rule]]]
  ) else (
    (* introduce the required axiom, with polarity as needed *)
    let f' = match polarity with
      | `Pos -> F.imply proxy form
      | `Neg -> F.imply form proxy
      | `Both -> F.equiv proxy form
    in
    let proof = proof in
    [ Stmt.ty_decl ~proof proxy_id proxy_ty;
      Stmt.assert_ ~proof f'
    ]
  )

let define_form ?(pattern="zip_tseitin") ~ctx ~rw_rules ~polarity ~parents form =
  incr_counter ctx;
  let tyvars, vars = collect_vars Var.Subst.empty form in
  let vars_t = List.map (fun v->T.var v) vars in
  let tyvars_t = List.map (fun v->T.Ty.var v) tyvars in
  (* similar to {!skolem_form}, but always return [prop] *)
  let ty = ty_forall_l tyvars (T.Ty.fun_ (List.map Var.ty vars) T.Ty.prop) in
  (* not a skolem (but a defined term). Will be defined, not declared. *)
  let f = fresh_id ~start0:true ~ctx pattern in
  let proxy = T.app ~ty:T.Ty.prop (T.const ~ty f) (tyvars_t @ vars_t) in
  let proof = Proof.Step.define_internal f parents in
  (* register the new definition *)
  let def = {
    form;
    proxy_id=f;
    proxy_ty=ty;
    rw_rules;
    proxy;
    polarity;
    proof;
    as_stmt=lazy (stmt_of_form rw_rules polarity proxy f ty form proof);
  } in
  ctx.sc_new_defs <- Def_form def :: ctx.sc_new_defs;
  Util.debugf ~section 5 "@[<2>define_form@ %a@ :proof %a@]"
    (fun k->k pp_form_definition def Proof.Step.pp proof);
  def

let pp_rules =
  Fmt.(Util.pp_list Dump.(pair (list T.pp_inner |> hovbox) T.pp) |> hovbox)

let stmt_of_term id ty rules proof : Stmt.input_t list =
  let module F = T.Form in
  [Stmt.def ~proof [Stmt.mk_def ~rewrite:true id ty rules]]

let define_term ?(pattern="fun_") ~ctx ~parents rules : term_definition =
  Util.debugf ~section 5
    "(@[<hv2>define_term@ :rules (@[<hv>%a@])@])" (fun k->k pp_rules rules);
  incr_counter ctx;
  let some_args, ty_ret = match rules with
    | [] -> assert false
    | (args, rhs) :: _ -> args, T.ty_exn rhs
  in
  (* separate type variables and type of arguments *)
  let ty_vars, ty_args =
    CCList.partition_map
      (fun t -> match T.view t with
         | T.Var v when T.Ty.is_tType (Var.ty v) -> `Left v
         | _ -> `Right (T.ty_exn t))
      some_args
  in
  (* checks *)
  List.iter
    (fun (args,_) ->
       let args' = CCList.drop (List.length ty_vars) args in
       assert (List.length args' = List.length ty_args);
       assert (List.for_all2 (fun t ty -> T.Ty.equal ty (T.ty_exn t)) args' ty_args);
       ())
    rules;
  let ty = T.Ty.forall_l ty_vars (T.Ty.fun_ ty_args ty_ret) in
  let is_prop = T.Ty.is_prop ty_ret in
  (* NOTE: not a skolem, just a mere constant undeclared so far. Will be
     a defined constant later on. *)
  let id = fresh_id ~start0:true ~ctx pattern in
  (* convert rules *)
  let rules =
    List.map
      (fun (args,rhs) ->
         let all_vars =
           Sequence.of_list (rhs::args)
           |> Sequence.flat_map T.Seq.free_vars
           |> Var.Set.of_seq |> Var.Set.to_list
         in
         if is_prop
         then (
           let atom = T.app ~ty:ty_ret (T.const ~ty id) args in
           Stmt.Def_form {
             vars=all_vars; lhs=SLiteral.atom atom true;
             rhs=[rhs]; polarity=`Equiv;
             as_form=[T.Form.eq atom rhs |> T.Form.close_forall];
           }
         ) else (
           Stmt.Def_term {
             vars=all_vars;id;ty;args;rhs;
             as_form=
               T.Form.eq (T.app (T.const ~ty id) ~ty:(T.ty_exn rhs) args) rhs
               |> T.Form.close_forall;
           }
         ))
      rules
  in
  let td_as_def = Stmt.mk_def ~rewrite:true id ty rules in
  let proof = Proof.Step.define_internal id parents in
  let def = {
    td_id=id;
    td_ty=ty;
    td_rules=rules;
    td_as_def;
    td_proof=proof;
    td_stmt=lazy (stmt_of_term id ty rules proof);
  } in
  ctx.sc_new_defs <- Def_term def :: ctx.sc_new_defs;
  Util.debugf ~section 4 "@[<2>define_term@ %a@ :proof %a@]"
    (fun k->k pp_term_definition def Proof.Step.pp proof);
  def

let new_definitions ~ctx = ctx.sc_new_defs

let pop_new_definitions ~ctx =
  let l = ctx.sc_new_defs in
  ctx.sc_new_defs <- [];
  l

let rule_def = Proof.Rule.mk "define"

let def_as_stmt (d:definition): Stmt.input_t list = match d with
  | Def_form d -> Lazy.force d.as_stmt
  | Def_term d -> Lazy.force d.td_stmt
