
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Skolem symbols} *)

module T = TypedSTerm
module Stmt = Statement

type type_ = TypedSTerm.t
type term = TypedSTerm.t
type form = TypedSTerm.t

let section = Util.Section.(make ~parent:zip "skolem")

exception Attr_skolem

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
  (* the defined object *)
  proxy : term;
  (* atom/term standing for the defined object *)
  add_rules: bool;
  (* do we add the add rules
     [proxy -> true if form]
     [proxy -> false if not form] (depending on polarity) *)
  polarity : polarity;
}

type term_definition = {
  td_id: ID.t;
  td_ty: type_;
  td_rules: (form, term, type_) Statement.def_rule list;
}

type definition =
  | Def_form of form_definition
  | Def_term of term_definition

type ctx = {
  sc_prefix : string;
  sc_prop_prefix : string;
  mutable sc_gensym: int;
  mutable sc_new_defs : definition list; (* "new" definitions *)
  mutable sc_new_ids: (ID.t * type_) list; (* "new" symbols *)
  sc_on_new : ID.t -> type_ -> unit;
}

let create
?(prefix="zip_sk_") ?(prop_prefix="zip_prop") ?(on_new=fun _ _->()) () =
  let ctx = {
    sc_prefix=prefix;
    sc_prop_prefix=prop_prefix;
    sc_new_defs = [];
    sc_gensym = 0;
    sc_new_ids = [];
    sc_on_new = on_new;
  } in
  ctx

let fresh_id ~ctx prefix =
  let n = ctx.sc_gensym in
  ctx.sc_gensym <- n+1;
  ID.make (prefix ^ string_of_int n)

let fresh_skolem_prefix ~ctx ~ty prefix =
  let s = fresh_id ~ctx prefix in
  ID.add_payload s Attr_skolem;
  ctx.sc_new_ids <- (s,ty) :: ctx.sc_new_ids;
  ctx.sc_on_new s ty;
  Util.debugf ~section 3 "@[<2>new skolem symbol %a@ with type @[%a@]@]"
    (fun k->k ID.pp s T.pp ty);
  s

let fresh_skolem ~ctx ~ty = fresh_skolem_prefix ~ctx ~ty ctx.sc_prefix

let collect_vars ?(filter=fun _->true) f =
  let is_ty_var v = T.Ty.is_tType (Var.ty v) in
  T.Seq.free_vars f
    |> Sequence.filter filter
    |> Var.Set.of_seq
    |> Var.Set.to_list
    |> List.partition is_ty_var

let skolem_form ~ctx subst ty_var form =
  (* only free variables we are interested in, are those bound to actual
     free variables (the universal variables), not the existential ones
     (bound to Skolem symbols) *)
  let filter v =
    try match T.view (Var.Subst.find_exn subst v) with
      | T.Var _ -> true
      | _ -> false
    with Not_found -> true
  in
  let tyvars, vars = collect_vars form ~filter in
  Util.debugf ~section 5
    "@[<2>creating skolem for@ `@[%a@]`@ with tyvars=@[%a@],@ vars=@[%a@],@ subst={@[%a@]}@]"
    (fun k->k T.pp form (Util.pp_list Var.pp_full) tyvars
        (Util.pp_list Var.pp_full) vars (Var.Subst.pp T.pp) subst);
  let vars_t = List.map (fun v->T.var v) vars in
  let tyvars_t = List.map (fun v->T.Ty.var v) tyvars in
  (* type of the symbol: quantify over type vars, apply to vars' types *)
  let ty_var = T.Subst.eval subst ty_var in
  let ty = T.Ty.forall_l tyvars (T.Ty.fun_ (List.map Var.ty vars) ty_var) in
  let prefix = (T.Ty.mangle ty_var) ^ "_" in
  let f = fresh_skolem_prefix ~ctx ~ty prefix in
  T.app ~ty:T.Ty.prop (T.const ~ty f) (tyvars_t @ vars_t)

let pop_new_skolem_symbols ~ctx =
  let l = ctx.sc_new_ids in
  ctx.sc_new_ids <- [];
  l

(** {2 Definitions} *)

let pp_form_definition out def =
  Format.fprintf out "(@[<hv>def %a@ for: %a@ add_rules: %B@ polarity: %a@])"
    T.pp def.proxy T.pp def.form def.add_rules pp_polarity def.polarity

let pp_term_definition out def =
  let pp_rule out r = Stmt.pp_def_rule T.pp T.pp out r in
  Format.fprintf out "(@[<hv>def_term `%a : %a`@ rules: (@[<hv>%a@])@])"
    ID.pp def.td_id T.pp def.td_ty (Util.pp_list pp_rule) def.td_rules

let pp_definition out = function
  | Def_form f -> pp_form_definition out f
  | Def_term t -> pp_term_definition out t

let define_form ~ctx ~add_rules ~polarity form =
  let tyvars, vars = collect_vars form in
  let vars_t = List.map (fun v->T.var v) vars in
  let tyvars_t = List.map (fun v->T.Ty.var v) tyvars in
  (* similar to {!skolem_form}, but always return [prop] *)
  let ty = T.Ty.forall_l tyvars (T.Ty.fun_ (List.map Var.ty vars) T.Ty.prop) in
  let f = fresh_skolem_prefix ~ctx ~ty "zip_tseitin" in
  let proxy = T.app ~ty:T.Ty.prop (T.const ~ty f) (tyvars_t @ vars_t) in
  (* register the new definition *)
  let def = {
    form;
    add_rules;
    proxy;
    polarity;
  } in
  ctx.sc_new_defs <- Def_form def :: ctx.sc_new_defs;
  Util.debugf ~section 5 "@[<2>define_form@ %a@]" (fun k->k pp_form_definition def);
  def

let define_term ~ctx rules : term_definition =
  let ty_args, ty_ret = match rules with
    | [] -> assert false
    | (args, rhs) :: _ ->
      List.map T.ty_exn args, T.ty_exn rhs
  in
  (* checks *)
  List.iter
    (fun (args,_) ->
       assert (List.length args = List.length ty_args);
       assert (List.for_all2 (fun t ty -> T.Ty.equal ty (T.ty_exn t)) args ty_args);
       ())
    rules;
  let ty = T.Ty.fun_ ty_args ty_ret in
  let is_prop = T.Ty.is_prop ty_ret in
  (* NOTE: not a skolem, just a mere constant undeclared so far *)
  let id = fresh_id ~ctx "fun_" in
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
         then
           let atom = T.app ~ty:ty_ret (T.const ~ty id) args in
           Stmt.Def_form (all_vars, SLiteral.atom atom true, [rhs])
         else
           Stmt.Def_term (all_vars, id, ty, args, rhs))
      rules
  in
  let def = {
    td_id=id;
    td_ty=ty;
    td_rules=rules;
  } in
  ctx.sc_new_defs <- Def_term def :: ctx.sc_new_defs;
  Util.debugf ~section 5 "@[<2>define_term@ %a@]" (fun k->k pp_term_definition def);
  def

let pop_new_definitions ~ctx =
  let l = ctx.sc_new_defs in
  ctx.sc_new_defs <- [];
  l

let is_skolem id =
  List.exists (function Attr_skolem -> true | _ -> false) (ID.payload id)
