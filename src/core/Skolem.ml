
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Skolem symbols} *)

module T = TypedSTerm

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

type definition = {
  form : form;
  proxy : form;
  polarity : polarity;
}

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

let fresh_sym_with ~ctx ~ty prefix =
  let n = ctx.sc_gensym in
  ctx.sc_gensym <- n+1;
  let s = ID.make (prefix ^ string_of_int n) in
  ID.add_payload s Attr_skolem;
  ctx.sc_new_ids <- (s,ty) :: ctx.sc_new_ids;
  ctx.sc_on_new s ty;
  Util.debugf ~section 3 "@[<2>new skolem symbol %a@ with type @[%a@]@]"
    (fun k->k ID.pp s T.pp ty);
  s

let fresh_sym ~ctx ~ty = fresh_sym_with ~ctx ~ty ctx.sc_prefix

let collect_vars ?(filter=fun _->true) f =
  let is_ty_var v = T.Ty.is_tType (Var.ty v) in
  T.Seq.free_vars f
    |> Sequence.filter filter
    |> Var.Set.of_seq
    |> Var.Set.to_list
    |> List.partition is_ty_var

let skolem_form ~ctx subst ty form =
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
  let ty =
    T.Ty.forall_l tyvars
      (T.Ty.fun_ (List.map Var.ty vars) (T.Subst.eval subst ty)) in
  let f = fresh_sym ~ctx ~ty in
  T.app ~ty:T.Ty.prop (T.const ~ty f) (tyvars_t @ vars_t)

let pop_new_symbols ~ctx =
  let l = ctx.sc_new_ids in
  ctx.sc_new_ids <- [];
  l

(** {2 Definitions} *)

let define ~ctx ~polarity form =
  let tyvars, vars = collect_vars form in
  let vars_t = List.map (fun v->T.var v) vars in
  let tyvars_t = List.map (fun v->T.Ty.var v) tyvars in
  (* similar to {!skolem_form}, but always return [prop] *)
  let ty = T.Ty.forall_l tyvars (T.Ty.fun_ (List.map Var.ty vars) T.Ty.prop) in
  let f = fresh_sym_with ~ctx ~ty "zip_tseitin" in
  let proxy = T.app ~ty:T.Ty.prop (T.const ~ty f) (tyvars_t @ vars_t) in
  (* register the new definition *)
  ctx.sc_new_defs <- {form; proxy; polarity; } :: ctx.sc_new_defs;
  Util.debugf ~section 5 "@[<2>define formula@ @[%a@]@ with @[%a@]@ and polarity %a@]"
    (fun k->k T.pp form T.pp proxy pp_polarity polarity);
  proxy

let pop_new_definitions ~ctx =
  let l = ctx.sc_new_defs in
  ctx.sc_new_defs <- [];
  l

let is_skolem id =
  List.exists (function Attr_skolem -> true | _ -> false) (ID.payload id)
