(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Basic context for literals, clauses...} *)

open Logtk
module T = Term
module S = Subst
module Unif = Logtk.Unif

module type S = Ctx_intf.S
(** Temporary backward-compatibility alias *)

type t = {
  mutable ord: Ordering.t;
  mutable select: Selection.t;
  bool_select: Bool_selection.t;
  mutable signature: Signature.t;
  mutable complete: bool;
  sk_ctx: Skolem.ctx;
  mutable inj_syms: CCBV.t Name.Map.t;
  renaming: Subst.Renaming.t;
  on_new_symbol: (Name.t * Type.t) Signal.t;
  on_signature_update: Signature.t Signal.t;
  mutable lit_from_hooks: Literal.Conv.hook_from list;
  mutable lit_to_hooks: Literal.Conv.hook_to list;
}

let create ~signature ~ord ~select ~bool_select ~sk_ctx =
  {
    ord;
    select;
    bool_select;
    signature;
    complete = true;
    sk_ctx;
    inj_syms = Name.Map.empty;
    renaming = S.Renaming.create ();
    on_new_symbol = Signal.create ();
    on_signature_update = Signal.create ();
    lit_from_hooks = [];
    lit_to_hooks = [];
  }

(** Global default context (used when ~ctx is not provided to Clause.create) *)

(** {2 Accessors / mutators} *)

let ord t = t.ord
let set_ord t o = t.ord <- o
let selection_fun t = t.select
let set_selection_fun t s = t.select <- s
let signature t = t.signature
let renaming t = t.renaming
let sk_ctx t = t.sk_ctx
let on_new_symbol t = t.on_new_symbol
let on_signature_update t = t.on_signature_update

(** {2 Utils} *)

let compare t t1 t2 = Ordering.compare t.ord t1 t2
let select t lits = t.select lits
let bool_select t lits = t.bool_select lits

let lost_completeness ctx =
  if ctx.complete then
    Util.debug ~section:Const.section 1 "completeness is lost";
  ctx.complete <- false

let is_completeness_preserved t = t.complete

(* declare [symb : ty], with precondition that [symb] is not declared yet *)
let declare_new_ t ~symb (ty, _) =
  Util.debugf ~section:Const.section 2
    "@[<2>@{<cyan>declare new symbol@}@ `@[%a:%a@]`@]" (fun k ->
      k Name.pp symb Type.pp ty);
  t.signature <- Signature.declare t.signature symb ty;
  Signal.send t.on_signature_update t.signature;
  Signal.send t.on_new_symbol (symb, ty);
  Ordering.add_list ~signature:t.signature t.ord [ symb ];
  ()

let add_signature t signature =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "ctx.add-signature" in
  let _diff = Signature.diff signature t.signature in
  (* declare new symbols *)
  Signature.iter _diff (fun symb ty -> declare_new_ t ~symb ty);
  ()

let declare t symb ty =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "ctx.declare" in
  let is_new = not (Signature.mem t.signature symb) in
  if is_new then declare_new_ t ~symb (ty, false);
  ()

let declare_syms t l =
  List.iter
    (fun (symb, ty) -> t.signature <- Signature.declare t.signature symb ty)
    l;
  Signal.send t.on_signature_update t.signature;
  List.iter (Signal.send t.on_new_symbol) l;
  Ordering.add_list ~signature:t.signature t.ord (List.map fst l)

let find_signature t s = Signature.find t.signature s

let find_signature_exn t s =
  try Signature.find_exn t.signature s
  with Not_found ->
    invalid_arg (CCFormat.sprintf "%a not found in signature" Name.pp s)

let set_injective_for_arg t sym i =
  let arg_bv =
    match Name.Map.find_opt sym t.inj_syms with
    | Some res -> res
    | None -> CCBV.empty ()
  in
  CCBV.set arg_bv i;
  t.inj_syms <- Name.Map.add sym arg_bv t.inj_syms

let is_injective_for_arg t sym i =
  match Name.Map.find_opt sym t.inj_syms with
  | Some res -> CCBV.get res i
  | None -> false

(** {2 Literal conversion} *)

let lit_of_form t f = Literal.Conv.of_form ~hooks:t.lit_from_hooks f
let lit_to_form t f = Literal.Conv.to_form ~hooks:t.lit_to_hooks f
let add_lit_from_hook t h = t.lit_from_hooks <- h :: t.lit_from_hooks
let add_lit_to_hook t h = t.lit_to_hooks <- h :: t.lit_to_hooks

module Lit = struct
  (* Bridge: uses global ref for backward compat *)
  let from_hooks () = Atomic.get Ctx_global.from_hooks

  let add_from_hook h =
    Atomic.set Ctx_global.from_hooks (h :: Atomic.get Ctx_global.from_hooks)

  let to_hooks () = Atomic.get Ctx_global.to_hooks

  let add_to_hook h =
    Atomic.set Ctx_global.to_hooks (h :: Atomic.get Ctx_global.to_hooks)

  let of_form f =
    Literal.Conv.of_form ~hooks:(Atomic.get Ctx_global.from_hooks) f

  let to_form f = Literal.Conv.to_form ~hooks:(Atomic.get Ctx_global.to_hooks) f
end

module Key = struct
  let lost_completeness = Flex_state.create_key ()
end
