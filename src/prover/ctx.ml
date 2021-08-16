
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Basic context for literals, clauses...} *)

open Logtk

module T = Term
module S = Subst
module Unif = Logtk.Unif

(** {2 Context for a Proof} *)
module type S = Ctx_intf.S

let prof_add_signature = ZProf.make "ctx.add_signature"
let prof_declare_sym= ZProf.make "ctx.declare"

module type PARAMETERS = sig
  val signature : Signature.t
  val ord : Ordering.t
  val select : Selection.t
  val bool_select : Bool_selection.t
  val sk_ctx : Skolem.ctx

end

module Key = struct
  let lost_completeness = Flex_state.create_key()
end

module Make(X : PARAMETERS) = struct
  let _ord = ref X.ord
  let _select = ref X.select
  let _b_select = ref X.bool_select
  let _signature = ref X.signature
  let _complete = ref true
  let _sk_ctx = ref X.sk_ctx

  let _inj_syms = ref ID.Map.empty

  let renaming = S.Renaming.create ()
  let ord () = !_ord
  let sk_ctx () = !_sk_ctx
  let set_ord o = _ord := o
  let selection_fun () = !_select
  let set_selection_fun s = _select := s
  let signature () = !_signature

  let on_new_symbol = Signal.create()
  let on_signature_update = Signal.create()

  let find_signature s = Signature.find !_signature s
  let find_signature_exn s = 
    try 
      Signature.find_exn !_signature s
    with Not_found ->
      invalid_arg (CCFormat.sprintf "%a not found in signature" ID.pp s)


  let compare t1 t2 = Ordering.compare !_ord t1 t2

  let select lits = !_select lits
  let bool_select lits = !_b_select lits

  let lost_completeness () =
    if !_complete then Util.debug ~section:Const.section 1 "completeness is lost";
    _complete := false

  let is_completeness_preserved () = !_complete

  (* declare [symb : ty], with precondition that [symb] is not declared yet *)
  let declare_new_ symb (ty,_) =
    Util.debugf ~section:Const.section 2 "@[<2>@{<cyan>declare new symbol@}@ `@[%a:%a@]`@]"
      (fun k->k ID.pp symb Type.pp ty);
    _signature := Signature.declare !_signature symb ty;
    Signal.send on_signature_update !_signature;
    Signal.send on_new_symbol (symb,ty);
    Ordering.add_list ~signature:!_signature (ord ()) [symb];
    ()

  let add_signature signature =
    let _span = ZProf.enter_prof prof_add_signature in
    let _diff = Signature.diff signature !_signature in
    (* declare new symbols *)
    Signature.iter _diff declare_new_;
    ZProf.exit_prof _span;
    ()

  let declare symb ty =
    let _span = ZProf.enter_prof prof_declare_sym in
    let is_new = not (Signature.mem !_signature symb) in
    if is_new then declare_new_ symb (ty,false);
    ZProf.exit_prof _span;
    ()

  let declare_syms l =
    List.iter (fun (symb,ty) -> _signature := Signature.declare !_signature symb ty) l;
    Signal.send on_signature_update !_signature;
    List.iter (Signal.send on_new_symbol) l;
    Ordering.add_list ~signature:!_signature (ord ()) (List.map fst l)

  let set_injective_for_arg sym i = 
    let arg_bv = 
      match ID.Map.find_opt sym !_inj_syms with
        Some res -> res
      | None -> CCBV.empty () in
    (CCBV.set arg_bv i);
    _inj_syms := ID.Map.add sym arg_bv !_inj_syms

  let is_injective_for_arg sym i  =
    match ID.Map.find_opt sym !_inj_syms with
      Some res -> CCBV.get res i
    | None -> false 


  module Lit = struct
    let _from = ref []
    let _to = ref []

    let from_hooks () = !_from
    let to_hooks () = !_to

    let add_to_hook h = _to := h :: !_to
    let add_from_hook h = _from := h :: !_from

    let of_form f = Literal.Conv.of_form ~hooks:!_from f
    let to_form f = Literal.Conv.to_form ~hooks:!_to f
  end
end
