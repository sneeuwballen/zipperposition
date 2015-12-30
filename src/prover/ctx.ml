
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Basic context for literals, clauses...} *)

open Libzipperposition

module T = FOTerm
module S = Substs
module Unif = Libzipperposition.Unif

(** {2 Context for a Proof} *)
module type S = Ctx_intf.S

let prof_add_signature = Util.mk_profiler "ctx.add_signature"
let prof_declare_sym= Util.mk_profiler "ctx.declare"

module type PARAMETERS = sig
  val signature : Signature.t
  val ord : Ordering.t
  val select : Selection.t
end

module Make(X : PARAMETERS) = struct
  let _ord = ref X.ord
  let _select = ref X.select
  let _signature = ref X.signature
  let _complete = ref true

  let skolem = Skolem.create ~prefix:"zsk" ()
  let renaming = S.Renaming.create ()
  let ord () = !_ord
  let set_ord o = _ord := o
  let selection_fun () = !_select
  let set_selection_fun s = _select := s
  let signature () = !_signature

  let on_new_symbol = Signal.create()
  let on_signature_update = Signal.create()

  let find_signature s = Signature.find !_signature s
  let find_signature_exn s = Signature.find_exn !_signature s

  let compare t1 t2 = Ordering.compare !_ord t1 t2

  let select lits = !_select lits

  let lost_completeness () =
    if !_complete then Util.debug ~section:Const.section 1 "completeness is lost";
    _complete := false

  let is_completeness_preserved () = !_complete

  let add_signature signature =
    Util.enter_prof prof_add_signature;
    let _diff = Signature.diff signature !_signature in
    _signature := Signature.merge !_signature signature;
    Signal.send on_signature_update !_signature;
    Signature.iter _diff (fun s ty -> Signal.send on_new_symbol (s,ty));
    !_signature
      |> Signature.Seq.to_seq
      |> Sequence.map fst
      |> Ordering.add_seq !_ord;
    Util.exit_prof prof_add_signature;
    ()

  let _declare_symb symb ty =
    let is_new = not (Signature.mem !_signature symb) in
    _signature := Signature.declare !_signature symb ty;
    if is_new then (
      Signal.send on_signature_update !_signature;
      Signal.send on_new_symbol (symb,ty);
    )

  let declare symb ty =
    Util.enter_prof prof_declare_sym;
    _declare_symb symb ty;
    Util.exit_prof prof_declare_sym;
    ()

  let renaming_clear () =
    S.Renaming.clear renaming;
    renaming

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

  module BoolBox = BBox.Make(struct end)
end
