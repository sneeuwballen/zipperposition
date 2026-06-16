(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** TEMPORARY BRIDGE — will be deleted once Env is rewritten to take Ctx.t
    directly *)

open Logtk

module type S = sig
  type t

  val sk_ctx : unit -> Skolem.ctx
  val ord : unit -> Ordering.t
  val selection_fun : unit -> Selection.t
  val set_selection_fun : Selection.t -> unit
  val set_ord : Ordering.t -> unit
  val signature : unit -> Signature.t
  val renaming : Subst.Renaming.t
  val compare : Term.t -> Term.t -> Comparison.t
  val select : Selection.t
  val bool_select : Bool_selection.t
  val lost_completeness : unit -> unit
  val is_completeness_preserved : unit -> bool
  val add_signature : Signature.t -> unit
  val find_signature : Name.t -> Type.t option
  val find_signature_exn : Name.t -> Type.t
  val declare : Name.t -> Type.t -> unit
  val declare_syms : (Name.t * Type.t) list -> unit
  val on_new_symbol : (Name.t * Type.t) Signal.t
  val on_signature_update : Signature.t Signal.t
  val set_injective_for_arg : Name.t -> int -> unit
  val is_injective_for_arg : Name.t -> int -> bool

  module Lit : sig
    val from_hooks : unit -> Literal.Conv.hook_from list
    val add_from_hook : Literal.Conv.hook_from -> unit
    val to_hooks : unit -> Literal.Conv.hook_to list
    val add_to_hook : Literal.Conv.hook_to -> unit
    val of_form : Term.t SLiteral.t -> Literal.t
    val to_form : Literal.t -> Term.t SLiteral.t
  end
end
