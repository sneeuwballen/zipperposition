(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Basic context for literals, clauses...} *)

open Logtk

type t

module type S = Ctx_intf.S
(** Temporary backward-compatibility alias *)

val create :
  signature:Signature.t ->
  ord:Ordering.t ->
  select:Selection.t ->
  bool_select:Bool_selection.t ->
  sk_ctx:Skolem.ctx ->
  t

(** {2 Accessors / mutators} *)

val ord : t -> Ordering.t
val set_ord : t -> Ordering.t -> unit
val selection_fun : t -> Selection.t
val set_selection_fun : t -> Selection.t -> unit
val signature : t -> Signature.t
val renaming : t -> Subst.Renaming.t
val sk_ctx : t -> Skolem.ctx

(** {2 Utils} *)

val compare : t -> Term.t -> Term.t -> Comparison.t
(** Compare two terms *)

val select : t -> Literals.t -> CCBV.t
val bool_select : t -> Literals.t -> (Term.t * Position.t) list

val lost_completeness : t -> unit
(** To be called when completeness is not preserved *)

val is_completeness_preserved : t -> bool
(** Check whether completeness was preserved so far *)

val add_signature : t -> Signature.t -> unit
(** Merge the given signature with the context's one *)

val find_signature : t -> Name.t -> Type.t option
(** Find the type of the given symbol *)

val find_signature_exn : t -> Name.t -> Type.t
(** Unsafe version of {!find_signature}.
    @raise Not_found for unknown symbols *)

val declare : t -> Name.t -> Type.t -> unit
(** Declare the type of a symbol (updates signature) *)

val declare_syms : t -> (Name.t * Type.t) list -> unit
(** Declare multiple symbols (more efficient that calling declare function
    incrementally) *)

val on_new_symbol : t -> (Name.t * Type.t) Signal.t
val on_signature_update : t -> Signature.t Signal.t
val set_injective_for_arg : t -> Name.t -> int -> unit
val is_injective_for_arg : t -> Name.t -> int -> bool

(** {2 Literals conversion} *)

val lit_of_form : t -> Term.t SLiteral.t -> Literal.t
(** @raise Invalid_argument if the formula is not atomic *)

val lit_to_form : t -> Literal.t -> Term.t SLiteral.t
val add_lit_from_hook : t -> Literal.Conv.hook_from -> unit
val add_lit_to_hook : t -> Literal.Conv.hook_to -> unit

module Lit : sig
  val from_hooks : unit -> Literal.Conv.hook_from list
  val add_from_hook : Literal.Conv.hook_from -> unit
  val to_hooks : unit -> Literal.Conv.hook_to list
  val add_to_hook : Literal.Conv.hook_to -> unit
  val of_form : Term.t SLiteral.t -> Literal.t
  val to_form : Literal.t -> Term.t SLiteral.t
end

module Key : sig
  val lost_completeness : bool Flex_state.key
end
