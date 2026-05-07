(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Basic context for literals, clauses...} *)

open Logtk

module type S = Ctx_intf.S
(** {2 Context for a Proof} *)

module Key : sig
  val lost_completeness : bool Flex_state.key
end

module type PARAMETERS = sig
  val signature : Signature.t
  val ord : Ordering.t
  val select : Selection.t
  val bool_select : Bool_selection.t
  val sk_ctx : Skolem.ctx
end

(** {2 Create a new context} *)
module Make (X : PARAMETERS) : S
