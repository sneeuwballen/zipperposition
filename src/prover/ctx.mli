
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Basic context for literals, clauses...} *)

open Logtk

(** {2 Context for a Proof} *)
module type S = Ctx_intf.S

module Key : sig
  val lost_completeness : bool Flex_state.key
end

module type PARAMETERS = sig
  val signature : Signature.t
  val ord : Ordering.t
  val select : Selection.t
end

(** {2 Create a new context} *)
module Make(X : PARAMETERS) : S
