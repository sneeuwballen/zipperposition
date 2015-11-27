
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Binders}

  @since NEXT_RELEASE *)

type t =
  | Exists
  | Forall
  | ForallTy
  | Lambda

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t
include Interfaces.PRINT with type t := t

val exists : t
val forall : t
val lambda : t
val forall_ty : t

module TPTP : sig
  include Interfaces.PRINT with type t := t
  (** printer for TPTP *)
end
