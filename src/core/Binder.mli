
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Binders}

  @since 1.0 *)

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
end

module ZF : sig
  include Interfaces.PRINT with type t := t
end
