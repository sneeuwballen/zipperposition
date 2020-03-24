
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Binders for Logic} *)

(** The classic binders for the internal AST and various term representations.

    @since 1.5 *)

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
