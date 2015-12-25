
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Boolean Literal} *)

type t = private int

include Libzipperposition.Interfaces.ORD with type t := t
include Libzipperposition.Interfaces.HASH with type t := t

val neg : t -> t
(** Negate the boolean literal *)

val sign : t -> bool
(** Current sign of the literal (positive or negative) *)

val abs : t -> t
(** Literal without its sign *)

val set_sign : bool -> t -> t
(** Set the sign of the literal to the given boolean *)

val apply_sign : bool -> t -> t
(** [apply_sign s lit] is [lit] if [s], [neg lit] otherwise *)

val make : int -> t

val pp : t CCFormat.printer

module Set : CCSet.S with type elt = t
