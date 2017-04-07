
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

module type S = sig
  type t

  type payload
  (** Additional data carried in the literal *)

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  val dummy : t
  (** Value that should not be used *)

  val neg : t -> t
  (** Negate the boolean literal *)

  val sign : t -> bool
  (** Current sign of the literal (positive or negative) *)

  val abs : t -> t
  (** Literal without its sign *)

  val norm : t -> t * bool
  (** [norm l = abs l, not (sign l)] *)

  val set_sign : bool -> t -> t
  (** Set the sign of the literal to the given boolean *)

  val apply_sign : bool -> t -> t
  (** [apply_sign s lit] is [lit] if [s], [neg lit] otherwise *)

  val make : payload -> t
  (** Make a fresh literal with the given payload *)

  val payload : t -> payload
  (** Obtain the payload *)

  val to_int : t -> int

  val fresh_id : unit -> int
  (** Make a fresh ID. Use with care. *)

  val pp : t CCFormat.printer

  module Set : CCSet.S with type elt = t
  module Tbl : CCHashtbl.S with type key = t
end
