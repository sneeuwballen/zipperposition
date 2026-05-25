(** Hashconsed string.

    Globally unique, fast equality and hash. To be used for constants, etc. *)

type t = private {
  str: string;
  mutable id: int;
  h: int;
}

val make : string -> t
(** intern the string *)

val makef : ('a, Format.formatter, unit, t) format4 -> 'a
val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int
val to_string : t -> string

module Map : CCMap.S with type key = t
module Set : CCSet.S with type elt = t
module Tbl : CCHashtbl.S with type key = t
