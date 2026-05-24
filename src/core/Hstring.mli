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
val pp : t CCFormat.printer
