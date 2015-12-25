
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Compact clause representation} *)

open Libzipperposition

type term = FOTerm.t

type bool_lit = bool * Literal.t array
(** A boolean literal, here, is a boxed (unsplittable) clause
    with a sign. *)

type t = {
  lits : Literal.t array;
  trail : bool_lit list;
}

val compare : t -> t -> int
include Interfaces.HASH with type t := t

val make : Literal.t array -> bool_lit list -> t
(** Make a compact clause *)

val is_empty : t -> bool
val has_absurd_lits : t -> bool

val iter : t -> (Literal.t -> unit) -> unit

val to_seq : t -> Literal.t Sequence.t

val to_forms : t -> term SLiteral.t array

val lits : t -> Literal.t array
val trail : t -> bool_lit list

val pp : t CCFormat.printer
val pp_tstp : t CCFormat.printer
val pp_trail_tstp : bool_lit list CCFormat.printer

val to_string : t -> string
