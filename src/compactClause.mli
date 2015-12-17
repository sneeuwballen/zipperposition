
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Compact clause representation} *)

open Logtk

type term = FOTerm.t

(* TODO: simplfy, only one case now *)
type bool_lit =
  bool *
  [ `Box_clause of Literal.t array
  | `Qbf_artifact of Qbf.Lit.t * string
  ]
(** A boolean literal, here, is a boxed (unsplittable) clause
    with a sign. The literal can be an explicit encoding of "lits are true"
    or some other QBF artifact (lit number + repr) *)

type t = {
  lits : Literal.t array;
  trail : bool_lit list;
}

val cmp : t -> t -> int
include Interfaces.HASH with type t := t

val compare : t -> t -> int

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
