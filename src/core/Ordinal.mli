
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Syntactic Ordinals} *)

(** Ordinals as defined in "Transfinite KBO", up to ε₀.
    Some basic operations are defined.

    An ordinal is either 0 or a finite sum [Σ_i nᵢ . ω^bᵢ] where each [bᵢ]
    is itself an ordinal.

    NOTE: experimental module; also performance might not be great.
*)

type t = private
  | Zero
  | Sum of (int * t) list

val zero : t

val const : int -> t

val mult_const : int -> t -> t

val of_list : (int * t) list -> t

val add : t -> t -> t

val mult : t -> t -> t

val equal : t -> t -> bool

val compare : t -> t -> int

val pp : t CCFormat.printer

val to_string : t -> string


(**/**)
val check_inv_ : t -> bool
(**/**)
