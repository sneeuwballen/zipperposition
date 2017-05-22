
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Classify Literals} *)

type shielded = [`Shielded | `Unshielded]
type sign = bool

type constraint_ =
  | C_arith (* arith negative equality, seen as constraint *)
  | C_ho (* F args != t *)
  | C_purify (* X != t, X of purifiable type *)

type class_ =
  | K_normal of sign (* any "normal" kind of literal *)
  | K_constr of constraint_ * shielded (* constraint *)

val is_unshielded_constr : class_ -> bool
(** Constraint on unshielded variable? *)

type t = class_ array

val pp_class : class_ CCFormat.printer
val pp : t CCFormat.printer

val classify : Literals.t -> t
(** [classify lits] gives a classification of the literals in this array,
    relative to other literals *)
