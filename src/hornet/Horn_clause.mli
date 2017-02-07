
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Horn Clause} *)

(** These clauses are typically used for non-ground reasoning based on
    Horn-Superposition (with a bound on termination-threatening inferences).

    We consider that a negative clause, [not p1 ∨ … ∨ not pn],
    is actually the Horn clause [p1 ∧ … ∧ pn => ⊥].
*)

open Libzipperposition

type clause = Hornet_types.clause
type proof = Hornet_types.proof
type constraint_ = Hornet_types.c_constraint_

type t = Hornet_types.horn_clause
type horn_clause = t

val make :
  ?constr:constraint_ list ->
  Lit.t ->
  Lit.t IArray.t ->
  proof ->
  t
(** Make a Horn Clause *)

val head : t -> Lit.t

val body : t -> Lit.t IArray.t

val proof : t -> proof

val body_seq : t -> Lit.t Sequence.t
(** Sequence of body elements *)

val body_l : t -> Lit.t list

val body_len : t -> int
(** Number of literals in the body.
    Invariant: always > 0 *)

val body1 : t -> Lit.t
(** Get the first body literal *)

val body_get : t -> int -> Lit.t
(** Get the [n]-th body literal.
    @raise Invariant if [n] is not within [0... body_len c - 1] *)

val concl_pos : t -> Lit.t Position.With.t

val body1_pos : t -> Lit.t Position.With.t

val body_pos : int -> t -> Lit.t Position.With.t

val pp : t CCFormat.printer

(** {2 Pairing with Position} *)

module With_pos : sig
  type t = horn_clause Position.With.t
  include Interfaces.ORD with type t := t
  include Interfaces.PRINT with type t := t
end
