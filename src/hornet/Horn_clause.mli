
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Horn Clause} *)

(** These clauses are typically used for non-ground reasoning based on
    Horn-Superposition (with a bound on termination-threatening inferences).

    We consider that a negative clause, [not p1 ∨ … ∨ not pn],
    is actually the Horn clause [p1 ∧ … ∧ pn => ⊥].
*)

open Libzipperposition
open Hornet_types

type t = horn_clause
type horn_clause = t

(** {2 Basics} *)

val make :
  trail:bool_trail ->
  constr:c_constraint ->
  unordered_depth:int ->
  label:label ->
  Lit.t ->
  Lit.t IArray.t ->
  proof ->
  t
(** Make a Horn Clause *)

val head : t -> Lit.t

val body : t -> Lit.t IArray.t

val constr : t -> c_constraint
(** The constraints attached to this clause *)

val trail : t -> bool_trail

val label : t -> label

val proof : t -> proof
(** Proof of the clause *)

val unordered_depth : t -> int
(** The number of unordered inference steps required to prove this clause *)

val status : t -> horn_clause_status * int
(** status + cycle count *)

val set_status : t -> horn_clause_status -> int -> unit
(** change the status
    @raise Util.Error if the change is not following the order
      [Dead (n-1) -> Alive n -> Dead n] *)

val body_seq : t -> Lit.t Sequence.t
(** Sequence of body elements *)

val body_l : t -> Lit.t list

val body_len : t -> int
(** Number of literals in the body.
    Invariant: always > 0 *)

val body0 : t -> Lit.t option
(** Get the first body literal *)

val body0_exn : t -> Lit.t
(** Get the first body literal
    @raise Util.Error if the body is empty *)

val body_get : t -> int -> Lit.t
(** Get the [n]-th body literal.
    @raise Util.Error if [n] is not within [0... body_len c - 1] *)

val body_tail : t -> Lit.t IArray.t
(** All the body except literal 0.
    @raise Util.Error if the body is empty *)

val head_pos : t -> Lit.t Position.With.t

val body0_pos : t -> Lit.t Position.With.t

val body_pos : int -> t -> Lit.t Position.With.t

(** {2 Helpers} *)

val is_ground : t -> bool

val is_trivial : t -> bool

val is_absurd : t -> bool

val is_unit_pos : t -> bool

(** {2 Life Cycle} *)

(** A given clause can be alive, then dead, then alive again, … as many
    times as needed (typically because it comes from the splitting of
    a full clause, and depends on the boolean model).

    To become alive again after being dead, the "cycle" counter needs
    to be increased, signalling that the boolean model might have changed. *)

val current_cycle : unit -> int

val start_new_cycle : unit -> unit

val make_alive_again : t -> unit
(** The clause is dead or alive with an old cycle; we make it alive again
    because the boolean trail has changed *)

val is_alive : t -> bool

val is_dead : t -> bool

val kill : t -> unit

(** {2 Unification} *)

val variant :
  ?subst:Subst.t ->
  t Scoped.t ->
  t Scoped.t ->
  Subst.t Sequence.t

val hash_mod_alpha : t -> int

val equal_mod_alpha : t -> t -> bool

(** {2 Containers} *)

include Interfaces.PRINT with type t := t
include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t

module Tbl : CCHashtbl.S with type key = t
module Set : CCSet.S with type elt = t

module Tbl_mod_alpha : CCHashtbl.S with type key = t
(** table that uses {!equal_mod_alpha} and {!hash_mod_alpha} *)

(** {2 Pairing with Position} *)

module With_pos : sig
  type t = horn_clause Position.With.t
  include Interfaces.ORD with type t := t
  include Interfaces.PRINT with type t := t
end
