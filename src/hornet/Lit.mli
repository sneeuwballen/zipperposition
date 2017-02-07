
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Literal} *)

(** Literals occurring in clauses *)

open Libzipperposition

type ty = Type.t
type term = FOTerm.t

type t = Hornet_types.lit =
  | Bool of bool
  | Atom of term * bool
  | Eq of term * term * bool

type lit = t

val true_ : t
val false_: t
val bool : bool -> t
val atom : ?sign:bool -> term -> t
val eq : ?sign:bool -> term -> term -> t

val sign : t -> bool

val neg : t -> t
(** negate literal *)

val map : (term -> term) -> t -> t
(** functor *)

include Interfaces.EQ with type t := t
include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t
include Interfaces.PRINT with type t := t

(** {2 Helpers} *)

val vars_seq : t -> ty HVar.t Sequence.t
val vars_list : t -> ty HVar.t list
val vars_set : t -> ty HVar.t list (** unique *)

val is_ground : t -> bool

val weight : t -> int

val hash_mod_alpha : t -> int

val is_trivial : t -> bool

(** {2 Containers} *)

module Set : CCSet.S with type elt = t

(** {2 Positions} *)

module With_pos : sig
  type t = lit Position.With.t

  include Interfaces.PRINT with type t := t
  include Interfaces.ORD with type t := t
end

val direction : Ordering.t -> t -> Comparison.t option

val at_pos_exn : Position.t -> t -> term
(** Get the term at the given pos
    @raise Not_found if the position is not valid or if it
    empty (would return the lit itself) *)

val active_terms : Ordering.t -> t -> term Position.With.t Sequence.t
(** Terms in active position for paramodulation/resolution *)

val passive_terms : Ordering.t -> t -> term Position.With.t Sequence.t
(** Terms in passive position for paramodulation/resolution *)

(** {2 Unification and Matching} *)

val variant :
  ?subst:Subst.t ->
  t Scoped.t ->
  t Scoped.t ->
  Subst.t Sequence.t

val are_variant : t -> t -> bool

val subsumes :
  ?subst:Subst.t ->
  t Scoped.t ->
  t Scoped.t ->
  Subst.t Sequence.t

val unify :
  ?subst:Subst.t ->
  t Scoped.t ->
  t Scoped.t ->
  Subst.t Sequence.t

val matching :
  ?subst:Subst.t ->
  pattern:t Scoped.t ->
  t Scoped.t ->
  Subst.t Sequence.t

val apply_subst :
  renaming:Subst.Renaming.t ->
  Subst.t ->
  t Scoped.t ->
  t

val apply_subst_no_renaming : Subst.t -> t Scoped.t -> t

val apply_subst_no_simp :
  renaming:Subst.Renaming.t ->
  Subst.t ->
  t Scoped.t ->
  t
