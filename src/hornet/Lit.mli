
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Literal} *)

(** Literals occurring in clauses *)

open Logtk

type ty = Type.t
type term = Term.t

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
val is_pos : t -> bool (** alias to {!sign} *)
val is_neg : t -> bool

val neg : t -> t
(** negate literal *)

val map : (term -> term) -> t -> t
(** functor *)

include Interfaces.EQ with type t := t
include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t
include Interfaces.PRINT with type t := t

(** {2 Helpers} *)

val terms : t -> term Sequence.t
val vars_seq : t -> ty HVar.t Sequence.t
val vars_list : t -> ty HVar.t list
val vars_set : t -> ty HVar.t list (** unique *)

val is_ground : t -> bool

val weight : t -> int
val depth : t -> int

val var_occurs : var:ty HVar.t -> t -> bool

val hash_mod_alpha : t -> int

val is_trivial : t -> bool
val is_absurd : t -> bool

val to_slit : t -> term SLiteral.t

(** {2 Containers} *)

module Set : CCSet.S with type elt = t
module Tbl : CCHashtbl.S with type key = t

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

val active_terms : ?pos:Position.t -> Ordering.t -> t -> term Position.With.t Sequence.t
(** Terms in active position for paramodulation/resolution *)

val passive_terms : ?pos:Position.t -> Ordering.t -> t -> term Position.With.t Sequence.t
(** Terms in passive position for paramodulation/resolution *)

val seq_terms : t -> term Sequence.t

module Pos : sig
  val at : t -> Position.t -> term
  (** retrieve subterm at pos
      @raise Invalid_argument if the position is invalid *)

  val replace : t -> at:Position.t -> by:term -> t
  (** [replace t ~at:pos ~by] replaces the subterm at position [pos]
      in [t] by the term [by]. The two terms should have the same type.
      @raise Invalid_argument if the position is not valid *)
end

val get_eqn : t -> Position.t -> (term * term * bool) option
(** View of a Atom or Eq literal, oriented by the position. If the
    position selects its left term, return l, r, otherwise r, l.
    for propositions it will always be p, true.
    @return None for other literals
    @raise Invalid_argument if the position doesn't match the literal. *)

val get_eqn_exn : t -> Position.t -> term * term * bool
(** Same as {!get_eqn}, but
    @raise Error if the literal is not equational *)

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

val subsumes_pred : t -> t -> bool

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

(** {2 Arrays of Lits} *)

val variant_arr :
  ?subst:Subst.t ->
  t IArray.t Scoped.t ->
  t IArray.t Scoped.t ->
  Subst.t Sequence.t

val apply_subst_arr :
  renaming:Subst.Renaming.t ->
  Subst.t ->
  t IArray.t Scoped.t ->
  t IArray.t

val apply_subst_arr_no_renaming :
  Subst.t ->
  t IArray.t Scoped.t ->
  t IArray.t
