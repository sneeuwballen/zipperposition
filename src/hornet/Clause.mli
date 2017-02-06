
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 General representation of Clauses} *)

open Libzipperposition

type t
type clause = t

type proof = private
  | P_from_stmt of Statement.clause_t
  | P_instance of clause * Subst.t
  | P_avatar_split of clause (* split into var-disjoint components *)
  | P_split of clause (* model-driven recursive splitting *)

(* constraint on the literals *)
type constraint_ =
  | Dismatch of Dismatching_constr.t

val make : ?constrs:constraint_ list -> Lit.t IArray.t -> proof -> t
val make_l : ?constrs:constraint_ list -> Lit.t list -> proof -> t

val proof : t -> proof
val lits : t -> Lit.t IArray.t

val is_empty : t -> bool

include Interfaces.EQ with type t := t
include Interfaces.HASH with type t := t
include Interfaces.PRINT with type t := t

val hash_mod_alpha : t -> int

(** {2 Horn Clauses} *)

module Horn : sig
  type t = private clause
  val as_clause : t -> clause

  val concl : t -> Lit.t

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
end

(** {2 Unit Clauses} *)
module Unit : sig
  type t = private clause

  val get: t -> Lit.t

  val pp : t CCFormat.printer
end

(** {2 General Clause} *)

(** Such clauses are not Horn nor unit. They have at least two positive
    literals or 0 positive literals. *)

module General : sig
  type t = private clause
  val as_clause : t -> clause

  type idx = private int
  (** The index of a literal in the clause *)

  val lits_seq : t -> (idx * Lit.t) Sequence.t

  val pp : t CCFormat.printer
end

(** {2 Proof} *)

(** Each clause contains its own proof, that is, a derivation from
    axioms through instanciations/resolution/… *)

module Proof : sig
  type t = proof

  val from_stmt : Statement.clause_t -> t
  val instance : clause -> Subst.t -> t
  val avatar_split : clause -> t
  val split : clause -> t

  include Interfaces.PRINT with type t := t
end

(** {2 Pairing with Position} *)

module With_pos : sig
  type t = clause Position.With.t
  include Interfaces.ORD with type t := t
  include Interfaces.PRINT with type t := t
end

(** {2 Classification} *)

(** Some clauses are Horn, some are unit equations, some are unit,
    and the others are general *)

type kind =
  | Unit_atom of Unit.t
  | Horn of Horn.t
  | General of General.t
(* | Unit_eq of Lit.  *) (* TODO *)


val classify : t -> kind

val is_unit_ground : t -> bool

(** {2 Utils} *)

val of_slit_l :
  stmt:Statement.clause_t ->
  FOTerm.t SLiteral.t list ->
  t
(** Conversion from some clause in the given statement *)

val is_trivial : t -> bool
(** Is the clause trivial? *)

(** {2 Unif} *)

val variant :
  ?subst:Subst.t ->
  t Scoped.t ->
  t Scoped.t ->
  Subst.t Sequence.t

val equal_mod_alpha : t -> t -> bool

module Tbl_mod_alpha : CCHashtbl.S with type key = clause
