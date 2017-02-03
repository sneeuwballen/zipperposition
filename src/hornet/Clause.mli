
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 General representation of Clauses} *)

open Libzipperposition

type t
type clause = t

val make : Lit.t IArray.t -> t

include Interfaces.EQ with type t := t
include Interfaces.HASH with type t := t
include Interfaces.PRINT with type t := t

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

  val body_get : t -> int -> Lit.t
  (** Get the [n]-th body literal.
      @raise Invariant if [n] is not within [0... body_len c - 1] *)

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

(** {2 With Position} *)

module With_pos : sig
  type t = private {
    clause: clause;
    pos: Position.t;
  }
  val clause : t -> clause
  val pos : t -> Position.t

  val make : clause -> Position.t -> t

  include Interfaces.PRINT with type t := t
  include Interfaces.HASH with type t := t
  include Interfaces.ORD with type t := t
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



