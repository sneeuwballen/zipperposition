
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

(** {2 Classification} *)

(** Some clauses are Horn, some are unit equations, some are unit,
    and the others are general *)

type kind =
  | Unit_atom of Lit.t
  | Horn of Horn.t
  | General of General.t
(* | Unit_eq of Lit.  *) (* TODO *)


val classify : t -> kind



