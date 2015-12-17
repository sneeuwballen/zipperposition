
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {6 Formulas with Proofs} *)

open Logtk

type form = TypedSTerm.t

type t

val proof : t -> Proof.t
val form : t -> form
val id : t -> int
val is_conjecture : t -> bool

val eq : t -> t -> bool
val cmp : t -> t -> int
include Interfaces.HASH with type t := t

val eq_noproof : t -> t -> bool
val cmp_noproof : t -> t -> int
(** Compare only by formula, not by proof *)

val create : ?is_conjecture:bool -> ?follow:bool ->
  form -> Proof.t -> t
(** Create a formula from a proof. If the formula already has a proof,
    then the old proof is kept. PFormulas are hashconsed.
    @param follow follow simpl_to links if the formula has any (default false)
    @param is_conjecture is the formula a goal? (default [false]) *)

val of_sourced : ?role:string -> TypedSTerm.t Sourced.t -> t
val to_sourced : t -> TypedSTerm.t Sourced.t option

val follow_simpl : t -> t
(** Follow the "simplify to" links until the formula has None *)

val simpl_to : from:t -> into:t -> unit
(** [simpl_to ~from ~into] sets the link of [from] to [into], so that
    the simplification of [from] into [into] is cached. *)

val symbols : ?init:ID.Set.t -> t -> ID.Set.t

val pp : t CCFormat.printer
val pp_tstp : t CCFormat.printer
val to_string : t -> string

(** {2 Set of formulas} *)

(** PFormulas are compared by their formulas, not their proofs. A set
    can contain at most one proof for a given formula. *)

module Set : sig
  include CCSet.S with type elt = t

  val symbols : ?init:ID.Set.t -> t -> ID.Set.t
end

