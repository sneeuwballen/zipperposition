
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Rewrite Rules} *)

open Logtk

val section : Util.Section.t

type rule

val lhs : rule -> Literal.t
val rhs : rule -> Literal.t list list
val pp_rule : rule CCFormat.printer

(** {6 Set of Rewrite Rules} *)
module Set : sig
  type t

  val empty : t

  val is_empty : t -> bool

  val add_stmt : Statement.clause_t -> t -> t
  (** [add_stmt st set] adds rewrite rules from [st] to [set], if any *)

  val to_seq : t -> rule Sequence.t

  val pp : t CCFormat.printer
end

val normalize_clause : Set.t -> Literal.t list -> Literal.t list list option
(** normalize literals of the clause w.r.t. rules, or return [None]
    if no rule applies *)

val narrow_lit :
  ?subst:Subst.t ->
  Set.t Scoped.t ->
  Literal.t Scoped.t ->
  (rule * Subst.t) Sequence.t
(** [narrow_term rules lit] finds the set of rules [(l --> clauses) in rules]
    and substitutions [sigma] such that [sigma(l) = sigma(tit)] *)
