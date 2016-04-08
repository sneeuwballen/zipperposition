
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Rewrite Rules} *)

open Libzipperposition

val section : Util.Section.t

type rule_term
val pp_rule_term : rule_term CCFormat.printer

type rule_clause
val pp_rule_clause : rule_clause CCFormat.printer

(** {6 Set of Rewrite Rules} *)
module Set : sig
  type t

  val empty : t

  val is_empty : t -> bool

  val add_stmt : Statement.clause_t -> t -> t
  (** [add_stmt st set] adds rewrite rules from [st] to [set], if any *)

  val pp : t CCFormat.printer
end

val normalize_term : Set.t -> FOTerm.t -> FOTerm.t
(** [normalize rules t] computes the normal form of [t] w.r.t the set
    of rewrite rules *)

val normalize_clause : Set.t -> Literal.t list -> Literal.t list list option
(** normalize literals of the clause w.r.t. rules, or return [None]
    if no rule applies *)
