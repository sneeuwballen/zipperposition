
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Rewriting}

    Deal with definitions as rewrite rules *)

open Libzipperposition

(** {2 Rewrite Rule} *)

type rule

val pp_rule : rule CCFormat.printer

(** {2 Set of Rewrite Rules} *)
module RuleSet : sig
  type t
  val empty : t

  val add : rule -> t -> t
  val add_list : rule list -> t -> t

  val add_stmt : (_, FOTerm.t, Type.t, _) Statement.t -> t -> t
  (** [add_stmt st set] adds rewrite rules from [st] to [set], if any *)

  val to_seq : t -> rule Sequence.t
  val to_list : t -> rule list

  val pp : t CCFormat.printer
end

val normalize : RuleSet.t -> FOTerm.t -> FOTerm.t
(** [normalize rules t] computes the normal form of [t] w.r.t the set
    of rewrite rules *)

(** {2 Pre-processing Rules} *)

val extension : Extensions.t

module Key : sig
  val set: RuleSet.t Flex_state.key
  (** After CNF, a set of rewrite rules is available in the flex state *)
end
