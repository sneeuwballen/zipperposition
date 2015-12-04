
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Rewriting on HO terms} *)

type term = HOTerm.t
type rule = term * term

exception IllFormedRule of rule

(** {2 Building rewrite systems} *)

type t
(** rewrite system *)

val empty : t
(** No rules *)

val add : t -> rule -> t
(** Add a rule. A rule must satisfy several conditions:
    - every free variable on the RHS must occur in the LHS
    - every free variable on the RHS must not occur under
      any binder (would cause problems with De Bruijn indices)
    @raise IllFormedRule if the rule isn't valid. *)

val merge : t -> t -> t
(** Merge two rewrite systems *)

module Seq : sig
  val of_seq : t -> rule Sequence.t -> t
  val to_seq : t -> rule Sequence.t
end

val of_list : rule list -> t
val to_list : t -> rule list

include Interfaces.PRINT with type t := t
include Interfaces.ORD with type t := t
include Interfaces.HASH with type t := t

(** {2 Normalizing terms} *)

val normalize : t -> term -> term
(** Normalize  the term w.r.t to the rewrite system *)

val normalize_collect : t -> term -> term * rule list
(** Normalize the term, and returns a list of rules used to normalize it. *)
