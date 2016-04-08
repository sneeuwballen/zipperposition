
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Manipulate proofs} *)

open Libzipperposition

module Loc = ParseLocation

type form = TypedSTerm.t
type 'a sequence = ('a -> unit) -> unit

val section : Util.Section.t

type rule_info =
  | I_subst of Substs.t
  | I_pos of Position.t
  | I_comment of string

type rule = {
  rule_name: string;
  rule_info: rule_info list;
}

val mk_rule :
  ?subst:Substs.t list ->
  ?pos:Position.t list ->
  ?comment:string list ->
  string ->
  rule

(** Classification of proof steps *)
type kind =
  | Inference of rule
  | Simplification of rule
  | Esa of rule
  | Assert of StatementSrc.t
  | Goal of StatementSrc.t
  | Data of StatementSrc.t * Type.t Statement.data
  | Trivial (** trivial, or trivial within theories *)

type 'clause result =
  | Form of form
  | Clause of 'clause

(** A proof step, without the conclusion *)
type +'a t = private {
  id: int; (* unique ID *)
  kind: kind;
  dist_to_goal: int option; (* distance to goal *)
  parents: 'a of_ list;
}

(** Proof Step with its conclusion *)
and +'a of_ = {
  step: 'a t;
  result : 'a result
}

type 'a proof = 'a of_

val result : 'c of_ -> 'c result
val step : 'c of_ -> 'c t
val kind : _ t -> kind
val parents : 'c t -> 'c of_ list

val result_as_clause : 'c of_ -> 'c
val result_as_form : _ of_ -> form

(** {2 Constructors and utils}
    In all the following constructors, [theories] defaults to the empty list.
    Axiom constructors have default role "axiom" *)

val mk_trivial : 'c t

val mk_data : StatementSrc.t -> Type.t Statement.data -> _ t

val mk_assert : StatementSrc.t -> 'c t

val mk_goal : StatementSrc.t -> 'c t

val mk_assert' : ?loc:Loc.t -> file:string -> name:string -> unit -> 'c t

val mk_goal' : ?loc:Loc.t -> file:string -> name:string -> unit -> 'c t

val mk_inference : rule:rule -> 'c of_ list -> 'c t

val mk_simp : rule:rule -> 'c of_ list -> 'c t

val mk_esa : rule:rule -> 'c of_ list -> 'c t

val mk_f_trivial : form -> _ of_

val mk_f_inference : rule:rule -> form -> 'c of_ list -> 'c of_

val mk_f_simp : rule:rule -> form -> 'c of_ list -> 'c of_

val mk_f_esa : rule:rule -> form -> 'c of_ list -> 'c of_

val mk_c : 'c t -> 'c -> 'c of_

val adapt_f : 'c of_ -> form -> 'c of_
val adapt_c : 'c of_ -> 'c -> 'c of_

val is_trivial : _ t -> bool

val is_assert : _ t -> bool
(** Proof: the statement was asserted in some file *)

val is_goal : _ t -> bool
(** The statement comes from the negation of a goal in some file *)

val rule : _ t -> rule option
(** Rule name for Esa/Simplification/Inference steps *)

val equal : _ t -> _ t -> bool
val hash : _ t -> int
val compare : _ t -> _ t -> int

(** {2 Proof traversal} *)

module Tbl : CCHashtbl.S with type key = int

val traverse : ?traversed:unit Tbl.t -> 'c t -> 'c t sequence
(** Traverse the proof. Each proof node is traversed only once,
    using the set to recognize already traversed proofs. *)

val traverse_depth : ?traversed:unit Tbl.t -> 'c t -> ('c t * int) sequence
(** Traverse the proof, yielding each proof node along with its
    depth from the initial proof. Each proof node is traversed only once,
    using the set to recognize already traversed proofs. *)

val distance_to_goal : 'c t -> int option
(** [distance_to_conjecture p] returns [None] if [p] has no ancestor
    that is a conjecture (including [p] itself). It returns [Some d]
    if [d] is the distance, in the proof graph, to the closest
    conjecture ancestor of [p] *)

val to_seq : 'c t -> 'c t Sequence.t
(** Traverse the subproofs, once each *)

val depth : _ t -> int
(** Max depth of the proof *)

(** {2 IO} *)

val pp_rule : info:bool -> rule CCFormat.printer

val pp_kind : kind CCFormat.printer
val pp_kind_tstp : kind CCFormat.printer

