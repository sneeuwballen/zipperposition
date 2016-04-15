
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Manipulate proofs} *)

open Libzipperposition

module Loc = ParseLocation

type form = TypedSTerm.t
type bool_lit = BBox.Lit.t
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

type result =
  | Form of form
  | Clause of SClause.t
  | BoolClause of bool_lit list

(** A proof step, without the conclusion *)
type t = private {
  id: int; (* unique ID *)
  kind: kind;
  dist_to_goal: int option; (* distance to goal *)
  parents: of_ list;
}

(** Proof Step with its conclusion *)
and of_ = {
  step: t;
  result : result
}

type proof = of_

val result : of_ -> result
val step : of_ -> t
val kind : t -> kind
val parents : t -> of_ list

val result_as_clause : of_ -> SClause.t
val result_as_form : of_ -> form

(** {2 Constructors and utils}
    In all the following constructors, [theories] defaults to the empty list.
    Axiom constructors have default role "axiom" *)

val mk_trivial : t

val mk_data : StatementSrc.t -> Type.t Statement.data -> t

val mk_assert : StatementSrc.t -> t

val mk_goal : StatementSrc.t -> t

val mk_assert' : ?loc:Loc.t -> file:string -> name:string -> unit -> t

val mk_goal' : ?loc:Loc.t -> file:string -> name:string -> unit -> t

val mk_inference : rule:rule -> of_ list -> t

val mk_simp : rule:rule -> of_ list -> t

val mk_esa : rule:rule -> of_ list -> t

val mk_f_trivial : form -> of_

val mk_f_inference : rule:rule -> form -> of_ list -> of_

val mk_f_simp : rule:rule -> form -> of_ list -> of_

val mk_f_esa : rule:rule -> form -> of_ list -> of_

val mk_c : t -> SClause.t -> of_

val mk_bc : t -> bool_lit list -> of_

val adapt_f : of_ -> form -> of_
val adapt_c : of_ -> SClause.t -> of_

val is_trivial : t -> bool

val is_assert : t -> bool
(** Proof: the statement was asserted in some file *)

val is_goal : t -> bool
(** The statement comes from the negation of a goal in some file *)

val rule : t -> rule option
(** Rule name for Esa/Simplification/Inference steps *)

val equal : t -> t -> bool
val hash : t -> int
val compare : t -> t -> int

(** {2 Proof traversal} *)

module Tbl : CCHashtbl.S with type key = int

val traverse : ?traversed:unit Tbl.t -> t -> t sequence
(** Traverse the proof. Each proof node is traversed only once,
    using the set to recognize already traversed proofs. *)

val traverse_depth : ?traversed:unit Tbl.t -> t -> (t * int) sequence
(** Traverse the proof, yielding each proof node along with its
    depth from the initial proof. Each proof node is traversed only once,
    using the set to recognize already traversed proofs. *)

val distance_to_goal : t -> int option
(** [distance_to_conjecture p] returns [None] if [p] has no ancestor
    that is a conjecture (including [p] itself). It returns [Some d]
    if [d] is the distance, in the proof graph, to the closest
    conjecture ancestor of [p] *)

val to_seq : t -> t Sequence.t
(** Traverse the subproofs, once each *)

val depth : t -> int
(** Max depth of the proof *)

(** {2 IO} *)

val pp_rule : info:bool -> rule CCFormat.printer

val pp_kind : kind CCFormat.printer
val pp_kind_tstp : kind CCFormat.printer

