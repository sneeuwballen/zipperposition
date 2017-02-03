
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Manipulate proofs} *)

open Libzipperposition

module Loc = ParseLocation

type form = TypedSTerm.t
type bool_lit = BBox.Lit.t
type 'a sequence = ('a -> unit) -> unit

val section : Util.Section.t

type statement_src = Statement.source

type rule_info =
  | I_subst of Subst.t
  | I_pos of Position.t
  | I_comment of string

type rule = {
  rule_name: string;
  rule_info: rule_info list;
}

val mk_rule :
  ?subst:Subst.t list ->
  ?pos:Position.t list ->
  ?comment:string list ->
  string ->
  rule

(** Classification of proof steps *)
type kind =
  | Inference of rule
  | Simplification of rule
  | Esa of rule
  | Assert of statement_src
  | Goal of statement_src
  | Data of statement_src * Type.t Statement.data
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

val compare : t -> t -> int
val hash : t -> int
val equal : t -> t -> bool

val compare_proof : of_ -> of_ -> int
val equal_proof : of_ -> of_ -> bool
val hash_proof : of_ -> int

module PTbl : CCHashtbl.S with type key = of_

(** {2 Constructors and utils}
    In all the following constructors, [theories] defaults to the empty list.
    Axiom constructors have default role "axiom" *)

val mk_trivial : t

val mk_data : statement_src -> Type.t Statement.data -> t

val mk_assert : statement_src -> t

val mk_goal : statement_src -> t

val mk_assert' : ?loc:Loc.t -> file:string -> name:string -> unit -> t

val mk_goal' : ?loc:Loc.t -> file:string -> name:string -> unit -> t

val mk_inference : rule:rule -> of_ list -> t

val mk_simp : rule:rule -> of_ list -> t

val mk_esa : rule:rule -> of_ list -> t

val mk_f : t -> form -> of_

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

val compare_by_result : of_ -> of_ -> int
(** Compare proofs by their result *)

(** {2 Proof traversal} *)

val distance_to_goal : t -> int option
(** [distance_to_conjecture p] returns [None] if [p] has no ancestor
    that is a conjecture (including [p] itself). It returns [Some d]
    if [d] is the distance, in the proof graph, to the closest
    conjecture ancestor of [p] *)

(** {2 IO} *)

val pp_rule : info:bool -> rule CCFormat.printer

val pp_src : statement_src CCFormat.printer
val pp_src_tstp : statement_src CCFormat.printer

val pp_kind : kind CCFormat.printer
val pp_kind_tstp : kind CCFormat.printer

