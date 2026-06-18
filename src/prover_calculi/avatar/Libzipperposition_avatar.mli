(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Basic Splitting à la Avatar}

    We don't implement all the stuff from Avatar, in particular all clauses are
    active whether or not their trail is satisfied in the current model. Trails
    are only used to make splits easier {b currently}.

    Future work may include locking clauses whose trails are unsatisfied.

    Depends optionally on the "meta" extension. *)

open Logtk
open Libzipperposition
module UnionFind = UnionFind

type 'a printer = Format.formatter -> 'a -> unit

(** {2 Avatar: splitting+sat} *)

val flag_cut_introduced : SClause.flag

module Solver : Sat_solver_intf.STATIC

module type S = Avatar_intf.S

val k_simplify_trail : bool Flex_state.key
val k_avatar_enabled : bool Flex_state.key
val split : Env.multi_simpl_rule
val check_empty : Env.unary_inf_rule
val before_check_sat : unit Signal.t
val after_check_sat : unit Signal.t
val filter_absurd_trails : (Trail.t -> bool) -> unit
val check_satisfiability : Env.generate_rule

type cut_res = private {
  cut_form: Cut_form.t;
  cut_pos: Clause.t list;
  cut_lit: BBox.Lit.t;
  cut_depth: int;
  cut_proof: Proof.Step.t;
  cut_proof_parent: Proof.Parent.t;
  cut_reason: unit CCFormat.printer option;
}

val cut_form : cut_res -> Cut_form.t
val cut_pos : cut_res -> Clause.t list
val cut_lit : cut_res -> BBox.Lit.t
val cut_depth : cut_res -> int
val cut_proof : cut_res -> Proof.Step.t
val cut_proof_parent : cut_res -> Proof.Parent.t
val pp_cut_res : cut_res CCFormat.printer
val cut_res_clauses : cut_res -> Clause.t Iter.t
val print_lemmas : unit CCFormat.printer

val introduce_cut :
  ?reason:unit CCFormat.printer ->
  ?penalty:int ->
  ?depth:int ->
  Cut_form.t ->
  Proof.Step.t ->
  cut_res

val add_prove_lemma : (cut_res -> Clause.t list Env.conversion_result) -> unit
val add_lemma : cut_res -> unit
val add_imply : cut_res list -> cut_res -> Proof.Step.t -> unit
val on_input_lemma : cut_res Signal.t
val on_lemma : cut_res Signal.t
val convert_lemma : Env.clause_conversion_rule
val register : split_kind:[< `Eager | `Lazy | `Off > `Off ] -> unit -> unit
val extension : Extensions.t
