
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {6 Transformations on Formulas and Terms} *)

type term = FOTerm.t
type form = Formula.FO.t

(** Provides some transformations on formulas, and sets of formulas.
    Transformations include definition expansion and term rewriting *)

type t =
| RwTerm of Rewriting.TRS.t
| RwForm of Rewriting.FormRW.t
| Tr of string * (form -> form list)
  (** the function can return a conjunction of formulas. The
      string is a short name/description of the transformation *)

type transformation = t

val of_term_rule : (term * term) -> t

val of_term_rules : (term * term) list -> t

val of_term_rules_seq : (term * term) Sequence.t -> t

val of_form_rule : (term * form) -> t

val of_form_rules : (term * form) list -> t

val of_form_rules_seq : (term * form) Sequence.t -> t

val of_term_tr : string -> (term -> term) -> t
  (** Lift a term transformation to the formula level *)

val open_and : t
  (** transformation that opens outermost "and" connectives *)

val remove_trivial : t
  (** Tranformation that removes trivial formulas *)

val apply : t -> form -> form list
  (** Apply the transformations to a formula *)

include Interfaces.PRINT with type t := t

(** {2 Transformation DAG} *)

(** Abstraction over formulas with additional information. A FORM.t
    contains a formula, and is built from parent formula wrappers
    upon a transformation.
*)

module type FORM = sig
  type t

  val of_form : rule:string -> parents:t list -> form -> t

  val to_form : t -> form
end

(** This module provides an infrastructure to efficiently compute
    the fixpoint of a set of transformations on a set of formulas.
    Formulas form a DAG, whose edges go from a formula to the formulas it
    transforms into; result set is the set of leaves reachable from the
    initial formulas.
*)

module type DAG = sig
  module Form : FORM

  type t

  val create : (string * transformation) list -> t
    (** Create a DAG that implements the given list of transformations *)

  val transform : t -> Form.t list -> Form.t list
    (** Transform a set of formulas recursively *)
end

(** Build a DAG *)
module MakeDAG(Form : FORM) : DAG with module Form = Form

(** Trivial instance, with bare formulas *)
module FormDag : DAG with type Form.t = form
