
(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

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
