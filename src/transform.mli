
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

(** Provides some transformations on formulas, and sets of formulas.
    Transformations include definition expansion and term rewriting *)

type t =
| RwTerm of Rewriting.TRS.t
| RwForm of Rewriting.FormRW.t
| Tr of string * (Formula.t -> Formula.t list)
  (** the function can return a conjunction of formulas. The
      string is a short name/description of the transformation *)

val of_term_rule : (Term.t * Term.t) -> t

val of_term_rules : (Term.t * Term.t) list -> t

val of_term_rules_seq : (Term.t * Term.t) Sequence.t -> t

val of_form_rule : (Term.t * Formula.t) -> t

val of_form_rules : (Term.t * Formula.t) list -> t

val of_form_rules_seq : (Term.t * Formula.t) Sequence.t -> t

val of_term_tr : string -> (Term.t -> Term.t) -> t
  (** Lift a term transformation to the formula level *)

val open_and : t
  (** transformation that opens outermost "and" connectives *)

val apply : t -> Formula.t -> Formula.t list
  (** Apply the transformations to a formula *)

val apply_set : t -> Formula.FSet.t -> Formula.FSet.t
  (** Apply the transformations to a set of formulas *)

val fix : t list -> Formula.FSet.t -> Formula.FSet.t
  (** Apply the given transformations to the set until a fixpoint
      is reached. *)

val pp : Buffer.t -> t -> unit
val fmt : Format.formatter -> t -> unit
