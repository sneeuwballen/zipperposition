
(*
Zipperposition: a functional superposition prover for prototyping
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

(** {1 Preprocessing Env} *)

open Logtk

(** This module is used for preprocessing problems. It computes the fixpoint
    of operations on a set of {!PFormula.t}. *)

(** {2 Transformations} *)

type operation_result =
  | SimplifyInto of PFormula.t  (** replace by formula *)
  | Remove                      (** remove formula *)
  | Esa of PFormula.t list      (** replace by list of formulas *)
  | Add of PFormula.t list      (** add given formulas, and restart! *)
  | AddOps of operation list    (** New operations to perform, and restart *)

and operation = PFormula.Set.t -> PFormula.t -> operation_result list
  (** An operation can have several results *)

val fix : operation list -> PFormula.Set.t -> PFormula.Set.t
  (** Fixpoint of the given set of operations on the initial set. For a
      clause or formula f, if any operation in the list returns:

      - DoNothing: does nothing
      - Esa l: fixpoint for each element of l
      - Add l: keep f, but also take the fixpoint of l
      - SimplifyInto f': continue with f' instead of f
  *)

val remove_trivial : operation
  (** Remove trivial formulas *)

val cnf : ctx:Skolem.ctx -> operation
  (** Transform clauses into their clausal normal form *)

val meta_prover : meta:MetaProverState.t -> operation
  (** Detect theories in the set, and add lemmas to the set *)

val rw_term : ?rule:string -> premises:PFormula.Set.t ->
              Rewriting.TRS.t -> operation
  (** Rewrite terms in the formula.
      @param premises is the set of formulas that justify why the
        transformtion is correct *)

val rw_form : ?rule:string -> premises:PFormula.Set.t ->
              Rewriting.FormRW.t -> operation
  (** Rewrite formulas into other formulas
      @param premises is the set of formulas that justify why the
        transformtion is correct *)

val fmap_term : rule:string -> (FOTerm.t -> FOTerm.t) -> operation
  (** Transformation on terms *)

val expand_def : operation
  (** Expand definitions of terms and predicates *)

(** {2 Preprocessing} *)

type t
  (** Environment used for preprocessing of the problem *)

val create : ?base:Signature.t -> ?meta:MetaProverState.t -> Params.t -> t
  (** Create a new preprocessing env.
      @param meta is a meta-prover that can be used for processing.
  *)

val copy : t -> t
  (** Copy of the preprocessing env. Shares the same meta prover, if any *)

val get_params : penv:t -> Params.t
  (** Parameters *)

val signature : penv:t -> Signature.t
  (** Base signature *)

val add_base_sig : penv:t -> Signature.t -> unit
  (** Declare a set of base symbols *)

val add_axiom : penv:t -> PFormula.t -> unit
  (** Add a single axiom. Preprocessed sets will be enriched with the
      axiom. *)

val add_axioms : penv:t -> PFormula.t Sequence.t -> unit
  (** Add a set of axioms *)

val add_operation : penv:t -> prio:int -> operation -> unit
  (** Add a preprocessing operation. [prio] is the priority of the operation;
      the higher the priority, the latter the operation is run (ie
      operations with low priority are tried first) *)

val add_operation_rule : penv:t -> prio:int -> (PFormula.Set.t -> operation) -> unit
  (** Add an operation that depends on the initial set of formulas to process *)

val add_constr : penv:t -> Precedence.Constr.t -> unit
  (** Add a precedence constraint *)

val add_constrs : penv:t -> Precedence.Constr.t list -> unit

val add_constr_rule : penv:t -> (PFormula.Set.t -> Precedence.Constr.t) -> unit
  (** Add a precedence constraint rule *)

val mk_precedence : penv:t -> PFormula.Set.t -> Precedence.t
  (** Make a precedence out of the formulas and constraints *)

val process : penv:t -> PFormula.Set.t -> PFormula.Set.t
  (** Process the input formulas recursively *)
