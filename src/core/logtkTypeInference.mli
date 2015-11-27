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

(** {1 LogtkType Inference}

This module is used for two things that overlap:
- inferring the types of symbols that have not been declared (e.g. in
  "fof" or "cnf" TPTP statements) so as to enrich a {!Logtk.LogtkSignature.t}
- converting {i untyped} terms or formulas into {i typed} formulas, by inferring
  the exact type of each subterm (and possibly inferring type parameters).

In this context, {b generalizing} type variables means that if some symbol
whose type was unknown and its type still contains variables after the
type inference, those variables are quantified instead of being bound to
a default type (typically {!Logtk.LogtkType.i}).

For instance: say [f] is not declared and occurs in the term [f(f(nil))]
with the declared constructor [nil : list(A)]. The inferred type for
[f] should be something like [list(B) -> list(B)].
- If we generalize, we declare that [f : list(A) -> list(A)] (for all [A]).
- If we don't, we declare that [f : list($i) -> list($i)].

Here we use a single scope when we unify and substitute type variables,
the scope 0.

Many functions will use an Error monad to make errors explicit. The error
type is {!or_error}. The module {!CCError} in containers can be used
to deal with errors (including monadic operators).
*)

type 'a or_error = [`Error of string | `Ok of 'a]

(** {2 Default LogtkTypes} *)

type default_types = {
  default_i : LogtkType.t;
  default_prop : LogtkType.t;
  default_int : LogtkType.t;
  default_rat : LogtkType.t;
}

val tptp_default : default_types
  (** Default TPTP types for ints, rats, etc. *)

(** {2 Typing context}

This module provides a typing context, with an applicative interface.
The context is used to map terms to types locally during type
inference. It also keeps and updates a signature when symbols' types
are inferred.

This module is quite low-level, and shouldn't be used in simple cases
(see the following modules)
*)

module Ctx : sig
  type t

  val create : ?default:default_types -> LogtkSignature.t -> t
    (** New context with a signature and default types.
        @param default which types are inferred by default (if not provided
          then {!tptp_default} will be used) *)

  val clear : t -> unit
    (** Reset totally the context *)

  val copy : t -> t
    (** Copy of the context *)

  val exit_scope : t -> unit
    (** Exit the current scope (formula, clause), meaning that all free
        variables' types are forgotten. *)

  val add_signature : t -> LogtkSignature.t -> unit
    (** Specify the type of some symbols *)

  val declare : t -> LogtkSymbol.t -> LogtkType.t -> unit
    (** Declare the type of a symbol. The type {b must} be equal to
        the current type of the symbol, if any.
        @raise LogtkType.Error if an inconsistency (with inferred types) is
          detected. *)

  val ty_of_prolog : t -> LogtkPrologTerm.t -> LogtkType.t option
    (** LogtkType conversion from LogtkPrologTerm *)

  val bind_to_default : t -> unit
    (** Free constructor variables are bound to the [default] type provided
        at creation of the context. *)

  val generalize : t -> unit
    (** Free constructor variables will be generalized, i.e., kept as variables *)

  val to_signature : t -> LogtkSignature.t
    (** Obtain the type of all symbols whose type has been inferred.
        If some instantiated variables remain, they are bound to the
        context's [default] parameter. *)

  val constrain_type_type : t -> LogtkType.t -> LogtkType.t -> unit
    (** Constrain the two types to be equal
        @raise LogtkType.Error if it is not possible *)
end

(** {2 Closures}
A closure is a function that returns a ['a] value if provided with a proper
type inference context. It is useful to delay the translation
from untyped terms to typed terms, because locally we don't have applied all
the type constraints resulting from the context yet. Therefore, when inferring
the type of a subterm, we apply local constraints, and return a {b closure}
that, given the final environment, converts the subterm into a typed term.

For instance, if we infer the type of [nil] in the terms
[cons(1, nil)] and [cons("foo", nil)], we can't infer the most specialized
type for [nil] unless we also take its context (cons(_,_)) into account.
The same closure will therefore be used to build [nil:$int] and [nil:$string]
respectively.
*)

module Closure : sig
  type 'a t = Ctx.t -> 'a
  val return : 'a -> 'a t
  val map : 'a t -> ('a -> 'b) -> 'b t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t

  type 'a monad = 'a t (** monad used for traversal *)
  val fold : 'a Sequence.t -> 'b monad -> ('b -> 'a -> 'b monad) -> 'b monad
  val fold_l : 'a list -> 'b monad -> ('b -> 'a -> 'b monad) -> 'b monad
  val map_l : 'a list -> ('a -> 'b monad) -> 'b list monad
  val seq : 'a monad list -> 'a list monad
end

(** {2 Hindley-Milner LogtkType Inference}

This module, abstract in the exact kind of term it types, takes as input
a signature and an {b untyped term}, and updates the typing context
so that the {b untyped term} can be converted into a {b typed term}. *)

module type S = sig
  type untyped (** untyped term *)
  type typed   (** typed term *)

  val infer_exn : Ctx.t -> untyped -> LogtkType.t * typed Closure.t
    (** Infer the type of this term under the given signature. This updates
        the context's typing environment!

        @param ctx the context
        @param untyped the untyped term whose type must be inferred

        @return the inferred type of the untyped term (possibly a type var)
          along with a closure to produce a typed term once every
          constraint has been solved
        @raise LogtkType.Error if the types are inconsistent *)

  val infer : Ctx.t -> untyped -> (LogtkType.t * typed Closure.t) or_error
    (** Safe version of {!infer_exn}. It returns [`Error s] rather
        than raising {!LogtkType.Error} if the typechecking fails. *)

  (** {3 Constraining types}

  This section is mostly useful for inferring a signature without
  converting untyped_terms into typed_terms. *)

  val constrain_term_term_exn : Ctx.t -> untyped -> untyped -> unit
    (** Force the two terms to have the same type in this context
        @raise LogtkType.Error if an inconsistency is detected *)

  val constrain_term_type_exn : Ctx.t -> untyped -> LogtkType.t -> unit
    (** Force the term's type and the given type to be the same.
        @raise LogtkType.Error if an inconsistency is detected *)

  val constrain_term_term : Ctx.t -> untyped -> untyped -> unit or_error
    (** Safe version of {!constrain_term_term_exn} *)

  val constrain_term_type : Ctx.t -> untyped -> LogtkType.t -> unit or_error
    (** Safe version of {!constrain_term_type_exn} *)
end

val map_error_seq : ('a -> 'b or_error) -> 'a Sequence.t -> 'b Sequence.t or_error
  (** [map_error_seq f seq] maps [f] over the sequence [seq], and returns
      the (persistent) sequence of result if all calls to [f] succeed.
      Otherwise it returns the first encountered error. *)

module FO : sig
  include S with type untyped = LogtkPrologTerm.t and type typed = LogtkFOTerm.t

  type typed_form = LogtkFormula.FO.t

  val constrain_form : Ctx.t -> untyped -> unit or_error
    (** Force the untyped term to be typable as a formula. *)

  val infer_form : Ctx.t -> untyped -> typed_form Closure.t or_error
    (** Inferring the type of a formula is trivial, it's always {!LogtkType.o}.
        However, here we can still return a closure that produces a
        type formula *)

  val signature_forms : LogtkSignature.t -> untyped Sequence.t -> LogtkSignature.t or_error
    (** Infer signature for this sequence of formulas, starting with
        an initial signature *)

  val convert : ?generalize:bool -> ctx:Ctx.t -> untyped -> typed or_error
    (** Convert a term into a typed term.
        @param generalize if [true], constructor types are generalized,
          which means any type variable still not bound after type inference
          will become a type parameter. If [false] then those variables
          are bound to the default type (for instance {!LogtkType.TPTP.i}).
          Default is [false] *)

  val convert_form : ?generalize:bool -> ctx:Ctx.t ->
                     untyped -> LogtkFormula.FO.t or_error
    (** Convert a formula into a typed formula.
        @param generalize see {!convert}
        @param ctx the typing context to use. Updated in place.
        @raise LogtkType.Error if types are inconsistent *)

  val convert_clause : ?generalize:bool ->
                       ctx:Ctx.t -> untyped list -> LogtkFormula.FO.t list or_error
    (** Convert a "clause" (i.e. just a list of atomic formulas) into its
        type version.
        @param generalize see {!convert} *)

  val convert_seq : ?generalize:bool ->
                    [`sign of LogtkSignature.t | `ctx of Ctx.t] ->
                    untyped Sequence.t -> LogtkFormula.FO.t list or_error
    (** Given the signature for those formulas, infer their type and convert
        untyped formulas into typed formulas. Also updates the context if
        provided.
        @param generalize see {!convert} *)

  (** {3 Unsafe API}
  All those functions can raise {!LogtkType.Error} in case of type mismatch, rather
  than use {!or_error}.  *)

  val infer_form_exn : Ctx.t -> untyped -> typed_form Closure.t

  val constrain_form_exn : Ctx.t -> untyped -> unit

  val signature_forms_exn : LogtkSignature.t -> untyped Sequence.t -> LogtkSignature.t

  val convert_exn : ?generalize:bool -> ctx:Ctx.t -> untyped -> typed

  val convert_form_exn : ?generalize:bool -> ctx:Ctx.t -> untyped -> LogtkFormula.FO.t

  val convert_clause_exn : ?generalize:bool ->
                           ctx:Ctx.t -> untyped list -> LogtkFormula.FO.t list
  val convert_seq_exn : ?generalize:bool ->
                        [`sign of LogtkSignature.t | `ctx of Ctx.t] ->
                        untyped Sequence.t -> LogtkFormula.FO.t list
end

module HO : sig
  include S with type untyped = LogtkPrologTerm.t and type typed= LogtkHOTerm.t

  val constrain : ctx:Ctx.t -> untyped -> unit or_error
    (** Constrain the term to be well-typed and of boolean type *)

  val convert : ?generalize:bool -> ?ret:LogtkType.t -> ctx:Ctx.t ->
                untyped -> typed or_error
    (** Convert a single untyped term to a typed term. Binds free constructor
        variables to default.
        @param ctx context used for type inference
        @param ret is the type we expect for this term (default: {!LogtkType.o})
        @param generalize if true, constructor types are generalized
          (see {!FO.convert} more more details). Default is [false] *)

  val convert_seq : ?generalize:bool -> ctx:Ctx.t ->
                    untyped Sequence.t -> typed list or_error
    (** Infer the types of those terms and annotate each term and subterm with
        its type. Also updates the context's signature.
        All terms are assumed to be boolean. *)

  (** {3 Unsafe API}
  Functions that can raise {!LogtkType.Error} *)

  val constrain_exn : ctx:Ctx.t -> untyped -> unit

  val convert_exn : ?generalize:bool -> ?ret:LogtkType.t ->
                    ctx:Ctx.t -> untyped -> typed

  val convert_seq_exn : ?generalize:bool -> ctx:Ctx.t ->
                        untyped Sequence.t -> typed list
end
