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

(** {1 Type Inference}

This module is used for two things that overlap:
- inferring the types of symbols that have not been declared (e.g. in
  "fof" or "cnf" TPTP statements) so as to enrich a {!Signature.t}
- converting {!Basic} terms or formulas into typed formulas, by inferring
  the exact type of each subterm (and possibly inferring type parameters).

In this context, {b generalizing} type variables means that if some symbol
whose type was unknown and its type still contains variables after the
type inference, those variables are quantified instead of being bound to
a default type (typically {!Type.i}).

For instance: say [f] is not declared and occurs in the term [f(f(nil))]
with the declared constructor [nil : list(A)]. The inferred type for
[f] should be something like [list(B) -> list(B)].
- If we generalize, we declare that [f : list(A) -> list(A)] (for all [A]).
- If we don't, we declare that [f : list($i) -> list($i)].

Here we use a single scope when we unify and substitute type variables,
the scope 0.
*)

(** {2 Default Types} *)

type default_types = {
  default_i : Type.t;
  default_prop : Type.t;
  default_int : Type.t;
  default_rat : Type.t;
}

val tptp_default : default_types
  (** Default TPTP types for ints, rats, etc. *)

(** {2 Typing context}

This module provides a typing context, with an applicative interface.
The context is used to map terms to types locally during type
inference. It also keeps and updates a signature when symbols' types
are inferred.
*)

module Ctx : sig
  type t

  val create : ?default:default_types -> Signature.t -> t
    (** New context with a signature and default types.
        @param default which types are inferred by default (if not provided
          then {!tptp_default} will be used) *)

  val copy : t -> t
    (** Copy of the context *)

  val exit_scope : t -> unit
    (** Exit the current scope (formula, clause), meaning that all free
        variables' types are forgotten. *)

  val add_signature : t -> Signature.t -> unit
    (** Specify the type of some symbols *)

  val declare : t -> Symbol.t -> Type.t -> unit
    (** Declare the type of a symbol. The type {b must} be equal to
        the current type of the symbol, if any.
        @raise Type.Error if an inconsistency (with inferred types) is
          detected. *)

  val bind_to_default : t -> unit
    (** Free constructor variables are bound to the [default] type provided
        at creation of the context. *)

  val generalize : t -> unit
    (** Free constructor variables will be generalized, i.e., kept as variables *)

  val to_signature : t -> Signature.t
    (** Obtain the type of all symbols whose type has been inferred.
        If some instantiated variables remain, they are bound to the
        context's [default] parameter. *)
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

module Closure : module type of Monad.Fun(Ctx)

(** {2 Hindley-Milner}

This module, abstract in the exact kind of term it types, takes as input
a signature and an {b untyped term}, and updates the typing context
so that the {b untyped term} can be converted into a {b typed term}. *)

module type S = sig
  type untyped (* untyped term *)
  type typed   (* typed term *)

  val infer : Ctx.t -> untyped -> Type.t * typed Closure.t
    (** Infer the type of this term under the given signature. This updates
        the context's typing environment!

        @param ctx the context
        @param untyped the untyped term whose type must be inferred

        @return the inferred type of the untyped term (possibly a type var)
          along with a closure to produce a typed term once every
          constraint has been solved
        @raise Type.Error if the types are inconsistent *)

  (** {3 Constraining types}

  This section is mostly useful for inferring a signature without
  converting untyped_terms into typed_terms. *)

  val constrain_term_term : Ctx.t -> untyped -> untyped -> unit
    (** Force the two terms to have the same type in this context
        @raise Type.Error if an inconsistency is detected *)

  val constrain_term_type : Ctx.t -> untyped -> Type.t -> unit
    (** Force the term's type and the given type to be the same.
        @raise Type.Error if an inconsistency is detected *)
end

module FO : sig
  include S with type untyped = PrologTerm.t and type typed = FOTerm.t

  val infer_form : Ctx.t -> PrologTerm.t -> Formula.FO.t Closure.t
    (** Inferring the type of a formula is trivial, it's always {!Type.o}.
        However, here we can still return a closure that produces a
        type formula *)

  val constrain_form : Ctx.t -> untyped -> unit
    (** Assert that the formula should be well-typed. *)

  val signature_forms : Signature.t -> untyped Sequence.t -> Signature.t
    (** Infer signature for this sequence of formulas *)

  val convert : ?generalize:bool -> ctx:Ctx.t -> untyped -> typed
    (** Convert a term into a typed term.
        @param generalize if true, constructor types are generalized (default false)  *)

  val convert_form : ?generalize:bool -> ctx:Ctx.t -> untyped -> Formula.FO.t
    (** Convert a formula into a typed formula.
        @param generalize see {!convert} *)

  val convert_clause : ?generalize:bool -> ctx:Ctx.t -> untyped list -> Formula.FO.t list
    (** Convert a "clause". 
        @param generalize see {!convert} *)

  val convert_seq : ?generalize:bool -> ctx:Ctx.t -> untyped Sequence.t -> Formula.FO.t list
    (** Given the signature for those formulas, infer their type and convert
        untyped formulas into typed formulas. Also updates the context.
        @param generalize see {!convert} *)
 
end

(*
module HO : sig
  include S with type untyped = Basic.HO.t and type typed= HOTerm.t

  val constrain : ctx:Ctx.t -> Basic.HO.t -> unit
    (** Constrain the term to be well-typed and of boolean type *)

  val convert : ?generalize:bool -> ?ret:Type.t -> ctx:Ctx.t ->
                Basic.HO.t -> HOTerm.t
    (** Convert a single untyped term to a typed term. Binds free constructor
        variables to default.
        @param ret is the type we expect for this term (default: {!Type.o})
        @param generalize if true, constructor types are generalized (default false) *)

  val convert_seq : ?generalize:bool -> ctx:Ctx.t ->
                    Basic.HO.t Sequence.t -> HOTerm.t list
    (** Infer the types of those terms and annotate each term and subterm with
        its type. Also updates the context's signature.
        All terms are assumed to be boolean. *)
end
*)
