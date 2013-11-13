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
- converting {!Untyped} terms or formulas into typed formulas, by inferring
  the exact type of each subterm.

In this context, {b generalizing} type variables means that if some symbol
whose type was unknown and its type still contains variables after the
type inference, those variables are kept instead of being bound to
a default type (typically {!Type.i}).

For instance: say [f] is not declared and occurs in the term [f(f(nil))]
with the declared constructor [nil : list(A)]. The inferred type for
[f] should be something like [list(B) -> list(B)].
- If we generalize, we declare that [f : list(A) -> list(A)] (for all [A]).
- If we don't, we declare that [f : list($i) -> list($i)].
*)

type scope = Substs.scope

(** {2 Closures} 
A closure is a function that returns a ['a] value if provided with a proper
type substitution and renaming. It is useful to delay the translation
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
  include Monad.S with type 'a t = Substs.Ty.Renaming.t -> Substs.Ty.t -> 'a

  val pure_ty : Type.t -> scope -> Type.t t
    (** Evaluate the type,scope in some renaming,subst *)
end

module TraverseClosure : Monad.TRAVERSE with type 'a M.t = 'a Closure.t

(** {2 Typing context}

This module provides a typing context, with an imperative interface.
The context is used to map terms to types locally during type
inference. It also keeps and updates a signature when symbols' types
are inferred.
*)

module Ctx : sig
  type t

  val create : ?default:Type.t -> ?base:bool -> unit -> t
    (** Fresh environment.
        @param base is true (default), then the
          base signature is used from start, rather than an empty
          signature
        @param default is the type to use for non-declared symbols,
          their return type defaults to {!Type.i}. Please note that
          non-declared symbols therefore cannot have polymorphic types. *)

  val of_signature : ?default:Type.t -> Signature.t -> t
    (** Shortcut that calls {!create} and then adds the given signature. *)

  val clear : t -> unit
    (** Remove every data from the context. It is as new. *)

  val exit_scope : t -> unit
    (** Exit the current scope (formula, clause). *)

  val add_signature : t -> Signature.t -> unit
    (** Specify the type of some symbols *)

  val set_signature : t -> Signature.t -> unit
    (** Set the exact signature. The old one will be erased. *)

  val declare : t -> Symbol.t -> Type.t -> unit
    (** Declare the type of a symbol. The type {b must} be compatible
        (unifiable with) the current type of the symbol, if any.
        @raise TypeUnif.Error if an inconsistency (with inferred types) is
          detected. *)

  val declare_basic : t -> Symbol.t -> Basic.Ty.t -> unit
    (** Declare the type of a symbol, in raw form *)

  val unify_and_set : t -> Type.t -> scope -> Type.t -> scope -> unit
    (** Unify the two types in the given scopes.
        @raise TypeUnif.Error if types are not unifiable in the context *)

  val to_signature : t -> Signature.t
    (** Obtain the type of all symbols whose type has been inferred.
        If some instantiated variables remain, they are bound to the
        context's [default] parameter. *)

  val bind_to_default : t -> unit
    (** Free constructor variables are bound to the [default] type provided
        at creation of the context. *)

  val generalize : t -> unit
    (** Free constructor variables will be generalized, i.e., kept as variables *)

  val renaming_clear : t -> Substs.Ty.Renaming.t
    (** Clear and return the default renaming stored in this context *)

  val apply_closure : ?default:bool ->
                      ?renaming:Substs.Ty.Renaming.t ->
                      t -> 'a Closure.t -> 'a
    (** Apply the given closure to the substitution contained in the context
        (type constraints). A renaming can be provided

        @param default if true, !{bind_to_default} is called first to ensure
          that symbols whose type is inferred are not generalized.
          if not provided, [default] is true.
        @param renaming a renaming that is used to apply the substitution
    *)
end

(** {2 Hindley-Milner}

This module, abstract in the exact kind of term it types, takes as input
a signature and an {b untyped term}, and updates the typing context
so that the {b untyped term} can be converted into a {b typed term}. *)

exception Error of string
  (** Raised when type inference fails *)

module type S = sig
  type untyped (* untyped term *)
  type typed   (* typed term *)

  val infer : Ctx.t -> untyped -> scope -> Type.t * typed Closure.t
    (** Infer the type of this term under the given signature. This updates
        the context's typing environment! The resulting type's variables
        belong to the given scope.

        @param ctx the context
        @param untyped the untyped term whose type must be inferred
        @param scope where the term's type variables live

        @return the inferred type of the untyped term (possibly a type var)
          along with a closure to produce a typed term once every
          constraint has been solved
        @raise Error if the types are inconsistent *)

  (** {3 Constraining types}
  
  This section is mostly useful for inferring a signature without
  converting untyped_terms into typed_terms. *)

  val constrain_term_term : Ctx.t -> untyped -> scope -> untyped -> scope -> unit
    (** Force the two terms to have the same type in this context
        @raise Error if an inconsistency is detected *)

  val constrain_term_type : Ctx.t -> untyped -> scope -> Type.t -> scope -> unit
    (** Force the term's type and the given type to be the same.
        @raise Error if an inconsistency is detected *)
end

module FO : sig
  include S with type untyped = Basic.FO.t and type typed = FOTerm.t

  val infer_form : Ctx.t -> Basic.Form.t -> scope -> FOFormula.t Closure.t
    (** Inferring the type of a formula is trivial, it's always {!Type.o}.
        However, here we can still return a closure that produces a
        type formula *)

  val constrain_form : Ctx.t -> Basic.Form.t -> unit
    (** Assert that the formula should be well-typed. *)

  val signature_forms : Signature.t -> Basic.Form.t Sequence.t -> Signature.t
    (** Infer signature for this sequence of formulas *)

  val convert : ?generalize:bool -> ctx:Ctx.t ->
                Basic.FO.t -> FOTerm.t
    (** Convert a term into a typed term.
        @param generalize if true, constructor types are generalized (default false)  *)

  val convert_form : ?generalize:bool -> ctx:Ctx.t ->
                      Basic.Form.t -> FOFormula.t
    (** Convert a formula into a typed formula.
        @param generalize see {!convert} *)

  val convert_clause : ?generalize:bool -> ctx:Ctx.t ->
                        Basic.Form.t list -> FOFormula.t list
    (** Convert a "clause". Type variables are bound in the same scope
        for all formulas in the list. 
        @param generalize see {!convert} *)

  val convert_seq : ?generalize:bool -> ctx:Ctx.t ->
                    Basic.Form.t Sequence.t -> FOFormula.t list
    (** Given the signature for those formulas, infer their type and convert
        untyped formulas into typed formulas. Also updates the context. Type
        variables of each formulas live in distinct scopes. 
        @param generalize see {!convert} *)
 
end

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
