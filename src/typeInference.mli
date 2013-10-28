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

(** {1 Type Inference} *)

type scope = Substs.scope

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
          defaults to {!Type.i}. Please not that non-declared
          symbols cannot have polymorphic types. *)

  val of_signature : ?default:Type.t -> Signature.t -> t
    (** Shortcut that calls {!create} and then adds the given signature. *)

  val add_signature : t -> Signature.t -> unit
    (** Specify the type of some symbols *)

  val within_binder : t -> ty:Type.t -> (Type.t -> scope -> 'a) -> 'a
    (** Provides a context, corresponding to a term binding environment.
        Within the local context, the bound De Bruijn variable will
        have the (type,scope) that are passed as arguments to the closure. *)

  val db_type : t -> int -> Type.t * scope
    (** The type of the bound variable of De Bruijn index [i].
        [within_binder] must have been used enough times before, so
        that a type is attributed to the [i]-th bound variable.
        @raise Invalid_argument if the [i]-th variable is not bound.*)

  val type_of_fun : t -> Symbol.t -> Type.t * scope
    (** If the function symbol has an unknown type, a fresh variable
        (in a fresh scope, that is) is returned. Otherwise the known
        type of the symbol is returned along with a new scope,
        so that, for instance, "nil" (the empty list) can have several
        concrete types (using distinct scopes). *)

  val declare : t -> Symbol.t -> Type.t -> unit
    (** Declare the type of a symbol. The type {b must} be compatible
        (unifiable with) the current type of the symbol, if any.
        @raise TypeUnif.Error if an inconsistency (with inferred types) is
          detected. *)

  val eval_type : ?renaming:Substs.Ty.Renaming.t ->
                  t -> Type.t -> scope -> Type.t
    (** Evaluate the type within the given context and scope.
        A renaming can be provided instead of using the context's one *)

  val to_signature : t -> Signature.t
    (** Obtain the type of all symbols whose type has been inferred.
        If some instantiated variables remain, they are bound to the
        context's [default] parameter. *)
end

(** {2 Hindley-Milner} *)

module type S = sig
  type term

  val infer : Ctx.t -> term -> scope -> Type.t
    (** Infer the type of this term under the given signature. This updates
        the context's typing environment! The resulting type
        is to be used within the same scope as the last argument.
        @raise TypeUnif.Error if the types are inconsistent *)

  val infer_eval : ?renaming:Substs.Ty.Renaming.t ->
                    Ctx.t -> term -> scope -> Type.t
    (** Infer the type of the given term, and then evaluate the type
        in the given renaming (desambiguate scopes). *)

  (** {3 Constraining types} *)

  val constrain_term_term : Ctx.t -> term -> scope -> term -> scope -> unit
    (** Force the two terms to have the same type in this context
        @raise TypeUnif.Error if an inconsistency is detected *)

  val constrain_term_type : Ctx.t -> term -> scope -> Type.t -> scope -> unit
    (** Force the term to have the given type in the given scope.
        @raise TypeUnif.Error if an inconsistency is detected *)

  (** {3 Checking compatibility} *)

  val check_term_type : Ctx.t -> term -> scope -> Type.t -> scope -> bool
    (** Check whether this term can be used with this type. *)

  val check_term_term : Ctx.t -> term -> scope -> term -> scope -> bool
    (** Can we unify the terms' types? *)

  val check_term_term_sig : Signature.t -> term -> scope -> term -> scope -> bool

  val check_term_type_sig : Signature.t -> term -> scope -> Type.t -> scope -> bool

  (** {3 Handy shortcuts for type inference}
  This module provides an easy way to specify constraints. Every term and
  type is assumed to live in the scope 0. *)

  module Quick : sig
    (* type constraints *)
    type constr =
      | WellTyped of term
      | SameType of term * term
      | HasType of term * Type.t

    val constrain : ?ctx:Ctx.t -> constr list -> Ctx.t
    
    val constrain_seq : ?ctx:Ctx.t -> constr Sequence.t -> Ctx.t

    val signature : ?signature:Signature.t -> constr list -> Signature.t

    val signature_seq : ?signature:Signature.t -> constr Sequence.t -> Signature.t
  end
end

module FO : sig
  include S with type term = FOTerm.t

  val constrain_form : Ctx.t -> FOFormula.t -> scope -> unit
    (** Assert that the formula should be well-typed. *)

  val signature_forms : ?signature:Signature.t -> FOFormula.t Sequence.t -> Signature.t
    (** Infer signature for this sequence of formulas *)
end

module HO : S with type term = HOTerm.t
