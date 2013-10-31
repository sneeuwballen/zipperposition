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

type 'a closure = Substs.Ty.Renaming.t -> Substs.Ty.t -> 'a
  (** Function that returns a ['a] value if provided with a proper
      type substitution and renaming *)

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

  val add_signature : t -> Signature.t -> unit
    (** Specify the type of some symbols *)

  val set_signature : t -> Signature.t -> unit
    (** Set the exact signature. The old one will be erased. *)

  val declare : t -> Symbol.t -> Type.t -> unit
    (** Declare the type of a symbol. The type {b must} be compatible
        (unifiable with) the current type of the symbol, if any.
        @raise TypeUnif.Error if an inconsistency (with inferred types) is
          detected. *)

  val declare_parsed : t -> Symbol.t -> Type.Parsed.t -> unit
    (** Declare the type of a symbol, in raw form *)

  val to_signature : t -> Signature.t
    (** Obtain the type of all symbols whose type has been inferred.
        If some instantiated variables remain, they are bound to the
        context's [default] parameter. *)

  val bind_to_default : t -> unit
    (** Free constructor variables are bound to the [default] type provided
        at creation of the context. *)

  val generalize : t -> unit
    (** Free constructor variables will be generalized, i.e., kept as variables *)

  val apply_closure : ?default:bool -> renaming:Substs.Ty.Renaming.t ->
                      t -> 'a closure -> 'a
    (** Apply the given closure to the substitution contained in the context
        (type constraints). A renaming needs be provided.
        @param default if true, !{bind_to_default} is called first to ensure
          that symbols whose type is inferred are not generalized.
          if not provided, [default] is true. *)
end

(** {2 Hindley-Milner}

This module, abstract in the exact kind of term it types, takes as input
a signature and an {b untyped term}, and updates the typing context
so that the {b untyped term} can be converted into a {b typed term}. *)

module type S = sig
  type untyped (* untyped term *)
  type typed   (* typed term *)

  val infer : ?pred:bool -> Ctx.t -> untyped -> scope -> Type.t * typed closure
    (** Infer the type of this term under the given signature. This updates
        the context's typing environment! The resulting type's variables
        belong to the given scope.

        @param ctx the context
        @param untyped the untyped term whose type must be inferred
        @param scope where the term's type variables live
        @param pred true if we expect a predicate (return type {!Type.o})

        @return the inferred type of the untyped term (possibly a type var)
          along with a closure to produce a typed term once every
          constraint has been solved
        @raise TypeUnif.Error if the types are inconsistent *)

  (** {3 Constraining types}
  
  This section is mostly useful for inferring a signature without
  converting untyped_terms into typed_terms. *)

  val constrain_term_term : Ctx.t -> untyped -> scope -> untyped -> scope -> unit
    (** Force the two terms to have the same type in this context
        @raise TypeUnif.Error if an inconsistency is detected *)

  val constrain_term_type : Ctx.t -> untyped -> scope -> Type.t -> scope -> unit
    (** Force the term's type and the given type to be the same.
        @raise TypeUnif.Error if an inconsistency is detected *)
end

module FO : sig
  include S with type untyped = Untyped.FO.t and type typed = FOTerm.t

  val infer_form : Ctx.t -> Untyped.Form.t -> scope -> FOFormula.t closure
    (** Inferring the type of a formula is trivial, it's always {!Type.o}.
        However, here we can still return a closure that produces a
        type formula *)

  val constrain_form : Ctx.t -> Untyped.Form.t -> unit
    (** Assert that the formula should be well-typed. *)

  val signature_forms : Signature.t -> Untyped.Form.t Sequence.t -> Signature.t
    (** Infer signature for this sequence of formulas *)

  val convert : ctx:Ctx.t -> Untyped.Form.t -> FOFormula.t
    (** Convert a formula into a typed formula. Free constructor variables
        are bound to [default]. *)

  val convert_clause : ctx:Ctx.t -> Untyped.Form.t list -> FOFormula.t list
    (** Convert a "clause". Type variables are bound in the same scope
        for all formulas in the list. Binds free constructor
        variables to default. *)

  val convert_seq : ctx:Ctx.t -> Untyped.Form.t Sequence.t -> FOFormula.t list
    (** Given the signature for those formulas, infer their type and
        convert untyped formulas into typed formulas. Also updates
        the context. Type variables of each formulas live in distinct
        scopes. Bind free constructor variables to default *)
end

module HO : sig
  include S with type untyped = Untyped.HO.t and type typed= HOTerm.t

  val constrain : ctx:Ctx.t -> Untyped.HO.t -> unit
    (** Constrain the term to be well-typed and of boolean type *)

  val convert : ctx:Ctx.t -> Untyped.HO.t -> HOTerm.t
    (** Convert a single untyped term to a typed term. Binds free constructor
        variables to default. *)

  val convert_seq : ctx:Ctx.t -> Untyped.HO.t Sequence.t -> HOTerm.t list
    (** Infer the types of those terms and annotate each term and subterm with
        its type. Also updates the context's signature.
        All terms must be boolean. *)
end
