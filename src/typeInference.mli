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

(** {2 Typing context} *)

(** This module provides a typing context, with an imperative interface.
    The context is used to map terms to types locally during type
    inference. It also keeps and updates a signature when symbols' types
    are inferred.
*)

module Ctx : sig
  type t

  val create : ?base:bool -> unit -> t
    (** Fresh environment. if [base] is true (default), then the
        base signature is used from start, rather than an empty
        signature *)

  val of_signature : Signature.t -> t

  val add_signature : t -> Signature.t -> unit
    (** Specify the type of some symbols *)

  val within_binder : t -> (Type.t -> 'a) -> 'a
    (** Provides a context, corresponding to a term binding environement,
        in which the first De Bruijn variable will have the type
        passed as argument *)

  val db_type : t -> int -> Type.t
    (** The type of the bound variable of De Bruijn index [i].
        [within_binder] must have been used enough times before, so
        that a type is attributed to the [i]-th bound variable.
        @raise Invalid_argument if the [i]-th variable is not bound.*)

  val unify : t -> Type.t -> Type.t -> unit
    (** Unify the two types. On success it may update some GVar's bindings.
        @raise Type.Error if unification is impossible. *)

  val type_of_symbol : t -> Symbol.t -> Type.t
    (** Returns the type of this symbol. If the symbol has an unknown type,
        a fresh instantiated variable is returned. Otherwise the known
        type of the symbol is returned and instantiated with fresh variables,
        so that, for instance, "nil" (the empty list) can have several
        concrete types. *)

  val declare : t -> Symbol.t -> Type.t -> unit
    (** Declare the type of a symbol. The type {b must} be closed.
        @raise Type.Error if an inconsistency (with inferred type) is detected. *)

  val to_signature : t -> Signature.t
    (** Obtain the type of all symbols whose type has been inferred *)

  val unwind_protect : t -> (unit -> 'a) -> 'a
    (** Transaction for variable bindings
        see {!Type.Stack.unwind_protect} *)

  val protect : t -> (unit -> 'a) -> 'a
    (** Provide a local environment to perform typing, and then
        remove all intermediate variable bindings.
        see {!Type.Stack.protect} *)
end

(** {2 Hindley-Milner} *)

val infer : Ctx.t -> Term.t -> Type.t
  (** Infer the type of this term under the given signature.  This updates
      the context's typing environment!
      @raise Type.Error if the types are inconsistent *)

val infer_sig : Signature.t -> Term.t -> Type.t
  (** Inference from a signature (shortcut) *)

val infer_no_check : Ctx.t -> Term.t -> Type.t
  (** Infer the type of the term, but does not recurse if it's not needed. *)

(** {3 Constraining types} *)

val default_to_i : Ctx.t -> unit
  (** For all symbols seen in the context that still have un-instantiated
      type variables in their type, set this type to {!Type.i}. *)

val generalize_all : Ctx.t -> unit
  (** For all symbols seen in the context that still have un-instantiated
      type variables in their type, generalize those type variables
      (ie, quantify over them) *)

val constrain_term_term : Ctx.t -> Term.t -> Term.t -> unit
  (** Force the two terms to have the same type
      @raise Type.Error if an inconsistency is detected *)

val constrain_term_type : Ctx.t -> Term.t -> Type.t -> unit
  (** Force the term to have the given type.
      @raise Type.Error if an inconsistency is detected *)

(** {3 Checking compatibility} *)

val check_term_type : Ctx.t -> Term.t -> Type.t -> bool
  (** Check whether this term can be used with this type *)

val check_term_term : Ctx.t -> Term.t -> Term.t -> bool
  (** Can we unify the terms' types? *)

val check_type_type : Ctx.t -> Type.t -> Type.t -> bool
  (** Can we unify the two types? *)
