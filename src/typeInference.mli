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

module Ctx : sig
  type t

  val empty : t

  val of_signature : Signature.t -> t

  val add_signature : t -> Signature.t -> t
    (** Specify the type of some symbols *)

  val enter_binder : t -> t * Type.t
    (** Enter a new binding context (with a variable type) *)

  val exit_binder : t -> t
    (** Exit the innermost binding context *)

  val unify : t -> Type.t -> Type.t -> t
    (** Unify the two types *)

  val new_var : t -> t * Type.t
  val new_vars : t -> int -> t * Type.t list

  val rename_type : t -> Type.t -> t * Type.t
    (** Rename variables of the given type to fresh variables *)

  val rename_var : t -> Type.t -> int -> t * Type.t
    (** Rename the given type variable in the given type *)

  val type_of_symbol : t -> Symbol.t -> t * Type.t
    (** Returns the type of this symbol. If the symbol has an unknown type,
        a fresh variable is returned. Otherwise the type is instantiated
        with fresh variables. *)

  val eval_type : t -> Type.t -> Type.t
    (** Evaluate the given type in this context *)

  val to_signature : t -> Signature.t
    (** Obtain the type of all symbols whose type has been inferred *)
end

(** {2 Hindley-Milner} *)

val infer_update : Ctx.t -> Term.t -> Ctx.t * Type.t
  (** Update the context with symbols that may occur in the
      term, and yet have no known type.
      @raise Type.Error if types are inconsistent. *)

val infer : Ctx.t -> Term.t -> Type.t
  (** Infer the type of this term under the given signature. 
      @raise Type.Error if the types are inconsistent *)

val infer_sig : Signature.t -> Term.t -> Type.t
  (** Inference from a signature (shortcut) *)

(** {3 Constraining types} *)

val constrain_term_term : Ctx.t -> Term.t -> Term.t -> Ctx.t
  (** Force the two terms to have the same type *)

val constrain_term_type : Ctx.t -> Term.t -> Type.t -> Ctx.t
  (** Force the term to have the given type *)

(** {3 Checking compatibility} *)

val check_term_type : Ctx.t -> Term.t -> Type.t -> bool
  (** Check whether this term can be used with this type *)

val check_term_term : Ctx.t -> Term.t -> Term.t -> bool
  (** Can we unify the terms' types? *)

val check_type_type : Ctx.t -> Type.t -> Type.t -> bool
  (** Can we unify the two types? *)
