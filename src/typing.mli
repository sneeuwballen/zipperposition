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

(** {1 Typing} *)

type ty = Term.t

val is_atom : ty -> bool

val is_fo : ty -> bool

val is_simple_typed : ty -> bool

val arity : ty -> int

(** {2 Signatures} *)

module Signature : sig
  type t
    (** A signature maps symbols to types *)

  val empty : t
    (** Empty signature *)
  
  val mem : t -> Symbol.t -> bool
    (** Is the symbol declared? *)

  val declare : t -> Symbol.t -> ty -> t
    (** Declare the symbol, or
        @raise Invalid_argument if the symbol is already defined *)

  val find : t -> Symbol.t -> ty
    (** Lookup the type of this symbol, or
        @raise Not_found if the symbol is not in the signature *)

  val arity : t -> Symbol.t -> int
    (** Arity of the given symbol, or
        @raise Not_found if the symbol is not in the signature *)

  val cardinal : t -> int
    (** Number of symbols *)

  val is_fo : t -> bool
    (** Is the signature a First Order signature (no higher order symbols)? *)

  val is_simple_typed : t -> bool
    (** Are all types simple (ie, no type variable)? *)

  val to_seq : t -> (Symbol.t * ty) Sequence.t
  val of_seq : (Symbol.t * ty) Sequence.t -> t

  val pp : Buffer.t -> t -> unit
  val to_string : t -> string
  val fmt : Format.formatter -> t -> unit
end

(** {2 Type inference} *)

type env
  (** Environment used for typing. It is mutable, and contains a signature *)

val create_env : unit -> env

val copy_env : env -> env

val sig_of_env : env -> Signature.t

exception TypeError

val infer : env -> Term.t -> ty
  (** Infer the type of this term under the given signature. Will also
      update the environment if some new 
      @raise TypeError if the types are inconsistent *)

