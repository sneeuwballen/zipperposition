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

(** {1 Term Orderings} *)

(* TODO: compute orderings modulo a substitution *)

(** {2 Type definitions} *)

module type S = sig
  module Prec : Precedence.S with type symbol = Symbol.t

  type term

  type symbol = Prec.symbol

  type t
    (** Partial ordering on terms *)

  type ordering = t

  val compare : t -> term -> term -> Comparison.t
    (** Compare two terms using the given ordering *)

  val precedence : t -> Prec.t
    (** Current precedence *)

  val set_precedence : t -> Prec.t -> t
    (** Change the precedence. The new one must be a superset of the old one.
        @raise Invalid_argument if the new precedence is not compatible
          with the old one *)

  val update_precedence : t -> (Prec.t -> Prec.t) -> t
    (** Update the precedence with a function.
        @raise Invalid_argument if the new precedence is not compatible
          with the previous one (see {!set_precedence}). *)

  val add_list : t -> symbol list -> t
    (** Update precedence with symbols *)

  val add_seq : t -> symbol Sequence.t -> t
    (** Update precedence with signature *)

  val name : t -> string
    (** Name that describes this ordering *)

  val clear_cache : t -> unit

  val pp : Buffer.t -> t -> unit
  val fmt : Format.formatter -> t -> unit
  val to_string : t -> string

  (** {2 Ordering implementations}
      An ordering is a partial ordering on terms. Several implementations
      are simplification orderings (compatible with substitution,
      with the subterm property, and monotonic), some other are not. *)

  val kbo : Prec.t -> t
    (** Knuth-Bendix simplification ordering *)

  val rpo6 : Prec.t -> t
    (** Efficient implementation of RPO (recursive path ordering) *)

  val none : t
    (** All terms are incomparable (equality still works).
        Not a simplification ordering. *)

  val subterm : t
    (** Subterm ordering. Not a simplification ordering. *)

  (** {2 Global table of Orders} *)

  val default_of_list : symbol list -> t
    (** default ordering on terms (RPO6) using default precedence *)

  val default_of_prec : Prec.t -> t

  val by_name : string -> Prec.t -> t
    (** Choose ordering by name among registered ones, or
        @raise Invalid_argument if no ordering with the given name are registered. *)

  val register : string -> (Prec.t -> t) -> unit
    (** Register a new ordering, which can depend on a precedence.
        The name must not be registered already.
        @raise Invalid_argument if the name is already used. *)
end

(** {2 Functor} *)

module Make(P : Precedence.S with type symbol = Symbol.t) :
  S with module Prec = P and type term = FOTerm.t

module Default : S with module Prec = Precedence.Default and type term = FOTerm.t

include module type of Default with module Prec = Default.Prec
