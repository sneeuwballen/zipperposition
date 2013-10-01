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

(** {2 Type definitions} *)

type t = {
  ord_clear_cache : unit -> unit;                 (** Clear underlying cache *)
  ord_compare : Term.t -> Term.t -> Comparison.t; (** Compare two terms *)
  ord_precedence : Precedence.t;                  (** Current precedence *)
  ord_set_precedence : Precedence.t -> t;         (** Change the precedence *)
  ord_name : string;                              (** Name of the ordering *)
} (** A reduction ordering on terms *)

val compare : t -> Term.t -> Term.t -> Comparison.t
  (** Compare two terms *)

val precedence : t -> Precedence.t
  (** Current precedence *)

val set_precedence : t -> Precedence.t -> t
  (** Change the precedence. The new one must be a superset of the old one. *)

val add_symbols : t -> Symbol.t list -> t
  (** Update precedence with symbols *)

val add_signature : t -> Signature.t -> t
  (** Update precedence with signature *)

val name : t -> string
val clear_cache : t -> unit

val pp : Buffer.t -> t -> unit
val fmt : Format.formatter -> t -> unit
val to_string : t -> string
  
(** {2 Ordering implementations} *)

(** Partial ordering on terms. This provides several
    simplification orderings (compatible with substitution,
    with the subterm property, and monotonic).
    
    Some orderings do not satisfy all those properties.*)

val kbo : Precedence.t -> t
  (** Knuth-Bendix simplification ordering *)

val rpo6 : Precedence.t -> t
  (** Efficient implementation of RPO (recursive path ordering) *)

val none : t
  (** All terms are incomparable (equality still works). Not a simplification
      ordering. *)

val subterm : t
  (** Subterm ordering. Not a simplification ordering. *)

(** {2 Global table of Orders} *)

val default : Signature.t -> t
  (** default ordering on terms (RPO6) *)

val choose : string -> Precedence.t -> t
  (** Choose ordering by name among registered ones, or
      @raise Failure if no ordering with the given name are registered. *)

val register : string -> (Precedence.t -> t) -> unit
  (** Register a new ordering, which can depend on a precedence.
      The name must not be registered already.
      @raise Invalid_argument if the name is already used. *)
