
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

(** {1 Simple and Lightweight Congruence and order} *)

module type S = sig
  type term

  type t
    (** Represents a congruence *)

  val create : ?size:int -> unit -> t
    (** New congruence *)

  val find : t -> term -> term
    (** Current representative of this term *)

  val mk_eq : t -> term -> term -> unit
    (** [mk_eq congruence t1 t2] asserts that [t1 = t2] belongs to
        the congruence *)

  val mk_less : t -> term -> term -> unit
    (** [mk_less congruence t1 t2] asserts that [t1 < t2] belongs to
        the congruence *)

  val is_eq : t -> term -> term -> bool
    (** Returns true if the two terms are equal in the congruence *)

  val is_less : t -> term -> term -> bool
    (** Returns true if the first term is lower than the second one in the
        congruence *)

  val no_cycles : t -> bool
    (** Checks whether there are cycles in inequalities.
        @return true if all calls to [mk_less] are compatible with
        irreflexivity and transitivity of less. *)
end

module FO : S with type term = FOTerm.t

module HO : S with type term = HOTerm.t
