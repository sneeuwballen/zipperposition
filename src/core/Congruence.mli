
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

(** {1 Simple and Lightweight Congruence and order}

The congruence stores a finite representation of a set of (ground)
equations an inequalities. It is {b backtrackable}, ie one can go
back to a previous point as long as it is compatible with
a LIFO usage. *)

module type S = sig
  type term

  type t
    (** Represents a congruence *)

  val create : ?size:int -> unit -> t
    (** New congruence.
        @param size a hint for the initial size of the hashtable. *)

  val clear : t -> unit
    (** Clear the content of the congruence. It is now equivalent to
        the empty congruence. *)

  val push : t -> unit
    (** Push a checkpoint on the stack of the congruence. An equivalent call
        to {!pop} will restore the congruence to its current state. *)

  val pop : t -> unit
    (** Restore to the previous checkpoint.
        @raise Invalid_argument if there is no checkpoint to restore to
          (ie if no call to {!push} matches this call to {!pop}) *)

  val stack_size : t -> int
    (** Number of calls to {!push} that lead to the current state of the
        congruence. Also, how many times {!pop} can be called. *)

  val find : t -> term -> term
    (** Current representative of this term *)

  val iter : t -> (mem:term -> repr:term -> unit) -> unit
    (** Iterate on terms that are explicitely present in the congruence.
        The callback is given [mem], the term itself, and [repr],
        the current representative of the term [mem].

        Invariant: when calling [iter cc f], if [f ~mem ~repr] is called,
        then [find cc mem == repr] holds.
    *)

  val iter_roots : t -> (term -> unit) -> unit
    (** Iterate on the congruence classes' representative elements.
        Exactly one term per congruence class will be passed to the
        function. *)

  val mk_eq : t -> term -> term -> unit
    (** [mk_eq congruence t1 t2] asserts that [t1 = t2] belongs to
        the congruence *)

  val mk_less : t -> term -> term -> unit
    (** [mk_less congruence t1 t2] asserts that [t1 < t2] belongs to
        the congruence *)

  val is_eq : t -> term -> term -> bool
    (** Returns true if the two terms are equal in the congruence. This
        updates the congruence, because the two terms need to be added. *)

  val is_less : t -> term -> term -> bool
    (** Returns true if the first term is strictly lower than the second
        one in the congruence *)

  val cycles : t -> bool
    (** Checks whether there are cycles in inequalities.
        @return true if calls to [mk_eq] and [mk_less] entail a cycle in
        the ordering (hence contradicting irreflexivity/transitivity of less) *)
end

(** {2 Functor} *)

module type TERM = sig
  type t

  val equal : t -> t -> bool
    (** Syntactic equality on terms *)

  val hash : t -> int
    (** Hash function on terms *)

  val subterms : t -> t list
    (** Subterms of the term (possibly empty list) *)

  val update_subterms : t -> t list -> t
    (** Replace immediate subterms by the given list.
        This is used to test for equality *)
end

module Make(T : TERM) : S with type term = T.t

(** {2 Common implementations} *)

module FO : S with type term = FOTerm.t

module HO : S with type term = HOTerm.t
