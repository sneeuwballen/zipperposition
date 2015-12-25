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

(** {1 Precedence (total ordering) on symbols} *)

type symbol_status =
  | Multiset
  | Lexicographic

(** {2 Constraints} *)
module Constr : sig

  type 'a t = private ID.t -> ID.t -> int
  constraint 'a = [< `partial | `total]
  (** A partial order on symbols, used to make the precedence more
      precise.
      ['a] encodes the kind of ordering: partial or total
      {b NOTE}: the ordering must partition the set of ALL symbols into
        equivalence classes, within which all symbols are equal, but
        symbols of distinct equivalence classes are always ordered. *)

  val arity : (ID.t -> int) -> [`partial] t
  (** decreasing arity constraint (big arity => high in precedence) *)

  val invfreq : ID.t Sequence.t -> [`partial] t
  (** symbols with high frequency are smaller. Elements of unknown
      frequency are assumed to have a frequency of 0. *)

  val max : ID.t list -> [`partial] t
  (** maximal symbols, in decreasing order *)

  val min : ID.t list -> [`partial] t
  (** minimal symbols, in decreasing order *)

  val alpha : [`total] t
  (** alphabetic ordering on symbols *)

  val compose : [`partial] t -> ([<`partial | `total] as 'a) t -> 'a t
  (** [compose a b] uses [a] to compare symbols; if [a] cannot decide,
      then we use [b]. *)

  val compose_sort : (int * [`partial] t) list -> [`partial] t
  (** [compose_sort l] sorts the list by decreasing priority (the higher,
      the earlier an ordering is applied) before composing *)

  val make : (ID.t -> ID.t -> int) -> [`partial] t
  (** Create a new partial order.
      {b CAUTION}, this order must respect some properties (see {!'a t}) *)
end

exception Error of string

type t
(** Total Ordering on a finite number of symbols, plus a few more
    data (weight for KBO, status for RPC) *)

type precedence = t

val equal : t -> t -> bool
(** Check whether the two precedences are equal (same snapshot) *)

val snapshot : t -> ID.t list
(** Current list of symbols, in increasing order *)

val compare : t -> ID.t -> ID.t -> int
(** Compare two symbols using the precedence *)

val mem : t -> ID.t -> bool
(** Is the ID.t part of the precedence? *)

val status : t -> ID.t -> symbol_status
(** Status of the symbol *)

val weight : t -> ID.t -> int
(** Weight of a symbol (for KBO). Strictly positive int. *)

val add_list : t -> ID.t list -> unit
(** Update the precedence with the given symbols *)

val add_seq : t -> ID.t Sequence.t -> unit

val declare_status : t -> ID.t -> symbol_status -> unit
(** Change the status of the given precedence
    @raise Error if the symbol is not in the the precedence already *)

module Seq : sig
  val symbols : t -> ID.t Sequence.t
end

val pp_snapshot : ID.t list CCFormat.printer
val pp_debugf : t CCFormat.printer
include Interfaces.PRINT with type t := t

type weight_fun = ID.t -> int

val weight_modarity : arity:(ID.t -> int) -> weight_fun

val weight_constant : weight_fun

val set_weight : t -> weight_fun -> unit
(** Change the weight function of the precedence
    @since 0.5.3 *)

(** {2 Creation of a precedence from constraints} *)

val create : ?weight:weight_fun -> [`total] Constr.t -> ID.t list -> t
(** make a precedence from the given constraints. Constraints near
    the head of the list are {b more important} than constraints close
    to the tail. Only the very first constraint is assured to be totally
    satisfied if constraints do not agree with one another. *)

val default : ID.t list -> t
(** default precedence. Default status for symbols is {!Lexicographic}. *)

val default_seq : ID.t Sequence.t -> t
(** default precedence on the given sequence of symbols *)

val constr : t -> [`total] Constr.t
(** Obtain the constraint *)
