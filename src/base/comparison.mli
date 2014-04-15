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

(** {1 Partial Ordering values} *)

type t = Lt | Eq | Gt | Incomparable
  (** partial order *)

type comparison = t

val to_string : t -> string
  (** Infix Representation *)

val combine : t -> t -> t
  (** combine two partial comparisons, that are assumed to be
      compatible, ie they do not order differently if
      Incomparable is not one of the values.
      @raise Invalid_argument if the comparisons are inconsistent. *)

val opp : t -> t
  (** Opposite of the relation: a R b becomes b R a *)

val to_total : t -> int
  (** Conversion to a total ordering. Incomparable is translated
      to 0 (equal). *)

val of_total : int -> t
  (** Conversion from a total order *)

val lexico : t -> t -> t
  (** Lexicographic combination (the second is used only if the first
      is [Incomparable] *)

val (++) : t -> t -> t
  (** Infix version of {!lexico} *)

type 'a comparator = 'a -> 'a -> t

val (@>>) : 'a comparator -> 'a comparator -> 'a comparator
  (** Combination of comparators that work on the same values *)

type ('a, 'b) combination
  (** Lexicographic combination of comparators. It is, roughly,
      equivalent to ['a -> 'a -> 'b] *)

val (>>>) : 'a comparator -> ('b, 'c) combination -> ('a, 'b -> 'b -> 'c) combination
  (** Lexicographic combination starting with the given function *)

val last : 'a comparator -> ('a, t) combination
  (** Last comparator *)

val call : ('a, 'b) combination -> 'a -> 'a -> 'b
  (** Call a lexicographic combination on arguments *)

module type PARTIAL_ORD = sig
  type t

  val partial_cmp : t -> t -> comparison
end
