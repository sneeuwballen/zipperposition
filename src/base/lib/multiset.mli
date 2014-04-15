
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

(** {6 Generic multisets} *)

(** Those multiset are not optimized for high-cardinality of single
    elements, but rather for operations such as multiset comparisons
*)

type 'a t
  (** A multiset of elements of type 'a *)

val of_list : 'a list -> 'a t
  (** Multiset from list *)

val create : 'a IArray.t -> 'a t
  (** Non-copying creation. *)

val create_unsafe : 'a array -> 'a t
  (** Non-copying creation.  The array is used by the multiset, so it should
      not be modified later! *)

val size : 'a t -> int
  (** Number of distinct occurrences of elements *)

val is_empty : 'a t -> bool
  (** Is the multiset empty? *)

val iter : 'a t -> ('a -> unit) -> unit
  (** Iterate on distinct occurrences of elements *)

val to_array : 'a t -> 'a IArray.t
  (** Extract the underlying array *)

val to_list : 'a t -> 'a list

val get : 'a t -> int -> 'a
  (** [get m i] returns the i-th element ([i] must be < [size m]) *)

val eq : ('a -> 'a -> Comparison.t) -> 'a t -> 'a t -> bool
  (** Check equality of two multisets *)

val compare : ('a -> 'a -> Comparison.t) -> 'a t -> 'a t -> Comparison.t
  (** Compare two multisets with the multiset extension of the
      given ordering *)

val is_max : ('a -> 'a -> Comparison.t) -> 'a -> 'a t -> bool
  (** Is the given element maximal (ie not dominated) within the multiset? *)

val max : ('a -> 'a -> Comparison.t) -> 'a t -> BV.t
  (** Maximal elements of the multiset, w.r.t the given ordering. *)

val max_l : ('a -> 'a -> Comparison.t) -> 'a list -> 'a list
  (** Maximal elements of a list *)

val compare_l : ('a -> 'a -> Comparison.t) -> 'a list -> 'a list -> Comparison.t
  (** Compare two multisets represented as list of elements *)
