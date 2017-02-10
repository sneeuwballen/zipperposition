
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

(** {1 Immutable Arrays} *)

type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int

type 'a t
(** Array of values of type 'a *)

val of_list : 'a list -> 'a t

val to_list : 'a t -> 'a list

val to_array : 'a t -> 'a array
(** make a copy into a mutable array *)

val to_array_unsafe : 'a t -> 'a array
(** Show the underlying array. DO NOT MODIFY *)

val of_array_unsafe : 'a array -> 'a t
(** Take ownership of the given array. Careful, the array must {b NOT}
    be modified afterwards! *)

val empty : 'a t

val length : _ t -> int

val singleton : 'a -> 'a t

val doubleton : 'a -> 'a -> 'a t

val make : int -> 'a -> 'a t

val init : int -> (int -> 'a) -> 'a t

val get : 'a t -> int -> 'a

val set : 'a t -> int -> 'a -> 'a t
(** Copy the array and modify its copy *)

val map : ('a -> 'b) -> 'a t -> 'b t

val map_arr : ('a -> 'b) -> 'a t -> 'b array

val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

val mapi_arr : (int -> 'a -> 'b) -> 'a t -> 'b array

val append : 'a t -> 'a t -> 'a t

val iter : ('a -> unit) -> 'a t -> unit

val iteri : (int -> 'a -> unit) -> 'a t -> unit

val foldi : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

val for_all : ('a -> bool) -> 'a t -> bool

val exists : ('a -> bool) -> 'a t -> bool

val equal : 'a equal -> 'a t equal

val compare : 'a ord -> 'a t ord

val hash : 'a Hash.t -> 'a t Hash.t

val hash_comm : 'a Hash.t -> 'a t Hash.t
(** Commutative hash *)

val to_seq : 'a t -> 'a Sequence.t
val to_seqi : 'a t -> (int * 'a) Sequence.t
val of_seq : 'a Sequence.t -> 'a t
