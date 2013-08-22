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

(** {1 Some helpers} *)

(** {2 Time facilities} *)

(** time elapsed since start of program *)
val get_total_time : unit -> float

(** time at which the program started *)
val get_start_time : unit -> float

(** {2 Misc} *)
 
val set_debug : int -> unit     (** Set debug level *)
val get_debug : unit -> int     (** Current debug level *)

val debug : int -> ('a, Buffer.t, unit, unit) format4 -> 'a
  (** debug message *)

val pp_pos : Lexing.position -> string

(** {2 Infix operators} *)

module Infix : sig
  val (|>) : 'a -> ('a -> 'b) -> 'b
    (** Application pipeline *)

  val (%>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
    (** Function composition *)

  val (%%) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
    (** Function composition *)
end

(** {2 profiling facilities} *)

type profiler
val enable_profiling : bool ref           (** Enable/disable profiling *)
val mk_profiler : string -> profiler      (** Create a named profiler *)
val enter_prof : profiler -> unit         (** Enter the profiler *)
val exit_prof : profiler -> unit          (** Exit the profiler *)

(** {2 Ordering utils} *)

(** lexicographic order on lists l1,l2 which elements are ordered by f *)
val lexicograph : ('a -> 'b -> int) -> 'a list -> 'b list -> int
(** combine comparisons by lexicographic order *)
val lexicograph_combine : int list -> int

(** the opposite order, that sorts elements the opposite way *)
val opposite_order : ('a -> 'b -> int) -> 'a -> 'b -> int

(** {2 List utils} *)

(** get n-th element of list (linear), or Not_found *)
val list_get : 'a list -> int -> 'a
(** set n-th element of list (linear) *)
val list_set : 'a list -> int -> 'a -> 'a list
(** map with index *)
val list_mapi : 'a list -> (int -> 'a -> 'b) -> 'b list
(** iter with index *)
val list_iteri : 'a list -> (int -> 'a -> unit) -> unit
(** all the list but i-th element (linear) *)
val list_remove : 'a list -> int -> 'a list
(** zip the list with positions (starting at 0) *)
val list_pos : 'a list -> ('a * int) list
(** test for membership using the given comparison function *)
val list_mem : ('a -> 'a -> bool) -> 'a -> 'a list -> bool
(** test for inclusion *)
val list_subset : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
(** list uniq: remove duplicates w.r.t the equality predicate *)
val list_uniq : ('a -> 'a -> bool) -> 'a list -> 'a list
(** merges elements from both sorted list, removing duplicates *)
val list_merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
(** list union, given the comparison function *)
val list_union : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
(** list intersection, given the comparison function *)
val list_inter : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list
(** find the first index of element, elemnt s.t. the element satisfies the predicate *)
val list_find : ('a -> bool) -> 'a list -> (int * 'a) option
(** filter map *)
val list_fmap : ('a -> 'b option) -> 'a list -> 'b list
(** flatten map *)
val list_flatmap : ('a -> 'b list) -> 'a list -> 'b list
(** take n elements *)
val list_take : int -> 'a list -> 'a list
(** drop n elements *)
val list_drop : int -> 'a list -> 'a list
(** range from i to j *)
val list_range : int -> int -> int list
(** call the function n times with unit *)
val times : int -> (unit -> 'a) -> 'a list

(** shuffle randomly the array, in place *)
val array_shuffle : 'a array -> unit
(** shuffle randomly the list *)
val list_shuffle : 'a list -> 'a list

(** {2 Array utils} *)

(** fold left on array, with index *)
val array_foldi : ('b -> int -> 'a -> 'b) -> 'b -> 'a array -> 'b
(** Forall on array *)
val array_forall : ('a -> bool) -> 'a array -> bool
(** Forall on pairs of arrays (Invalid_argument if they have distinct lengths) *)
val array_forall2 : ('a -> 'a -> bool) -> 'a array -> 'a array -> bool
(** exists on array *)
val array_exists : ('a -> bool) -> 'a array -> bool
(** Elements of array except the one at given index (reverse list) *)
val array_except_idx : 'a array -> int -> 'a list

(** {2 Finite Bijections} *)

module Bijection(X : Hashtbl.HashedType) : sig
  type elt = X.t
  type t
  
  val of_array : elt array -> elt array -> t
  val of_list : elt list -> elt list -> t

  val is_permutation : t -> bool
    (** Is the bijection a permutation (ie, domain = codomain)? *)

  val apply : t -> elt -> elt

  val apply_strict : t -> elt -> elt
    (** Like {! apply}, but fails if the element is not in the
        domain of the bijection by raising Not_found *)

  val apply_list : t -> elt list -> elt list

  val compose : t -> t -> t
    (** [compose p1 p2 x] is the same as [p1 (p2 x)] *)

  val compose_list : t list -> t

  val rev : t -> t
    (** Reverse bijection *)

  val to_list : t -> (elt * elt) list
end

(** {2 String utils} *)

val str_sub : sub:string -> int -> string -> int -> bool
  (** Equality from the given start position *)

val str_split : by:string -> string -> string list

val str_find : ?start:int -> sub:string -> string -> int
  (** Find [sub] in the string, returns its first index or -1 *)

(** {2 File utils} *)

(** perform the action with a lock on the given file *)
val with_lock_file : string -> (unit -> 'a) -> 'a

(** Open the given file for reading, and returns
    the result of the action applied to the input channel *)
val with_input : string -> (in_channel -> 'a) -> 'a option

(** Open the given file for writing, and returns
    the result of the action applied to the output channel *)
val with_output : string -> (out_channel -> 'a) -> 'a option

(** {2 Printing utils} *)

(** print into a string *)
val sprintf : ('a, Buffer.t, unit, string) format4 -> 'a

val fprintf : out_channel -> ('a, Buffer.t, unit, unit) format4 -> 'a

val printf : ('a, Buffer.t, unit, unit) format4 -> 'a
val eprintf : ('a, Buffer.t, unit, unit) format4 -> 'a

val pp_pair: ?sep:string -> (Buffer.t -> 'a -> unit) ->
              (Buffer.t -> 'b -> unit) -> Buffer.t -> ('a * 'b) -> unit

(** print a list of items using the printing function *)
val pp_list: ?sep:string -> (Buffer.t -> 'a -> unit)
          -> Buffer.t -> 'a list -> unit

(** print an array of items with printing function *)
val pp_array: ?sep:string -> (Buffer.t -> 'a -> unit)
          -> Buffer.t -> 'a array -> unit

(** print an array, giving the printing function both index and item *)
val pp_arrayi: ?sep:string -> (Buffer.t -> int -> 'a -> unit)
          -> Buffer.t -> 'a array -> unit

(** Print the sequence *)
val pp_seq : ?sep:string -> (Buffer.t -> 'a -> unit)
          -> Buffer.t -> 'a Sequence.t -> unit
