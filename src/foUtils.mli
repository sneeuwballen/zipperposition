(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** {1 Some helpers} *)

open Basic

(** {2 debugging facilities} *)

val set_debug : int -> unit               (** set the level of debug *)
val debug : int ->  ('a, Format.formatter, unit) format -> 'a  (** debug message with level *)
val debug_level : unit -> int             (** current debug level *)

val need_cleanup : bool ref               (** Is cleanup of current line necessary? *)

(** {2 Time facilities} *)

(** time elapsed since start of program *)
val get_total_time : unit -> float

(** time at which the program started *)
val get_start_time : unit -> float

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
(** lexicographic partial order on lists. If lengths are different, returns Incomparable *)
val lexicograph_partial : ('a -> 'b -> comparison) -> 'a list -> 'b list -> comparison

(** conversion from partial order to a total order, in which incomparable
    elements are considered to be in the same congruence class *)
val partial_to_total : ('a -> 'b -> comparison) -> 'a -> 'b -> int
val total_to_partial : ('a -> 'b -> int) -> 'a -> 'b -> comparison
val or_partial : comparison -> comparison -> comparison
val not_partial : comparison -> comparison

(** the opposite order, that sorts elements the opposite way *)
val opposite_order : ('a -> 'b -> int) -> 'a -> 'b -> int

(** multiset equality given partial order f *)
val multiset_eq : ('a -> 'a -> comparison) -> 'a list -> 'a list -> bool
(** multiset order on lists which elements are ordered by f *)
val multiset_partial : ('a -> 'a -> comparison) -> 'a list -> 'a list -> comparison

(** {2 Hashconsing with non-weak semantic} *)

(** Hashconsed elements are kept forever by default;  *)

module KeepHashcons(H : Hashcons.HashedType) : Hashcons.S with type t = H.t

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
(** flatten map *)
val list_flatmap : ('a -> 'b list) -> 'a list -> 'b list
(** take n elements *)
val list_take : int -> 'a list -> 'a list
(** drop n elements *)
val list_drop : int -> 'a list -> 'a list
(** Extract the minimum objects of the list *)
val list_min : ('a -> 'a -> comparison) -> 'a list -> 'a list
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

(** {2 String utils} *)

val str_split : by:string -> string -> string list

(** {2 File utils} *)

(** perform the action with a lock on the given file *)
val with_lock_file : string -> (unit -> 'a) -> 'a

(** Open the given file for reading, and returns
    the result of the action applied to the input channel *)
val with_input : string -> (in_channel -> 'a) -> 'a option

(** Open the given file for writing, and returns
    the result of the action applied to the output channel *)
val with_output : string -> (out_channel -> 'a) -> 'a option

(** {2 Pretty-printing utils} *)

(** pretty-print into a string *)
val on_buffer: ?margin:int -> (Format.formatter -> 'a -> 'b) -> 'a -> string
val sprintf : ('a, Format.formatter, unit, string) format4 -> 'a
(** print a list of items using the printing function *)
val pp_list: ?sep:string -> (Format.formatter -> 'a -> unit)
          -> Format.formatter -> 'a list -> unit
(** print an array of items with printing function *)
val pp_array: ?sep:string -> (Format.formatter -> 'a -> unit)
          -> Format.formatter -> 'a array -> unit
(** print an array, giving the printing function both index and item *)
val pp_arrayi: ?sep:string -> (Format.formatter -> int -> 'a -> unit)
          -> Format.formatter -> 'a array -> unit
