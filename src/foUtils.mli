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

(** Some helpers *)

open Types

val set_debug : int -> unit               (** set the level of debug *)
val debug : int -> string Lazy.t -> unit  (** debug message with level *)
val debug_level : unit -> int             (** current debug level *)

(** hashing on ints, cf http://en.wikipedia.org/wiki/MurmurHash *)
val murmur_hash : int -> int

(** Hashset for strings *)
module SHashSet :
  sig
    type t
    val create : unit -> t
    val member : t -> string -> bool
    val add : t -> string -> unit
    val from_list : string list -> t
  end

(** lexicographic order on lists l1,l2 which elements are ordered by f *)
val lexicograph : ('a -> 'b -> int) -> 'a list -> 'b list -> int
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

(* TODO merge this and hExtlib *)

(** get n-th element of list (linear), or Not_found *)
val list_get : 'a list -> int -> 'a
(** set n-th element of list (linear) *)
val list_set : 'a list -> int -> 'a -> 'a list
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
(** flatten map *)
val list_flatmap : ('a -> 'b list) -> 'a list -> 'b list
(** take n elements *)
val list_take : int -> 'a list -> 'a list
(** Extract the minimum objects of the list *)
val list_min : ('a -> 'a -> comparison) -> 'a list -> 'a list
(** range from i to j *)
val list_range : int -> int -> int list
(** call the function n times with unit *)
val times : int -> (unit -> 'a) -> 'a list

(** pretty-print into a string *)
val on_buffer: ?margin:int -> (Format.formatter -> 'a -> 'b) -> 'a -> string
val sprintf : ('a, Format.formatter, unit, string) format4 -> 'a
(* print a list of items using the printing function *)
val pp_list: ?sep:string -> (Format.formatter -> 'a -> unit)
          -> Format.formatter -> 'a list -> unit
