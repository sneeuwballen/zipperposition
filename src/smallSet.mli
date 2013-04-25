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

(** {1 Small set structure} *)

type 'a t
  (** Set of elements of type 'a *)

val empty : cmp:('a -> 'a -> int) -> 'a t
  (** Create an empty set *)

val is_empty : 'a t -> bool
  (** Is the set empty? *)

val mem : 'a t -> 'a -> bool
  (** Is the element member of the set? *)

val add : 'a t -> 'a -> 'a t
  (** add an element *)

val remove : 'a t -> 'a -> 'a t
  (** Remove element *)

val choose : 'a t -> 'a
  (** Some element of the set, of Not_found *)

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** Fold on elements *)

val iter : ('a -> unit) -> 'a t -> unit
  (** Iterate on elements *)

val size : _ t -> int
  (** Number of elements *)

val to_seq : 'a t -> 'a Sequence.t

val of_seq : 'a t -> 'a Sequence.t -> 'a t

val to_list : 'a t -> 'a list

val of_list : 'a t -> 'a list -> 'a t
