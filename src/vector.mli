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

(** Growable, mutable vector *)

type 'a vector
  (** the type of a vector of 'a *)

val create : int -> 'a vector
  (** create a vector of given initial capacity *)

val clear : 'a vector -> unit
  (** clear the content of the vector *)

val is_empty : 'a vector -> bool
  (** is the vector empty? *)

val push : 'a vector -> 'a -> unit
  (** add an element at the end of the vector *)

val pop : 'a vector -> 'a
  (** remove last element, or raise a Failure if empty *)

val shrink : 'a vector -> int -> unit
  (** shrink to the given size (remove elements above this size) *)

val iter : 'a vector -> ('a -> unit) -> unit
  (** iterate on the vector *)

val iteri : 'a vector -> (int -> 'a -> unit) -> unit
  (** iterate on the vector with indexes *)

val map : 'a vector -> ('a -> 'b) -> 'b vector
  (** map elements of the vector *)

val fold : 'a vector -> 'b -> ('b -> 'a -> 'b) -> 'b
  (** fold on elements of the vector *)

val get : 'a vector -> int -> 'a
  (** access element, or Failure if bad index *)

val set : 'a vector -> int -> 'a -> unit
  (** access element, or Failure if bad index *)

val size : 'a vector -> int
  (** number of elements in vector *)

val from_array : 'a array -> 'a vector
val from_list : 'a list -> 'a vector
val to_array : 'a vector -> 'a array
val to_list : 'a vector -> 'a list

