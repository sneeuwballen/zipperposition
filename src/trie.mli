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

module type Map =
  sig
    type key
    type (+'a) t
    val empty: 'a t
    val is_empty: 'a t -> bool
    val mem: key -> 'a t -> bool
    val add: key -> 'a -> 'a t -> 'a t
    val remove: key -> 'a t -> 'a t
    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val find: key -> 'a t -> 'a
    val map: ('a -> 'b) -> 'a t -> 'b t
    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
  end

module Make :
  functor (M : Map) ->
    sig
      type key = M.key list
      type 'a t = Node of 'a option * 'a t M.t
      val empty : 'a t
      val find : M.key list -> 'a t -> 'a
      val mem : M.key list -> 'a t -> bool
      val add : M.key list -> 'a -> 'a t -> 'a t
      val remove : M.key list -> 'a t -> 'a t
      val map : ('a -> 'b) -> 'a t -> 'b t
      val mapi : (M.key list -> 'a -> 'b) -> 'a t -> 'b t
      val iter : (M.key list -> 'a -> 'b) -> 'a t -> unit
      val fold : (M.key list -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
      val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
      val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
      val is_empty : 'a t -> bool
    end
