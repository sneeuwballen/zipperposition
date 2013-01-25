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

(** Basic union-find for arbitrary objects. It operates on hashed objects. *)

(** We need to be able to hash and compare keys, and values need to form
    a monoid *)
module type PAIR =
  sig
    type key
    type value

    val hash : key -> int
    val equal : key -> key -> bool

    val merge : value -> value -> value
    val zero : value
  end

(** Build a union-find module from a key/value specification *)
module Make(P : PAIR) :
  sig
    type key = P.key
      (** Elements that can be compared *)

    type value = P.value
      (** Values associated with elements *)

    type t
      (** The union-find imperative structure itself*)

    val create : key list -> t
      (** Create a union-find for the given elements *)

    val find : t -> key -> value
      (** Find value for the given element *)

    val union : t -> key -> key -> unit
      (** Merge two elements *)

    val add : t -> key -> value -> unit
      (** Add the given value to the key (monoid) *)

    val iter : t -> (key -> value -> unit) -> unit
      (** Iterate on representative and their value *)
  end
