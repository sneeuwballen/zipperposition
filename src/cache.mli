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

(** An imperative cache for memoization *)

module type S =
  sig 
    type key

    module H : Hashtbl.S with type key = key

    type 'a t = 'a H.t 

    (** create a cache with given size *)
    val create : int -> 'a t

    (** try to find the value associated to the argument in the cache.
        If not found, the value will be computed using the function, then
        saved in the cache. *)
    val with_cache : 'a t -> (key -> 'a) -> key -> 'a

    (** clear the cache from its content *)
    val clear : 'a t -> unit
  end

module type CachedType =
  sig
    include Hashtbl.HashedType

    (** decide whether this value should be memoized in the cache *)
    val should_cache : t -> bool
  end

(** functorial implementation *)
module Make(CType : CachedType) : S with type key = CType.t
