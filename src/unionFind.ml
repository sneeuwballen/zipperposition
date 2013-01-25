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
module Make(P : PAIR) =
  struct
    (** Elements that can be compared *)
    type key = P.key

    (** Values associated with elements *)
    type value = P.value

    module H = Hashtbl.Make(struct include P type t = P.key end)

    (** The union-find imperative structure itself*)
    type t = (key * value) H.t

    (** Elements that can be compared *)
    let create keys =
      let t = H.create 5 in
      (* add k -> zero for each key k *)
      List.iter (fun key -> H.replace t key (key, P.zero)) keys;
      t

    (** Find representative value for this key. No path compression (yet) *)
    let rec find_root t key =
      let key', value = H.find t key in
      (* if key is its own representative, done; otherwise recurse toward key's root *)
      if P.equal key key' then key, value else find_root t key'

    (** Get value of the root for this key. *)
    let rec find t key = snd (find_root t key)

    (** Merge two representatives *)
    let union t k1 k2 =
      let (k1,v1), (k2,v2) = find_root t k1, find_root t k2 in
      (* k2 points to k1, and k1 points to the new value *)
      H.replace t k1 (k1, P.merge v1 v2);
      H.replace t k2 (k1, P.zero)

    (** Add the given value to the key (monoid) *)
    let add t key value =
      let key',value' = find_root t key in
      H.replace t key' (key', P.merge value value')

    (** Iterate on representative and their value *)
    let iter t f =
      H.iter
        (fun key (key', value) -> if P.equal key key' then f key value)
        t
  end
