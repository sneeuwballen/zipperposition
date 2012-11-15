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

(** an imperative cache for memoization *)

module type S =
  sig 
    type key

    type 'a t

    (** create a cache with given size *)
    val create : int -> (key -> 'a) -> 'a t

    (** change the producing function *)
    val set_fun : 'a t -> (key -> 'a) -> unit

    (** find a value in the cache *)
    val lookup : 'a t -> key -> 'a

    (** clear the cache from its content *)
    val clear : 'a t -> unit
  end

module type CachedType =
  sig
    type t
    val hash : t -> int
    val equal : t -> t -> bool
  end


module Make(HType : CachedType) =
  struct
    type key = HType.t

    (** A slot of the array contains a (key, value, true)
        if key->value is stored there (at index hash(key) % length),
        (null, null, false) otherwise.
        
        The first slot in the array contains the function
        used to produce the value upon a cache miss. *)
    type 'a t = (key * 'a * bool) array

    let my_null = (Obj.magic None, Obj.magic None, false)

    let set_fun c f = c.(0) <- Obj.magic f

    let create size f =
      let c = Array.create (size+1) my_null in
      c.(0) <- Obj.magic f;
      c

    let lookup c k =
      let i = ((HType.hash k) mod (Array.length c -1)) + 1 in
      match c.(i) with
      | (_, _, false) ->
        let v = ((Obj.magic c.(0)) : key -> 'a) k in
        c.(i) <- (k, v, true); v
      | (k', _, true) when not (HType.equal k k') ->
        let v = ((Obj.magic c.(0)) : key -> 'a) k in
        c.(i) <- (k, v, true); v
      | (_, v, true) -> v

    let clear c =
      let f = c.(0) in
      Array.iteri (fun i _ -> c.(i) <- my_null) c;
      c.(0) <- f
  end
