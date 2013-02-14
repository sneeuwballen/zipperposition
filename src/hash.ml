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

(** SDBM simple hash (see for instance http://www.cse.yorku.ca/~oz/hash.html) *)
let combine hash i =
  abs (hash * 65599 + i)

(** Hash a single int *)
let hash_int i = combine 0 i

(** Hash two ints *)
let hash_int2 i j = combine (combine 0 i) j

(** Hash three ints *)
let hash_int3 i j k = combine (combine (combine 0 i) j) k

(** Hash four ints *)
let hash_int4 i j k l =
  combine (combine (combine (combine 0 i) j) k) l

(** Hash a list. Each element is hashed using [f]. *)
let hash_list f h l = List.fold_left (fun h x -> combine h (f x)) h l

(** Hash string *)
let hash_string s = Hashtbl.hash s

