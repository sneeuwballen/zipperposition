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

(* bitvectors of at most 31 bits *)

(** a bitvector is just an int *)
type t = int

let max_len = Sys.word_size - 1

(** make a bitvector of size n with all bits set *)
let make n =
  assert (n <= max_len);
  let rec shift bv n = if n = 0 then bv else shift ((bv lsl 1) lor 1) (n-1)
  in shift 0 n

(** Empty bitvector *)
let empty = 0

(** bitvector n-th element is true? *)
let get bv n = (bv land (1 lsl n)) <> 0 

(** set n-th element of bitvector *)
let set bv n = bv lor (1 lsl n)

(** reset n-th element of bitvector *)
let clear bv n = bv land (lnot (1 lsl n))

(** is bitvector empty? *)
let is_empty bv = bv = 0

(** intersection of BV *)
let inter a b = a land b

(** union of BV *)
let union a b = a lor b

(** Iterate on set bits (naive) *)
let iter bv f =
  for i = 0 to max_len do
    if get bv i then f i;
  done

(** From a list of ints *)
let from_list l = List.fold_left set empty l

(** To a list of ints (naive) *)
let to_list bv =
  let l = ref [] in
  for i = 0 to max_len do
    if get bv i then l := i :: !l;
  done;
  !l

(** select elements of the array, that are flaged as true, into a ('a, int) list *)
let select bv a =
  assert (Array.length a <= max_len);
  let rec select i =
    if i = Array.length a then []
    else if get bv i then (a.(i), i) :: select (i+1)
    else select (i+1)
  in select 0
