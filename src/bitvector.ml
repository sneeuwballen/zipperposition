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
type bitvector = int

(** make a bitvector of size n with all bits set *)
let bv_make n =
  assert (n <= 31);
  let rec shift bv n = if n = 0 then bv else shift ((bv lsl 1) lor 1) (n-1)
  in shift 0 n

(** bitvector n-th element is true? *)
let bv_get bv n = (bv land (1 lsl n)) <> 0 

(** set n-th element of bitvector *)
let bv_set bv n = bv lor (1 lsl n)

(** reset n-th element of bitvector *)
let bv_clear bv n = bv land (lnot (1 lsl n))

(** is bitvector empty? *)
let bv_empty bv = bv = 0
