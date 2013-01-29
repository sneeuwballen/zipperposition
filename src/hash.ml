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

(** Murmur hash for ints, cf http://en.wikipedia.org/wiki/MurmurHash *)
let hash_4bytes hash i =
  let m = 0x1bd1e995 in  (* 0x5bd1e995 in 31 bits *)
  let r = 24 in
  let k = i in
  let k = k * m in
  let k = k lxor (k lsr r) in
  let k = k * m in
  let hash = (hash * m) lxor k in
  let hash = hash lxor (hash lsr 13) in
  let hash = hash lxor (hash lsr 15) in
  hash

(* initial hash for a sequence of [len] bytes *)
let initial_hash len = 
  let seed = 0x1747b28c in  (* 0x9747b28c in 31 bits *)
  let hash = seed lxor len in
  hash

(* to call after hashing bytes of the sequence *)
let finish_hash hash = 
  let m = 0x1bd1e995 in  (* 0x5bd1e995 in 31 bits *)
  let hash = hash lxor (hash lsr 13) in
  let hash = hash * m in
  let hash = hash lxor (hash lsr 15) in
  hash

(** Hash a single int *)
let hash_int i =
  let hash = initial_hash 4 in
  let hash = hash_4bytes hash i in
  let hash = finish_hash hash in
  abs hash

(** Hash two ints *)
let hash_int2 i j =
  let hash = initial_hash 8 in
  let hash = hash_4bytes hash i in
  let hash = hash_4bytes hash j in
  let hash = finish_hash hash in
  abs hash

(** Hash three ints *)
let hash_int3 i j k =
  let hash = initial_hash 12 in
  let hash = hash_4bytes hash i in
  let hash = hash_4bytes hash j in
  let hash = hash_4bytes hash k in
  let hash = finish_hash hash in
  abs hash

(** Hash four ints *)
let hash_int4 i j k l =
  let hash = initial_hash 16 in
  let hash = hash_4bytes hash i in
  let hash = hash_4bytes hash j in
  let hash = hash_4bytes hash k in
  let hash = hash_4bytes hash l in
  let hash = finish_hash hash in
  abs hash

(** Hash int list *)
let hash_int_list l =
  let hash = initial_hash (List.length l * 4) in
  let rec aux hash l = match l with
  | [] -> hash
  | x::l' ->
    let hash = hash_4bytes hash x in
    aux hash l'
  in
  let hash = aux hash l in
  let hash = finish_hash hash in
  abs hash

(** Hash string *)
let hash_string s = Hashtbl.hash s

(* pseudo code from wikipedia:
Murmur2(key, len, seed)
    m \gets 0x5bd1e995
    r \gets 24
    seed \gets 0x9747b28c


    hash \gets seed XOR len

    for each fourByteChunk of key
        k \gets fourByteChunk

        k \gets k * m
        k \gets k XOR (k >> r)
        k \gets k * m

        hash \gets hash * m
        hash \gets hash XOR k

    with any remainingBytesInKey
        remainingBytes \gets SwapEndianOrderOf(remainingBytesInKey)

        hash \gets hash XOR remainingBytes
        hash \gets hash * m

    hash \gets hash XOR (hash >> 13)
    hash \gets hash * m
    hash \gets hash XOR (hash >> 15)
*)
