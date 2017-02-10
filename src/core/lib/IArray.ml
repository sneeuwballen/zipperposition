
(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Immutable Arrays} *)

type 'a equal = 'a -> 'a -> bool
type 'a ord = 'a -> 'a -> int

type 'a t = 'a array

let of_list = Array.of_list

let to_list = Array.to_list

let to_array = Array.copy

let to_array_unsafe a = a
let of_array_unsafe a = a (* bleh. *)

let empty = [| |]

let length = Array.length

let singleton x = [| x |]

let doubleton x y = [| x; y |]

let make n x = Array.make n x

let init n f = Array.init n f

let get = Array.get

let set a n x =
  let a' = Array.copy a in
  a'.(n) <- x;
  a'

let map = Array.map
let map_arr = Array.map

let mapi = Array.mapi
let mapi_arr = Array.mapi

let append a b =
  let na = length a in
  Array.init (na + length b)
    (fun i -> if i < na then a.(i) else b.(i-na))

let iter = Array.iter

let iteri = Array.iteri

let fold = Array.fold_left

let foldi f acc a =
  let n = ref 0 in
  Array.fold_left
    (fun acc x ->
       let acc = f acc !n x in
       incr n;
       acc)
    acc a

exception ExitNow

let for_all p a =
  try
    Array.iter (fun x -> if not (p x) then raise ExitNow) a;
    true
  with ExitNow -> false

let exists p a =
  try
    Array.iter (fun x -> if p x then raise ExitNow) a;
    false
  with ExitNow -> true

let equal eq a b =
  let rec aux i =
    if i = Array.length a then true
    else eq a.(i) b.(i) && aux (i+1)
  in
  Array.length a = Array.length b
  &&
  aux 0

let compare cmp a b =
  let rec aux i =
    if i = Array.length a
    then if i = Array.length b then 0 else -1
    else if i = Array.length b
    then 1
    else
      let c = cmp a.(i) b.(i) in
      if c = 0 then aux (i+1) else c
  in
  aux 0

let to_seq a k = iter k a
let to_seqi a k = iteri (fun i x -> k(i,x)) a

let of_seq s =
  let l = ref [] in
  s (fun x -> l := x :: !l);
  Array.of_list (List.rev !l)

let hash f a = Hash.array f a

let hash_comm f a =
  let arr = Array.init (Array.length a) (fun i -> f a.(i)) in
  Array.sort CCInt.compare arr; (* sort the hashes, so their order does not matter *)
  Hash.array (fun h->h) arr
