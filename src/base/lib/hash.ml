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

(** SDBM simple hash (see for instance http://www.cse.yorku.ca/~oz/hash.html) *)
let combine hash i =
  abs (hash * 65599 + i)

(** Hash a single int *)
let hash_int i = combine 0 i

(** Hash two ints *)
let hash_int2 i j = combine i j

(** Hash three ints *)
let hash_int3 i j k = combine (combine i j) k

(** Hash four ints *)
let hash_int4 i j k l =
  combine (combine (combine i j) k) l

(** Hash a list. Each element is hashed using [f]. *)
let rec hash_list f h l = match l with
  | [] -> h
  | x::l' -> hash_list f (combine h (f x)) l'

let hash_array f h a =
  let h = ref h in
  Array.iter (fun x -> h := combine !h (f x)) a;
  !h

(** Hash string *)
let hash_string s = Hashtbl.hash s

(** Hash sequence *)
let hash_seq f h seq =
  let h = ref h in
  seq (fun x -> h := combine !h (f x));
  !h
