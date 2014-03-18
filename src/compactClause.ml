(*
Zipperposition: a functional superposition prover for prototyping
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

(** {1 Compact clause representation} *)

open Logtk

module F = Formula.FO

type form = F.t

type t = form array lazy_t

let eq (lazy c1) (lazy c2) =
  try
    Util.array_forall2 F.eq c1 c2
  with Invalid_argument _ ->
    false

let hash (lazy c) =
  Array.fold_left (fun h f -> Hash.combine h (F.hash f)) 143 c

(* TODO: optimize *)
let cmp (lazy c1) (lazy c2) =
  Util.lexicograph F.cmp (Array.to_list c1) (Array.to_list c2)

let is_empty (lazy c) = Array.length c = 0

let iter (lazy c) f = Array.iter f c

let to_seq c = Sequence.from_iter (fun k -> Array.iter k (Lazy.force c))

let pp buf c =
  match c with
  | lazy [| |] -> Buffer.add_string buf "$false"
  | lazy [| x |] -> F.pp buf x
  | lazy l -> Printf.bprintf buf "[%a]" (Util.pp_array ~sep:" | " F.pp) l

let pp_tstp buf c =
  match c with
  | lazy [| |] -> Buffer.add_string buf "$false"
  | lazy [| x |] -> F.TPTP.pp buf x
  | lazy l -> Printf.bprintf buf "(%a)" (Util.pp_array ~sep:" | " F.TPTP.pp) l

let to_string c =
  Util.on_buffer pp c

let fmt fmt c =
  Format.pp_print_string fmt (to_string c)

