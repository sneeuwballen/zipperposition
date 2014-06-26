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
module Lit = Literal
module Lits = Literals

type form = F.t

type t = Literal.t array

let eq = Lits.eq_com
let hash_fun = Lits.hash_fun
let hash = Lits.hash
let cmp = Lits.compare

let is_empty c = Array.length c = 0

let iter c f = Array.iter f c

let to_seq c = Sequence.of_array c

let pp buf c =
  match c with
  | [| |] -> Buffer.add_string buf "⊥"
  | [| x |] -> Lit.pp buf x
  | l -> Printf.bprintf buf "%a" (Util.pp_array ~sep:" ∨ " Lit.pp) l

let pp_tstp buf c =
  match c with
  | [| |] -> Buffer.add_string buf "$false"
  | [| x |] -> Lit.pp_tstp buf x
  | l -> Printf.bprintf buf "(%a)" (Util.pp_array ~sep:" | " Lit.pp_tstp) l

let to_forms c =
  Array.map (fun l -> Lit.Conv.to_form l) c

let to_string c =
  Util.on_buffer pp c

let fmt fmt c =
  Format.pp_print_string fmt (to_string c)

