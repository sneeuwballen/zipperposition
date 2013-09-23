
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

open Logtk

module T = Term
module C = Clause
module Lit = Literal

let prof_simplify = Util.mk_profiler "ac.simplify"

type spec = {
  is_ac : Symbol.t -> bool;
} (** Specification: which symbols are AC? *)

let is_trivial_lit ~spec lit =
  let is_ac = spec.is_ac in
  match lit with
  | Lit.Equation (l, r, true, _) -> T.ac_eq ~is_ac l r
  | Lit.Equation _
  | Lit.Prop _
  | Lit.False -> false
  | Lit.True -> true

let is_trivial ~spec c =
  Util.array_exists (is_trivial_lit ~spec) c.C.hclits

(* simplify: remove literals that are redundant modulo AC *)
let simplify ~spec ~ctx c =
  Util.enter_prof prof_simplify;
  let n = Array.length c.C.hclits in
  let is_ac = spec.is_ac in
  let lits = Array.to_list c.C.hclits in
  let lits = List.filter
    (fun lit -> match lit with
    | Lit.Equation (l, r, false, _) -> not (T.ac_eq ~is_ac l r)
    | Lit.Equation _
    | Lit.Prop _
    | Lit.False
    | Lit.True -> true)
    lits
  in
  let n' = List.length lits in
  if n' < n && not (C.get_flag C.flag_persistent c)
    then begin
      let proof cc = Proof.mk_c_step cc ~rule:"ac" [c.C.hcproof] in
      let parents = c :: c.C.hcparents in
      let new_c = C.create ~parents ~ctx lits proof in
      Util.exit_prof prof_simplify;
      Util.debug 3 "%a AC-simplify into %a" C.pp c C.pp new_c;
      new_c
    end else
      let _ = Util.exit_prof prof_simplify in
      c (* no simplification *)
