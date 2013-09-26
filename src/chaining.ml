
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

(** {6 Chaining Inferences} *)

open Logtk

module T = Term
module C = Clause
module I = ProofState.TermIndex
module Lit = Literal
module Lits = Literal.Arr
module TO = Theories.TotalOrder

let prof_eq_chaining_active = Util.mk_profiler "chaining.eq_active"
let prof_eq_chaining_passive = Util.mk_profiler "chaining.eq_passive"
let prof_ineq_chaining_active = Util.mk_profiler "chaining.ineq_active"
let prof_ineq_chaining_passive = Util.mk_profiler "chaining.ineq_passive"

(* perform equality chaining *)
let do_eq_chaining ~ctx active s_a active_pos passive s_p passive_pos subst acc =
  assert false (* TODO *)

(* use [s = t] to rewrite subterms of active clauses that unify with [s]
  into [t] *)
let eq_chaining_active active_set c =
  Util.enter_prof prof_eq_chaining_active;
  let ctx = active_set#ctx in
  let scope = T.max_var c.C.hcvars + 1 in
  let eligible = C.Eligible.param c in
  (* fold on eq lits *)
  let new_clauses = Lits.fold_eqn ~both:true ~eligible c.C.hclits []
    (fun acc s t _ s_pos ->
      I.retrieve_unifiables active_set#idx_ord_subterm scope s 0 acc
        (fun acc u_p with_pos subst ->
          let passive = with_pos.C.WithPos.clause in
          let u_pos = with_pos.C.WithPos.pos in
          do_eq_chaining ~ctx c 0 s_pos passive scope u_pos subst acc
        )
    )
  in
  Util.exit_prof prof_eq_chaining_active;
  new_clauses

let eq_chaining_passive active_set c =
  assert false (* TODO *)

let ineq_chaining_active active_set c =
  assert false (* TODO *)

let ineq_chaining_passive active_set c =
  assert false (* TODO *)

let reflexivity_res c =
  assert false (* TODO *)

let is_tautology c =
  false (* TODO *)

let simplify c =
  assert false (* TODO *)

let setup_env ~env =
  assert false (* TODO *)
