
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

(** {1 Chaining on Sets} *)

open Logtk

(** {2 Inference Rules} *)
module type S = sig
  module Env : Env.S
  module C : module type of Env.C
  module PS : module type of Env.ProofState

  val idx_left : unit -> PS.TermIndex.t     (* terms at LHS of subseteq *)
  val idx_right : unit -> PS.TermIndex.t    (* terms at RHS of subseteq *)

  val preprocess : Formula.FO.t -> Formula.FO.t
  (** Preprocessing of formula, during CNF, to remove most set operators,
      keeping only those of the form
      a \cap b \cap ...  \subseteq a' \cup b' \cup ... *)

  val positive_chaining: Env.binary_inf_rule
    (* positive chaining *)

  val negative_chaining_left: Env.binary_inf_rule
    (* negative chaining where the left side of literals are treated *)

  val negative_chaining_right: Env.binary_inf_rule
    (* negative chaining where the right side of literals are treated *)

  val reflexivity_res: Env.unary_inf_rule
    (* reflexivity resolution *)

  val factoring_left: Env.unary_inf_rule
    (* factoring terms that are on the left side of literals *)

  val rewrite_set_eq: Env.multi_simpl_rule
    (* rewrite A=B into A subseteq B and B subseteq A *)

  val rewrite_set_neq: Env.multi_simpl_rule
    (* rewrite A!=B into A notsubseteq B or B notsubseteq A *)

  val power_neg_left: Env.multi_simpl_rule
    (* rewrite P(A) when at the left-hand side of a negative subset *)
			 
  val singleton_pos: Env.rw_simplify_rule
    (* choice of a witness for all terms appearing in a singleton on the left
     * side of a subseteq *)

  val singleton_neg: Env.multi_simpl_rule
    (* choice of a witness for all terms appearing in a singleton on the left
     * side of a notsubseteq *)

  val singleton_elim: Env.multi_simpl_rule
    (* eliminates a variable that appears in singletons only on the left side
     * of set literals and in equalities *)

  val var_elim: Env.multi_simpl_rule
    (* eliminates a variable that appears only on one side of set literals *)

  val reflexivity: Env.is_trivial_rule
    (* reflexivity tautology : when the same term appears in each side of a
     * positive literal*)

  val is_tautology: Env.is_trivial_rule
    (* finds the following tautologies :
     * A inter A' subseteq B union B' or A notsubseteq B *)

  val is_absurd: Env.lit_rewrite_rule
    (* eliminates negative literals that have the same term appearing in each
     * side *)

  val setup : unit -> unit
end

module Make(E : Env.S) : S with module Env = E
