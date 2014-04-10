
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

(** We follow the paper "ordered chaining for total orderings" by
    L. Bachmair and H. Ganzinger.

    Chaining allows to deal with total order in a very efficient manner.
    Here it needs to know which symbols are orderings, an ordering instance
    being a pair of strict order symbol, and non-strict order symbol.
    Equality can only be the regular, "builtin" equality symbol.
*)

open Logtk

(** {2 Inference Rules} *)
module type S = sig
  module Env : Env.S
  module C : module type of Env.C
  module PS : module type of Env.ProofState

  (** {6 Term Indices} *)

  val idx_ord_left : unit -> PS.TermIndex.t       (** terms at LHS of inequality *)
  val idx_ord_right : unit -> PS.TermIndex.t      (** terms at RHS of inequality *)
  val idx_ord_subterm : unit -> PS.TermIndex.t    (** subterms of inequality literals *)

  val eq_chaining_active : Env.binary_inf_rule
    (** Equality chaining where the clause is active *)

  val eq_chaining_passive : Env.binary_inf_rule
    (** Equality chaining where the clause is passive *)

  val ineq_chaining_left : Env.binary_inf_rule
    (** Inequality chaining where the clause is on the left. *)

  val ineq_chaining_right : Env.binary_inf_rule
    (** Inequality chaining where the clause is on the right. *)

  (* TODO: redundancy criterion:
     a<b subsumes c<d if it is known from unit facts
     that c<a and b<=d, or c<=a and b<d.

     Same as simplify-reflect for equality: maintain a global graph of
     ordering relations used to cut inconsistent literals. *)

  val is_semantic_tautology : C.t -> bool
    (** Check whether the clause is tautological for some ordering completion *)

  val reflexivity_res : Env.unary_inf_rule
    (** Reflexivity resolution *)

  val is_tautology : C.t -> bool
    (** C is always true in ordering models? *)

  val simplify : C.t -> C.t
    (** Simplify the clause, by removing impossible literals *)

  val axioms : instance:Theories.TotalOrder.instance -> C.t list
    (** Additional axioms for a total ordering *)

  (** {6 Env} *)

  val add_order : ?proof:Proof.t list ->
                  less:Symbol.t -> lesseq:Symbol.t -> ty:Type.t -> unit
    (** Declare a new total ordering instance *)

  val add_tstp_order : unit -> unit
end

module Make(Sup : Superposition.S) : S with module Env = Sup.Env

val extension : Extensions.t
