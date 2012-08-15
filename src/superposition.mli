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

open Types

(** a conclusion is a clause *)
type conclusion = clause

(** raised when the empty clause is found *)
exception Success of hclause

(** inferences *)
type inference_rule = ProofState.active_set -> clause -> conclusion list

val infer_active : inference_rule  (* superposition where given clause is active *)

val infer_passive : inference_rule (* superposition where given clause is passive *)

val infer_equality_resolution : inference_rule

val infer_equality_factoring : inference_rule

(* simplifications *)

val is_tautology : clause -> bool

val basic_simplify : ord:ordering -> clause -> clause   (** basic simplifications *)

val demodulate : ProofState.active_set
                -> int list (** the IDs of active clauses to ignore for rewriting *)
                -> clause   (** the clause to simplify *)
		-> clause   (** the simplified clause *)

(** subsumes c1 c2 iff c1 subsumes c2 *)
val subsumes : clause -> clause -> bool

(** check whether the clause is subsumed by any clause in the set *)
val subsumed_by_set : ProofState.active_set -> clause -> bool

(** remove from the set the clauses subsumed by c *)
val subsumed_in_set : ProofState.active_set -> clause -> hclause list

(** remove from the passive_set the list of orphans of clause *)
val orphan_murder: ProofState.passive_set
                -> clause   (** the clause whose orphans are to be deleted *)
                -> ProofState.passive_set
