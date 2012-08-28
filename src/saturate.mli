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

(** Main saturation algorithm. It uses inference rules and
    simplification rules from Superposition. *)

open Types

(** The SZS status of a state *)
type szs_status = 
  | Unsat of hclause
  | Sat
  | Unknown
  | Error of string 
  | Timeout

(** A clausal calculus for first order reasoning *)
type calculus = {
  calc_rules : (string * Superposition.inference_rule) list;
  calc_axioms : clause list;
  calc_constraint : ordering_constraint;
}

(** Superposition calculus *)
val superposition : calculus
(** Superposition with Equivalence Reasoning and Delayed CNF calculus *)
val delayed_superposition : calculus

(** Add the given axioms to the set of support *)
val set_of_support: ProofState.state -> clause list -> ProofState.state

(** Perform one step of the given clause algorithm *)
val given_clause_step : rules:(string * Superposition.inference_rule) list ->
                        ProofState.state ->
                        ProofState.state * szs_status

(** run the given clause until a timeout occurs or a result
    is found. It returns a tuple (new state, result, number of steps done) *)
val given_clause: ?steps:int -> ?timeout:float ->
                  rules:(string * Superposition.inference_rule) list ->
                  ProofState.state ->
                  ProofState.state * szs_status * int
