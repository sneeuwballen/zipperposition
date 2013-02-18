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
type 'a szs_status = 
  | Unsat of 'a
  | Sat
  | Unknown
  | Error of string 
  | Timeout

(** check whether we still have some time w.r.t timeout *)
val check_timeout : float option -> bool

(** maximum number of nested unary inferences *)
val unary_max_depth : int ref

(** Is splitting enabled? *)
val enable_split : bool ref

(** perform backward simplification of the active set *)
val backward_simplify : calculus:Calculus.calculus ->
                        ProofState.active_set -> ProofState.simpl_set ->
                        hclause -> Clauses.CSet.t * hclause list

(** generation of new clauses (unary inferences are repeated at most !unary_max_depth) *)
val generate : calculus:Calculus.calculus -> ProofState.active_set -> hclause -> hclause list

(** remove orphans of the clauses *)
val remove_orphans : ProofState.passive_set -> hclause list -> unit

val find_lemmas : ctx:context -> Meta.Prover.t option -> hclause -> hclause list
  (** Use the meta-prover (if any) to prove new lemmas *)

(** Perform one step of the given clause algorithm.
    It performs generating inferences only if [generating] is true (default);
    other parameters are the iteration number, the global state and the calculus *)
val given_clause_step : ?generating:bool ->
                        calculus:Calculus.calculus ->
                        int -> ProofState.state ->
                        hclause szs_status

(** run the given clause until a timeout occurs or a result
    is found. It returns a tuple (new state, result, number of steps done).
    It performs generating inferences only if [generating] is true (default) *)
val given_clause: ?generating:bool -> ?steps:int -> ?timeout:float -> ?progress:bool ->
                  calculus:Calculus.calculus ->
                  ProofState.state ->
                  hclause szs_status * int

(** Interreduction of the given state, without generating inferences. Returns
    the number of steps done for presaturation, with status of the set. *)
val presaturate : calculus:Calculus.calculus ->
                  ProofState.state ->
                  hclause szs_status * int

(** time elapsed since start of program *)
val get_total_time : unit -> float

(** time at which the program started *)
val get_start_time : unit -> float

