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

open Basic

(** The SZS status of a state *)
type 'a szs_status = 
  | Unsat of 'a
  | Sat
  | Unknown
  | Error of string 
  | Timeout

(** check whether we still have some time w.r.t timeout *)
val check_timeout : float option -> bool

(** Perform one step of the given clause algorithm.
    It performs generating inferences only if [generating] is true (default);
    other parameters are the iteration number and the environment *)
val given_clause_step : ?generating:bool -> env:Env.t -> int ->
                        hclause szs_status

(** run the given clause until a timeout occurs or a result
    is found. It returns a tuple (new state, result, number of steps done).
    It performs generating inferences only if [generating] is true (default) *)
val given_clause: ?generating:bool -> ?steps:int -> ?timeout:float ->
                  env:Env.t -> hclause szs_status * int

(** Interreduction of the given state, without generating inferences. Returns
    the number of steps done for presaturation, with status of the set. *)
val presaturate : env:Env.t -> hclause szs_status * int

