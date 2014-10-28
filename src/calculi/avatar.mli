
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

(** {1 Basic Splitting à la Avatar}

We don't implement all the stuff from Avatar, in particular all clauses are
active whether or not their trail is satisfied in the current model.
Trails are only used to make splits easier {b currently}.

Future work may include locking clauses whose trails are unsatisfied. *)

type 'a printer = Format.formatter -> 'a -> unit

(** {2 Sat-Solvers} *)

module BoolLit : sig
  type t = int
  val neg : t -> t
  val to_int : t -> int
  val of_int : int -> t
end

(** A running SAT-solver *)
type sat_solver_instance = {
  add_lits : BoolLit.t list -> unit;
  add_clauses : BoolLit.t list list -> unit;
  check : unit -> [`Sat | `Unsat];
  set_printer : int printer -> unit;
}

(** A factory of SAT-solver instances *)
type sat_solver = {
  create : unit -> sat_solver_instance;
  name : string;
}

val register_solver : sat_solver -> unit
(** [register_solver s] adds a new solver to the list of known solvers.
    @raise Failure if a solver is already registered with this name *)

val solver_by_name : string -> sat_solver option
(** Obtain the solver whose name corresponds, if any *)

val current_solver : unit -> sat_solver option
(** The current solver by-default *)

val set_current_solver : sat_solver -> unit
(** Set the current SAT-solver (obtained with {!current_solver}), also
    registers the solver if it's not already the case. *)

(** {2 Avatar: splitting+sat} *)

module Make(E : Env.S) : sig
  val split : E.multi_simpl_rule
  (** Split a clause into components *)

  val check_empty : E.unary_inf_rule
  (** Forbid empty clauses with trails, i.e. adds the negation of their
      trails to the SAT-solver *)

  val check_satisfiability : E.generate_rule
  (** Checks  that the SAT context is still valid *)

  val register : unit -> unit
  (** Register inference rules to the environment *)
end

val extension : Extensions.t
