
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

(** {1 Boolean Solver Abstraction} *)

type 'a printer = Format.formatter -> 'a -> unit

(** {2 Type Definitions} *)

module type SAT = BoolSolver_intf.SAT
module type QBF = BoolSolver_intf.QBF

(** In the following, [strength] represents how "powerful" (or efficient)
the registered solver is. Solvers with high strengths will be favored
over weak solvers when available. *)

type 'a solver = {
  create: unit -> 'a;
  strength : int;
  name: string;
}

type sat_solver = (module SAT) solver
type qbf_solver = (module QBF) solver

(** {2 Registering Solvers} *)

val register_sat : sat_solver -> unit
(** Register a new SAT-solver generator *)

val register_qbf : qbf_solver -> unit
(** Register a new QBF-solver generator. Also registers it
    as a SAT solver. *)

val sat_of_qbf : qbf_solver -> sat_solver
(** Convert a QBF solver into a SAT solver *)

val get_sat : unit -> (module SAT)
(** Create a new SAT solver, using the best registered solver.
    QBF solvers can also be used if they are present.
    @raise Failure if no solver was registered *)

val get_qbf : unit -> (module QBF)
(** Create a new QBF solver, using the best registered solver.
    @raise Failure if no solver was registered *)
