
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

type 'a solver = {
  create: unit -> 'a;
  strength : int;
  name: string;
}

type sat_solver = (module SAT) solver
type qbf_solver = (module QBF) solver

let sat_of_qbf (s:qbf_solver): sat_solver =
  { s with
    create=(fun () ->
      let module Solver = (val s.create() : QBF) in
      (module Solver : SAT)
    );
  }

let _sat_solvers = ref []
let _qbf_solvers = ref []

(* compare by decreasing priority *)
let _sort_list l =
  List.sort (fun s1 s2 -> CCInt.compare s2.strength s1.strength) l

let register_sat s =
  _sat_solvers := _sort_list (s :: !_sat_solvers)

let register_qbf s =
  register_sat (sat_of_qbf s);
  _qbf_solvers := _sort_list (s :: !_qbf_solvers);
  ()

let get_sat () = match !_sat_solvers with
  | [] -> failwith "no SAT solver registered"
  | s :: _ -> s.create ()

let get_qbf () = match !_qbf_solvers with
  | [] -> failwith "no QBF solver registered"
  | s :: _ -> s.create ()
