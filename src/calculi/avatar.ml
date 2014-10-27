
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

(** {1 Basic Splitting à la Avatar} *)

open Logtk

(** {2 Sat-Solvers} *)

module BoolLit = struct
  type t = int
  let neg i = - i
  let to_int i = abs i
  let of_int i = i
end

(** A running SAT-solver *)
type sat_solver_instance = {
  add_lits : BoolLit.t list -> unit;
  add_clauses : BoolLit.t list list -> unit;
  check : unit -> [`Sat | `Unsat]
}

(** A factory of SAT-solver instances *)
type sat_solver = {
  create : unit -> sat_solver_instance;
  name : string;
}

let _solvers = Hashtbl.create 5
let _default = ref None

let register_solver s =
  if Hashtbl.mem _solvers s.name
    then failwith (Printf.sprintf "solver \"%s\" already registered" s.name);
  (* be sure we have a default solver *)
  if !_default = None
    then _default := Some s;
  Hashtbl.add _solvers s.name s

let solver_by_name n =
  try Some (Hashtbl.find _solvers n)
  with Not_found -> None

let current_solver () = !_default

let set_current_solver s =
  if not (Hashtbl.mem _solvers s.name)
    then register_solver s;
  _default := Some s

(** {2 Avatar} *)

module Make(E : Env.S) = struct
  module Ctx = E.Ctx
  module C = E.C

  let _solver = ref None

  let _get_solver () = match !_solver with
    | None -> failwith "Avatar: error, solver shouldn't be None"
    | Some s -> s

  let split c =
    None (* TODO *)

  (* ignore the clause. We should just check the solver *)
  let check_satisfiability _c =
    let s = _get_solver () in
    match s.check () with
    | `Sat ->
        Util.debug 3 "Avatar: SAT-solver reports \"SAT\"";
        []
    | `Unsat ->
        Util.debug 1 "Avatar: SAT-solver reports \"UNSAT\"";
        (* TODO: proper proof handling (collect unsat core? collect all clauses?)*)
        let proof cc = Proof.mk_c_inference ~rule:"avatar" ~theories:["sat"] cc [] in
        let c = C.create [] proof in
        [c]

  let register () =
    Util.debug 2 "register extension Avatar";
    begin match current_solver() with
      | None -> failwith "Avatar: expect a default SAT solver to be defined."
      | Some s ->
          Util.debug 2 "Avatar: create a new solver (kind %s)" s.name;
          _solver := Some (s.create ())
    end;
    E.add_multi_simpl_rule split;
    E.add_unary_inf "avatar_check" check_satisfiability;
    ()
end

let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module A = Make(E) in
    A.register()
  in
  Extensions.({default with name="avatar"; actions=[Do action]})

