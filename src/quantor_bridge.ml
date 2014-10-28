
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

(** {1 Bridge to the Qbf.Quantor module} *)

module A = Avatar

let rec _rev_append_map f l acc = match l with
  | [] -> acc
  | x::tail -> _rev_append_map f tail (f x :: acc)

module ISet = Sequence.Set.Make(CCInt)

(* add a list of literals to the set *)
let _add_list set l =
  List.fold_left (fun s x -> ISet.add (abs x) s) set l

(* make a new solver *)
let solver_instance () =
  let _lits = ref ISet.empty in
  let _clauses = ref [] in
  let _pp = ref Qbf.Lit.print in
  A.({
    add_lits=(fun l -> _lits := _add_list !_lits l);
    add_clauses=(fun l -> _clauses := List.rev_append l !_clauses);
    check=(fun () ->
      let f = Qbf.CNF.exists (ISet.to_list !_lits) (Qbf.CNF.cnf !_clauses) in
      if Logtk.Util.get_debug() >= 5 then (
        Format.printf "QBF formula: @[<hov>%a@]@."
          (Qbf.CNF.print_with ~pp_lit:!_pp) f
      );
      match Quantor.solve f with
      | Qbf.Unsat -> `Unsat
      | Qbf.Sat _ -> `Sat
      | Qbf.Timeout
      | Qbf.Spaceout -> assert false
      | Qbf.Unknown -> failwith "quantor: return unknown"
    );
    set_printer=(fun p -> _pp := p);
  })

let sat_solver =
  A.({create=solver_instance; name="quantor"; })

let () =
  A.register_solver sat_solver;
  ()
