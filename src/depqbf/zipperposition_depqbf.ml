
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

(** {1 Bridge to the [Qbf.Depqbf] module} *)

module BS = BoolSolver

let section = BS.section

module Make(X : sig end) : BS.QBF = struct
  module LitSet = Sequence.Set.Make(CCInt)

  type quantifier = Qbf.quantifier

  let level0 = 0

  type lit = int

  let pp_ = ref Qbf.Lit.print

  let root_save_level = 0
  let level_ = ref 0

  let solver = Depqbf.create ()

  (* configure solver *)
  let () =
    Depqbf.configure solver "--dep-man=simple";
    Depqbf.configure solver "--incremental-use";
    ()

  (* last result *)
  let result_= ref Qbf.Unknown

  (* reset if needed *)
  let reset_ () = match !result_ with
    | Qbf.Sat _
    | Qbf.Unsat ->
        result_ := Qbf.Unknown;
        Depqbf.reset solver
    | Qbf.Unknown -> ()
    | _ -> assert false

  let save () =
    reset_ ();
    let l = !level_ in
    ignore (Depqbf.push solver);
    incr level_;
    l

  let restore l =
    assert(l>=0);
    Depqbf.reset solver;
    while !level_ > l do
      Logtk.Util.debug ~section 5 "pop depqbf once";
      ignore (Depqbf.pop solver);
      decr level_;
    done;
    Depqbf.gc solver;
    ()

  type quant_level = int
  type save_level = int

  type result =
    | Sat
    | Unsat

  let pp_result buf = function
    | Sat -> Buffer.add_string buf "sat"
    | Unsat -> Buffer.add_string buf "unsat"

  let set_printer p = pp_ := p
  let name = "quantor"

  let add_clause_ c =
    List.iter (Depqbf.add solver) c;
    Depqbf.add solver 0

  let add_clause c =
    reset_ ();
    add_clause_ c

  let valuation l =
    if l<=0 then invalid_arg "valuation";
    match !result_ with
    | Qbf.Unsat
    | Qbf.Unknown -> failwith "QBF solver didn't return \"SAT\""
    | Qbf.Sat v ->
        begin match v (abs l) with
          | Qbf.Undef -> failwith "literal not valued in the model"
          | Qbf.True -> true
          | Qbf.False -> false
        end
    | _ ->  failwith "QBF solver didn't return \"SAT\""

  let add_clauses l =
    reset_ ();
    List.iter add_clause_ l

  let add_clause_seq seq =
    reset_ ();
    seq add_clause_

  let push q lits =
    reset_ ();
    let level = Depqbf.new_scope solver q in
    List.iter
      (fun lit -> Depqbf.add solver (abs lit))
      lits;
    Depqbf.add solver 0;
    level

  let quantify_lits level lits =
    reset_ ();
    List.iter
      (fun lit -> Depqbf.add_var_to_scope solver (abs lit) level)
      lits

  let check () =
    reset_ ();
    result_ := Depqbf.check solver;
    let res = match !result_ with
    | Qbf.Unsat -> Unsat
    | Qbf.Sat _ -> Sat
    | Qbf.Timeout
    | Qbf.Spaceout -> assert false
    | Qbf.Unknown -> failwith "depqbf: return unknown"
    in
    Logtk.Util.debug ~section 3 "depqbf check: %a" pp_result res;
    res
end

let qbf_solver =
  BS.{
    name="depqbf";
    strength=90;
    create=(fun () ->
      let module S = Make(struct end) in
      (module S : BS.QBF)
    );
  }

(* register as an extension *)
let () =
  let open Extensions in
  let e = {
    default with
    name="depqbf";
    init_actions=[Init_do(fun () -> BS.register_qbf qbf_solver)]
  } in
  register e

