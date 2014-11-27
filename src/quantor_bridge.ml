
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

(** {1 Bridge to the [Qbf.Quantor] module} *)

module BS = BoolSolver

let section = BS.section

let rec _rev_append_map f l acc = match l with
  | [] -> acc
  | x::tail -> _rev_append_map f tail (f x :: acc)

module Make(X : sig end) : BS.QBF = struct
  module LitSet = Sequence.Set.Make(CCInt)

  (* add a list of literals to the set *)
  let _add_list set l =
    List.fold_left (fun s x -> LitSet.add (abs x) s) set l

  type quantifier = Qbf.quantifier

  (* stack of quantified literals *)
  let _lits : (quantifier * LitSet.t, CCVector.rw) CCVector.t =
    let v = CCVector.create () in
    (* create level 0 *)
    CCVector.push v (Qbf.Exists, LitSet.empty);
    v

  let level0 = 0

  type lit = int

  let pp_ = ref Qbf.Lit.print

  (* current (backtrackable) state  *)
  type state = {
    mutable clauses : lit list list;
    mutable result : Qbf.result;
  }

  (* stack of states *)
  let stack : state CCVector.vector =
    let v = CCVector.create () in
    CCVector.push v {clauses=[]; result=Qbf.Unknown};
    v

  let get_state_ () =
    CCVector.get stack (CCVector.length stack-1)

  let root_save_level = 0

  let save () =
    let state = get_state_ () in
    let i = CCVector.length stack in
    CCVector.push stack state;
    i

  let restore l =
    assert(l>=0);
    if l>= CCVector.length stack then failwith "restore: level too high";
    CCVector.shrink stack (l+1)  (* so vec.(l) is the new state *)

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

  let add_clause c =
    let st = get_state_ () in
    st.clauses <- c :: st.clauses

  let valuation l =
    if l<=0 then invalid_arg "valuation";
    let st = get_state_ () in
    match st.result with
    | Qbf.Sat v ->
        begin match v (abs l) with
          | Qbf.Undef -> failwith "literal not valued in the model"
          | Qbf.True -> true
          | Qbf.False -> false
        end
    | _ ->  failwith "QBF solver didn't return \"SAT\""

  let add_clauses = List.iter add_clause

  let quant_at_level l = fst (CCVector.get _lits l)

  let lits_at_level l = snd (CCVector.get _lits l)

  let push q lits =
    let l = CCVector.length _lits in
    CCVector.push _lits (q, LitSet.of_list lits);
    l

  let quantify_lits l lits =
    let q, set = CCVector.get _lits l in
    let set' = _add_list set lits in
    CCVector.set _lits l (q,set')

  let _mk_form () =
    (* at quantifier level [i] *)
    let rec _recurse_quant i =
      if i = CCVector.length _lits
        then
          let st = get_state_ () in
          Qbf.CNF.cnf st.clauses
        else
          let q, lits = CCVector.get _lits i in
          Qbf.CNF.quantify q (LitSet.to_list lits) (_recurse_quant (i+1))
    in
    _recurse_quant 0

  let check () =
    let f = _mk_form () in
    if Logtk.Util.Section.cur_level section >= 5 then (
      Format.printf "QBF formula: @[<hov>%a@]@."
        (Qbf.CNF.print_with ~pp_lit:!pp_) f
    );
    let st = get_state_ () in
    st.result <- Quantor.solve f;
    let res = match st.result with
    | Qbf.Unsat -> Unsat
    | Qbf.Sat _ -> Sat
    | Qbf.Timeout
    | Qbf.Spaceout -> assert false
    | Qbf.Unknown -> failwith "quantor: return unknown"
    in
    Logtk.Util.debug ~section 3 "quantor check: %a" pp_result res;
    res
end

let qbf_solver =
  BS.{
    name="quantor";
    strength=20;
    create=(fun () ->
      let module S = Make(struct end) in
      (module S : BS.QBF)
    );
  }

let () =
  BS.register_qbf qbf_solver;
  ()
