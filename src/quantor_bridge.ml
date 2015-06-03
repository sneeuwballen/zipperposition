
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013-2015, Simon Cruanes
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

(** {1 Interface to Quantor} *)

module BS = BoolSolver

let section = BS.section

let rec _rev_append_map f l acc = match l with
  | [] -> acc
  | x::tail -> _rev_append_map f tail (f x :: acc)

type lit = Qbf.Lit.t

module Make(X : sig end) : BS.QBF = struct
  module LitSet = Sequence.Set.Make(Qbf.Lit)
  module IntMap = CCMap.Make(CCInt)

  [@@@warning "-39"]

  type form =
    | Clause of LitSet.t
    | Form of Qbf.Formula.t
    [@@deriving ord]

  [@@@warning "+39"]

  module FormSet = Sequence.Set.Make(struct
    type t = form
    let compare = compare_form
  end)

  let _add set l = LitSet.add (Qbf.Lit.abs l) set

  (* add a list of literals to the set *)
  let _add_list set l =
    List.fold_left (fun s x -> LitSet.add (Qbf.Lit.abs x) s) set l

  type quantifier = Qbf.quantifier

  let level0 = 0

  let pp_ = ref Qbf.Lit.print

  (* current (backtrackable) state  *)
  type state = {
    mutable lits : (quantifier * LitSet.t) IntMap.t; (* level -> literals *)
    mutable forms : FormSet.t;
    mutable result : Qbf.result;
  }

  (* stack of states *)
  let stack : state CCVector.vector =
    let v = CCVector.create () in
    CCVector.push v {
      forms=FormSet.empty;
      lits=IntMap.singleton 0 (Qbf.Exists, LitSet.empty);
      result=Qbf.Unknown
    };
    v

  let get_state_ () =
    CCVector.get stack (CCVector.length stack-1)

  let clause_of_lits_ l = LitSet.of_list l

  let root_save_level = 0

  let save () =
    let state = get_state_ () in
    let state' = {state with result=state.result} in (* copy *)
    let i = CCVector.length stack in
    CCVector.push stack state';
    i

  let restore l =
    assert(l>=0);
    if l>= CCVector.length stack then failwith "restore: level too high";
    CCVector.shrink stack l  (* so vec.(l-1) is the new state *)

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
    let st = get_state_ () in
    st.forms <- FormSet.add (Clause (clause_of_lits_ c)) st.forms

  let add_clause ?tag c = add_clause_ c

  let valuation l =
    if not (Qbf.Lit.sign l) then invalid_arg "valuation";
    let st = get_state_ () in
    match st.result with
    | Qbf.Sat v ->
        begin match v (Qbf.Lit.abs l) with
          | Qbf.Undef -> failwith "literal not valued in the model"
          | Qbf.True -> true
          | Qbf.False -> false
        end
    | _ ->  failwith "QBF solver didn't return \"SAT\""

  let add_clauses ?tag = List.iter add_clause_

  let add_form ?tag f =
    let st = get_state_ () in
    st.forms <- FormSet.add (Form f) st.forms

  let add_clause_seq ?tag seq = seq add_clause_

  let push q lits =
    let st = get_state_ () in
    let l = fst (IntMap.max_binding st.lits) + 1 in
    st.lits <- IntMap.add l (q, LitSet.of_list lits) st.lits;
    l

  let quantify_lit l lit =
    let st = get_state_ () in
    let q, set = IntMap.find l st.lits in
    let set' = _add set lit in
    st.lits <- IntMap.add l (q,set') st.lits

  let quantify_lits l lits =
    let st = get_state_ () in
    let q, set = IntMap.find l st.lits in
    let set' = _add_list set lits in
    st.lits <- IntMap.add l (q,set') st.lits

  let to_cnf_ clauses =
    let new_lits = ref [] in
    let clauses =
      FormSet.to_seq clauses
      |> Sequence.flat_map
          (function
            | Clause lits -> Sequence.singleton (LitSet.to_list lits)
            | Form f ->
                let clauses, new_lits' =
                  Qbf.Formula.cnf ~gensym:Qbf.Lit.fresh f in
                CCList.Ref.push_list new_lits new_lits';
                Sequence.of_list clauses
          )
      |> Sequence.to_rev_list
    in
    Qbf.QCNF.quantify Qbf.Exists !new_lits (Qbf.QCNF.prop clauses)

  let get_form_ () =
    let st = get_state_ () in
    st.forms

  let unsat_core = None

  let mk_qcnf_ () =
    (* at quantifier level [i] *)
    let rec recurse st i max =
      if i > max
        then to_cnf_ st.forms
        else
          let q, lits = IntMap.find i st.lits in
          let sub = recurse st (i+1) max in
          Qbf.QCNF.quantify q (LitSet.to_list lits) sub
    in
    let st = get_state_ () in
    let max, _ = IntMap.max_binding st.lits in
    recurse st 0 max

  let pp_form fmt = function
    | Clause c ->
        Format.fprintf fmt "@[<hv2>%a@]"
        (Sequence.pp_seq ~sep:" ⊔ " !pp_) (LitSet.to_seq c)
    | Form f ->
        Qbf.Formula.print_with ~pp_lit:!pp_ fmt (Qbf.Formula.simplify f)

  let check () =
    let f = mk_qcnf_ () in
    if Logtk.Util.Section.cur_level section >= 5 then (
      Format.printf "@[<hv2>QCNF sent to solver:@ %a@]@."
        (Qbf.QCNF.print_with ~pp_lit:!pp_) f;
      Format.printf "@[<hv2>formula before CNF:@ %a@]@."
        (Sequence.pp_seq pp_form) (FormSet.to_seq (get_form_ ()))
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

(* register as an extension *)
let () =
  let open Extensions in
  let e = {
    default with
    name="quantor";
    init_actions=[Init_do(fun () -> BS.register_qbf qbf_solver)]
  } in
  register e

