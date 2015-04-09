
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

let section = Logtk.Util.Section.make ~parent:BS.section "depqbf"

module Make(X : sig end) : BS.QBF = struct
  module LitSet = Sequence.Set.Make(Qbf.Lit)
  module IntMap = CCMap.Make(CCInt)

  [@@@warning "-39"]

  type form =
    | Clauses of LitSet.t list * int option (* tag *)
    | Form of Qbf.Formula.t * int option (* tag *)
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

  let name = "depqbf"
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

  let add_clauses ?tag l =
    let st = get_state_ () in
    let form = Clauses (List.map clause_of_lits_ l, tag) in
    st.forms <- FormSet.add form st.forms

  let add_clause ?tag c = add_clauses ?tag [c]

  let add_form ?tag f =
    let st = get_state_ () in
    st.forms <- FormSet.add (Form (f, tag)) st.forms

  let add_clause_seq ?tag seq =
    let l = Sequence.to_list seq in
    add_clauses ?tag l

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

  (** Interface to Solver itself *)

  let valuation l =
    if not (Qbf.Lit.sign l) then invalid_arg "valuation";
    let st = get_state_ () in
    match st.result with
    | Qbf.Unsat
    | Qbf.Unknown -> failwith "QBF solver didn't return \"SAT\""
    | Qbf.Sat v ->
        begin match v (Qbf.Lit.abs l) with
          | Qbf.Undef -> failwith "literal not valued in the model"
          | Qbf.True -> true
          | Qbf.False -> false
        end
    | _ ->  failwith "QBF solver didn't return \"SAT\""

  (* we can't use tags directly as literals (+ assumptions), because they
      might have been "allocated" to other literals already. Therefore
      we maintain a mapping and use fresh lits to represent tags *)
  let lit_to_tag_ = Hashtbl.create 15
  let tag_to_lit_ = Hashtbl.create 15

  let tag_to_lit ~tag =
    try Hashtbl.find tag_to_lit_ tag
    with Not_found ->
      let i = Qbf.Lit.fresh () in
      Hashtbl.add lit_to_tag_ i tag;
      Hashtbl.add tag_to_lit_ tag i;
      i

  let lit_to_tag id = Hashtbl.find lit_to_tag_ id

  let unsat_core_ = ref (fun _ -> failwith "no unsat core")

  let unsat_core = Some (fun k -> !unsat_core_ k)

  let to_cnf_ clauses =
    let new_lits = ref [] in
    let assumptions = ref [] in
    let clauses =
      FormSet.to_list clauses
      |> CCList.flat_map
          (function
            | Clauses (l, tag) ->
                let clauses = List.map LitSet.to_list l in
                begin match tag with
                | None -> clauses
                | Some tag ->
                    let id = tag_to_lit ~tag in
                    CCList.Ref.push assumptions id;
                    List.map (fun c -> Qbf.Lit.neg id :: c) clauses
                end
            | Form (f, tag) ->
                let clauses, new_lits' =
                  Qbf.Formula.cnf ~gensym:Qbf.Lit.fresh f in
                CCList.Ref.push_list new_lits new_lits';
                begin match tag with
                | None -> clauses
                | Some tag ->
                    let id = tag_to_lit ~tag in
                    CCList.Ref.push assumptions id;
                    List.map (fun c -> Qbf.Lit.neg id :: c) clauses
                end
          )
    in
    let f = Qbf.QCNF.quantify Qbf.Exists !new_lits
      (Qbf.QCNF.prop clauses)
    and assumptions = CCList.sort_uniq ~cmp:Qbf.Lit.compare !assumptions in
    assumptions, f

  let get_form_ () =
    let st = get_state_ () in
    st.forms

  let mk_qcnf_ () =
    (* at quantifier level [i] *)
    let rec recurse st i max =
      if i > max
        then to_cnf_ st.forms
        else
          let q, lits = IntMap.find i st.lits in
          let assumptions, sub = recurse st (i+1) max in
          assumptions, Qbf.QCNF.quantify q (LitSet.to_list lits) sub
    in
    let st = get_state_ () in
    let max, _ = IntMap.max_binding st.lits in
    recurse st 0 max

  let pp_clause fmt c =
    Format.fprintf fmt "@[<hv2>%a@]"
      (Sequence.pp_seq ~sep:" ⊔ " !pp_) (LitSet.to_seq c)

  let pp_form fmt = function
    | Clauses (c,_) ->
        CCList.print ~start:"" ~stop:"" ~sep:"" pp_clause fmt c
    | Form (f,_) ->
        Qbf.Formula.print_with ~pp_lit:!pp_ fmt (Qbf.Formula.simplify f)

  let add_clause_ solver c =
    List.iter (Depqbf.add solver) c;
    Depqbf.add0 solver

  let add_clauses_ solver l =
    List.iter (add_clause_ solver) l

  let rec add_form_ solver f = match f with
    | Qbf.QCNF.Quant (q, lits, f') ->
        let _ = Depqbf.new_scope solver q in
        List.iter (Depqbf.add solver) lits;
        Depqbf.add0 solver;
        add_form_ solver f'
    | Qbf.QCNF.Prop l ->
        add_clauses_ solver l

  let add_assumptions_ solver l =
    List.iter (Depqbf.assume solver) l

  let check () =
    let st = get_state_ () in
    (* add problem and assumptions *)
    let assumptions, f = mk_qcnf_ () in
    if Logtk.Util.Section.cur_level section >= 5 then (
      Format.printf "@[<hv2>QCNF sent to solver:@ %a@]@."
        (Qbf.QCNF.print_with ~pp_lit:!pp_) f;
      Format.printf "@[<hv2>formula before CNF:@ %a@]@."
        (Sequence.pp_seq pp_form) (FormSet.to_seq (get_form_ ()))
    );
    let solver = Depqbf.create () in
    add_form_ solver f;
    add_assumptions_ solver assumptions;
    (* check *)
    st.result <- Depqbf.sat solver;
    let res = match st.result with
    | Qbf.Unsat ->
        unsat_core_ := (fun k ->
          let l = Depqbf.get_relevant_assumptions solver in
          List.iter (fun lit -> k (lit_to_tag lit)) l
        );
        Unsat
    | Qbf.Sat _ ->
        Sat
    | Qbf.Timeout
    | Qbf.Spaceout -> assert false
    | Qbf.Unknown -> failwith "depqbf: return unknown"
    in
    Logtk.Util.debug ~section 3 "check: %a" pp_result res;
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

