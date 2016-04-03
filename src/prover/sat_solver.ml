
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Bridge to [MSat] prover} *)

open Libzipperposition

let section = Util.Section.make ~parent:Const.section "msat"
let prof_call_msat = Util.mk_profiler "msat.call"

type result = Sat_solver_intf.result =
  | Sat
  | Unsat

exception WrongState of string

let wrong_state_ msg = raise (WrongState msg)

module type S = Sat_solver_intf.S

module Make(L : Bool_lit_intf.S)(Dummy : sig end)
: Sat_solver_intf.S with module Lit = L
= struct
  module Lit = L

  type clause = Lit.t list

  (* queue of clauses waiting for being pushed into the solver *)
  let queue_ = Queue.create()

  let add_clause ?tag (c:clause) = Queue.push ([c], tag) queue_

  let add_clauses ?tag l = Queue.push (l, tag) queue_

  let add_clause_seq ?tag (seq:clause Sequence.t) =
    add_clauses ?tag (Sequence.to_rev_list seq)

  let result_ = ref Sat

  (* invariant:
    when result_ = Sat, only eval/eval_level are defined
    when result_ = Unsat, only unsat_core_ is defined
  *)

  let eval_fail_ _ = assert (!result_ = Unsat); wrong_state_ "eval"
  let unsat_core_fail_ _ = assert (!result_ = Sat); wrong_state_ "unsat core"

  let eval_ = ref eval_fail_
  let eval_level_ = ref eval_fail_
  let unsat_core_ : int Sequence.t ref = ref unsat_core_fail_

  let pp_ = ref Lit.pp

  let pp_clause out c =
    Format.fprintf out "[@[<hv>%a@]]" (Util.pp_list ~sep:" ⊔ " !pp_) c

  let tag_ = snd

  let pp_form_simpl out (c,_) = Util.pp_list ~sep:"" pp_clause out c

  let pp_form fmt f =
    match tag_ f with
    | None -> pp_form_simpl fmt f
    | Some tag -> Format.fprintf fmt "%a/%d" pp_form_simpl f tag

  let last_result () = !result_
  let valuation l = !eval_ l
  let valuation_level l = !eval_level_ l
  let unsat_core k = !unsat_core_ k

  (* TODO: use specific interface of MSat *)

  module SatForm = struct
    include Lit
    type proof = unit
    let fresh () = Lit.make (Lit.payload Lit.dummy)
    let label _ = assert false
    let add_label _ _ = assert false
    let print = Lit.pp
  end

  (* Instantiate solver *)
  module S =
    Msat.Solver.Make
      (SatForm)
      (Msat.Solver.DummyTheory(SatForm))
      (struct end)

  exception UndecidedLit = S.UndecidedLit

  (* call [S.solve()] in any case, and enforce invariant about eval/unsat_core *)
  let check_unconditional_ () =
    (* reset functions, so they will fail if called in the wrong state *)
    unsat_core_ := unsat_core_fail_;
    eval_ := eval_fail_;
    eval_level_ := eval_fail_;
    (* add pending clauses *)
    while not (Queue.is_empty queue_) do
      let c, tag = Queue.pop queue_ in
      Util.debugf ~section 4 "@[<hv2>assume@ @[%a@]@]"
        (fun k->k pp_form (c,tag));
      S.assume ?tag c
    done;
    (* solve *)
    begin match S.solve () with
    | S.Sat ->
      eval_ := S.eval;
      eval_level_ := S.eval_level;
      result_ := Sat;
    | S.Unsat ->
      result_ := Unsat;
      unsat_core_ := begin
        (* compute Unsat core *)
        let us = S.get_proof () |> S.unsat_core in
        (fun k ->
          us
          |> CCList.to_seq
          |> CCFun.tap
            (fun seq ->
              Util.debugf ~section 4 "@[unsat_core:@ @[<hv>%a@]@]"
                (fun k->k (CCFormat.seq S.Proof.print_clause) seq))
          |> Sequence.filter_map S.get_tag
          |> Sequence.sort_uniq ~cmp:CCInt.compare
          |> Sequence.iter k)
      end;
    end;
    !result_

  let check_ () =
    if Queue.is_empty queue_ then !result_
    else check_unconditional_ ()

  (* initialize eval/eval_level to enforce invariant *)
  let () =
    let res = check_unconditional_ () in
    assert (res = Sat)

  let check () = Util.with_prof prof_call_msat check_ ()

  let set_printer pp = pp_ := pp

  type save_level = int

  let root_save_level = 0

  let save () = assert false

  let restore _ = assert false
end
