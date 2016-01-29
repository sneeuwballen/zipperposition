
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

module Make(L : Bool_lit_intf.S)
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

  let eval_fail_ _ = wrong_state_ "eval"
  let unsat_core_fail_ _ = wrong_state_ "unsat core"

  let result_ = ref Sat
  let eval_ = ref eval_fail_
  let eval_level_ = ref eval_fail_
  let unsat_core_ : int Sequence.t ref = ref unsat_core_fail_
  let pp_ = ref Lit.pp

  let pp_clause out c =
    Format.fprintf out "[@[<hv>%a@]]"
      (CCFormat.list ~start:"" ~stop:"" ~sep:" ⊔ " !pp_) c

  let tag_ = snd

  let pp_form_simpl out (c,_) =
    CCFormat.list ~start:"" ~stop:"" ~sep:"" pp_clause out c

  let pp_form fmt f =
    match tag_ f with
    | None -> pp_form_simpl fmt f
    | Some tag -> Format.fprintf fmt "%a/%d" pp_form_simpl f tag

  let valuation l = !eval_ l
  let valuation_level l = !eval_level_ l
  let unsat_core k = !unsat_core_ k

  (* TODO: use specific interface of MSat *)

  module SatForm = struct
    include Lit
    type proof = unit
    let fresh () = Lit.make (Lit.payload Lit.dummy) (* FIXME *)
    let label _ = assert false
    let add_label _ _ = assert false
    let print = Lit.pp
  end

  (* Instantiate solver *)
  module S = Msat.Solver.Make(SatForm)(Msat.Solver.DummyTheory(SatForm))

  let assume_ ?tag (l:Lit.t list list) = S.assume ?tag l

  let check_ () =
    if Queue.is_empty queue_ then !result_
    else (
      unsat_core_ := unsat_core_fail_;
      eval_ := eval_fail_;
      begin try
        (* add pending clauses *)
        while not (Queue.is_empty queue_) do
          let c, tag = Queue.pop queue_ in
          Util.debugf ~section 4 "@[<hv2>assume@ @[%a@]@]" (fun k->k pp_form (c,tag));
          assume_ ?tag c
        done;
        (* solve *)
        S.solve ();
        eval_ := S.eval;
        eval_level_ := S.eval_level;
        result_ := Sat;
      with S.Unsat ->
        unsat_core_ := begin
          (* compute Unsat core *)
          let us = match S.unsat_conflict () with
            | None -> assert false
            | Some c -> S.Proof.prove_unsat c |> S.Proof.unsat_core
          in
          (fun k ->
            us
            |> CCList.to_seq
            |> CCFun.tap
              (fun seq ->
                Util.debugf ~section 4 "@[unsat_core:@ @[<hv>%a@]@]"
                  (fun k->k (CCFormat.seq S.Proof.print_clause) seq))
            |> Sequence.filter_map S.tag_clause
            |> Sequence.sort_uniq ~cmp:CCInt.compare
            |> Sequence.iter k)
        end;
        result_ := Unsat;
      end;
      !result_
    )

  let check () = Util.with_prof prof_call_msat check_ ()

  let set_printer pp = pp_ := pp

  type save_level = int

  let root_save_level = 0

  let save () = assert false

  let restore _ = assert false
end
