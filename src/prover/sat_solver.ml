
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Bridge to [MSat] prover} *)

open Libzipperposition

let section = Util.Section.make ~parent:Const.section "msat"
let prof_call_msat = Util.mk_profiler "msat.call"

include Sat_solver_intf

(* a taged set of clauses *)
type form = clause list * int option

let compare_form (c1,o1) (c2,o2) =
  let open CCOrd in
  list_ (list_ (Lit.compare)) c1 c2
  <?> (option CCInt.compare, o1, o2)

module FormSet = CCSet.Make(struct
  type t = form
  let compare = compare_form
end)

module Make(X : sig end) = struct
  (* queue of clauses waiting for being pushed into the solver *)
  let queue_ = Queue.create()

  let add_clause ?tag (c:clause) = Queue.push ([c], tag) queue_

  let add_clauses ?tag l = Queue.push (l, tag) queue_

  let add_clause_seq ?tag (seq:clause Sequence.t) =
    add_clauses ?tag (Sequence.to_rev_list seq)

  let eval_fail_ _ = invalid_arg "eval"
  let unsat_core_fail_ _ = invalid_arg "unsat core"

  let result_ = ref Sat
  let eval_ = ref eval_fail_
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
  let unsat_core k = !unsat_core_ k

  (* Instantiate solver *)
  module S = Msat.Sat.Make(struct
    let buf_ = Buffer.create 16
    let debug _ msg = Printf.ifprintf buf_ msg (* ignore *)
  end)

  let assume_ ?tag (l:Lit.t list list) =
    let l = (l :> int list list)
      |> List.map (List.map S.make)
    in
    S.assume ?tag l

  (* TODO incrementality *)

  let check_ () =
    if Queue.is_empty queue_ then !result_
    else (
      unsat_core_ := unsat_core_fail_;
      eval_ := eval_fail_;
      (* add pending clauses *)
      while not (Queue.is_empty queue_) do
        let c, tag = Queue.pop queue_ in
        Util.debugf ~section 4 "@[<hv2>assume@ @[%a@]@]" (fun k->k pp_form (c,tag));
        assume_ ?tag c
      done;
      (* solve *)
      let res = match S.solve () with
        | S.Sat ->
            eval_ := (fun (l:Lit.t) -> S.eval (S.make (l:>int)));
            Sat
        | S.Unsat ->
            unsat_core_ := begin
              let us = S.get_proof () |> S.unsat_core in
              (fun k ->
                us
                |> CCList.to_seq
                |> CCFun.tap
                  (fun seq ->
                    Util.debugf ~section 4 "@[unsat_core:@ @[<hv>%a@]@]"
                      (fun k->k (CCFormat.seq S.print_clause) seq))
                |> Sequence.filter_map S.tag_clause
                |> Sequence.sort_uniq ~cmp:CCInt.compare
                |> Sequence.iter k)
            end;
            Unsat
      in
      result_ := res;
      res
    )

  let check () = Util.with_prof prof_call_msat check_ ()

  let set_printer pp = pp_ := pp

  type save_level = int

  let root_save_level = 0

  let save () = assert false

  let restore _ = assert false
end
