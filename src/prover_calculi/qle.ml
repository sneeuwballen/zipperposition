
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Quasipure Literal Elimination} *)
open Logtk
open Libzipperposition

module type S = sig
  module Env : Env.S
  val setup : unit -> unit
end

module Make(E : Env.S) : S with module Env = E = struct
  module Env = E
  module C = Env.C
  module CS = C.ClauseSet
  module L = Literal
  module T = Term
  module SAT = Sat_solver.Make ()

  let do_qle cs =
    (* TODO: Check first-orderness of problem. *)
    let pred_of_lit lit =
      if L.is_predicate_lit lit then (
        match lit with
        | L.Equation(lhs, _, _) ->
          if (T.is_const (T.head_term lhs)) then (
            let sym = T.as_const_exn (T.head_term lhs) in
            Some sym
          ) else None
        | _ -> None
      ) else None
    in
    let syms = ID.Tbl.create 128 in
    CS.iter (fun c ->
      Array.iter (fun pred ->
          if not (ID.Tbl.mem syms pred) then
            ID.Tbl.replace syms pred (BBox.make_fresh (), BBox.make_fresh ()))
        (CCArray.filter_map pred_of_lit (C.lits c))) cs;
    CCFormat.printf "%a@." (ID.Tbl.pp ID.pp (CCPair.pp SAT.Lit.pp SAT.Lit.pp))
      syms;
    (* Given a predicate p, generate a clause ~p+ \/ ~p-. *)
    Iter.iter (fun (pos_var, neg_var) ->
        SAT.add_clause ~proof:Proof.Step.trivial
          [SAT.Lit.neg pos_var; SAT.Lit.neg neg_var])
      (ID.Tbl.values syms);
    (* Generate a clause p1+ \/ p1- \/ ... \/ pN+ \/ pN-. *)
    SAT.add_clause ~proof:Proof.Step.trivial
      (CCList.flat_map (fun (pos_var, neg_var) -> [pos_var; neg_var])
        (CCList.of_iter (ID.Tbl.values syms)));
    (match SAT.check ~full:true () with
    | Sat_solver.Sat ->
      CCFormat.printf "SATISFIABLE\n";
      Iter.iter (fun (pos_var, neg_var) ->
          CCFormat.printf "%a%b\n" SAT.Lit.pp pos_var (SAT.valuation pos_var);
          CCFormat.printf "%a%b\n" SAT.Lit.pp neg_var (SAT.valuation neg_var))
        (ID.Tbl.values syms)
    | _ -> ())

  let setup () =
    Signal.once Env.on_start
      (fun () -> do_qle (CS.of_iter (Env.get_passive ())))
end

let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module QLE = Make(E) in

    QLE.setup ()
  in
  { Extensions.default with Extensions.
                         name = "qle";
                         prio = 50;
                         env_actions = [action];
  }
