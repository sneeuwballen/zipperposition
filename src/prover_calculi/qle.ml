
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
      match lit with
      | L.Equation(lhs, rhs, true) ->
        if (T.is_const (T.head_term lhs)) then
          let sym = T.as_const_exn (T.head_term lhs) in
          if Term.equal Term.true_ rhs then
            Some (true, sym)
          else if Term.equal Term.false_ rhs then
            Some (false, sym)
          else
            None
        else None
      | _ -> None
    in
    let syms = ID.Tbl.create 128 in
    SAT.clear ();
    (* For each clause l1 \/ ... \/ lN (ignoring equality literals), generate N
       SAT clauses v1 \/ ... \/ vI-1 \/ ~wI \/ vI+1 \/ ... \/ vN, where vJ is the
       variable associated with lJ (sign and predicate symbol) and wJ is the
       variable associated with its negation. *)
    CS.iter (fun c ->
      let pred_subcl = CCArray.filter_map pred_of_lit (C.lits c) in
      Array.iter (fun (_, pred) ->
          if not (ID.Tbl.mem syms pred) then
            ID.Tbl.replace syms pred (BBox.make_fresh (), BBox.make_fresh ()))
        pred_subcl;
      Array.iter (fun special ->
            Array.map (fun ((pol, pred) as lit) ->
              let (make_lit_pos, use_pos_var) =
                if lit = special then (false, not pol) else (true, pol)
              in
              let (pos_var, neg_var) = ID.Tbl.find syms pred in
                (if use_pos_var then pos_var else neg_var)
                |> (if make_lit_pos then (fun lit -> lit) else SAT.Lit.neg))
          pred_subcl
          |> Array.to_list
          |> SAT.add_clause ~proof:Proof.Step.trivial)
        pred_subcl) cs;
    CCFormat.printf "%a@." (ID.Tbl.pp ID.pp (CCPair.pp SAT.Lit.pp SAT.Lit.pp))
      syms;
    (* For each predicate p, generate a SAT clause ~p+ \/ ~p-. *)
    Iter.iter (fun (pos_var, neg_var) ->
        SAT.add_clause ~proof:Proof.Step.trivial
          [SAT.Lit.neg pos_var; SAT.Lit.neg neg_var])
      (ID.Tbl.values syms);
    (* Generate a SAT clause p1+ \/ p1- \/ ... \/ pN+ \/ pN-. *)
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
    | _ -> ());
    SAT.clear ()

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
