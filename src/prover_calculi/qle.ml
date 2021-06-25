
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Quasipure Literal Elimination} *)
open Logtk
open Libzipperposition

let k_enabled = Flex_state.create_key ()
(* let k_inprocessing = Flex_state.create_key () *)
(* let k_check_at = Flex_state.create_key () *)

module A = Libzipperposition_avatar

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
  module BCE = Bce.Make(E)

  let do_qle cs =
    (* TODO: Check first-orderness of problem. *)

    let add_SAT_clause c =
      CCFormat.printf "add %a\n" (CCList.pp SAT.Lit.pp) c;
      SAT.add_clause ~proof:Proof.Step.trivial c
    in
    let pred_of_lit lit =
      match lit with
      | L.Equation (lhs, rhs, true) ->
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
    let all_syms = ID.Tbl.create 128 in

    SAT.clear ();
    (* For each clause l1 \/ ... \/ lN (ignoring equality literals), generate N
       SAT clauses v1 \/ ... \/ vI-1 \/ ~wI \/ vI+1 \/ ... \/ vN, where vJ is the
       variable associated with lJ (sign and predicate symbol) and wJ is the
       variable associated with its negation. *)
    CS.iter (fun c ->
      CCFormat.printf "----> %a\n" C.pp c;
      let pred_subcl = CCArray.filter_map pred_of_lit (C.lits c) in
      CCFormat.printf "-----> %d\n" (Array.length pred_subcl);
      Array.iter (fun (_, pred) ->
          if not (ID.Tbl.mem all_syms pred) then
            ID.Tbl.replace all_syms pred (BBox.make_fresh (), BBox.make_fresh ()))
        pred_subcl;
      Array.iter (fun special ->
            Array.map (fun ((pol, pred) as lit) ->
              let (make_lit_pos, use_pos_var) =
                if lit = special then (false, not pol) else (true, pol)
              in
              let (pos_var, neg_var) = ID.Tbl.find all_syms pred in
                (if use_pos_var then pos_var else neg_var)
                |> (if make_lit_pos then (fun lit -> lit) else SAT.Lit.neg))
          pred_subcl
          |> Array.to_list
          |> add_SAT_clause)
        pred_subcl) cs;
    CCFormat.printf "%a\n" (ID.Tbl.pp ID.pp (CCPair.pp SAT.Lit.pp SAT.Lit.pp))
      all_syms;
    (* For each predicate p, generate a SAT clause ~p+ \/ ~p-. *)
    Iter.iter (fun (pos_var, neg_var) ->
        add_SAT_clause [SAT.Lit.neg pos_var; SAT.Lit.neg neg_var])
      (ID.Tbl.values all_syms);

    let unknown_syms = ID.Tbl.copy all_syms in
    let quasipure_syms = ID.Tbl.create 32 in

    (* Generate a SAT clause p1+ \/ p1- \/ ... \/ pN+ \/ pN-, where the pIs are
       the predicate symbols of unknown purity status (initially all). *)
    let generate_nontrivial_solution_SAT_clause () =
      add_SAT_clause (CCList.flat_map
        (fun (pos_var, neg_var) -> [pos_var; neg_var])
        (CCList.of_iter (ID.Tbl.values unknown_syms)))
    in

    let rec maximize_valuation () =
      Iter.iter (fun (pred, (pos_var, neg_var)) ->
          if SAT.valuation pos_var then (
            add_SAT_clause [pos_var];
            ID.Tbl.replace quasipure_syms pred pos_var;
            ID.Tbl.remove unknown_syms pred
          );
          if SAT.valuation neg_var then (
            add_SAT_clause [neg_var];
            ID.Tbl.replace quasipure_syms pred neg_var;
            ID.Tbl.remove unknown_syms pred
          ))
        (ID.Tbl.to_iter unknown_syms);
      generate_nontrivial_solution_SAT_clause ();
      (match SAT.check ~full:true () with
      | Sat_solver.Sat -> maximize_valuation ()
      | _ -> ())
    in
    let filter_clauses () =
      let is_quasipure_lit lit =
        match lit with
        | L.Equation (lhs, rhs, true) ->
          if (T.is_const (T.head_term lhs)) then
            let sym = T.as_const_exn (T.head_term lhs) in
            ID.Tbl.mem quasipure_syms sym
          else false
        | _ -> false
      in
      let contains_quasipure_sym c =
        CCArray.exists is_quasipure_lit (C.lits c)
      in
      CS.iter (fun c ->
          if contains_quasipure_sym c then (
            CCFormat.printf "Removing %a\n" C.pp c;
            BCE.remove_from_proof_state c
          ))
        cs
    in

    generate_nontrivial_solution_SAT_clause ();
    (match SAT.check ~full:true () with
    | Sat_solver.Sat ->
      CCFormat.printf "Satisfiable\n";
      maximize_valuation ();
      CCFormat.printf "Solution:\n";
      Iter.iter (fun var -> CCFormat.printf "%a\n" SAT.Lit.pp var)
        (ID.Tbl.values quasipure_syms);
      filter_clauses ()
    | _ -> ());
    SAT.clear ()

  let setup () =
    if E.flex_get k_enabled then
      if not (Env.flex_get A.k_avatar_enabled) then
        Signal.once Env.on_start
          (fun () -> do_qle (CS.of_iter (Env.get_passive ())))
      else
        CCFormat.printf "AVATAR is not yet compatible with QLE@."
end

let _enabled = ref false
(* let _inprocessing = ref false *)
(* let _check_at = ref 10 *)

let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module QLE = Make(E) in

    E.flex_add k_enabled !_enabled;
    (* E.flex_add k_inprocessing !_inprocessing; *)
    QLE.setup ()
  in
  { Extensions.default with Extensions.
                         name = "qle";
                         prio = 50;
                         env_actions = [action];
  }

let () =
  Options.add_opts [
    "--qle", Arg.Bool ((:=) _enabled), " enable QLE";
(*
    "--qle-inprocessing", Arg.Bool ((:=) _inprocessing), " enable QLE as inprocessing rule";
    "--qle-check-at", Arg.Int ((:=) _check_at), " QLE inprocessing periodicity";
*)
  ]
