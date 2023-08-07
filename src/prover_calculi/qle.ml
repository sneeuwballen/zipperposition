
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Quasipure Literal Elimination} *)
open Logtk
open Libzipperposition

let k_enabled = Flex_state.create_key ()
let k_inprocessing = Flex_state.create_key ()
let k_check_at = Flex_state.create_key ()
let k_pure_only = Flex_state.create_key ()

let section = Util.Section.make ~parent:Const.section "qle"

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

  let remove_from_proof_state c =
    Util.debugf ~section 1 "removing @[%a@]" (fun k -> k C.pp c);

    C.mark_redundant c;
    Env.remove_active (Iter.singleton c);
    Env.remove_passive (Iter.singleton c);
    Env.remove_simpl (Iter.singleton c)

  let do_qle pure_only c_iter =
    Util.debugf ~section 2 "init: @[%a@]@." (fun k -> k (Iter.pp_seq C.pp) c_iter);

    let add_SAT_clause c =
      SAT.add_clause ~proof:Proof.Step.trivial c
    in
    let pred_of_lit lit =
      match lit with
      | L.Equation (lhs, _, _) when L.is_predicate_lit lit ->
        if (T.is_const (T.head_term lhs)) then
          let sym = T.as_const_exn (T.head_term lhs) in
          Some (L.is_positivoid lit, sym)
        else None
      | _ -> None
    in
    let all_syms = ID.Tbl.create 128 in

    SAT.clear ();

    (* For each clause l1 \/ ... \/ lN (ignoring equality literals), if
       pure_only is false, generate N SAT clauses
       v1 \/ ... \/ vI-1 \/ ~wI \/ vI+1 \/ ... \/ vN,
       where vJ is the variable associated with lJ (sign and predicate symbol)
       and wJ is the variable associated with its negation.

       If pure_only is true, generate N SAT clauses ~wI. *)
    Iter.iter (fun c ->
        let pred_subcl = CCArray.filter_map pred_of_lit (C.lits c) in

        let mk_pure_clauses (pol, pred) =
          let (pos_var, neg_var) = ID.Tbl.find all_syms pred in
          [SAT.Lit.neg (if not pol then pos_var else neg_var)]
          |> add_SAT_clause
        in
        let mk_quasipure_clauses special =
          Array.map (fun ((pol, pred) as lit) ->
              let (make_lit_pos, use_pos_var) =
                if lit = special then (false, not pol) else (true, pol)
              in
              let (pos_var, neg_var) = ID.Tbl.find all_syms pred in
                (if use_pos_var then pos_var else neg_var)
                |> (if make_lit_pos then (fun lit -> lit) else SAT.Lit.neg))
            pred_subcl
          |> Array.to_list
          |> add_SAT_clause
        in

        (* Create p+, p- variables for each predicate symbol p. *)
        Array.iter (fun (_, pred) ->
            if not (ID.Tbl.mem all_syms pred) then
              ID.Tbl.replace all_syms pred
                (BBox.make_fresh (), BBox.make_fresh ()))
          pred_subcl;

        (* Create a number of SAT clauses for each clause. *)
        Array.iter (if pure_only then mk_pure_clauses else mk_quasipure_clauses)
          pred_subcl)
      c_iter;
    (* Detect problematic higher-order features and forget about some predicate
       symbols if necessary. *)
    Iter.iter (fun c ->
        let forget_syms = Iter.iter (fun bad ->
          ID.Tbl.update all_syms ~f:(fun _ _ -> None) ~k:bad)
        in
        Array.iter (fun lit ->
            match lit with
            | L.Equation (lhs, _, _) when L.is_predicate_lit lit ->
              if (T.is_const (T.head_term lhs)) then
                let bad_syms =
                  Iter.flat_map T.Seq.symbols (Iter.of_list (T.args lhs)) in
                forget_syms bad_syms
              else
                forget_syms (T.Seq.symbols lhs)
            | L.Equation (lhs, rhs, _) ->
              forget_syms (T.Seq.symbols lhs);
              forget_syms (T.Seq.symbols rhs)
            | _ -> ())
          (C.lits c))
      c_iter;

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
      Util.debugf ~section 1 "quasipure syms: @[%a@]" (fun k -> k (CCList.pp ID.pp) (ID.Tbl.keys_list quasipure_syms));
      Iter.iter (fun c ->
          if contains_quasipure_sym c then remove_from_proof_state c)
        c_iter
    in

    Util.debugf ~section 1 "In do_qle()@." CCFun.id;
    generate_nontrivial_solution_SAT_clause ();
    (match SAT.check ~full:true () with
    | Sat_solver.Sat ->
      Util.debugf ~section 1 "Maximizing()@." CCFun.id;
      maximize_valuation ();
      filter_clauses ()
    | _ -> Util.debugf ~section 1 "Unsat()@." CCFun.id; ());
    SAT.clear ()

  let get_clauses () = Iter.append (Env.get_passive ()) (Env.get_active ())

  let steps = ref 0
  let inprocessing () =
    if !steps = 0 then (
      Util.debugf ~section 1 "doing inprocessing@." CCFun.id;
      do_qle (E.flex_get k_pure_only) (get_clauses ()));
    steps := (!steps + 1) mod Env.flex_get k_check_at

  let setup () =
    if E.flex_get k_enabled then
      if not (Env.flex_get A.k_avatar_enabled) then
        if E.flex_get k_inprocessing then
          E.add_clause_elimination_rule ~priority:4 "qle" inprocessing
        else
          Signal.once Env.on_start (fun () ->
            do_qle (E.flex_get k_pure_only) (get_clauses ()))
      else
        CCFormat.printf "AVATAR is not yet compatible with QLE@."
end

let _enabled = ref false
let _inprocessing = ref false
let _check_at = ref 100
let _pure_only = ref false

let extension =
  let action env =
    let module E = (val env : Env.S) in
    let module QLE = Make(E) in

    E.flex_add k_enabled !_enabled;
    E.flex_add k_inprocessing !_inprocessing;
    E.flex_add k_check_at !_check_at;
    E.flex_add k_pure_only !_pure_only;
    QLE.setup ()
  in
  { Extensions.default with Extensions.
                         name = "qle";
                         prio = 40;
                         env_actions = [action]
  }

let () =
  Options.add_opts [
    "--qle", Arg.Bool ((:=) _enabled), " enable QLE";
    "--qle-inprocessing", Arg.Bool ((:=) _inprocessing), " enable QLE as inprocessing rule";
    "--qle-check-at", Arg.Int ((:=) _check_at), " QLE inprocessing periodicity";
    "--qle-pure-only", Arg.Bool ((:=) _pure_only), " enable PLE";
  ]
