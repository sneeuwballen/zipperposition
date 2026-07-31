(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk
(** {1 Quasipure Literal Elimination} *)

open Libzipperposition

let k_enabled = Flex_state.create_key ()
let k_inprocessing = Flex_state.create_key ()
let k_check_at = Flex_state.create_key ()
let k_pure_only = Flex_state.create_key ()
let section = Util.Section.make ~parent:Const.section "qle"

module A = Libzipperposition_avatar
module C = Clause
module CS = Clause.ClauseSet
module L = Literal
module T = Term
module SAT = Sat_solver

let sat = SAT.create ()

let remove_from_proof_state env c =
  Util.debugf ~section 1 "removing @[%a@]" (fun k -> k C.pp c);

  C.mark_redundant c;
  Env.remove_active env (Iter.singleton c);
  Env.remove_passive env (Iter.singleton c);
  Env.remove_simpl env (Iter.singleton c)

let do_qle env pure_only c_iter =
  Util.debugf ~section 2 "init: @[%a@]@." (fun k -> k (Iter.pp_seq C.pp) c_iter);

  let add_SAT_clause c = SAT.add_clause sat ~proof:Proof.Step.trivial c in
  let pred_of_lit lit =
    match lit with
    | L.Equation (lhs, _, _) when L.is_predicate_lit lit ->
      if T.is_const (T.head_term lhs) then (
        let sym = T.as_const_exn (T.head_term lhs) in
        Some (L.is_positivoid lit, sym)
      ) else
        None
    | _ -> None
  in
  let all_syms = Name.Tbl.create 128 in

  SAT.clear sat ();

  (* For each clause l1 \/ ... \/ lN (ignoring equality literals), if
     pure_only is false, generate N SAT clauses
     v1 \/ ... \/ vI-1 \/ ~wI \/ vI+1 \/ ... \/ vN,
     where vJ is the variable associated with lJ (sign and predicate symbol)
     and wJ is the variable associated with its negation.

     If pure_only is true, generate N SAT clauses ~wI. *)
  Iter.iter
    (fun c ->
      let pred_subcl = CCArray.filter_map pred_of_lit (C.lits c) in

      let mk_pure_clauses (pol, pred) =
        let pos_var, neg_var = Name.Tbl.find all_syms pred in
        [
          SAT.Lit.neg
            (if not pol then
               pos_var
             else
               neg_var);
        ]
        |> add_SAT_clause
      in
      let mk_quasipure_clauses special =
        Array.map
          (fun ((pol, pred) as lit) ->
            let make_lit_pos, use_pos_var =
              if lit = special then
                false, not pol
              else
                true, pol
            in
            let pos_var, neg_var = Name.Tbl.find all_syms pred in
            (if use_pos_var then
               pos_var
             else
               neg_var)
            |>
            if make_lit_pos then
              fun lit ->
            lit
            else
              SAT.Lit.neg)
          pred_subcl
        |> Array.to_list |> add_SAT_clause
      in

      (* Create p+, p- variables for each predicate symbol p. *)
      Array.iter
        (fun (_, pred) ->
          if not (Name.Tbl.mem all_syms pred) then
            Name.Tbl.replace all_syms pred
              (BBox.make_fresh (), BBox.make_fresh ()))
        pred_subcl;

      (* Create a number of SAT clauses for each clause. *)
      Array.iter
        (if pure_only then
           mk_pure_clauses
         else
           mk_quasipure_clauses)
        pred_subcl)
    c_iter;

  (* Make sure that deep, higher-order occurrences of predicate symbols are
     protected by other symbols. If pure_only is true, prevent such symbols
     from being pure. *)
  Iter.iter
    (fun c ->
      let forget_or_protect_syms =
        Iter.iter (fun bad ->
            if Name.Tbl.mem all_syms bad then
              if pure_only then
                Name.Tbl.update all_syms ~f:(fun _ _ -> None) ~k:bad
              else (
                let bad_pos_var, bad_neg_var = Name.Tbl.find all_syms bad in
                let mk_clause bad_var =
                  Array.append
                    (Array.make 1 (SAT.Lit.neg bad_var))
                    (Array.map
                       (fun (pol, pred) ->
                         let pos_var, neg_var = Name.Tbl.find all_syms pred in
                         if pol then
                           pos_var
                         else
                           neg_var)
                       (CCArray.filter_map pred_of_lit (C.lits c)))
                  |> Array.to_list |> add_SAT_clause
                in
                mk_clause bad_pos_var;
                mk_clause bad_neg_var
              ))
      in
      Array.iter
        (fun lit ->
          match lit with
          | L.Equation (lhs, _, _) when L.is_predicate_lit lit ->
            if T.is_const (T.head_term lhs) then (
              let bad_syms =
                Iter.flat_map T.Seq.symbols (Iter.of_list (T.args lhs))
              in
              forget_or_protect_syms bad_syms
            ) else
              forget_or_protect_syms (T.Seq.symbols lhs)
          | L.Equation (lhs, rhs, _) ->
            forget_or_protect_syms (T.Seq.symbols lhs);
            forget_or_protect_syms (T.Seq.symbols rhs)
          | _ -> ())
        (C.lits c))
    c_iter;

  (* For each predicate p, generate a SAT clause ~p+ \/ ~p-. *)
  Iter.iter
    (fun (pos_var, neg_var) ->
      add_SAT_clause [ SAT.Lit.neg pos_var; SAT.Lit.neg neg_var ])
    (Name.Tbl.values all_syms);

  let unknown_syms = Name.Tbl.copy all_syms in
  let quasipure_syms = Name.Tbl.create 32 in

  (* Generate a SAT clause p1+ \/ p1- \/ ... \/ pN+ \/ pN-, where the pIs are
     the predicate symbols of unknown purity status (initially all). *)
  let generate_nontrivial_solution_SAT_clause () =
    add_SAT_clause
      (CCList.flat_map
         (fun (pos_var, neg_var) -> [ pos_var; neg_var ])
         (CCList.of_iter (Name.Tbl.values unknown_syms)))
  in

  let rec maximize_valuation () =
    Iter.iter
      (fun (pred, (pos_var, neg_var)) ->
        if SAT.valuation sat pos_var then (
          add_SAT_clause [ pos_var ];
          Name.Tbl.replace quasipure_syms pred pos_var;
          Name.Tbl.remove unknown_syms pred
        );
        if SAT.valuation sat neg_var then (
          add_SAT_clause [ neg_var ];
          Name.Tbl.replace quasipure_syms pred neg_var;
          Name.Tbl.remove unknown_syms pred
        ))
      (Name.Tbl.to_iter unknown_syms);
    generate_nontrivial_solution_SAT_clause ();
    match SAT.check sat ~full:true () with
    | SAT.Sat -> maximize_valuation ()
    | _ -> ()
  in
  let filter_clauses () =
    let is_quasipure_lit lit =
      match lit with
      | L.Equation (lhs, rhs, true) ->
        if T.is_const (T.head_term lhs) then (
          let sym = T.as_const_exn (T.head_term lhs) in
          Name.Tbl.mem quasipure_syms sym
        ) else
          false
      | _ -> false
    in
    let contains_quasipure_sym c = CCArray.exists is_quasipure_lit (C.lits c) in
    Util.debugf ~section 1
      (if pure_only then
         "pure syms: @[%a@]"
       else
         "quasipure syms: @[%a@]")
      (fun k -> k (CCList.pp Name.pp) (Name.Tbl.keys_list quasipure_syms));
    Iter.iter
      (fun c -> if contains_quasipure_sym c then remove_from_proof_state env c)
      c_iter
  in

  Util.debugf ~section 1 "In do_qle()@." CCFun.id;
  generate_nontrivial_solution_SAT_clause ();
  (match SAT.check sat ~full:true () with
  | SAT.Sat ->
    Util.debugf ~section 1 "Maximizing()@." CCFun.id;
    maximize_valuation ();
    filter_clauses ()
  | _ ->
    Util.debugf ~section 1 "Unsat()@." CCFun.id;
    ());
  SAT.clear sat ()

let get_clauses env =
  Iter.append (Env.get_passive env ()) (Env.get_active env ())

let steps = ref 0

let inprocessing env =
  if !steps = 0 then (
    Util.debugf ~section 1 "doing inprocessing@." CCFun.id;
    do_qle env (Env.flex_get_of env k_pure_only) (get_clauses env)
  );
  steps := (!steps + 1) mod Env.flex_get_of env k_check_at

let setup env =
  if Env.flex_get_of env k_enabled then
    if not (Env.flex_get_of env A.k_avatar_enabled) then
      if Env.flex_get_of env k_inprocessing then
        Env.add_clause_elimination_rule env ~priority:4 "qle" inprocessing
      else
        Signal.once (Env.on_start env) (fun () ->
            do_qle env (Env.flex_get_of env k_pure_only) (get_clauses env))
    else
      CCFormat.printf "AVATAR is not yet compatible with QLE@."

(* CLI refs migrated to Params.add_flex_opts *)

let extension =
  let action (env : Env.t) =
    Env.flex_ensure env k_enabled false;
    Env.flex_ensure env k_inprocessing false;
    Env.flex_ensure env k_check_at 100;
    Env.flex_ensure env k_pure_only false;
    setup env
  in
  {
    Extensions.default with
    Extensions.name = "qle";
    prio = 40;
    env_actions = [ action ];
  }

let () =
  Params.add_flex_opts (fun ~flex_ref ->
      [
        ( "--qle",
          Arg.Bool (fun v -> flex_ref := Flex_state.add k_enabled v !flex_ref),
          " enable/disable QLE" );
        ( "--qle-inprocessing",
          Arg.Bool
            (fun v -> flex_ref := Flex_state.add k_inprocessing v !flex_ref),
          " QLE as inprocessing rule" );
        ( "--qle-check-at",
          Arg.Int (fun n -> flex_ref := Flex_state.add k_check_at n !flex_ref),
          " QLE inprocessing periodicity" );
        ( "--qle-pure-only",
          Arg.Bool (fun v -> flex_ref := Flex_state.add k_pure_only v !flex_ref),
          " restrict QLE to pure literals" );
      ])
