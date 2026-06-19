(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Main saturation algorithm.}
    It uses inference rules and simplification rules from Superposition. *)

open Logtk
module C = Clause
module O = Ordering
module PS = ProofState
module Sel = Selection
module EIntf = Eprover_interface

let stat_redundant_given = Util.mk_stat "saturate.redundant given clauses"
let stat_processed_given = Util.mk_stat "saturate.processed given clauses"
let stat_steps = Util.mk_stat "saturate.steps"
let section = Util.Section.make ~parent:Const.section "saturate"
let k_abort_after_fragment_check = Flex_state.create_key ()

let check_timeout = function
  | None -> false
  | Some timeout -> Util.total_time_s () > timeout

let e_path = ref (None : string option)
let tried_e = ref false
let e_call_point = ref 0.2

let should_try_e = function
  | Some timeout when CCOpt.is_some !e_path ->
    let passed = Util.total_time_s () in
    if (not !tried_e) && passed > !e_call_point *. timeout then (
      tried_e := true;
      true
    ) else
      false
  | _ -> false

let _progress = ref false (* progress bar? *)
let _check_types = ref false
let _max_multi_simpl = ref (-1)

(* print progress (i out of steps) *)
let print_progress i ~steps =
  let prefix = Printf.sprintf "\r\027[K[%.2fs] " (Util.total_time_s ()) in
  match steps with
  | Some j ->
    let n = i * 40 / j in
    let bar =
      CCString.init 40 (fun i ->
          if i <= n then
            '#'
          else
            ' ')
    in
    Printf.printf "%s [%s] %d/%d%!" prefix bar i j
  | None -> Printf.printf "%s %d steps%!" prefix i

(** The SZS status of a state *)
type szs_status =
  | Unsat of Proof.S.t
  | Sat
  | Unknown
  | Error of string
  | Timeout

(* Lazy E-prover interface — first call triggers setup *)
let _e_setup_done = ref false

let _ensure_e_setup env =
  if not !_e_setup_done then (
    _e_setup_done := true;
    Eprover_interface.setup env
  )

let eprover_set_e_bin path = Eprover_interface.set_e_bin path

let eprover_try_e env active passive =
  _ensure_e_setup env;
  Eprover_interface.try_e env active passive

let check_fragment env =
  if not (Env.get_passive env () |> Iter.for_all (Env.check_fragment env)) then
    invalid_arg "Problem out of fragment"
  else if
    try Env.flex_get_of env k_abort_after_fragment_check
    with Not_found -> false
  then (
    print_endline "Problem in fragment";
    exit 0
  )

let register_conjectures env =
  Env.get_passive env () |> Iter.iter ClauseQueue.register_conjecture_clause

let _setup_done = ref false

let setup_once env =
  if !_setup_done then
    ()
  else (
    _setup_done := true;
    Env.flex_add_of env Env.k_max_multi_simpl_depth !_max_multi_simpl;
    Signal.on_every (Env.on_start env) (fun () -> check_fragment env);
    Signal.on_every (Env.on_start env) (fun () -> register_conjectures env)
  )

let[@inline] check_clause_ c =
  if !_check_types then Env.C.check_types c;
  assert (Env.C.Seq.terms c |> Iter.for_all Term.DB.is_closed);
  assert (Env.C.Seq.terms c |> Iter.for_all Term.is_properly_encoded);
  if not (Env.C.lits c |> Literals.vars_distinct) then (
    CCFormat.printf "Vars not distinct: @[%a@].@." Env.C.pp_tstp c;
    CCFormat.printf "proof:@[%a@].@." Proof.S.pp_normal (Env.C.proof c);
    assert false
  );
  CCArray.iter (fun t -> assert (Literal.no_prop_invariant t)) (Env.C.lits c)

let[@inline] check_clauses_ seq = Iter.iter check_clause_ seq

(** One iteration of the main loop ("given clause loop") *)
let given_clause_step env ?(generating = true) num =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "saturate.step" in
  Env.step_init env;
  (* select next given clause *)
  Env.do_clause_eliminate env;
  match Env.next_passive env () with
  | None ->
    (* final check: might generate other clauses *)
    let clauses = Env.do_generate env ~full:true () in
    if Iter.is_empty clauses then (
      Util.debugf ~section 3 "saturated set: @[%a@]@." (fun k ->
          k (Iter.pp_seq Env.C.pp_tstp_full) (Env.get_active env ()));
      Sat
    ) else (
      let clauses =
        clauses
        |> Iter.filter_map (fun c ->
               check_clause_ c;
               let c, _ = Env.unary_simplify env c in
               if
                 Env.is_trivial env c || Env.is_active env c
                 || Env.is_passive env c
               then
                 None
               else
                 Some c)
        |> Iter.to_list
      in
      Util.debugf 5 ~section "@[<2>inferred @{<green>new clauses@}@ @[<v>%a@]@]"
        (fun k -> k (CCFormat.list Env.C.pp) clauses);
      Env.add_passive env (Iter.of_list clauses);
      Unknown
    )
  | Some c ->
    let picked_clause = c in
    Util.debugf ~section 3
      "@[<2>@{<green>given@} (before simplification):@ `@[%a@]`@]" (fun k ->
        k Env.C.pp c);
    Util.debugf ~section 10 "@[proof:@[%a@]@]" (fun k ->
        k Proof.S.pp_tstp (Env.C.proof c));
    Trace.messagef (fun k -> k "given: %a" Env.C.pp_tstp c);

    check_clause_ c;
    Util.incr_stat stat_steps;
    (match Env.all_simplify env c with
    | [], _ ->
      Util.incr_stat stat_redundant_given;
      Util.debugf ~section 2 "@[@{<Yellow>### step %5d ###@}@]" (fun k -> k num);
      Util.debugf ~section 1 "@[<2>given clause dropped@ @[%a@]@]" (fun k ->
          k Env.C.pp c);
      Util.debugf ~section 3 "@[proof:@[%a@]@]" (fun k ->
          k Proof.S.pp_zf (Env.C.proof c));
      Signal.send (Env.on_forward_simplified env) (c, None);
      Unknown
    | l, _ when List.exists Env.C.is_empty l ->
      (* empty clause found *)
      let proof = Env.C.proof (List.find Env.C.is_empty l) in
      (* not sending any signal, because WE HAVE WON!!! *)
      Unsat proof
    | c :: l', state ->
      (* put clauses of [l'] back in passive set *)
      Util.debugf ~section 3 "@[ remaining after simplification:@.@[%a@]@. @]"
        (fun k -> k (CCList.pp Env.C.pp) l');

      Env.add_passive env (Iter.of_list l');

      Signal.send (Env.on_forward_simplified env) (picked_clause, Some c);

      (* assert(not (Env.C.is_redundant c)); *)

      (* clause might have been removed *)
      if Env.C.is_redundant c then
        Unknown
      else (
        (* process the clause [c] *)
        let new_clauses = CCVector.create () in
        (* very expensive assert *)
        (* assert (not (Env.is_redundant env c)); *)
        (* process the given clause! *)
        Util.incr_stat stat_processed_given;
        Util.debugf ~section 2 "@[@{<Yellow>### step %5d ###@}@]" (fun k ->
            k num);
        Util.debugf ~section 1
          "@[<2>@{<green>given@} (%d steps, penalty %d):@ `@[%a@]`@]" (fun k ->
            k num (Env.C.penalty c) Env.C.pp c);
        Util.debugf ~section 3 "@[proof:@[%a@]@]" (fun k ->
            k Proof.S.pp_tstp (Env.C.proof c));
        (* find clauses that are subsumed by given in active_set *)
        let subsumed_active =
          C.ClauseSet.to_seq (Env.subsumed_by env c) |> Iter.of_seq
        in
        Env.remove_active env subsumed_active;
        Env.remove_simpl env subsumed_active;
        (* add given clause to simpl_set *)
        Env.add_simpl env (Iter.singleton c);
        (* simplify active set using c *)
        let simplified_actives, newly_simplified =
          Env.backward_simplify env c
        in
        let simplified_actives =
          C.ClauseSet.to_seq simplified_actives |> Iter.of_seq
        in
        (* the simplified active clauses are removed from active set and
            added to the set of new clauses. Their descendants are also removed
            from passive set *)
        check_clauses_ simplified_actives;
        check_clauses_ newly_simplified;
        Env.remove_active env simplified_actives;
        Env.remove_simpl env simplified_actives;
        CCVector.append_iter new_clauses newly_simplified;

        if not (Iter.is_empty simplified_actives) then
          Util.debugf ~section 1 "simplified_actives:@ @[%a@]@." (fun k ->
              k (Iter.pp_seq Env.C.pp) simplified_actives);
        Util.debugf ~section 5 "newly_simplified:@ @[%a@]@." (fun k ->
            k (Iter.pp_seq Env.C.pp) newly_simplified);

        (* add given clause to active set *)
        Env.add_active env (Iter.singleton c);
        (* do inferences between c and the active set (including c),
            if [generate] is set to true *)
        let inferred_clauses =
          if generating then
            Env.generate env c
          else
            Iter.empty
        in
        (* simplification of inferred clauses w.r.t active set; only the non-trivial ones
            are kept (by list-simplify) *)
        let inferred_clauses =
          Iter.filter_map
            (fun c ->
              Util.debugf ~section 4 "inferred: `@[%a@]`" (fun k ->
                  k Env.C.pp c);
              let c, _ = Env.forward_simplify env c in
              check_clause_ c;
              (* keep clauses  that are not redundant *)
              if
                Env.is_trivial env c || Env.is_active env c
                || Env.is_passive env c
              then (
                Util.debugf ~section 4 "clause `@[%a@]` is trivial, dump"
                  (fun k -> k Env.C.pp c);
                Util.debugf ~section 10 "@[proof:@[%a@]@]" (fun k ->
                    k Proof.S.pp_tstp (Env.C.proof c));

                None
              ) else
                Some c)
            inferred_clauses
        in
        let inferred_clauses = Env.immediate_simplify env c inferred_clauses in
        let inferred_clauses =
          (* After forward simplification, do cheap multi simplification like AVATAR *)
          Iter.flat_map_l
            (fun c ->
              CCOpt.get_or ~default:[ c ] (Env.cheap_multi_simplify env c))
            inferred_clauses
        in
        CCVector.append_iter new_clauses inferred_clauses;
        Util.debugf ~section 2
          "@[<2>inferred @{<green>new clauses@}:@ [@[<v>%a@]]@]" (fun k ->
            k (Util.pp_iter Env.C.pp) (CCVector.to_iter new_clauses));
        (* add new clauses (including simplified active clauses)
            to passive set and simpl_set *)
        Env.add_passive env (CCVector.to_iter new_clauses);
        (* test whether the empty clause has been found *)
        match Env.get_some_empty_clause env with
        | None -> Unknown
        | Some c ->
          let pr = Env.C.proof c in
          Unsat pr
      ))

let given_clause env ?(generating = true) ?steps ?timeout () =
  setup_once env;
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "saturate.given-clause-algo" in
  if CCOpt.is_some !e_path then eprover_set_e_bin (CCOpt.get_exn !e_path);

  (* num: number of steps done so far *)
  let rec do_step num =
    if check_timeout timeout then
      Timeout, num
    else (
      match steps with
      | Some i when num >= i -> Unknown, num
      | _ ->
        (* do one step *)
        if !_progress then print_progress num ~steps;

        if should_try_e timeout then (
          let res =
            eprover_try_e env (Env.get_active env ()) (Env.get_passive env ())
          in
          match res with
          | Some c -> Env.add_passive env (Iter.singleton c)
          | _ -> ()
        );

        let status = given_clause_step env ~generating num in
        (match status with
        | Sat | Unsat _ | Error _ -> status, num (* finished *)
        | Timeout -> assert false
        | Unknown -> do_step (num + 1))
    )
  in
  do_step 0

let presaturate env =
  setup_once env;
  given_clause env ?steps:None ?timeout:None ~generating:false ()

let () =
  Params.add_opts
    [
      "--progress", Arg.Set _progress, " progress bar";
      "-p", Arg.Set _progress, " alias for --progress";
      "--check-types", Arg.Set _check_types, " check types in new clauses";
      ( "--max-multi-simpl-depth",
        Arg.Int (( := ) _max_multi_simpl),
        " maixmum depth of multi step simplification. -1 disables maximum \
         depth." );
      ( "--try-e",
        Arg.String (fun path -> e_path := Some path),
        " try the given eprover binary on the problem" );
      ( "--disable-e",
        Arg.Unit (fun () -> e_path := None),
        " disable E background reasoner" );
      ( "--e-call-point",
        Arg.Float
          (fun v ->
            if v > 1.0 || v < 0.0 then
              invalid_arg "0 <= e-call-point <= 1.0"
            else
              e_call_point := v),
        " point in the runtime when E is called in range 0.0 to 1.0 " );
    ]
