
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

let check_timeout = function
  | None -> false
  | Some timeout -> Util.total_time_s () > timeout

let e_path = ref (None : string option)
let tried_e = ref false 
let should_try_e = function
  | Some timeout when CCOpt.is_some !e_path -> 
    let passed = Util.total_time_s () in
    if not !tried_e && passed > timeout /. 5.0 then (
      tried_e := true;
      true
    ) else false
  | _ -> false

let _progress = ref false (* progress bar? *)
let _check_types = ref false

(* print progress (i out of steps) *)
let print_progress i ~steps =
  let prefix = Printf.sprintf "\r\027[K[%.2fs] " (Util.total_time_s ()) in
  match steps with
  | Some j ->
    let n = i * 40 /j in
    let bar = CCString.init 40 (fun i -> if i<=n then '#' else ' ') in
    Printf.printf "%s [%s] %d/%d%!" prefix bar i j;
  | None ->
    Printf.printf "%s %d steps%!" prefix i;

    (** The SZS status of a state *)
type szs_status =
  | Unsat of Proof.S.t
  | Sat
  | Unknown
  | Error of string
  | Timeout

module type S = sig
  module Env : Env.S

  val given_clause_step : ?generating:bool -> int -> szs_status
  (** Perform one step of the given clause algorithm.
      It performs generating inferences only if [generating] is true (default);
      other parameters are the iteration number and the environment *)

  val given_clause:
    ?generating:bool -> ?steps:int -> ?timeout:float ->
    unit -> szs_status * int
  (** run the given clause until a timeout occurs or a result
      is found. It returns a tuple (new state, result, number of steps done).
      It performs generating inferences only if [generating] is true (default) *)

  val presaturate : unit -> szs_status * int
  (** Interreduction of the given state, without generating inferences. Returns
      the number of steps done for presaturation, with status of the set. *)
end


module Make(E : Env.S) = struct
  module Env = E
  module EInterface = EIntf.Make(E)

  let[@inline] check_clause_ c = 
    if !_check_types then Env.C.check_types c;
    assert (Env.C.Seq.terms c |> Iter.for_all Term.DB.is_closed);
    assert (Env.C.lits c |> Literals.vars_distinct);
    if not (Env.C.Seq.terms c |> Iter.for_all Lambda.is_properly_encoded) then (
      CCFormat.printf "ENCODED WRONGLY: %a:%d.\n" Env.C.pp_tstp c (Env.C.proof_depth c);
      CCFormat.printf "proof : %a.\n" Proof.S.pp_normal (Env.C.proof c);
      assert(false);
    );
    CCArray.iter (fun t -> assert(Literal.no_prop_invariant t)) (Env.C.lits c)

  let[@inline] check_clauses_ seq =
    Iter.iter check_clause_ seq

  (** One iteration of the main loop ("given clause loop") *)
  let given_clause_step ?(generating=true) num =
    E.step_init();
    (* select next given clause *)
    match Env.next_passive () with
    | None ->
      (* final check: might generate other clauses *)
      let clauses =
        Env.do_generate ~full:true ()
      in
      if Iter.is_empty clauses
      then Sat
      else (
        let clauses = clauses
                      |> Iter.filter_map
                        (fun c ->
                           check_clause_ c;
                           let c, _ = Env.unary_simplify c in
                           if Env.is_trivial c || Env.is_active c || Env.is_passive c
                           then None
                           else Some c)
                      |> Iter.to_list in
        Util.debugf 2 ~section "@[<2>inferred @{<green>new clauses@}@ @[<v>%a@]@]"
          (fun k->k (CCFormat.list Env.C.pp) clauses);
        Env.add_passive (Iter.of_list clauses);
        Unknown
      )
    | Some c ->
      check_clause_ c;
      Util.incr_stat stat_steps;
      begin match Env.all_simplify c with
        | [], _ ->
          Util.incr_stat stat_redundant_given;
          Util.debugf ~section 2 "@[<2>given clause dropped @[%a@]@@]"
            (fun k->k Env.C.pp c);
          Unknown
        | l, _ when List.exists Env.C.is_empty l ->
          (* empty clause found *)
          let proof = Env.C.proof (List.find Env.C.is_empty l) in
          Unsat proof
        | c :: l', _ ->
          (* put clauses of [l'] back in passive set *)
          assert(List.for_all (fun c -> 
              let is_prop_encoded = 
                Env.C.Seq.terms c |> Iter.for_all  Lambda.is_properly_encoded in
              if not is_prop_encoded then (
                CCFormat.printf "IMPROPERLY ENCODED: %a" Env.C.pp c;
              );
              is_prop_encoded) (c::l'));
          Env.add_passive (Iter.of_list l');
          (* process the clause [c] *)
          let new_clauses = CCVector.create () in
          (* very expensive assert *)
          (* assert (not (Env.is_redundant c)); *)
          (* process the given clause! *)
          (* (match Env.C.is_inj_axiom c with
             | Some (sym,i) -> Env.Ctx.set_injective_for_arg sym i
             | None -> ()); *)
          Util.incr_stat stat_processed_given;
          Util.debugf ~section 1 "@[@{<Yellow>### step %5d ###@}@]"(fun k->k num);
          Util.debugf ~section 1 "@[<2>@{<green>given@} (%d steps, penalty %d):@ `@[%a@]`@]"
            (fun k->k num (Env.C.penalty c) Env.C.pp c);
          (* find clauses that are subsumed by given in active_set *)
          let subsumed_active = Env.C.ClauseSet.to_seq (Env.subsumed_by c) in
          Env.remove_active subsumed_active;
          Env.remove_simpl subsumed_active;
          (* add given clause to simpl_set *)
          Env.add_simpl (Iter.singleton c);
          (* simplify active set using c *)
          let simplified_actives, newly_simplified = Env.backward_simplify c in
          let simplified_actives = Env.C.ClauseSet.to_seq simplified_actives in
          (* the simplified active clauses are removed from active set and
             added to the set of new clauses. Their descendants are also removed
             from passive set *)
          check_clauses_ simplified_actives;
          check_clauses_ newly_simplified;
          Env.remove_active simplified_actives;
          Env.remove_simpl simplified_actives;
          CCVector.append_seq new_clauses newly_simplified;
          (* add given clause to active set *)
          Env.add_active (Iter.singleton c);
          (* do inferences between c and the active set (including c),
             if [generate] is set to true *)
          let inferred_clauses = if generating
            then Env.generate c
            else Iter.empty in
          (* simplification of inferred clauses w.r.t active set; only the non-trivial ones
             are kept (by list-simplify) *)
          let inferred_clauses =
            Iter.filter_map
              (fun c ->
                 check_clause_ c;
                 let c, _ = Env.forward_simplify c in
                 check_clause_ c;
                 (* keep clauses  that are not redundant *)
                 if Env.is_trivial c || Env.is_active c || Env.is_passive c
                 then (
                   Util.debugf ~section 5 "clause `@[%a@]` is trivial, dump" (fun k->k Env.C.pp c);
                   None
                 ) else Some c)
              inferred_clauses
          in
          CCVector.append_seq new_clauses inferred_clauses;
          Util.debugf ~section 2 "@[<2>inferred @{<green>new clauses@}:@ [@[<v>%a@]]@]"
            (fun k->k (Util.pp_seq Env.C.pp) (CCVector.to_seq new_clauses));
          (* add new clauses (including simplified active clauses)
             to passive set and simpl_set *)
          Env.add_passive (CCVector.to_seq new_clauses);
          (* test whether the empty clause has been found *)
          match Env.get_some_empty_clause () with
          | None -> Unknown
          | Some c -> Unsat (Env.C.proof c)
      end

  let given_clause ?(generating=true) ?steps ?timeout () =
    if CCOpt.is_some !e_path then (
      EInterface.set_e_bin (CCOpt.get_exn !e_path)
    );

    (* num: number of steps done so far *)
    let rec do_step num =
      if check_timeout timeout then Timeout, num
      else match steps with
        | Some i when num >= i -> Unknown, num
        | _ ->
          (* do one step *)
          if !_progress then print_progress num ~steps;

          if should_try_e timeout then (
            let res = EInterface.try_e (Env.get_active ()) (Env.get_passive ()) in
            match res with 
            | Some c -> Env.add_passive (Iter.singleton c);
            | _ -> ()
          );

          let status = given_clause_step ~generating num in
          match status with
          | Sat | Unsat _ | Error _ -> status, num (* finished *)
          | Timeout -> assert false
          | Unknown -> do_step (num+1)
    in
    do_step 0

  let presaturate () =
    given_clause ?steps:None ?timeout:None ~generating:false ()
end

let () =
  Params.add_opts
    [ "--progress", Arg.Set _progress, " progress bar";
      "-p", Arg.Set _progress, " alias to --progress";
      "--check-types", Arg.Set _check_types, " check types in new clauses";
      "--try-e", Arg.String (fun path -> e_path := Some path), " try the given eprover binary on the problem"
    ]
