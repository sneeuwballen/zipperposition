
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Implementation of Phases} *)

open Logtk
open Logtk_parsers
open Logtk_proofs
open Libzipperposition

open Phases.Infix

module T = Term
module O = Ordering
module Lit = Literal

let section = Const.section

(* setup an alarm for abrupt stop *)
let setup_alarm timeout =
  let handler _ =
    Format.printf "%% SZS status ResourceOut@.";
    exit 0
  in
  ignore (Sys.signal Sys.sigalrm (Sys.Signal_handle handler));
  Unix.alarm (max 1 (int_of_float timeout))

(* TODO: move into Zipperposition *)
let print_version ~params =
  if params.Params.version then (
    Format.printf "zipperposition %s@." Const.version;
    exit 0
  )

(* have a list of extensions that should be loaded, and load them
   in phase Phases.LoadExtension
   FIXME: still too global? *)
(* TODO: just use a list, not "register" *)
let load_extensions =
  let open Libzipperposition_calculi in
  Phases.start_phase Phases.LoadExtensions >>= fun () ->
  Extensions.register Superposition.extension;
  Extensions.register AC.extension;
  Extensions.register Heuristics.extension;
  Extensions.register Avatar.extension;
  Extensions.register EnumTypes.extension;
  Extensions.register Induction.extension;
  Extensions.register Rewriting.extension;
  Extensions.register Arith_int.extension;
  Extensions.register Arith_rat.extension;
  Extensions.register Ind_types.extension;
  Extensions.register Fool.extension;
  Extensions.register Higher_order.extension;
  let l = Extensions.extensions () in
  Phases.return_phase l

(* apply functions of [field e], for each extensions [e], to update
   the current state given some parameter [x]. *)
let do_extensions ~x ~field =
  Extensions.extensions ()
  |> Phases.fold_l ~x:()
    ~f:(fun () e ->
      Phases.fold_l (field e) ~x:()
        ~f:(fun () f -> Phases.update ~f:(f x)))

let start_file file =
  Phases.start_phase Phases.Start_file >>= fun () ->
  Util.debugf ~section 1 "@[@{<Yellow>### process file@ `%s` ###@}@]"
    (fun k->k file);
  do_extensions ~field:(fun e -> e.Extensions.start_file_actions)
    ~x:file >>= fun () ->
  Phases.return_phase ()

let parse_prelude (params:Params.t) =
  Phases.start_phase Phases.Parse_prelude >>= fun () ->
  let prelude_files = params.Params.prelude in
  let res =
    if CCVector.is_empty prelude_files
    then CCResult.return Sequence.empty
    else (
      CCVector.to_list prelude_files
      |> CCResult.map_l
        (fun file ->
           Util.debugf ~section 1 "@[@{<Yellow>### parse prelude file@ `%s` ###@}@]"
             (fun k->k file);
           let fmt = Parsing_utils.guess_input file in
           Parsing_utils.parse_file fmt file)
      |> CCResult.map Sequence.of_list
      |> CCResult.map Sequence.flatten
    )
  in
  Phases.return_phase_err res

let parse_file file =
  Phases.start_phase Phases.Parse_file >>= fun () ->
  let input = Parsing_utils.input_of_file file in
  Parsing_utils.parse_file input file >>?= fun parsed ->
  do_extensions ~field:(fun e -> e.Extensions.post_parse_actions)
    ~x:parsed >>= fun () ->
  Phases.return_phase (input,parsed)

let typing ~file prelude (input,stmts) =
  Phases.start_phase Phases.Typing >>= fun () ->
  Phases.get_key Params.key >>= fun params ->
  let def_as_rewrite = params.Params.def_as_rewrite in
  TypeInference.infer_statements
    ~on_var:(Input_format.on_var input)
    ~on_undef:(Input_format.on_undef_id input)
    ~on_shadow:(Input_format.on_shadow input)
    ~implicit_ty_args:(Input_format.implicit_ty_args input)
    ~def_as_rewrite ?ctx:None ~file
    (Sequence.append prelude stmts)
  >>?= fun stmts ->
  Util.debugf ~section 3 "@[<hv2>@{<green>typed statements@}@ %a@]"
    (fun k->k (Util.pp_seq Statement.pp_input) (CCVector.to_seq stmts));
  do_extensions ~field:(fun e -> e.Extensions.post_typing_actions)
    ~x:stmts >>= fun () ->
  Phases.return_phase stmts

(* obtain clauses  *)
let cnf decls =
  Phases.start_phase Phases.CNF >>= fun () ->
  let stmts =
    decls
    |> CCVector.to_seq
    |> Cnf.cnf_of_seq
    |> CCVector.to_seq
    |> Cnf.convert
  in
  do_extensions ~field:(fun e -> e.Extensions.post_cnf_actions)
    ~x:stmts >>= fun () ->
  Phases.return_phase stmts

(* compute a precedence *)
let compute_prec stmts =
  Phases.start_phase Phases.Compute_prec >>= fun () ->
  (* use extensions *)
  Phases.get >>= fun state ->
  let cp =
    Extensions.extensions ()
    |> List.fold_left
      (fun cp e -> List.fold_left (fun cp f -> f state cp) cp e.Extensions.prec_actions)
      Compute_prec.empty

    (* add constraint about inductive constructors, etc. *)
    |> Compute_prec.add_constr 10 Classify_cst.prec_constr
    |> Compute_prec.set_weight_rule (fun _ -> Classify_cst.weight_fun)

    (* use "invfreq", with low priority *)
    |> Compute_prec.add_constr_rule 90
      (fun seq ->
         seq
         |> Sequence.flat_map Statement.Seq.terms
         |> Sequence.flat_map Term.Seq.symbols
         |> Precedence.Constr.invfreq)
  in
  let prec = Compute_prec.mk_precedence cp stmts in
  Phases.return_phase prec

let compute_ord_select precedence =
  Phases.start_phase Phases.Compute_ord_select >>= fun () ->
  Phases.get_key Params.key >>= fun params ->
  let ord = Ordering.by_name params.Params.ord precedence in
  Util.debugf ~section 2 "@[<2>ordering %s@]" (fun k->k (Ordering.name ord));
  let select = Selection.from_string ~ord params.Params.select in
  do_extensions ~field:(fun e->e.Extensions.ord_select_actions)
    ~x:(ord,select) >>= fun () ->
  Util.debugf ~section 2 "@[<2>selection function:@ %s@]" (fun k->k params.Params.select);
  Phases.return_phase (ord, select)

let make_ctx ~signature ~ord ~select () =
  Phases.start_phase Phases.MakeCtx >>= fun () ->
  let module Res = struct
    let signature = signature
    let ord = ord
    let select = select
  end in
  let module MyCtx = Ctx.Make(Res) in
  let ctx = (module MyCtx : Ctx_intf.S) in
  Phases.get >>= fun st ->
  (* did any previous extension break completeness? *)
  let lost_comp = Flex_state.get_or ~or_:false Ctx.Key.lost_completeness st in
  if lost_comp then MyCtx.lost_completeness ();
  do_extensions ~field:(fun e->e.Extensions.ctx_actions)
    ~x:ctx >>= fun () ->
  Phases.return_phase ctx

let make_env ~ctx:(module Ctx : Ctx_intf.S) ~params stmts =
  Phases.start_phase Phases.MakeEnv >>= fun () ->
  Phases.get >>= fun state ->
  let module MyEnv = Env.Make(struct
      module Ctx = Ctx
      let params = params
      let flex_state = state
    end) in
  let env1 = (module MyEnv : Env.S) in
  (* use extensions to customize env *)
  Extensions.extensions ()
  |> List.iter
    (fun e -> List.iter (fun f -> f env1) e.Extensions.env_actions);
  (* convert statements to clauses *)
  let c_sets = MyEnv.convert_input_statements stmts in
  let env2 = (module MyEnv : Env.S with type C.t = MyEnv.C.t) in
  Phases.return_phase (Phases.Env_clauses (env2, c_sets))

(* FIXME: move this into Env! *)
let has_goal_ = ref false

(* print stats *)
let print_stats_env (type c) (module Env : Env.S with type C.t = c) =
  let comment = Options.comment() in
  let print_hashcons_stats what (sz, num, sum_length, small, median, big) =
    Format.printf
      "@[<h>%shashcons stats for %s: size %d, num %d, sum length %d, \
       buckets: small %d, median %d, big %d@]@."
      comment what sz num sum_length small median big
  and print_state_stats (num_active, num_passive, num_simpl) =
    Format.printf "%sproof state stats: {active %d, passive %d, simpl %d}@."
      comment num_active num_passive num_simpl;
  in
  if Env.params.Params.stats then (
    print_hashcons_stats "terms" (InnerTerm.hashcons_stats ());
    print_state_stats (Env.stats ());
  )

(* print stats *)
let print_stats () =
  Phases.start_phase Phases.Print_stats >>= fun () ->
  Signal.send Signals.on_print_stats ();
  let comment = Options.comment() in
  let print_gc () =
    let stats = Gc.stat () in
    Format.printf
      "@[<h>%sGC: minor words %.0f; major_words: %.0f; max_heap: %d; \
       minor collections %d; major collections %d@]@."
      comment
      stats.Gc.minor_words stats.Gc.major_words stats.Gc.top_heap_words
      stats.Gc.minor_collections stats.Gc.major_collections;
  in
  Phases.get_key Params.key >>= fun params ->
  if params.Params.stats then (
    print_gc ();
    Util.print_global_stats ~comment ();
  );
  Phases.return_phase ()

(* pre-saturation *)
let presaturate_clauses (type c)
    (module Env : Env.S with type C.t = c)
    (c_sets : c Clause.sets) =
  Phases.start_phase Phases.Pre_saturate >>= fun () ->
  let module Sat = Saturate.Make(Env) in
  let num_clauses = CCVector.length c_sets.Clause.c_set in
  if Env.params.Params.presaturate
  then (
    Util.debug ~section 1 "presaturate initial clauses";
    Env.add_passive (CCVector.to_seq c_sets.Clause.c_set);
    let result, num = Sat.presaturate () in
    Util.debugf ~section 1 "initial presaturation in %d steps" (fun k->k num);
    (* pre-saturated set of clauses *)
    let c_set = Env.get_active() |> CCVector.of_seq |> CCVector.freeze in
    let clauses = {c_sets with Clause.c_set; } in
    (* remove clauses from [env] *)
    Env.remove_active (CCVector.to_seq c_set);
    Env.remove_passive (CCVector.to_seq c_set);
    Util.debugf ~section 2 "@[<2>%d clauses pre-saturated into:@ @[<hv>%a@]@]"
      (fun k->k num_clauses (Util.pp_seq ~sep:" " Env.C.pp) (CCVector.to_seq c_set));
    Phases.return_phase (result, clauses)
  )
  else Phases.return_phase (Saturate.Unknown, c_sets)

(* try to refute the set of clauses contained in the [env]. Parameters are
   used to influence how saturation is done, for how long it runs, etc. *)
let try_to_refute (type c) (module Env : Env.S with type C.t = c) clauses result =
  Phases.start_phase Phases.Saturate >>= fun () ->
  let module Sat = Saturate.Make(Env) in
  (* add clauses to passive set of [env], and SOS to active set *)
  if not (CCVector.is_empty clauses.Clause.c_sos) then (
    Env.Ctx.lost_completeness();
  );
  Env.add_active (CCVector.to_seq clauses.Clause.c_sos);
  Env.add_passive (CCVector.to_seq clauses.Clause.c_set);
  let steps = if Env.params.Params.steps < 0
    then None
    else (
      Util.debugf ~section 1 "run for %d steps" (fun k->k Env.params.Params.steps);
      Some Env.params.Params.steps
    )
  and timeout = if Env.params.Params.timeout = 0.
    then None
    else (
      Util.debugf ~section 1 "run for %.3f s" (fun k->k Env.params.Params.timeout);
      (* FIXME: only do that for zipperposition, not the library? *)
      ignore (setup_alarm Env.params.Params.timeout);
      Some (Util.total_time_s () +. Env.params.Params.timeout -. 0.25)
    )
  in
  Signal.send Env.on_start ();
  let result, num = match result with
    | Saturate.Unsat _ -> result, 0  (* already found unsat during presaturation *)
    | _ -> Sat.given_clause ~generating:true ?steps ?timeout ()
  in
  let comment = Options.comment() in
  Format.printf "%sdone %d iterations in %.3fs@." comment num (Util.total_time_s());
  Util.debugf ~section 1 "@[<2>final precedence:@ @[%a@]@]"
    (fun k->k Precedence.pp (Env.precedence ()));
  Phases.return_phase result

(* Print some content of the state, based on environment variables *)
let print_dots (type c)
    (module Env : Env_intf.S with type C.t = c)
    (result : Saturate.szs_status) =
  Phases.start_phase Phases.Print_dot >>= fun () ->
  Signal.send Signals.on_dot_output ();
  (* see if we need to print proof state *)
  begin match Env.params.Params.dot_file, result with
    | Some dot_f, Saturate.Unsat proof ->
      let name = "unsat_graph" in
      (* print proof of false *)
      let proof =
        if Env.params.Params.dot_all_roots
        then
          Env.(Sequence.append (get_active()) (get_passive()))
          |> Sequence.filter_map
            (fun c ->
               if Literals.is_absurd (Env.C.lits c)
               then Some (Env.C.proof c)
               else None)
        else Sequence.singleton proof
      in
      Proof.S.pp_dot_seq_file ~name dot_f proof
    | Some dot_f, (Saturate.Sat | Saturate.Unknown) when Env.params.Params.dot_sat ->
      (* print saturated set *)
      let name = "sat_set" in
      let seq = Sequence.append (Env.get_active ()) (Env.get_passive ()) in
      let seq = Sequence.map Env.C.proof seq in
      Proof.S.pp_dot_seq_file ~name dot_f seq
    | _ -> ()
  end;
  Phases.return_phase ()

(* TODO: parametrize, remove side effect *)
let sat_to_str () =
  if !has_goal_ then "CounterSatisfiable" else "Satisfiable"
let unsat_to_str () =
  if !has_goal_ then "Theorem" else "Unsatisfiable"

let print_szs_result (type c) ~file
    (module Env : Env_intf.S with type C.t = c)
    (result : Saturate.szs_status) =
  Phases.start_phase Phases.Print_result >>= fun () ->
  let comment = Options.comment() in
  begin match result with
    | Saturate.Unknown
    | Saturate.Timeout ->
      Format.printf "%sSZS status ResourceOut for '%s'@." comment file
    | Saturate.Error s ->
      Format.printf "%sSZS status InternalError for '%s'@." comment file;
      Util.debugf ~section 1 "error is:@ %s" (fun k->k s);
    | Saturate.Sat when Env.Ctx.is_completeness_preserved () ->
      Format.printf "%sSZS status %s for '%s'@." comment (sat_to_str ()) file
    | Saturate.Sat ->
      Format.printf "%sSZS status GaveUp for '%s'@." comment file;
      begin match !Options.output with
        | Options.O_none -> ()
        | Options.O_zf -> failwith "not implemented: printing in ZF" (* TODO *)
        | Options.O_tptp ->
          Util.debugf ~section 1 "@[<2>saturated set:@ @[<hv>%a@]@]"
            (fun k->k (Util.pp_seq ~sep:" " Env.C.pp_tstp_full) (Env.get_active ()))
        | Options.O_normal ->
          Util.debugf ~section 1 "@[<2>saturated set:@ @[<hv>%a@]@]"
            (fun k->k (Util.pp_seq ~sep:" " Env.C.pp) (Env.get_active ()))
      end
    | Saturate.Unsat proof ->
      (* print status then proof *)
      Format.printf "%sSZS status %s for '%s'@." comment (unsat_to_str ()) file;
      Format.printf "%sSZS output start Refutation@." comment;
      Format.printf "%a@." (Proof.S.pp_in !Options.output) proof;
      Format.printf "%sSZS output end Refutation@." comment;
  end;
  Phases.return_phase ()

(* print weight of [s] within precedence [prec] *)
let pp_weight prec out s =
  Format.fprintf out "w(%a)=%a"
    ID.pp s Precedence.Weight.pp (Precedence.weight prec s)

(* does the sequence of declarations contain at least one conjecture? *)
let has_goal_decls_ decls =
  CCVector.exists
    (fun st -> match Statement.view st with
       | Statement.Goal _ -> true
       | _ -> false)
    decls

(* parse CLI options and list of files to deal with *)
let parse_cli =
  Phases.start_phase Phases.Parse_CLI >>= fun () ->
  CCFormat.set_color_default true;
  (* parse arguments *)
  let params = Params.parse_args () in
  let files = CCVector.to_list params.Params.files in
  Phases.set_key Params.key params >>= fun () ->
  print_version ~params;
  Phases.return_phase (files, params)

(* Process the given file (try to solve it) *)
let process_file ?(prelude=Sequence.empty) file =
  start_file file >>= fun () ->
  parse_file file >>= fun stmts ->
  typing ~file prelude stmts >>= fun decls ->
  (* declare inductive types and constants *)
  CCVector.iter Statement.scan_simple_stmt_for_ind_ty decls;
  let has_goal = has_goal_decls_ decls in
  Util.debugf ~section 1 "parsed %d declarations (%s goal(s))"
    (fun k->k (CCVector.length decls) (if has_goal then "some" else "no"));
  cnf decls >>= fun stmts ->
  (* compute signature, precedence, ordering *)
  let signature = Statement.signature (CCVector.to_seq stmts) in
  Util.debugf ~section 1 "@[<2>signature:@ @[<hv>%a@]@]" (fun k->k Signature.pp signature);
  Util.debugf ~section 2 "(@[classification:@ %a@])"
    (fun k->k Classify_cst.pp_signature signature);
  compute_prec (CCVector.to_seq stmts) >>= fun precedence ->
  Util.debugf ~section 1 "@[<2>precedence:@ @[%a@]@]" (fun k->k Precedence.pp precedence);
  compute_ord_select precedence >>= fun (ord, select) ->
  (* HO *)
  Phases.get_key Params.key >>= fun params ->
  (* build the context and env *)
  make_ctx ~signature ~ord ~select () >>= fun ctx ->
  make_env ~params ~ctx stmts >>= fun (Phases.Env_clauses (env,clauses)) ->
  (* main workload *)
  has_goal_ := has_goal; (* FIXME: should be computed at Env initialization *)
  (* pre-saturation *)
  presaturate_clauses env clauses >>= fun (result, clauses) ->
  (* saturate, possibly changing env *)
  try_to_refute env clauses result >>= fun result ->
  Phases.return (Phases.Env_result (env, result))

let print file env result =
  (* print some statistics *)
  print_stats_env env;
  print_szs_result ~file env result >>= fun () ->
  print_dots env result

let check res =
  Phases.start_phase Phases.Check_proof >>= fun () ->
  Phases.get_key Params.key >>= fun params ->
  let comment = Options.comment() in
  let errcode = match res with
    | Saturate.Unsat p when params.Params.check ->
      (* check proof! *)
      Util.debug ~section 1 "start checking proof…";
      let p' = LLProof_conv.conv p in
      (* check *)
      let start = Util.total_time_s () in
      let dot_prefix = params.Params.dot_check in
      let res, stats = LLProof_check.check ?dot_prefix p' in
      let stop = Util.total_time_s () in
      Format.printf "%s(@[<h>proof_check@ :res %a@ :stats %a :time %.3fs@])@."
        comment LLProof_check.pp_res res LLProof_check.pp_stats stats (stop-.start);
      (* print proof? (do it after check, results are cached) *)
      begin match params.Params.dot_llproof with
        | None -> ()
        | Some file ->
          Util.debugf ~section 2 "print LLProof into `%s`"(fun k->k file);
          LLProof.Dot.pp_dot_file file p';
      end;
      (* exit code *)
      if res = LLProof_check.R_fail then 15 else 0
    | _ -> 0
  in
  Phases.return_phase errcode

let setup_gc =
  Phases.start_phase Phases.Setup_gc >>= fun () ->
  Util.debug ~section 2 "setup GC";
  (* GC! increase max overhead because we want the GC to be faster, even if
      it implies more wasted memory. *)
  let gc = Gc.get () in
  Gc.set { gc with Gc.space_overhead=150; };
  Phases.return_phase ()

let setup_signal =
  Phases.start_phase Phases.Setup_signal >>= fun () ->
  Util.debug ~section 2 "setup signal handler";
  (* signal handler. Re-raise, bugs shouldn't keep hidden *)
  Signal.set_exn_handler
    (fun e ->
       let stack = Printexc.get_backtrace () in
       let msg = Printexc.to_string e in
       output_string stderr ("exception raised in signal: " ^ msg ^ "\n");
       output_string stderr stack;
       flush stderr;
       raise e);
  Phases.return_phase ()

(* process several files, printing the result *)
let process_files_and_print ?(params=Params.default) files =
  parse_prelude params >>= fun prelude ->
  let f file =
    process_file ~prelude file >>= fun (Phases.Env_result (env, res)) ->
    print file env res >>= fun () ->
    check res
  in
  let phases = List.map f files in
  Phases.run_parallel phases >>= fun r ->
  print_stats () >>= fun () ->
  Phases.return r

let main_cli ?setup_gc:(gc=true) () =
  let open Phases.Infix in
  (if gc then setup_gc else Phases.return ()) >>= fun () ->
  setup_signal >>= fun () ->
  parse_cli >>= fun (files, params) ->
  load_extensions >>= fun _ ->
  process_files_and_print ~params files >>= fun errcode ->
  Phases.exit >|= fun () ->
  errcode

let skip_parse_cli ?(params=Params.default) file =
  Phases.start_phase Phases.Parse_CLI >>= fun () ->
  CCFormat.set_color_default true;
  Phases.set_key Params.key params >>= fun () ->
  Phases.return_phase ([file], params)

let main ?setup_gc:(gc=true) ?params file =
  let open Phases.Infix in
  (if gc then setup_gc else Phases.return ()) >>= fun () ->
  (* pseudo-parse *)
  skip_parse_cli ?params file >>= fun (files, params) ->
  load_extensions >>= fun _ ->
  process_files_and_print ~params files >>= fun errcode ->
  Phases.exit >|= fun () ->
  errcode
