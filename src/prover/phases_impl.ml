
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Implementation of Phases} *)

open Libzipperposition
open Libzipperposition_parsers
open Params

open Phases.Infix

module T = FOTerm
module O = Ordering
module Lit = Literal
module S = Substs

let section = Const.section

(* setup an alarm for abrupt stop *)
let setup_alarm timeout =
  let handler _ =
    Format.printf "%% SZS Status ResourceOut@.";
    Unix.kill (Unix.getpid ()) Sys.sigterm
  in
  ignore (Sys.signal Sys.sigalrm (Sys.Signal_handle handler));
  Unix.alarm (max 1 (int_of_float timeout))

(* TODO: move into Zipperposition *)
let print_version ~params =
  if params.param_version then (
    Format.printf "zipperposition %s@." Const.version;
    exit 0
  )

(* have a list of extensions that should be loaded, and load them
   in phase Phases.LoadExtension
   FIXME: still too global? *)
(* TODO: just use a list, not "register" *)
let load_extensions =
  Phases.start_phase Phases.LoadExtensions >>= fun () ->
  Extensions.register Superposition.extension;
  Extensions.register AC.extension;
  Extensions.register Heuristics.extension;
  Extensions.register Avatar.extension;
  Extensions.register EnumTypes.extension;
  Extensions.register Induction.extension;
  Extensions.register Rewriting.extension;
  Extensions.register Ind_types.extension;
  Extensions.register Fool.extension;
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

let parse_file file =
  Phases.start_phase Phases.Parse_file >>= fun () ->
  Parsing_utils.parse file >>?= fun parsed ->
  do_extensions ~field:(fun e -> e.Extensions.post_parse_actions)
    ~x:parsed >>= fun () ->
  Phases.return_phase parsed

let typing stmts =
  Phases.start_phase Phases.Typing >>= fun () ->
  Phases.get_key Params.key >>= fun params ->
  let def_as_rewrite = params.Params.param_def_as_rewrite in
  TypeInference.infer_statements ~def_as_rewrite ?ctx:None stmts >>?= fun stmts ->
  do_extensions ~field:(fun e -> e.Extensions.post_typing_actions)
    ~x:stmts >>= fun () ->
  Phases.return_phase stmts

(* obtain clauses  *)
let cnf ~file decls =
  Phases.start_phase Phases.CNF >>= fun () ->
  let stmts =
    decls
    |> CCVector.to_seq
    |> Sequence.map (Statement.add_src ~file)
    |> Cnf.cnf_of_seq ~neg_src:StatementSrc.neg ~cnf_src:StatementSrc.cnf
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

    (* use "invfreq", with low priority *)
    |> Compute_prec.add_constr_rule 90
       (fun seq ->
         seq
         |> Sequence.flat_map Statement.Seq.terms
         |> Sequence.flat_map FOTerm.Seq.symbols
         |> Precedence.Constr.invfreq)
  in
  let prec = Compute_prec.mk_precedence cp stmts in
  Phases.return_phase prec

let compute_ord_select precedence =
  Phases.start_phase Phases.Compute_ord_select >>= fun () ->
  Phases.get_key Params.key >>= fun params ->
  let ord = Ordering.by_name params.param_ord precedence in
  let select = Selection.selection_from_string ~ord params.param_select in
  do_extensions ~field:(fun e->e.Extensions.ord_select_actions)
    ~x:(ord,select) >>= fun () ->
  Util.debugf ~section 2 "@[<2>selection function:@ %s@]" (fun k->k params.param_select);
  Phases.return_phase (ord, select)

let make_ctx ~signature ~ord ~select =
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
  let clauses = MyEnv.convert_input_statements stmts in
  let env2 = (module MyEnv : Env.S with type C.t = MyEnv.C.t) in
  Phases.return_phase (Phases.Env_clauses (env2, clauses))

(* FIXME: move this into Env! *)
let has_goal_ = ref false

(* print stats *)
let print_stats (type c) (module Env : Env.S with type C.t = c) =
  Phases.start_phase Phases.Print_stats >>= fun () ->
  Signal.send Signals.on_print_stats ();
  let print_hashcons_stats what (sz, num, sum_length, small, median, big) =
    Format.printf
      "@[<2>hashcons stats for %s: size %d, num %d, sum length %d, \
       buckets: small %d, median %d, big %d@]@."
      what sz num sum_length small median big
  and print_state_stats (num_active, num_passive, num_simpl) =
    Format.printf "proof state stats:@.";
    Format.printf "stat:  active clauses          %d@." num_active;
    Format.printf "stat:  passive clauses         %d@." num_passive;
    Format.printf "stat:  simplification clauses  %d@." num_simpl;
  and print_gc () =
    let stats = Gc.stat () in
    Format.printf
      "GC: minor words %.0f; major_words: %.0f; max_heap: %d; \
       minor collections %d; major collections %d@."
      stats.Gc.minor_words stats.Gc.major_words stats.Gc.top_heap_words
      stats.Gc.minor_collections stats.Gc.major_collections;
  in
  if Env.params.param_stats then (
    print_gc ();
    print_hashcons_stats "terms" (InnerTerm.hashcons_stats ());
    print_state_stats (Env.stats ());
    Util.print_global_stats ();
  );
  Phases.return_phase ()

(* pre-saturation *)
let presaturate_clauses (type c)
(module Env : Env.S with type C.t = c)
(clauses : c CCVector.ro_vector) =
  Phases.start_phase Phases.Pre_saturate >>= fun () ->
  let module Sat = Saturate.Make(Env) in
  let num_clauses = CCVector.length clauses in
  if Env.params.param_presaturate
  then (
    Util.debug ~section 1 "presaturate initial clauses";
    Env.add_passive (CCVector.to_seq clauses);
    let result, num = Sat.presaturate () in
    Util.debugf ~section 1 "initial presaturation in %d steps" (fun k->k num);
    (* pre-saturated set of clauses *)
    let clauses = Env.get_active () in
    (* remove clauses from [env] *)
    Env.remove_active clauses;
    Env.remove_passive clauses;
    Util.debugf ~section 2 "@[<2>%d clauses pre-saturated into:@ @[<hv>%a@]@]"
      (fun k->k num_clauses (CCFormat.seq ~sep:" " Env.C.pp) clauses);
    Phases.return_phase (result, clauses)
  )
  else Phases.return_phase (Saturate.Unknown, CCVector.to_seq clauses)

(* try to refute the set of clauses contained in the [env]. Parameters are
   used to influence how saturation is done, for how long it runs, etc. *)
let try_to_refute (type c) (module Env : Env.S with type C.t = c) clauses result =
  Phases.start_phase Phases.Saturate >>= fun () ->
  let module Sat = Saturate.Make(Env) in
  (* add clauses to passive set of [env] *)
  Env.add_passive clauses;
  let steps = if Env.params.param_steps < 0
    then None
    else (
      Util.debugf ~section 1 "run for %d steps" (fun k->k Env.params.param_steps);
      Some Env.params.param_steps
    )
  and timeout = if Env.params.param_timeout = 0.
    then None
    else (
      Util.debugf ~section 1 "run for %.2f s" (fun k->k Env.params.param_timeout);
      ignore (setup_alarm Env.params.param_timeout);
      Some (Util.total_time_s () +. Env.params.param_timeout -. 0.25)
    )
  in
  Signal.send Env.on_start ();
  let result, num = match result with
    | Saturate.Unsat _ -> result, 0  (* already found unsat during presaturation *)
    | _ -> Sat.given_clause ~generating:true ?steps ?timeout ()
  in
  Format.printf "%% done %d iterations@." num;
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
  begin match Env.params.param_dot_file, result with
    | Some dot_f, Saturate.Unsat proof ->
        let name = "unsat_graph" in
        (* print proof of false *)
        let proof =
          if Env.params.param_dot_all_roots
          then
            Env.(Sequence.append (get_active()) (get_passive()))
            |> Sequence.filter_map
              (fun c ->
                if Literals.is_absurd (Env.C.lits c)
                then Some (Env.C.proof c)
                else None)
          else Sequence.singleton proof
        in
        ProofPrint.pp_dot_seq_file ~name dot_f proof
    | Some dot_f, (Saturate.Sat | Saturate.Unknown) when Env.params.param_dot_sat ->
        (* print saturated set *)
        let name = "sat_set" in
        let seq = Sequence.append (Env.get_active ()) (Env.get_passive ()) in
        let seq = Sequence.map Env.C.proof seq in
        ProofPrint.pp_dot_seq_file ~name dot_f seq
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
  begin match result with
  | Saturate.Unknown
  | Saturate.Timeout ->
      Format.printf "%% SZS status ResourceOut for '%s'@." file
  | Saturate.Error s ->
      Format.printf "%% SZS status InternalError for '%s'@." file;
      Util.debugf ~section 1 "error is:@ %s" (fun k->k s);
  | Saturate.Sat when Env.Ctx.is_completeness_preserved () ->
      Format.printf "%% SZS status %s for '%s'@." (sat_to_str ()) file
  | Saturate.Sat ->
      Format.printf "%% SZS status GaveUp for '%s'@." file;
      begin match !Options.output with
        | Options.Print_none -> ()
        | Options.Print_zf -> failwith "not implemented: printing in ZF" (* TODO *)
        | Options.Print_tptp ->
            Util.debugf ~section 1 "@[<2>saturated set:@ @[<hv>%a@]@]"
              (fun k->k (CCFormat.seq ~sep:" " Env.C.pp_tstp_full) (Env.get_active ()))
        | Options.Print_normal ->
            Util.debugf ~section 1 "@[<2>saturated set:@ @[<hv>%a@]@]"
              (fun k->k (CCFormat.seq ~sep:" " Env.C.pp) (Env.get_active ()))
      end
  | Saturate.Unsat proof ->
      (* print status then proof *)
      Format.printf "%% SZS status %s for '%s'@." (unsat_to_str ()) file;
      Format.printf "%% SZS output start Refutation@.";
      Format.printf "%a@." (ProofPrint.pp !Options.output) proof;
      Format.printf "%% SZS output end Refutation@.";
  end;
  Phases.return_phase ()

(* print weight of [s] within precedence [prec] *)
let _pp_weight prec out s =
  Format.fprintf out "w(%a)=%d" ID.pp s (Precedence.weight prec s)

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
  let files = CCVector.to_list Params.files in
  Phases.set_key Params.key params >>= fun () ->
  print_version ~params;
  Phases.return_phase (files, params)

(* Process the given file (try to solve it) *)
let process_file file =
  start_file file >>= fun () ->
  parse_file file >>= fun stmts ->
  typing stmts >>= fun decls ->
  let has_goal = has_goal_decls_ decls in
  Util.debugf ~section 1 "parsed %d declarations (%s goal(s))"
    (fun k->k (CCVector.length decls) (if has_goal then "some" else "no"));
  cnf ~file decls >>= fun stmts ->
  (* declare inductive types and constants *)
  CCVector.iter Ind_ty.scan_stmt stmts;
  (* compute signature, precedence, ordering *)
  let signature = Statement.signature (CCVector.to_seq stmts) in
  Util.debugf ~section 1 "@[<2>signature:@ @[<hv>%a@]@]" (fun k->k Signature.pp signature);
  Util.debugf ~section 2 "(@[classification:@ %a@])"
    (fun k->k Classify_cst.pp_signature signature);
  compute_prec (CCVector.to_seq stmts) >>= fun precedence ->
  Util.debugf ~section 1 "@[<2>precedence:@ @[%a@]@]" (fun k->k Precedence.pp precedence);
  compute_ord_select precedence >>= fun (ord, select) ->
  (* build the context and env *)
  make_ctx ~signature ~ord ~select >>= fun ctx ->
  Phases.get_key Params.key >>= fun params ->
  make_env ~params ~ctx stmts >>= fun (Phases.Env_clauses (env,clauses)) ->
  (* main workload *)
  has_goal_ := has_goal; (* FIXME: should be computed at Env initialization *)
  (* pre-saturation *)
  presaturate_clauses env clauses >>= fun (result, clauses) ->
  (* saturate, possibly changing env *)
  try_to_refute env clauses result >>= fun result ->
  Phases.return (Phases.Env_result (env, result))

let print file env result =
  print_szs_result ~file env result >>= fun () ->
  (* print some statistics *)
  print_stats env >>= fun () ->
  print_dots env result

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
let process_files_and_print files =
  let f file =
    process_file file >>= fun (Phases.Env_result (env, res)) ->
    print file env res
  in
  let phases = List.map f files in
  Phases.run_parallel phases
