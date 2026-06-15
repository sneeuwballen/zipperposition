(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Implementation of Phases} *)

open Logtk
open Logtk_parsers
open Logtk_proofs
open Libzipperposition
open Libzipperposition_calculi
module T = Term
module O = Ordering
module Lit = Literal

let section = Util.Section.make ~parent:Const.section "phases"
let _db_w = ref 1
let _lmb_w = ref 1
let _kbo_wf = ref "invfreqrank"
let _prec_fun = ref "invfreq"
let _trim_implications = ref false
let _take_only_defs = ref false
let _ignore_k_most_common_symbols = ref None
let _take_conj_defs = ref true
let _sine_d_min = ref 1
let _sine_d_max = ref 5
let _sine_tolerance = ref 1.5
let _sine_threshold = ref (-1)

(** setup an alarm for abrupt stop *)
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
let load_extensions (st_ref : Phases.State.t ref) =
  let open Libzipperposition_calculi in
  let@ () = Phases.with_phase st_ref Phases.LoadExtensions in
  Extensions.register Lazy_cnf.extension;
  Extensions.register Combinators.extension;
  Extensions.register Higher_order.extension;
  Extensions.register Superposition.extension;
  Extensions.register Bce_pe_fixpoint.extension;
  Extensions.register Bce.extension;
  Extensions.register Pred_elim.extension;
  Extensions.register Qle.extension;
  Extensions.register Hlb_elim.extension;
  Extensions.register AC.extension;
  Extensions.register Heuristics.extension;
  Extensions.register Libzipperposition_avatar.extension;
  Extensions.register EnumTypes.extension;
  Extensions.register Libzipperposition_induction.extension;
  Extensions.register Rewriting.extension;
  Extensions.register Ind_types.extension;
  Extensions.register Fool.extension;
  Extensions.register Booleans.extension;
  Extensions.register Lift_lambdas.extension;
  Extensions.register Pure_literal_elim.extension;
  Extensions.register Bool_encode.extension;
  Extensions.register App_encode.extension;
  Extensions.register Eq_encode.extension;

  Extensions.extensions ()

(* apply functions of [field e], for each extensions [e], to update
   the current state given some parameter [x]. *)
let do_extensions ~x ~field (st_ref : Phases.State.t ref) =
  Extensions.extensions ()
  |> List.iter (fun e ->
         field e |> List.iter (fun f -> Phases.update ~f:(f x) st_ref))

let apply_modifiers ~field o =
  Extensions.extensions ()
  |> CCList.fold_left
       (fun o e -> field e |> CCList.fold_left (fun o m -> m o) o)
       o

let start_file (st_ref : Phases.State.t ref) file =
  let@ () = Phases.with_phase st_ref Phases.Start_file in
  Util.debugf ~section 2 "@[@{<Yellow>### process file@ `%s` ###@}@]" (fun k ->
      k file);
  do_extensions ~field:(fun e -> e.Extensions.start_file_actions) ~x:file st_ref

let parse_prelude (st_ref : Phases.State.t ref) (params : Params.t) =
  let@ () = Phases.with_phase st_ref Phases.Parse_prelude in
  let prelude_files = params.Params.prelude in
  let res =
    if CCVector.is_empty prelude_files then
      CCResult.return Iter.empty
    else
      CCVector.to_list prelude_files
      |> CCResult.map_l (fun file ->
             Util.debugf ~section 2
               "@[@{<Yellow>### parse prelude file@ `%s` ###@}@]" (fun k ->
                 k file);
             let fmt = Parsing_utils.guess_input file in
             Parsing_utils.parse_file fmt file)
      |> CCResult.map Iter.of_list |> CCResult.map Iter.flatten
  in
  match res with
  | Ok x -> x
  | Error msg -> Phases.failwith msg st_ref

let parse_file (st_ref : Phases.State.t ref) file =
  let@ () = Phases.with_phase st_ref Phases.Parse_file in
  let input = Parsing_utils.input_of_file file in
  let parsed =
    match Parsing_utils.parse_file input file with
    | Ok x -> x
    | Error msg -> Phases.failwith msg st_ref
  in
  do_extensions
    ~field:(fun e -> e.Extensions.post_parse_actions)
    ~x:parsed st_ref;
  input, parsed

let has_arith stmt : bool =
  let module TS = TypedSTerm in
  let ty_is_arith ty =
    TS.equal ty TS.Ty.real || TS.equal ty TS.Ty.rat || TS.equal ty TS.Ty.int
  in
  CCVector.to_iter stmt
  |> Iter.flat_map Statement.Seq.to_iter
  |> Iter.flat_map (function
       | `Ty ty -> Iter.return ty
       | `Term t | `Form t -> TS.Seq.subterms t |> Iter.filter_map TS.ty
       | `ID _ -> Iter.empty)
  |> Iter.exists ty_is_arith

let sine_filter stmts =
  if !_sine_threshold < 0 || CCVector.length stmts < !_sine_threshold then
    stmts
  else (
    let seq = CCVector.to_iter stmts in
    let filtered =
      Statement.sine_axiom_selector
        ~ignore_k_most_common_symbols:!_ignore_k_most_common_symbols
        ~take_conj_defs:!_take_conj_defs ~take_only_defs:!_take_only_defs
        ~trim_implications:!_trim_implications ~depth_start:!_sine_d_min
        ~depth_end:!_sine_d_max ~tolerance:!_sine_tolerance seq
    in
    CCVector.freeze (CCVector.of_iter filtered)
  )

let typing (st_ref : Phases.State.t ref) ~file prelude (input, stmts) =
  let@ () = Phases.with_phase st_ref Phases.Typing in
  let params = Phases.get_key Params.key st_ref in
  let def_as_rewrite = params.Params.def_as_rewrite in
  let stmts =
    match
      TypeInference.infer_statements
        ~on_var:(Input_format.on_var input)
        ~on_undef:(Input_format.on_undef_id input)
        ~on_shadow:(Input_format.on_shadow input)
        ~implicit_ty_args:(Input_format.implicit_ty_args input)
        ~def_as_rewrite ?ctx:None ~file
        (Iter.append prelude stmts)
    with
    | Ok x -> x
    | Error msg -> Phases.failwith msg st_ref
  in
  let stmts = sine_filter stmts in
  Util.debugf ~section 3 "@[<hv2>@{<green>typed statements@}@ %a@]" (fun k ->
      k (Util.pp_iter Statement.pp_input) (CCVector.to_iter stmts));
  if has_arith stmts then (
    Util.debug ~section 1 "problem contains arithmetic, lost completeness";
    Phases.set_key Ctx.Key.lost_completeness true st_ref
  ) else if !_sine_threshold >= 0 then (
    Util.debug ~section 2 "sine is applied, lost completeness";
    Phases.set_key Ctx.Key.lost_completeness true st_ref
  );
  do_extensions
    ~field:(fun e -> e.Extensions.post_typing_actions)
    ~x:stmts st_ref;
  stmts

(* obtain clauses  *)
let cnf (st_ref : Phases.State.t ref) ~sk_ctx decls =
  let@ () = Phases.with_phase st_ref Phases.CNF in
  let opts =
    if !Lazy_cnf.enabled then
      [ Cnf.LazyCnf ]
    else
      []
  in
  let stmts =
    decls |> CCVector.to_iter
    |> Cnf.cnf_of_iter ~ctx:sk_ctx ~opts
    |> CCVector.to_iter
    |> apply_modifiers ~field:(fun e -> e.Extensions.post_cnf_modifiers)
    |> Cnf.convert
  in
  do_extensions ~field:(fun e -> e.Extensions.post_cnf_actions) ~x:stmts st_ref;
  stmts

(* compute a precedence *)
let compute_prec (st_ref : Phases.State.t ref) ~signature stmts =
  let@ () = Phases.with_phase st_ref Phases.Compute_prec in
  let state = !st_ref in
  let cp =
    Extensions.extensions ()
    |> List.fold_left
         (fun cp e ->
           List.fold_left (fun cp f -> f state cp) cp e.Extensions.prec_actions)
         Compute_prec.empty
    |> Compute_prec.add_constr 10 Classify_cst.prec_constr
    |> Compute_prec.set_weight_rule (fun stmts ->
           let sym_depth =
             stmts
             |> Iter.flat_map Statement.Seq.terms
             |> Iter.flat_map (fun t ->
                    Term.Seq.subterms_depth t
                    |> Iter.filter_map (fun (st, d) ->
                           CCOpt.map (fun id -> id, d) (Term.head st)))
           in
           let clauses = Iter.map Statement.Seq.lits stmts in
           Precedence.weight_fun_of_string ~signature ~clauses ~lm_w:!_lmb_w
             ~db_w:!_db_w !_kbo_wf sym_depth)
    |> Compute_prec.add_constr_rule 90 (fun seq ->
           let syms = Signature.Seq.symbols signature in
           Precedence.Constr.prec_fun_of_str !_prec_fun ~signature syms)
  in
  Compute_prec.mk_precedence ~signature ~db_w:!_db_w ~lmb_w:!_lmb_w cp stmts

let compute_ord_select (st_ref : Phases.State.t ref) precedence =
  let@ () = Phases.with_phase st_ref Phases.Compute_ord_select in
  let params = Phases.get_key Params.key st_ref in
  let ord = Ordering.by_name !(params.Params.ord) precedence in
  Util.debugf ~section 2 "@[<2>ordering %s@]" (fun k -> k (Ordering.name ord));
  let select = Selection.from_string ~ord params.Params.select in
  let bool_select = Bool_selection.from_string ~ord params.Params.bool_select in
  do_extensions
    ~field:(fun e -> e.Extensions.ord_select_actions)
    ~x:(ord, fst select)
    st_ref;
  Util.debugf ~section 2 "@[<2>selection function:@ %s@]" (fun k ->
      k params.Params.select);
  ord, select, bool_select

let make_ctx (st_ref : Phases.State.t ref) ~signature ~ord ~select ~bool_select
    ~sk_ctx () =
  let select_fun, is_complete = select in
  let@ () = Phases.with_phase st_ref Phases.MakeCtx in
  let ctx =
    Ctx.create ~signature ~ord ~select:select_fun ~bool_select ~sk_ctx
  in
  let st = !st_ref in
  let lost_comp =
    Flex_state.get_or ~or_:false Ctx.Key.lost_completeness st || not is_complete
  in
  if lost_comp then Ctx.lost_completeness ctx;
  do_extensions ~field:(fun e -> e.Extensions.ctx_actions) ~x:ctx st_ref;
  ctx

let make_env (st_ref : Phases.State.t ref) ~ctx ~params stmts =
  let@ () = Phases.with_phase st_ref Phases.MakeEnv in
  let state = !st_ref in
  (* TEMPORARY adapter: wraps Ctx.t record into Ctx_intf.S module for Env.Make *)
  let module Ctx_mod = struct
    let sk_ctx () = Ctx.sk_ctx ctx
    let ord () = Ctx.ord ctx
    let set_ord o = Ctx.set_ord ctx o
    let selection_fun () = Ctx.selection_fun ctx
    let set_selection_fun s = Ctx.set_selection_fun ctx s
    let signature () = Ctx.signature ctx
    let renaming = Ctx.renaming ctx
    let compare = Ctx.compare ctx
    let select = Ctx.select ctx
    let bool_select = Ctx.bool_select ctx
    let lost_completeness () = Ctx.lost_completeness ctx
    let is_completeness_preserved () = Ctx.is_completeness_preserved ctx
    let add_signature s = Ctx.add_signature ctx s
    let find_signature n = Ctx.find_signature ctx n
    let find_signature_exn n = Ctx.find_signature_exn ctx n
    let declare n t = Ctx.declare ctx n t
    let declare_syms l = Ctx.declare_syms ctx l
    let on_new_symbol = Ctx.on_new_symbol ctx
    let on_signature_update = Ctx.on_signature_update ctx
    let set_injective_for_arg n i = Ctx.set_injective_for_arg ctx n i
    let is_injective_for_arg n i = Ctx.is_injective_for_arg ctx n i

    module Lit = struct
      let of_form f = Ctx.lit_of_form ctx f
      let to_form f = Ctx.lit_to_form ctx f
      let from_hooks () = [] (* unused *)
      let add_from_hook h = Ctx.add_lit_from_hook ctx h
      let to_hooks () = [] (* unused *)
      let add_to_hook h = Ctx.add_lit_to_hook ctx h
    end
  end in
  let module MyEnv = Env.Make (struct
    module Ctx = Ctx_mod

    let params = params
    let flex_state = state
  end) in
  let env1 = (module MyEnv : Env.S) in
  Extensions.extensions ()
  |> List.iter (fun e -> List.iter (fun f -> f env1) e.Extensions.env_actions);
  let c_sets = MyEnv.convert_input_statements stmts in
  Signal.send MyEnv.ProofState.CQueue.on_proof_state_init
    (Iter.append
       (CCVector.to_iter c_sets.c_set)
       (CCVector.to_iter c_sets.c_sos));
  let env2 = (module MyEnv : Env.S with type C.t = MyEnv.C.t) in
  Phases.Env_clauses (env2, c_sets)

(* FIXME: move this into Env! *)
let has_goal_ = ref false

(* print stats *)
let print_stats_env (type c) (module Env : Env.S with type C.t = c) =
  let comment = Options.comment () in
  let print_hashcons_stats what (sz, num, sum_length, small, median, big) =
    Format.printf
      "@[<h>%shashcons stats for %s: size %d, num %d, sum length %d, buckets: \
       small %d, median %d, big %d@]@."
      comment what sz num sum_length small median big
  and print_state_stats (num_active, num_passive, num_simpl) =
    Format.printf "%sproof state stats: {active %d, passive %d, simpl %d}@."
      comment num_active num_passive num_simpl
  in
  if Env.params.Params.stats then (
    print_hashcons_stats "terms" (InnerTerm.hashcons_stats ());
    print_state_stats (Env.stats ())
  )

(* print stats *)
let print_stats (st_ref : Phases.State.t ref) =
  let@ _sp = Phases.with_span ~__FILE__ ~__LINE__ "phase.print-stats" in
  let@ () = Phases.with_phase st_ref Phases.Print_stats in
  Signal.send Signals.on_print_stats ();
  let comment = Options.comment () in
  let print_gc () =
    let stats = Gc.stat () in
    Format.printf
      "@[<h>%sGC: minor words %.0f; major_words: %.0f; max_heap: %d; minor \
       collections %d; major collections %d@]@."
      comment stats.Gc.minor_words stats.Gc.major_words stats.Gc.top_heap_words
      stats.Gc.minor_collections stats.Gc.major_collections
  in
  let params = Phases.get_key Params.key st_ref in
  if params.Params.stats then (
    print_gc ();
    Util.print_global_stats ~comment ()
  )

(* pre-saturation *)
let presaturate_clauses (type c) (st_ref : Phases.State.t ref)
    (module Env : Env.S with type C.t = c) (c_sets : c Clause.sets) =
  let@ _sp = Phases.with_span ~__FILE__ ~__LINE__ "phase.presaturate" in
  let@ () = Phases.with_phase st_ref Phases.Pre_saturate in
  let module Sat = Saturate.Make (Env) in
  let num_clauses = CCVector.length c_sets.Clause.c_set in
  if Env.params.Params.presaturate then (
    Util.debug ~section 2 "presaturate initial clauses";
    Env.add_passive (CCVector.to_iter c_sets.Clause.c_set);
    let result, num = Sat.presaturate () in
    Util.debugf ~section 2 "initial presaturation in %d steps" (fun k -> k num);
    let c_set = Env.get_active () |> CCVector.of_iter |> CCVector.freeze in
    let clauses = { c_sets with Clause.c_set } in
    Env.remove_active (CCVector.to_iter c_set);
    Env.remove_passive (CCVector.to_iter c_set);
    Util.debugf ~section 2 "@[<2>%d clauses pre-saturated into:@ @[<hv>%a@]@]"
      (fun k ->
        k num_clauses
          (Util.pp_iter ~sep:" " Env.C.pp_tstp_full)
          (CCVector.to_iter c_set));
    result, clauses
  ) else
    Saturate.Unknown, c_sets

(* try to refute the set of clauses contained in the [env]. Parameters are
   used to influence how saturation is done, for how long it runs, etc. *)
let try_to_refute (type c) (st_ref : Phases.State.t ref)
    (module Env : Env.S with type C.t = c) clauses result =
  let@ _sp = Phases.with_span ~__FILE__ ~__LINE__ "phase.saturate" in
  let@ () = Phases.with_phase st_ref Phases.Saturate in
  let module Sat = Saturate.Make (Env) in
  if not (CCVector.is_empty clauses.Clause.c_sos) then
    Env.Ctx.lost_completeness ();
  Env.add_active (CCVector.to_iter clauses.Clause.c_sos);
  Env.add_passive (CCVector.to_iter clauses.Clause.c_set);
  let steps =
    if Env.params.Params.steps < 0 then
      None
    else (
      Util.debugf ~section 2 "run for %d steps" (fun k ->
          k Env.params.Params.steps);
      Some Env.params.Params.steps
    )
  and timeout =
    if Env.params.Params.timeout = 0. then
      None
    else (
      Util.debugf ~section 2 "run for %.3f s" (fun k ->
          k Env.params.Params.timeout);
      ignore (setup_alarm Env.params.Params.timeout);
      Some (Util.total_time_s () +. Env.params.Params.timeout -. 0.25)
    )
  in
  Util.debugf ~section 1 "active(%d): @[%a@]" (fun k ->
      k
        (Iter.length @@ Env.get_active ())
        (Iter.pp_seq Env.C.pp) (Env.get_active ()));
  Util.debugf ~section 1 "passive(%d): @[%a@]" (fun k ->
      k
        (Iter.length @@ Env.get_passive ())
        (Iter.pp_seq Env.C.pp) (Env.get_passive ()));
  Signal.send Env.on_start ();
  let result, num =
    match result with
    | Saturate.Unsat _ -> result, 0
    | _ -> Sat.given_clause ~generating:true ?steps ?timeout ()
  in
  let comment = Options.comment () in
  Format.printf "%sdone %d iterations in %.3fs@." comment num
    (Util.total_time_s ());
  Util.debugf ~section 2 "@[<2>final precedence:@ @[%a@]@]" (fun k ->
      k Precedence.pp (Env.precedence ()));
  result

(* Print some content of the state, based on environment variables *)
let print_dots (type c) (st_ref : Phases.State.t ref)
    (module Env : Env_intf.S with type C.t = c) (result : Saturate.szs_status) =
  let@ _sp = Phases.with_span ~__FILE__ ~__LINE__ "phase.print-dots" in
  let@ () = Phases.with_phase st_ref Phases.Print_dot in
  Signal.send Signals.on_dot_output ();
  match Env.params.Params.dot_file, result with
  | Some dot_f, Saturate.Unsat proof ->
    let name = "unsat_graph" in
    let proof =
      if Env.params.Params.dot_all_roots then
        Env.(Iter.append (get_active ()) (get_passive ()))
        |> Iter.filter_map (fun c ->
               if Literals.is_absurd (Env.C.lits c) then
                 Some (Env.C.proof c)
               else
                 None)
      else
        Iter.singleton proof
    in
    Proof.S.pp_dot_seq_file ~name dot_f proof
  | Some dot_f, (Saturate.Sat | Saturate.Unknown) when Env.params.Params.dot_sat
    ->
    let name = "sat_set" in
    let seq = Iter.append (Env.get_active ()) (Env.get_passive ()) in
    let seq = Iter.map Env.C.proof seq in
    Proof.S.pp_dot_seq_file ~name dot_f seq
  | _ -> ()

(* TODO: parametrize, remove side effect *)
let sat_to_str () =
  if !has_goal_ then
    "CounterSatisfiable"
  else
    "Satisfiable"

let unsat_to_str () =
  if !has_goal_ then
    "Theorem"
  else
    "Unsatisfiable"

let print_szs_result (type c) (st_ref : Phases.State.t ref) ~file
    (module Env : Env_intf.S with type C.t = c) (result : Saturate.szs_status) =
  let@ _sp = Phases.with_span ~__FILE__ ~__LINE__ "phase.print-szs-result" in
  let@ () = Phases.with_phase st_ref Phases.Print_result in
  let comment = Options.comment () in
  match result with
  | Saturate.Unknown | Saturate.Timeout ->
    Format.printf "%sSZS status ResourceOut for '%s'@." comment file
  | Saturate.Error s ->
    Format.printf "%sSZS status InternalError for '%s'@." comment file;
    Util.debugf ~section 2 "error is:@ %s" (fun k -> k s)
  | Saturate.Sat when Env.Ctx.is_completeness_preserved () ->
    Format.printf "%% Final clauses: %d@." (Iter.length (Env.get_active ()));
    Format.printf "Clauses:@.@[%a@]@."
      (Iter.pp_seq ~sep:"\n" Env.C.pp)
      (Env.get_active ());
    Format.printf "%sSZS status %s for '%s'@." comment (sat_to_str ()) file;
    Util.debugf ~section 2 "@[<2>saturated set:@ @[<hv>%a@]@]" (fun k ->
        k (Util.pp_iter ~sep:" " Env.C.pp_tstp_full) (Env.get_active ()))
  | Saturate.Sat ->
    Format.printf "%% Final clauses: %d@." (Iter.length (Env.get_active ()));
    Format.printf "%sSZS status GaveUp for '%s'@." comment file;
    (match !Options.output with
    | Options.O_none -> ()
    | Options.O_zf -> failwith "not implemented: printing in ZF"
    | Options.O_tptp ->
      Util.debugf ~section 2 "@[<2>saturated set:@ @[<hv>%a@]@]" (fun k ->
          k (Util.pp_iter ~sep:" " Env.C.pp_tstp_full) (Env.get_active ()))
    | Options.O_normal ->
      Util.debugf ~section 2 "@[<2>saturated set:@ @[<hv>%a@]@]" (fun k ->
          k (Util.pp_iter ~sep:" " Env.C.pp) (Env.get_active ())))
  | Saturate.Unsat proof ->
    Format.printf "%sSZS status %s for '%s'@." comment (unsat_to_str ()) file;
    Format.printf "%sSZS output start Refutation@." comment;
    Format.printf "%a@." (Proof.S.pp_in !Options.output) proof;
    Format.printf "%sSZS output end Refutation@." comment

(* print weight of [s] within precedence [prec] *)
let pp_weight prec out s =
  Format.fprintf out "w(%a)=%a" Name.pp s Precedence.Weight.pp
    (Precedence.weight prec s)

(* does the sequence of declarations contain at least one conjecture? *)
let has_goal_decls_ decls =
  CCVector.exists
    (fun st ->
      match Statement.view st with
      | Statement.Goal _ -> true
      | _ -> false)
    decls

(* parse CLI options and list of files to deal with *)
let parse_cli (st_ref : Phases.State.t ref) =
  let@ () = Phases.with_phase st_ref Phases.Parse_CLI in
  CCFormat.set_color_default true;
  let params = Params.parse_args () in
  let files = CCVector.to_list params.Params.files in
  Phases.set_key Params.key params st_ref;
  print_version ~params;
  files, params

let syms_in_conj decls =
  let open Iter in
  decls |> CCVector.to_iter
  |> flat_map (fun st ->
         let pr = Statement.proof_step st in
         if CCOpt.is_some (Proof.Step.distance_to_goal pr) then
           Statement.Seq.symbols st
         else
           empty)

let syms_in_conj_f decls =
  CCVector.fold
    (fun acc f ->
      match Statement.view f with
      | Statement.Goal _ | Statement.NegatedGoal _ ->
        Statement.Seq.forms f
        |> Iter.map (Statement.eliminate_long_implications ~is_goal:true)
        |> Iter.flat_map TypedSTerm.Seq.symbols
        |> Iter.append acc
      | _ -> acc)
    Iter.empty decls
  |> Name.Set.of_iter |> Name.Set.to_iter

(* Process the given file (try to solve it) *)
let process_file ?(prelude = Iter.empty) file (st_ref : Phases.State.t ref) =
  let@ _sp = Phases.with_span ~__FILE__ ~__LINE__ "phase.process-file" in
  start_file st_ref file;
  let input, stmts = parse_file st_ref file in
  Trace.add_data_to_span _sp [ "file", `String file ];
  let decls = typing st_ref ~file prelude (input, stmts) in
  CCVector.iter Statement.scan_simple_stmt_for_ind_ty decls;
  let has_goal = has_goal_decls_ decls in
  let conj_syms = syms_in_conj_f decls in
  Util.debugf ~section 1 "conj syms: @[%a@]@." (fun k ->
      k (Iter.pp_seq Name.pp) conj_syms);
  Util.debugf ~section 2 "parsed %d declarations (%s goal(s))" (fun k ->
      k (CCVector.length decls)
        (if has_goal then
           "some"
         else
           "no"));
  let transformed =
    Booleans.preprocess_booleans (Rewriting.unfold_def_before_cnf decls)
  in
  let sk_ctx = Skolem.create () in
  let stmts = cnf st_ref ~sk_ctx transformed in
  let stmts = Booleans.preprocess_cnf_booleans stmts in
  let signature = Statement.signature ~conj_syms (CCVector.to_iter stmts) in
  let precedence = compute_prec st_ref ~signature (CCVector.to_iter stmts) in
  Util.debugf ~section 2 "@[<2>precedence:@ @[%a@]@]" (fun k ->
      k Precedence.pp precedence);
  let ord, select, bool_select = compute_ord_select st_ref precedence in
  let params = Phases.get_key Params.key st_ref in
  let ctx = make_ctx st_ref ~signature ~ord ~select ~bool_select ~sk_ctx () in
  let (Phases.Env_clauses (env, clauses)) =
    make_env st_ref ~params ~ctx stmts
  in
  has_goal_ := has_goal;
  let result, clauses = presaturate_clauses st_ref env clauses in
  let result = try_to_refute st_ref env clauses result in
  Phases.Env_result (env, result)

let print file env result (st_ref : Phases.State.t ref) =
  let@ _sp = Phases.with_span ~__FILE__ ~__LINE__ "phase.print" in
  print_stats_env env;
  print_szs_result st_ref ~file env result;
  print_dots st_ref env result

let check res (st_ref : Phases.State.t ref) =
  let@ () = Phases.with_phase st_ref Phases.Check_proof in
  let params = Phases.get_key Params.key st_ref in
  let comment = Options.comment () in
  let errcode =
    match res with
    | Saturate.Unsat p
      when params.Params.check || CCOpt.is_some params.Params.proof_trace ->
      let p' = LLProof_conv.conv p in
      let errcode =
        if params.Params.check then (
          Util.debug ~section 2 "start checking proof…";
          let start = Util.total_time_s () in
          let dot_prefix = params.Params.dot_check in
          let res, stats = LLProof_check.check ?dot_prefix p' in
          let stop = Util.total_time_s () in
          Format.printf
            "%s(@[<h>proof_check@ :res %a@ :stats %a :time %.3fs@])@." comment
            LLProof_check.pp_res res LLProof_check.pp_stats stats (stop -. start);
          if res = LLProof_check.R_fail then
            15
          else
            0
        ) else
          0
      in
      (match params.Params.dot_llproof with
      | None -> ()
      | Some file ->
        Util.debugf ~section 2 "print LLProof into `%s`" (fun k -> k file);
        LLProof.Dot.pp_dot_file file p');
      (match params.Params.proof_trace with
      | None -> ()
      | Some file ->
        Util.debugf ~section 2 "write proof trace to `%s`" (fun k -> k file);
        let enc = Proof_trace.create (open_out_bin file) in
        ignore
          (Proof_trace.emit_proof enc
             ~get_lits:(fun p ->
               match Proof.Result.view (Proof.S.result p) with
               | SClause.SClause_view c -> SClause.lits c
               | _ -> [||])
             p);
        Proof_trace.close enc);
      errcode
    | _ -> 0
  in
  errcode

let setup_gc (st_ref : Phases.State.t ref) =
  let@ () = Phases.with_phase st_ref Phases.Setup_gc in
  let gc = Gc.get () in
  Gc.set { gc with Gc.space_overhead = 150 }

let setup_signal (st_ref : Phases.State.t ref) =
  let@ () = Phases.with_phase st_ref Phases.Setup_signal in
  Signal.set_exn_handler (fun e ->
      let stack = Printexc.get_backtrace () in
      let msg = Printexc.to_string e in
      output_string stderr ("exception raised in signal: " ^ msg ^ "\n");
      output_string stderr stack;
      flush stderr;
      raise e)

(* process several files, printing the result *)
let process_files_and_print ?(params = Params.default) files
    (st_ref : Phases.State.t ref) =
  let@ _sp =
    Phases.with_span ~__FILE__ ~__LINE__ "phase.process-files-and-print"
  in
  let prelude = parse_prelude st_ref params in
  let f file : _ Phases.t =
   fun st_ref ->
    let (Phases.Env_result (env, res)) = process_file ~prelude file st_ref in
    print file env res st_ref;
    check res st_ref
  in
  let phases = List.map f files in
  let r = Phases.run_and_discard_l phases st_ref in
  print_stats st_ref;
  r

let main_cli ?setup_gc:(gc = true) () (st_ref : Phases.State.t ref) =
  let@ _sp = Phases.with_span ~__FILE__ ~__LINE__ "phase.main-cli" in
  if gc then setup_gc st_ref;
  let files, params = parse_cli st_ref in
  let _exts = load_extensions st_ref in
  let errcode = process_files_and_print ~params files st_ref in
  Phases.exit st_ref;
  errcode

let skip_parse_cli (st_ref : Phases.State.t ref) ?(params = Params.default) file
    =
  let@ () = Phases.with_phase st_ref Parse_CLI in
  CCFormat.set_color_default true;
  Phases.set_key Params.key params st_ref;
  [ file ], params

let main ?setup_gc:(gc = true) ?params file (st_ref : Phases.State.t ref) =
  if gc then setup_gc st_ref;
  let files, params = skip_parse_cli st_ref ?params file in
  let _ = load_extensions st_ref in
  let errcode = process_files_and_print ~params files st_ref in
  Phases.exit st_ref;
  errcode

let () =
  let open Libzipperposition in
  Params.add_opts
    [
      ( "--de-bruijn-weight",
        Arg.Set_int _db_w,
        " Set weight of de Bruijn index for KBO" );
      ( "--lambda-weight",
        Arg.Set_int _lmb_w,
        " Set weight of lambda symbol for KBO" );
      ( "--kbo-weight-fun",
        Arg.Set_string _kbo_wf,
        " Set the function for symbol weight calculation." );
      ( "--prec-gen-fun",
        Arg.Set_string _prec_fun,
        " Set the function used for precedence generation" );
      ( "--sine-depth-min",
        Arg.Int
          (fun v ->
            if !_sine_threshold == -1 then _sine_threshold := 100;
            _sine_d_min := v),
        " Turn on SinE with threshold and set min SinE depth." );
      ( "--sine-depth-max",
        Arg.Int
          (fun v ->
            if !_sine_threshold == -1 then _sine_threshold := 100;
            _sine_d_max := v),
        " Turn on SinE with threshold and set max SinE depth." );
      ( "--sine-tolerance",
        Arg.Float
          (fun v ->
            if !_sine_threshold == -1 then _sine_threshold := 100;
            _sine_tolerance := v),
        " Turn on SinE with threshold of 100 and set SinE symbol tolerance." );
      ( "--sine-trim-implications",
        Arg.Bool (( := ) _trim_implications),
        " trim long implications while getting symbols from conjecture" );
      ( "--sine-take-only-defs",
        Arg.Bool (( := ) _take_only_defs),
        " take only axioms marked as definitions and the conjecture" );
      ( "--sine-ignore-k-most-common-syms",
        Arg.Int (fun v -> _ignore_k_most_common_symbols := Some v),
        " if conjecture symbol is within k most common occurring ones, then it \
         will be disregarded as conjecture symbol" );
      ( "--sine-take-conj-defs",
        Arg.Bool (( := ) _take_conj_defs),
        " force taking definitions of symbols ocurring in conjecture" );
      ( "--sine",
        Arg.Set_int _sine_threshold,
        " Set SinE axiom number threshold (negative number turns it off)"
        ^ " with default settings: depth in range 1-5 and tolerance 1.5" );
    ];

  Params.add_to_mode "best" (fun () ->
      _lmb_w := 20;
      _db_w := 10);

  Params.add_to_mode "ho-pragmatic" (fun () ->
      _lmb_w := 20;
      _db_w := 10);

  Params.add_to_mode "ho-complete-basic" (fun () ->
      _lmb_w := 20;
      _db_w := 10);

  Params.add_to_mode "ho-competitive" (fun () ->
      _lmb_w := 20;
      _db_w := 10)
