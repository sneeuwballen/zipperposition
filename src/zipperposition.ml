
(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Main file for the prover} *)

open Logtk
open Params

module T = Term
module O = Ordering
module C = Clause
module Lit = Literal
module PS = ProofState
module CQ = ClauseQueue
module S = Substs
module Sup = Superposition
module Sat = Saturate
module Sel = Selection

(** print stats *)
let print_stats ~env =
  let print_hashcons_stats what (sz, num, sum_length, small, median, big) =
    Printf.printf ("%% hashcons stats for %s: size %d, num %d, sum length %d, "
                ^^ "buckets: small %d, median %d, big %d\n")
      what sz num sum_length small median big
  and print_state_stats (num_active, num_passive, num_simpl) =
    Printf.printf "%% proof state stats:\n";
    Printf.printf "%%   active clauses          %d\n" num_active;
    Printf.printf "%%   passive clauses         %d\n" num_passive;
    Printf.printf "%%   simplification clauses  %d\n" num_simpl
  and print_gc () =
    let stats = Gc.stat () in
    Printf.printf ("%% GC: minor words %.0f; major_words: %.0f; max_heap: %d; "
                ^^ "minor collections %d; major collections %d\n")
      stats.Gc.minor_words stats.Gc.major_words stats.Gc.top_heap_words
      stats.Gc.minor_collections stats.Gc.major_collections
  in
  print_gc ();
  print_hashcons_stats "terms" (T.H.stats ());
  print_hashcons_stats "clauses" (C.CHashcons.stats ());
  print_state_stats (Env.stats ~env);
  Util.print_global_stats ()

let print_json_stats ~env =
  let open Sequence.Infix in
  let encode_hashcons (x1,x2,x3,x4,x5,x6) =
    Util.sprintf "[%d, %d, %d, %d, %d, %d]" x1 x2 x3 x4 x5 x6 in
  let theories = match (Env.get_meta ~env) with
    | None -> "[]"
    | Some meta ->
      let seq = MetaProverState.theories meta in
      Util.sprintf "[%a]" (Util.pp_seq MetaProverState.pp_theory) seq
  in
  let experts = Experts.Set.size (Env.get_experts ~env) in
  let o = Util.sprintf
    "{ \"terms\": %s, \"clauses\": %s, \"theories\": %s, \"experts\":%d }"
    (encode_hashcons (T.H.stats ()))
    (encode_hashcons (C.CHashcons.stats ())) theories experts
  in
  Util.debug 0 "json_stats: %s" o

(** print the final state to given file in DOT, with
    clauses in result if needed *)
let print_state ?name filename (state, result) =
  match result with
  | Sat.Unsat c -> Proof.pp_dot_file ?name filename c.C.hcproof
  | _ -> Util.debug 1 "no empty clause; do not print state"

(** setup an alarm for abrupt stop *)
let setup_alarm timeout =
  let handler s =
    begin
      Printf.printf "%% SZS Status ResourceOut\n";
      Unix.kill (Unix.getpid ()) Sys.sigterm
    end
  in
  ignore (Sys.signal Sys.sigalrm (Sys.Signal_handle handler));
  Unix.alarm (max 1 (int_of_float timeout))

let print_version ~params =
  if params.param_version
    then (Printf.printf "%% zipperposition v%s\n" Const.version; exit 0)

let setup_calculus ~env =
  match (Env.get_params ~env).param_calculus with
  | "superposition" -> Superposition.setup_env ~env
  | "delayed" -> Delayed.setup_env ~env
  | x -> failwith ("unknown calculus " ^ x)

(** Initialize the meta-prover *)
let mk_meta ~ctx ~kb params =
  if params.param_theories then
    (* create meta *)
    let meta = MetaProverState.create ~ctx kb in
    Some meta
  else None

(** Initial environment *)
let mk_initial_env ?(initial_signature=Signature.empty) ~kb ~params clauses =
  let signature = Signature.merge initial_signature (C.signature clauses) in
  let ord = Ordering.default signature in
  let ctx = Ctx.create ~ord ()  in
  let temp_meta = mk_meta ~ctx ~kb params in
  let env = Env.create ?meta:temp_meta ~ctx params signature in
  (* populate env with calculus *)
  setup_calculus ~env;
  env

(** Compute the ordering from the list of clauses and the signature,
    according to parameters *)
let compute_ord ?(initial_signature=Signature.empty) ~kb ~params clauses =
  let signature = Signature.merge initial_signature (C.signature clauses) in
  let symbols = Signature.to_symbols signature in
  (* environment is needed, to access constraints *)
  let env = mk_initial_env ~initial_signature ~kb ~params clauses in
  let constrs = Env.compute_constrs ~env clauses in
  let clauses' = Sequence.map C.to_prec_clause clauses in
  let so = if params.param_precedence
    (* use the heuristic to try to order definitions and rewrite rules *)
    then HeuristicPrecedence.compute ~initial_signature params.param_ord
      [Precedence.invfreq_constraint clauses'; Precedence.alpha_constraint]
      constrs
      clauses
    else
      let constrs = 
        (constrs @ [Precedence.invfreq_constraint clauses';
                    Precedence.alpha_constraint]) in
      Precedence.create constrs symbols
  in
  params.param_ord so

(** Parse the theory file and add its content to the KB *)
let parse_theory_file kb file =
  Util.debug 0 "read content of %s into the Knowledge Base" file;
  let kb' = Logtk.MetaKB.parse_theory_file file in
  Logtk.MetaKB.union kb kb'

(** Load plugins *)
let load_plugins ~params =
  Util.list_flatmap
    (fun filename ->
      let n = String.length filename in
      let filename =  (* plugin name, or file? *)
        if n > 4 && String.sub filename (n-5) 5 = ".cmxs"
          then filename
        else
          let local_filename = Util.sprintf "plugins/std/ext_%s.cmxs" filename in
          try
            ignore (Unix.stat local_filename);
            local_filename
          with Unix.Unix_error _ ->
            let home_filename = Util.sprintf "plugins/ext_%s.cmxs" filename in
            Filename.concat Const.home home_filename
      in
      match Extensions.dyn_load filename with
      | Extensions.Ext_failure msg -> (* Could not load plugin *)
        []
      | Extensions.Ext_success ext ->
        Util.debug 0 "%% loaded extension %s" ext.Extensions.name;
        [ext])
    params.param_plugins

(** Parses and populates the initial Knowledge Base *)
let initial_kb params =
  (* parse file into an initial empty KB *)
  let file = params.param_kb in
  let kb = match MetaKB.restore file with
  | None -> MetaKB.empty
  | Some kb -> kb
  in
  (* load required files *)
  List.fold_left parse_theory_file kb params.param_kb_load

(** Enrichment of the initial set of clauses by detecting some theories *)
let enrich_with_theories ~env clauses =
  List.fold_left
    (fun acc hc ->
      let lemmas = Env.meta_step ~env hc in
      Sequence.fold
        (fun acc hc -> hc::acc) acc lemmas)
    clauses clauses

(** Print some content of the state, based on environment variables *)
let print_dots ~env result =
  (match (Env.get_params ~env).param_dot_file with (* print state *)
  | None -> ()
  | Some dot_f ->
    match result with
    | Sat.Unsat c ->
      let name = "unsat_graph" in
      Proof.pp_dot_file ~name dot_f c.C.hcproof
    | _ -> Util.debug 1 "no empty clause; do not print state");
  (try (* write simplification index into the given file *)
    let dot_simpl = Sys.getenv "DOT_SIMPL" in
    Util.debug 0 "print simplification index to %s" dot_simpl;
    ignore
      (Util.with_output dot_simpl
        (fun out ->
          Util.fprintf out "%a\n" PS.UnitIndex.to_dot env.Env.state#simpl_set#idx_simpl;
          flush out))
   with Not_found -> ());
  ()

let print_meta ~env =
  (* print theories *)
  match Env.get_meta ~env with
  | Some meta ->
    Util.debug 0 "meta-prover results (%d): %a"
      (Sequence.length (MetaProverState.results meta))
      (Util.pp_seq MetaProverState.pp_result) (MetaProverState.results meta);
    Util.debug 0 "datalog contains %d clauses"
      (MetaReasoner.size (MetaProverState.reasoner meta))
  | None -> ()

let print_szs_result ~env result =
  match result with
  | Sat.Unknown | Sat.Timeout -> Format.printf "%% SZS status ResourceOut\n"
  | Sat.Error s -> Format.printf "%% error occurred: %s\n" s
  | Sat.Sat ->
      (if Ctx.is_completeness_preserved env.Env.ctx
        then Printf.printf "%% SZS status CounterSatisfiable\n"
        else Printf.printf "%% SZS status GaveUp\n");
      (if Util.get_debug () >= 1
        then Util.printf "%% saturated set: %a"
          C.pp_set_tstp (C.CSet.of_seq C.CSet.empty (Env.get_active ~env)));
  | Sat.Unsat c -> begin
      (* print status then proof *)
      Printf.printf "# SZS status Theorem\n";
      Util.printf "# SZS output start Refutation\n";
      Util.printf "%a\n" (Proof.pp (Env.get_params ~env).param_proof) c.C.hcproof;
      Printf.printf "# SZS output end Refutation\n";
      (* update knowledge base *)
      (*
      match meta with
      | Some meta when params.param_learn ->
        (* learning new lemmas *)
        LemmaLearning.learn_and_update meta c;
        (* merge with current file *)
        let file = params.param_kb in
        let kb_lock = lock_file file in
        (* do the update atomically *)
        Util.with_lock_file kb_lock
          (fun () ->
            (* read content of file (concurrent updates?) *)
            parse_theory_file kb file;
            (* save the union of both files *)
            Theories.save_kb ~file ~kb_printer:Theories.pp_disjunctions kb)
      | _ -> ()
      *)
    end

(** Given a list of clauses, and parameters, build an Env.t that fits the
    parameters (but does not contain the clauses yet) *)
let build_env ~kb ~params clauses =
  Util.debug 2 "build env from clauses %a"
    (Util.pp_list ~sep:"" C.pp_debug) clauses;
  let initial_signature = C.signature (Sequence.of_list clauses) in
  (* temporary environment *)
  let temp_env = mk_initial_env ~kb ~params (Sequence.of_list clauses) in
  let clauses = Env.preprocess ~env:temp_env clauses in
  (* first preprocessing, with a simple ordering. *)
  Util.debug 2 "clauses first-preprocessed into: %a"
     (Util.pp_list ~sep:"" C.pp_debug) clauses;
  (* meta-prover *)
  let clauses = enrich_with_theories ~env:temp_env clauses in
  (* choose an ord now, using clauses and initial_signature (some symbols
      may have disappeared from clauses after preprocessing). *)
  let ord = compute_ord ~initial_signature ~kb ~params (Sequence.of_list clauses) in
  Util.debug 0 "precedence: %a" Precedence.pp (O.precedence ord);
  (* selection function *)
  Util.debug 0 "selection function: %s" params.param_select;
  let select = Sel.selection_from_string ~ord params.param_select in
  (* build definitive context and env *)
  let ctx = Ctx.create ~ord ~select () in
  let meta = mk_meta ~ctx:ctx ~kb params in
  let env = Env.create ?meta ~ctx params (C.signature (Sequence.of_list clauses)) in
  (* set calculus again *)
  setup_calculus ~env;
  (* add already discovered experts *)
  Experts.Set.iter (Env.get_experts ~env:temp_env) (Env.add_expert ~env);
  env

(** Process the given file (try to solve it) *)
let process_file ~kb ~plugins params f =
  Util.debug 0 "===== process file %s =====" f;
  let steps = if params.param_steps = 0
    then None else (Util.debug 0 "run for %d steps" params.param_steps;
                    Some params.param_steps)
  and timeout = if params.param_timeout = 0.
    then None else (Util.debug 0 "run for %f s" params.param_timeout;
                    ignore (setup_alarm params.param_timeout);
                    Some (Util.get_start_time () +. params.param_timeout -. 0.25))
  in
  (* parse clauses *)
  let decls = Util_tptp.parse_file ~recursive:true f in
  Util.debug 0 "parsed %d declarations" (Sequence.length decls);
  let formulas = Sequence.to_list (Util_tptp.sourced_formulas ~file:f decls) in
  List.iter
    (fun (f,_,_) -> Util.debug 1 "  input formula: %a" T.pp f)
    formulas;
  (* convert formulas to clauses, first with a simple ordering *)
  let dummy_ctx = Ctx.create () in
  let clauses = List.map (C.from_term ~ctx:dummy_ctx) formulas in
  (* build an environment that takes parameters and clauses into account
    (choice of ordering, etc.) *)
  let env = build_env ~kb ~params clauses in
  (* load plugins *)
  List.iter (Extensions.apply_ext ~env) plugins;
  (* preprocess clauses (including calculus axioms), then possibly simplify them *)
  let clauses = List.rev_append env.Env.axioms clauses in
  let clauses = List.map (C.update_ctx ~ctx:env.Env.ctx) clauses in
  let num_clauses = List.length clauses in
  let clauses = Env.preprocess ~env clauses in
  (* maybe perform initial inter-reductions *)
  let result, clauses = if params.param_presaturate
    then begin
      Util.debug 0 "presaturate initial clauses";
      Env.add_passive ~env (Sequence.of_list clauses);
      let result, num = Sat.presaturate ~env in
      Util.debug 0 "initial presaturation in %d steps" num;
      (* pre-saturated set of clauses *)
      let clauses = Env.get_active ~env in
      (* remove clauses from [env] *)
      Env.remove_active ~env clauses;
      Env.remove_passive ~env clauses;
      result, clauses
    end else Sat.Unknown, Sequence.of_list clauses
  in
  Util.debug 1 "%d clauses processed into:" num_clauses;
  Sequence.iter
    (fun c -> Util.debug 1 "  %a" C.pp_debug c)
    clauses;
  (* add clauses to passive set of [env] *)
  Env.add_passive ~env clauses;
  (* saturate *)
  let result, num = match result with
    | Sat.Unsat _ -> result, 0  (* already found unsat during presaturation *)
    | _ -> Sat.given_clause ~generating:true ?steps ?timeout ~env
  in
  Util.debug 0 "===============================================";
  Util.debug 0 "done %d iterations" num;
  Util.debug 0 "final precedence: %a" Precedence.pp (Env.precedence env);
  (* print some statistics *)
  print_stats ~env;
  print_json_stats ~env;
  print_dots ~env result;
  print_meta ~env;
  Util.debug 0 "experts: %a" Experts.Set.pp (Env.get_experts ~env);
  print_szs_result ~env result;
  ()

(** Print the content of the KB, and exit *)
let print_kb ~kb =
  Util.printf "%a\n" MetaKB.pp kb;
  exit 0

(** Clear the Knowledge Base and exit *)
let clear_kb params =
  let file = params.param_kb in
  Util.with_lock_file file
    (fun () ->  (* remove file *)
      Unix.unlink file);
  exit 0

let print_kb_where params = 
  Util.debug 0 "KB located at %s" params.param_kb;
  exit 0

let () =
  (* parse arguments *)
  let params = Params.parse_args () in
  Random.init params.param_seed;
  print_version params;
  (* plugins *)
  let plugins = load_plugins ~params in
  (* operations on knowledge base *)
  let kb = initial_kb params in
  (if params.param_kb_where then print_kb_where params);
  (if params.param_kb_print then print_kb ~kb);
  (if params.param_kb_clear then clear_kb params);
  (* master process: process files *)
  List.iter (process_file ~kb ~plugins params) params.param_files

let _ =
  at_exit (fun () -> 
    Util.debug 0 "run time: %.3f" (Util.get_total_time ()))
