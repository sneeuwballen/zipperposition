(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** Main file for the prover *)

open Basic
open Symbols
open Params

module T = Terms
module O = Orderings
module C = Clauses
module I = Index
module PS = ProofState
module CQ = ClauseQueue
module S = FoSubst
module Utils = FoUtils
module Unif = FoUnif
module Sup = Superposition
module Sat = Saturate
module Sel = Selection

let version = "0.2"

(* FIXME: bug in the path traversal (initial '/' is removed but shouldn't) *)

(** find the given file from given directory *)
let find_file name dir =
  (* check if the file exists *)
  let rec file_exists name =
    try ignore (Unix.stat name); true
    with Unix.Unix_error (e, _, _) when e = Unix.ENOENT -> false
  (* search recursively from dir *)
  and search path cur_name =
    Utils.debug 3 "%% search %s as %s@." name cur_name;
    match path with
    | _ when file_exists cur_name -> cur_name (* found *)
    | [] -> failwith ("unable to find file " ^ name)
    | _::path' ->
      let new_dir = List.fold_left Filename.concat "" (List.rev path') in
      let new_name = Filename.concat new_dir name in
      search path' new_name
  in
  if Filename.is_relative name
    then
      let r = Str.regexp Filename.dir_sep in
      let path = List.rev (Str.split r dir) in
      search path (Filename.concat dir name)
    else if file_exists name then name else failwith ("unable to find file " ^ name)

(** parse given tptp file *)
let parse_file ~recursive f =
  let dir = Filename.dirname f in
  (* [aux files clauses] parses all files in files and add
     the resulting clauses to clauses *)
  let rec aux files clauses = match files with
  | [] -> clauses
  | f::tail ->
    let new_clauses, new_includes = parse_this f in
    if recursive
      then aux (List.rev_append new_includes tail) (List.rev_append new_clauses clauses)
      else (List.rev_append new_clauses clauses)
  (* parse the given file, raise exception in case of error *)
  and parse_this f =
    let input = match f with
    | "stdin" -> stdin
    | _ -> open_in (find_file f dir) in
    try
      let buf = Lexing.from_channel input in
      Const.cur_filename := f;
      Parser_tptp.parse_file Lexer_tptp.token buf
    with _ as e -> close_in input; raise e
  in aux [Filename.basename f] []

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
  print_hashcons_stats "terms" (T.stats ());
  print_hashcons_stats "clauses" (C.stats ());
  print_state_stats (Env.stats ~env);
  print_global_stats ()

let print_json_stats ~env =
  let open Sequence.Infix in
  let encode_hashcons (x1,x2,x3,x4,x5,x6) =
    `List [`Int x1; `Int x2; `Int x3; `Int x4; `Int x5; `Int x6] in
  let theories = match (Env.get_meta ~env) with None -> []
    | Some meta -> Meta.Prover.theories meta
      |> Sequence.map (fun th -> Utils.sprintf "%a" Meta.Prover.pp_theory th)
      |> Sequence.map (fun x -> `String x) |> Sequence.to_list in
  let experts = Experts.Set.size (Env.get_experts ~env) in
  let (o : json) =
    `Assoc [
      "terms", encode_hashcons (T.stats ());
      "clauses", encode_hashcons (C.stats ());
      "theories", `List theories;
      "experts", `Int experts;
    ]
  in
  Utils.debug 0 "%% json_stats: %s" (Yojson.Basic.to_string o)

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
  if params.param_version then (Format.printf "%% zipperposition v%s@." version; exit 0)

let setup_calculus ~env =
  match (Env.get_params ~env).param_calculus with
  | "superposition" -> Superposition.setup_env ~env
  | "delayed" -> Delayed.setup_env ~env
  | x -> failwith ("unknown calculus " ^ x)

(** Initialize the meta-prover *)
let mk_meta ~ctx ~kb params =
  if params.param_theories then
    (* create meta *)
    let meta = Meta.Prover.create ~ctx kb in
    Some meta
  else None

(** Initial environment *)
let mk_initial_env ?(initial_signature=Symbols.empty_signature) ~kb ~params clauses =
  let signature = Symbols.merge_signatures initial_signature (C.signature clauses) in
  let ord = Orderings.default_ordering signature in
  let ctx = mk_ctx ord no_select in
  let temp_meta = mk_meta ~ctx ~kb params in
  let env = Env.mk_env ?meta:temp_meta ~ctx params signature in
  (* populate env with calculus *)
  setup_calculus ~env;
  env
  
(** Compute the ordering from the list of clauses and the signature,
    according to parameters *)
let compute_ord ?(initial_signature=Symbols.empty_signature) ~kb ~params clauses =
  let signature = Symbols.merge_signatures initial_signature (C.signature clauses) in
  let symbols = Symbols.symbols_of_signature signature in
  (* environment is needed, to access constraints *)
  let env = mk_initial_env ~initial_signature ~kb ~params clauses in
  let constrs = Env.compute_constrs ~env clauses in
  let so = if params.param_precedence
    (* use the heuristic to try to order definitions and rewrite rules *)
    then Precedence.heuristic_precedence ~initial_signature params.param_ord
      [Precedence.invfreq_constraint clauses; Precedence.alpha_constraint]
      constrs
      clauses
    else Precedence.mk_precedence
      (constrs  @ [Precedence.invfreq_constraint clauses;
                   Precedence.alpha_constraint]) symbols
  in
  params.param_ord so

(** Parse the theory file and add its content to the KB *)
let parse_theory_file kb file =
  Format.printf "%% read content of %s into the Knowledge Base@." file;
  let ic = open_in file in
  try
    let lexbuf = Lexing.from_channel ic in
    let definitions = Parser_tptp.parse_meta (Lexer_tptp.token) lexbuf in
    Meta.KB.add_definitions kb (Sequence.of_list definitions)
  with e ->
    close_in ic;
    raise e

(** Load plugins *)
let load_plugins ~params =
  Utils.list_flatmap
    (fun filename ->
      let n = String.length filename in
      let filename =  (* plugin name, or file? *)
        if n > 4 && String.sub filename (n-5) 5 = ".cmxs"
          then filename
          else FoUtils.sprintf "plugins/std/ext_%s.cmxs" filename
      in
      match Extensions.dyn_load filename with
      | None ->  (* Could not load plugin *)
        Utils.debug 0 "%% could not load plugin %s" filename;
        []
      | Some factory ->
        let ext = factory () in
        Utils.debug 0 "%% loaded extension %s" ext.Extensions.name;
        [ext])
    params.param_plugins

(** Parses and populates the initial Knowledge Base *)
let initial_kb params =
  (* parse file into an initial empty KB *)
  let kb = Meta.KB.empty in
  let file = params.param_kb in
  (* parse file, with a lock *)
  let kb = Utils.with_lock_file file
    (fun () ->
      let kb = try Meta.KB.restore ~file kb
               with Yojson.Json_error _ -> Meta.KB.empty in
      (* load required files *)
      let kb = List.fold_left parse_theory_file kb params.param_kb_load in
      (* save new KB *)
      Meta.KB.save ~file kb;
      kb) in
  (* return KB *)
  kb

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
      Proof.pp_dot_file ~name dot_f c.hcproof
    | _ -> Utils.debug 1 "%% no empty clause; do not print state");
  (try (* write simplification index into the given file *)
    let dot_simpl = Sys.getenv "DOT_SIMPL" in
    Utils.debug 0 "%% print simplification index to %s" dot_simpl;
    ignore (Utils.with_output dot_simpl
      (fun out ->
        let fmt = Format.formatter_of_out_channel out in
        env.Env.state#simpl_set#idx_simpl#to_dot fmt;
        Format.pp_print_flush fmt ()))
   with Not_found -> ());
  ()

let print_meta ~env =
  (* print theories *)
  match Env.get_meta ~env with
  | Some meta ->
    Utils.debug 0 "%% @[<h>meta-prover results (%d):@ %a@]"
      (Sequence.length (Meta.Prover.results meta))
      Meta.Prover.pp_results (Meta.Prover.results meta);
    Format.printf "%% datalog contains %d clauses@."
      (Meta.KB.Logic.db_size (Meta.Prover.db meta))
  | None -> ()

let print_szs_result ~env result =
  match result with
  | Sat.Unknown | Sat.Timeout -> Printf.printf "%% SZS status ResourceOut\n"
  | Sat.Error s -> Printf.printf "%% error occurred: %s\n" s
  | Sat.Sat ->
      (if !Const.incompleteness
        then Printf.printf "%% SZS status GaveUp\n"
        else Printf.printf "%% SZS status CounterSatisfiable\n");
      Utils.debug 1 "%% saturated set: @[<v>%a@]@."
      C.pp_set (C.CSet.of_seq C.CSet.empty (Env.get_active ~env))
  | Sat.Unsat c -> begin
      (* print status then proof *)
      Printf.printf "# SZS status Theorem\n";
      Format.printf ("@.# SZS output start Refutation@.@[<v>%a@]@." ^^
                          "# SZS output end Refutation@.")
                    (Proof.pp_proof (Env.get_params ~env).param_proof) c.hcproof;
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
        Utils.with_lock_file kb_lock
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
  Utils.debug 2 "%% @[<hov2> build env from clauses@; %a@]"
    (Utils.pp_list ~sep:"" !C.pp_clause#pp_h) clauses;
  let initial_signature = C.signature clauses in
  (* temporary environment *)
  let temp_env = mk_initial_env ~kb ~params clauses in
  let clauses = Env.preprocess ~env:temp_env clauses in
  (* first preprocessing, with a simple ordering. *)
  Utils.debug 2 "%% clauses first-preprocessed into: @[<v>%a@]@."
                 (Utils.pp_list ~sep:"" !C.pp_clause#pp_h) clauses;
  (* meta-prover *)
  let clauses = enrich_with_theories ~env:temp_env clauses in
  (* choose an ord now, using clauses and initial_signature (some symbols
      may have disappeared from clauses after preprocessing). *)
  let ord = compute_ord ~initial_signature ~kb ~params clauses in
  Format.printf "%% precedence: %a@." pp_precedence ord.ord_precedence.prec_snapshot;
  (* selection function *)
  Format.printf "%% selection function: %s@." params.param_select;
  let select = Sel.selection_from_string ~ord params.param_select in
  (* build definitive context and env *)
  let ctx = mk_ctx ord select in
  let meta = mk_meta ~ctx:ctx ~kb params in
  let env = Env.mk_env ?meta ~ctx params (C.signature clauses) in
  (* set calculus again *)
  setup_calculus ~env;
  (* add already discovered experts *)
  Experts.Set.iter (Env.get_experts ~env:temp_env) (Env.add_expert ~env);
  env

(** Process the given file (try to solve it) *)
let process_file ~kb ~plugins params f =
  Format.printf "%% *** process file %s ***@." f;
  let steps = if params.param_steps = 0
    then None else (Format.printf "%% run for %d steps@." params.param_steps;
                    Some params.param_steps)
  and timeout = if params.param_timeout = 0.
    then None else (Format.printf "%% run for %f s@." params.param_timeout;
                    ignore (setup_alarm params.param_timeout);
                    Some (Utils.get_start_time () +. params.param_timeout -. 0.25))
  in
  Format.printf "%% got %d plugins@." (List.length plugins);  (* TODO use plugins *)
  (* parse clauses *)
  let clauses = parse_file ~recursive:true f in
  Printf.printf "%% parsed %d clauses\n" (List.length clauses);
  (* convert simple clauses to clauses, first with a simple ordering *)
  List.iter (fun (t,_,_) -> Utils.debug 1 "%% formula @[<h>%a@]" !T.pp_term#pp t) clauses;
  let dummy_ctx = mk_ctx Orderings.no_ordering no_select in
  let clauses = List.map (C.from_term ~ctx:dummy_ctx) clauses in
  (* build an environment that takes parameters and clauses into account
    (choice of ordering, etc.) *)
  let env = build_env ~kb ~params clauses in
  (* preprocess clauses (including calculus axioms), then possibly simplify them *)
  let clauses = List.rev_append env.Env.axioms clauses in
  let clauses = List.map (C.update_ctx ~ctx:env.Env.ctx) clauses in
  let num_clauses = List.length clauses in
  let clauses = Env.preprocess ~env clauses in
  (* maybe perform initial inter-reductions *)
  let result, clauses = if params.param_presaturate
    then begin
      Format.printf "%% presaturate initial clauses@.";
      Env.add_passive ~env (Sequence.of_list clauses);
      let result, num = Sat.presaturate ~env in
      Format.printf "%% initial presaturation in %d steps@." num;
      (* pre-saturated set of clauses *)
      let clauses = Env.get_active ~env in
      (* remove clauses from [env] *)
      Env.remove_active ~env clauses;
      Env.remove_passive ~env clauses;
      result, clauses
    end else Sat.Unknown, Sequence.of_list clauses
  in
  Utils.debug 1 "%% %d clauses processed into: @[<v>%a@]@."
                 num_clauses (Sequence.pp_seq ~sep:"" !C.pp_clause#pp_h) clauses;
  (* add clauses to passive set of [env] *)
  Env.add_passive ~env clauses;
  (* saturate *)
  let result, num = match result with
    | Sat.Unsat _ -> result, 0  (* already found unsat during presaturation *)
    | _ -> Sat.given_clause ~generating:true ?steps ?timeout ~env
  in
  Printf.printf "%% ===============================================\n";
  Printf.printf "%% done %d iterations\n" num;
  Format.printf "%% final precedence: %a@." pp_precedence
    env.Env.ctx.ctx_ord.ord_precedence.prec_snapshot;
  (* print some statistics *)
  print_stats ~env;
  print_json_stats ~env;
  print_dots ~env result;
  print_meta ~env;
  Utils.debug 0 "%% @[<h>experts: %a@]" Experts.Set.pp (Env.get_experts ~env);
  print_szs_result ~env result;
  ()

(** Print the content of the KB, and exit *)
let print_kb ~kb =
  Format.printf "%a@." Meta.KB.pp kb;
  exit 0

(** Clear the Knowledge Base and exit *)
let clear_kb params =
  let file = params.param_kb in
  Utils.with_lock_file file
    (fun () ->  (* remove file *)
      Unix.unlink file);
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
  (if params.param_kb_print then print_kb ~kb);
  (if params.param_kb_clear then clear_kb params);
  (* master process: process files *)
  List.iter (process_file ~kb ~plugins params) params.param_files

let _ =
  at_exit (fun () -> 
    Printf.printf "\n%% run time: %.3f\n" (Utils.get_total_time ()))
