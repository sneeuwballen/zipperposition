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

open Types
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
module Delayed = Delayed

let version = "0.3"

let lock_file file = file ^ ".lock"

(** find the given file from given directory *)
let find_file name dir =
  (* check if the file exists *)
  let rec file_exists name =
    try ignore (Unix.stat name); true
    with Unix.Unix_error (e, _, _) when e = Unix.ENOENT -> false
  (* search recursively from dir *)
  and search path cur_name =
    Utils.debug 3 (lazy (Utils.sprintf "%% search %s as %s@." name cur_name));
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
let print_stats state =
  let print_hashcons_stats what (sz, num, sum_length, small, median, big) =
    Printf.printf "%% hashcons stats for %s: size %d, num %d, sum length %d, buckets: small %d, median %d, big %d\n"
      what sz num sum_length small median big
  and print_state_stats (num_active, num_passive) =
    Printf.printf "%% proof state stats:\n";
    Printf.printf "%%   active clauses   %d\n" num_active;
    Printf.printf "%%   passive clauses  %d\n" num_passive
  and print_gc () =
    let stats = Gc.stat () in
    Printf.printf "%% GC: minor words %.0f; major_words: %.0f; max_heap: %d; minor collections %d; major collections %d\n"
      stats.Gc.minor_words stats.Gc.major_words stats.Gc.top_heap_words stats.Gc.minor_collections stats.Gc.major_collections
  in
  print_gc ();
  print_hashcons_stats "terms" (T.stats ());
  print_hashcons_stats "clauses" (C.stats ());
  print_state_stats (PS.stats state);
  print_global_stats ()

(** print the final state to given file in DOT, with
    clauses in result if needed *)
let print_state ?name filename (state, result) =
  (match result with
    | Sat.Unsat c ->
      state#active_set#add [c]; (* put empty clause in state *)
    | _ -> ());
  PS.pp_dot_file ?name filename state

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

(** setup output format and details *)
let setup_output ~params =
  (match params.param_output_syntax with
  | "tstp" ->
    C.pp_clause := C.pp_clause_tstp;
    T.pp_term := T.pp_term_tstp;
    C.pp_proof := C.pp_proof_tstp
  | "debug" ->
    C.pp_clause := C.pp_clause_debug;
    T.pp_term := (T.pp_term_debug :> T.pprinter_term);
    C.pp_proof := C.pp_proof_debug
  | s -> failwith ("unknown output syntax " ^ s));
  (if params.param_print_sort
    then T.pp_term_debug#sort true);
  (if params.param_print_all
    then begin T.pp_term_debug#skip_lambdas false; T.pp_term_debug#skip_db false end)

let print_version ~params =
  if params.param_version then (Format.printf "%% zipperposition v%s@." version; exit 0)

(** Get the calculus described in the arguments *)
let get_calculus ~params =
  match params.param_calculus with
  | "superposition" -> Superposition.superposition
  | "delayed" -> Delayed.delayed
  | x -> failwith ("unknown calculus " ^ x)

(** Compute the ordering from the list of clauses, according to parameters *)
let compute_ord ~params clauses =
  let calculus = get_calculus ~params in
  let signature = C.signature clauses in
  let symbols = Symbols.symbols_of_signature signature in
  let so = if params.param_precedence
    (* use the heuristic to try to order definitions and rewrite rules *)
    then Precedence.heuristic_precedence params.param_ord
      [Precedence.invfreq_constraint clauses; Precedence.alpha_constraint]
      (calculus#constr clauses)
      clauses
    else Precedence.mk_precedence
      (calculus#constr clauses @ [Precedence.invfreq_constraint clauses;
                                  Precedence.alpha_constraint]) symbols
  in
  params.param_ord so

(** Parse the theory file and add its content to the KB *)
let parse_theory_file kb file =
  Format.printf "%% read content of %s into the Knowledge Base@." file;
  let input = if file = "-" then stdin else open_in file in
  let lexbuf = Lexing.from_channel input in
  (* parse content *)
  let disjunctions = Parser_tptp.parse_theory_file Lexer_tptp.token lexbuf in
  (* load content *)
  Theories.load_theory kb disjunctions;
  if file <> "-" then close_in input else ()

(** Parses and populates the initial Knowledge Base *)
let initial_kb params =
  (* parse KB and update it*)
  let kb_lock = lock_file params.param_kb in
  let kb = Theories.update_kb ~file:params.param_kb ~lock:kb_lock
    (fun kb ->
      (* load required files *)
      List.iter
        (fun f -> parse_theory_file kb f)
        params.param_kb_load;
      kb)
  in
  Utils.debug 2 (lazy (Utils.sprintf "initial kb: %a@." Theories.pp_kb kb));
  kb

(** Initialize the meta-prover *)
let mk_meta ~ord ~kb params =
  if params.param_theories then
    (* create meta *)
    let meta = Theories.create_meta ~ord kb in
    Some meta
  else None

(** Enrichment of the initial set of clauses by detecting some theories *)
let enrich_with_theories ~ord meta clauses =
  match meta with
  | None -> clauses
  | Some meta ->
    List.fold_left
      (fun acc hc ->
        let lemmas = Theories.scan_clause meta hc in
        List.rev_append lemmas acc)
      clauses clauses

(** Process the given file (try to solve it) *)
let process_file ~kb params f =
  Format.printf "%% *** process file %s ***@." f;
  let steps = if params.param_steps = 0
    then None else (Format.printf "%% run for %d steps@." params.param_steps;
                    Some params.param_steps)
  and timeout = if params.param_timeout = 0.
    then None else (Format.printf "%% run for %f s@." params.param_timeout;
                    ignore (setup_alarm params.param_timeout);
                    Some (Sat.get_start_time () +. params.param_timeout -. 0.25))
  and progress = params.param_progress in
  let clauses = parse_file ~recursive:true f in
  Printf.printf "%% parsed %d clauses\n" (List.length clauses);
  (* find the calculus *)
  let calculus = get_calculus ~params in
  (* convert simple clauses to clauses, first with a simple ordering *)
  let signature = Simple.signature (List.map fst clauses) in
  let d_ord = O.default_ordering signature in
  let clauses = List.map (C.from_simple ~ord:d_ord) clauses in
  (* first preprocessing, with a simple ordering. *)
  let clauses = calculus#preprocess ~ord:d_ord ~select:no_select clauses in
  Utils.debug 2 (lazy (Utils.sprintf "%% clauses first-preprocessed into: @[<v>%a@]@."
                 (Utils.pp_list ~sep:"" !C.pp_clause#pp_h) clauses));
  (* meta-prover *)
  let meta = mk_meta ~ord:d_ord ~kb params in
  let clauses = enrich_with_theories ~ord:d_ord meta clauses in
  (* choose an ord now, using clauses *)
  let ord = compute_ord ~params clauses in
  (match meta with | None -> () | Some meta -> Theories.meta_update_ord ~ord meta);
  Format.printf "%% precedence: %a@." T.pp_precedence ord#precedence#snapshot;
  (* selection function *)
  Format.printf "%% selection function: %s@." params.param_select;
  let select = Sel.selection_from_string ~ord params.param_select in
  (* preprocess clauses (including calculus axioms), then possibly simplify them *)
  let clauses = List.rev_append calculus#axioms clauses in
  let num_clauses = List.length clauses in
  let clauses = calculus#preprocess ~ord ~select clauses in
  (* create state, and add clauses to the simpl_set *)
  let signature = C.signature clauses in
  let state = PS.mk_state ~ord ?meta params signature in
  (* maybe perform initial inter-reductions *)
  let clauses = if params.param_presaturate
    then begin
      state#passive_set#add clauses;
      let status, num = Sat.presaturate ~calculus state in
      Format.printf "%% initial presaturation in %d steps@." num;
      assert (C.CSet.is_empty state#passive_set#clauses);
      let clauses = C.CSet.to_list state#active_set#clauses in
      (* remove clauses from active set *)
      state#active_set#remove clauses;
      clauses
    end else clauses
  in
  Utils.debug 1 (lazy (Utils.sprintf "%% %d clauses processed into: @[<v>%a@]@."
                 num_clauses (Utils.pp_list ~sep:"" !C.pp_clause#pp_h) clauses));
  (* add clauses to passive_set *)
  state#passive_set#add clauses;
  (* saturate, using a given clause main loop as choosen by the user *)
  let result, num =
    let parallel = params.param_parallel in
    if params.param_pipeline
      then Distributed.given_clause ~parallel ?steps ?timeout ~progress ~calculus ~params state
      else Sat.given_clause ?steps ?timeout ~progress ~calculus state
  in
  Printf.printf "%% ===============================================\n";
  Printf.printf "%% done %d iterations\n" num;
  (* print some statistics *)
  print_stats state;
  Format.printf "%% final precedence: %a@." T.pp_precedence ord#precedence#snapshot;
  (match params.param_dot_file with (* print state *)
  | None -> ()
  | Some dot_f -> print_state ~name:("\""^f^"\"") dot_f (state, result));
  (* print theories *)
  (match meta with None -> ()
    | Some meta -> Format.printf "%% detected theories: @[<h>%a@]@."
    (Utils.pp_list (Datalog.Logic.pp_term ?to_s:None)) meta.Theories.meta_theories);
  match result with
  | Sat.Unknown | Sat.Timeout -> Printf.printf "%% SZS status ResourceOut\n"
  | Sat.Error s -> Printf.printf "%% error occurred: %s\n" s
  | Sat.Sat ->
      Printf.printf "%% SZS status CounterSatisfiable\n";
      Utils.debug 1 (lazy (Utils.sprintf "%% saturated set: @[<v>%a@]@."
                     C.pp_set state#active_set#clauses))
  | Sat.Unsat c -> begin
      (* print status then proof *)
      Printf.printf "# SZS status Theorem\n";
      (if params.param_proof
        then Format.printf ("@.# SZS output start Refutation@.@[<v>%a@]@." ^^
                          "# SZS output end Refutation@.") !C.pp_proof#pp c);
      (* update knowledge base *)
      match meta with
      | Some meta when params.param_learn ->
        (* learning *)
        let kb_lock = lock_file params.param_kb in
        ignore (Theories.update_kb ~file:params.param_kb ~lock:kb_lock
          (fun kb ->
            let new_meta = { meta with Theories.meta_kb=kb; } in
            LemmaLearning.learn_and_update new_meta c;
            kb))
      | _ -> ()
    end

(** Print the content of the KB, and exit *)
let print_kb ~kb =
  Format.printf "%a@." Theories.pp_kb kb;
  exit 0

(** Clear the Knowledge Base and exit *)
let clear_kb params =
  let kb_lock = lock_file params.param_kb in
  Theories.clear_kb ~lock:kb_lock ~file:params.param_kb;
  exit 0

let () =
  (* parse arguments *)
  let params = Params.parse_args () in
  Random.init params.param_seed;
  print_version params;
  (* operations on knowledge base *)
  let kb = initial_kb params in
  (if params.param_kb_print then print_kb ~kb);
  (if params.param_kb_clear then clear_kb params);
  (* setup printing *)
  setup_output params;
  match params.param_role with
  | Some role ->
    (* slave process: assume a role in the pipeline *)
    let calculus = get_calculus ~params in
    let ord = params.param_ord (Precedence.mk_precedence ~complete:false [] []) in
    let select = Selection.selection_from_string ~ord params.param_select in
    Distributed.assume_role ~calculus ~ord ~select role
  | None ->
    (* master process: process files *)
    List.iter (process_file ~kb params) params.param_files

let _ =
  at_exit (fun () -> 
    Printf.printf "\n%% run time: %.3f\n" (Sat.get_total_time ()))
