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
let print_stats state =
  let print_hashcons_stats what (sz, num, sum_length, small, median, big) =
    Printf.printf ("%% hashcons stats for %s: size %d, num %d, sum length %d, "
                ^^ "buckets: small %d, median %d, big %d\n")
      what sz num sum_length small median big
  and print_state_stats (num_active, num_passive) =
    Printf.printf "%% proof state stats:\n";
    Printf.printf "%%   active clauses   %d\n" num_active;
    Printf.printf "%%   passive clauses  %d\n" num_passive
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
  print_state_stats (PS.stats state);
  print_global_stats ()

(** print the final state to given file in DOT, with
    clauses in result if needed *)
let print_state ?name filename (state, result) =
  match result with
  | Sat.Unsat c -> Proof.pp_dot_file ?name filename c.hcproof
  | _ -> Utils.debug 1 "%% no empty clause; do not print state"

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
  let ic = open_in file in
  try
    let lexbuf = Lexing.from_channel ic in
    let definitions = Parser_tptp.parse_meta (Lexer_tptp.token) lexbuf in
    Meta.KB.add_definitions kb (Sequence.of_list definitions)
  with e ->
    close_in ic;
    raise e

(** Parses and populates the initial Knowledge Base *)
let initial_kb params =
  (* parse file into an initial empty KB *)
  let kb = Meta.KB.empty in
  let file = params.param_kb in
  (* parse file, with a lock *)
  let kb = Utils.with_lock_file file
    (fun () ->
      let kb = try parse_theory_file kb file
               with Yojson.Json_error _ -> Meta.KB.empty in
      (* load required files *)
      let kb = List.fold_left parse_theory_file kb params.param_kb_load in
      (* save new KB *)
      Meta.KB.save ~file kb;
      kb) in
  (* return KB *)
  kb

(** Initialize the meta-prover *)
let mk_meta ~ctx ~kb params =
  if params.param_theories then
    (* create meta *)
    let meta = Meta.Prover.create ~ctx kb in
    Some meta
  else None

(** Enrichment of the initial set of clauses by detecting some theories *)
let enrich_with_theories ~ctx meta clauses =
  match meta with
  | None -> clauses
  | Some meta ->
    List.fold_left
      (fun acc hc ->
        let lemmas = Saturate.find_lemmas ~ctx (Some meta) hc in
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
  List.iter (fun (t,_,_) -> Utils.debug 1 "%% formula @[<h>%a@]" !T.pp_term#pp t) clauses;
  let signature = T.signature
    (Sequence.map (fun (x,_,_) -> x) (Sequence.of_list clauses)) in
  let d_ctx = {ctx_ord=O.default_ordering signature; ctx_select=no_select; } in
  let clauses = List.map (C.from_term ~ctx:d_ctx) clauses in
  (* first preprocessing, with a simple ordering. *)
  let clauses = calculus#preprocess ~ctx:d_ctx clauses in
  Utils.debug 2 "%% clauses first-preprocessed into: @[<v>%a@]@."
                 (Utils.pp_list ~sep:"" !C.pp_clause#pp_h) clauses;
  (* meta-prover *)
  let meta = mk_meta ~ctx:d_ctx ~kb params in
  let clauses = enrich_with_theories ~ctx:d_ctx meta clauses in
  (* choose an ord now, using clauses *)
  let ord = compute_ord ~params clauses in
  Format.printf "%% precedence: %a@." T.pp_precedence ord#precedence#snapshot;
  (* selection function *)
  Format.printf "%% selection function: %s@." params.param_select;
  let select = Sel.selection_from_string ~ord params.param_select in
  (* at least, the context *)
  let ctx = { ctx_ord=ord; ctx_select=select; } in
  (match meta with | None -> () | Some meta -> Meta.Prover.update_ctx ~ctx meta);
  (* preprocess clauses (including calculus axioms), then possibly simplify them *)
  let clauses = List.rev_append calculus#axioms clauses in
  let num_clauses = List.length clauses in
  let clauses = calculus#preprocess ~ctx clauses in
  (* create state, and add clauses to the simpl_set *)
  let signature = C.signature clauses in
  let state = PS.mk_state ~ctx ?meta params signature in
  (* maybe perform initial inter-reductions *)
  let result, clauses = if params.param_presaturate
    then begin
      state#passive_set#add clauses;
      let result, num = Sat.presaturate ~calculus state in
      Format.printf "%% initial presaturation in %d steps@." num;
      assert (not (result = Sat.Sat || result = Sat.Unknown) ||
              C.CSet.is_empty state#passive_set#clauses);
      let clauses = C.CSet.to_list state#active_set#clauses in
      (* remove clauses from active set *)
      state#active_set#remove clauses;
      result, clauses
    end else Sat.Unknown, clauses
  in
  Utils.debug 1 "%% %d clauses processed into: @[<v>%a@]@."
                 num_clauses (Utils.pp_list ~sep:"" !C.pp_clause#pp_h) clauses;
  (* add clauses to passive_set *)
  state#passive_set#add clauses;
  (* saturate, using a given clause main loop as choosen by the user *)
  (if params.param_split then Sat.enable_split := true);
  let result, num = match result with
    | Sat.Unsat _ -> result, 0  (* already found unsat during presaturation *)
    | _ -> Sat.given_clause ?steps ?timeout ~progress ~calculus state
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
    | Some meta ->
      Utils.debug 1 "%% @[<hov2>meta-prover:@ %a@]"
        Meta.Prover.pp_results (Meta.Prover.results meta);
      Format.printf "%% datalog contains %d clauses@."
        (Meta.KB.Logic.db_size (Meta.Prover.db meta)));
  match result with
  | Sat.Unknown | Sat.Timeout -> Printf.printf "%% SZS status ResourceOut\n"
  | Sat.Error s -> Printf.printf "%% error occurred: %s\n" s
  | Sat.Sat ->
      Printf.printf "%% SZS status CounterSatisfiable\n";
      Utils.debug 1 "%% saturated set: @[<v>%a@]@." C.pp_set state#active_set#clauses
  | Sat.Unsat c -> begin
      (* print status then proof *)
      Printf.printf "# SZS status Theorem\n";
      Format.printf ("@.# SZS output start Refutation@.@[<v>%a@]@." ^^
                          "# SZS output end Refutation@.")
                    (Proof.pp_proof params.param_proof) c.hcproof;
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

(** Print the content of the KB, and exit *)
let print_kb ~kb =
  Format.printf "%a@." Meta.KB.pp kb;
  exit 0

(** Clear the Knowledge Base and exit *)
let clear_kb params =
  let file = params.param_kb in
  Utils.with_lock_file file
    (fun () ->  (* write an empty KB to file *)
      Meta.KB.save ~file Meta.KB.empty);
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
  (* master process: process files *)
  List.iter (process_file ~kb params) params.param_files

let _ =
  at_exit (fun () -> 
    Printf.printf "\n%% run time: %.3f\n" (Sat.get_total_time ()))
