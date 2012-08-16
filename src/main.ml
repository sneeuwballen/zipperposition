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
open Hashcons

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

(** get first file of command line arguments *)
let get_file () =
  let files = ref [] in
  Arg.parse [] (fun s -> files := s :: !files) "./prover file";
  match !files with
  | [] -> failwith "file required."
  | (x::_) -> x

(* hashtable string -> ordering module *)
let ords = Hashtbl.create 7
let _ =
  Hashtbl.add ords "lpo" (fun () -> new Orderings.lpo (Orderings.default_symbol_ordering ()));
  Hashtbl.add ords "kbo" (fun () -> new Orderings.kbo (Orderings.default_symbol_ordering ()));
  Hashtbl.add ords "nrkbo" (fun () -> new Orderings.nrkbo (Orderings.default_symbol_ordering ()))

(** parameters for the main procedure *)
type parameters = {
  param_ord : unit -> ordering;
  param_steps : int;
  param_timeout : float;
  param_files : string list;
}

(** parse_args returns parameters *)
let parse_args () =
  (* parameters *)
  let ord = ref Orderings.default_ordering
  and steps = ref 0
  and timeout = ref 0.
  and file = ref "stdin" in
  (* argument functions *)
  let set_ord s = (* select ordering *)
    try ord := Hashtbl.find ords s
    with Not_found as e -> (Printf.printf "unknown ordering: %s\n" s; raise e)
  and set_file s = file := s in
  (* options list (TODO parse something about heuristics) *) 
  let options =
    [ ("-ord", Arg.String set_ord, "choose ordering (lpo,kbo,nrkbo)");
      ("-debug", Arg.Int Utils.set_debug, "debug level");
      ("-steps", Arg.Set_int steps, "verbose mode");
      ("-profile", Arg.Set HExtlib.profiling_enabled, "enable profile");
      ("-timeout", Arg.Set_float timeout, "verbose mode");
    ]
  in
  Arg.parse options set_file "solve problem in first file";
  (* return parameter structure *)
  { param_ord = !ord; param_steps = !steps; param_timeout = !timeout; param_files = [!file] }

(** parse given tptp file (TODO also parse include()s *)
let parse_file f =
  let input = match f with
    | "stdin" -> stdin
    | _ -> open_in f
  in
  try
    let buf = Lexing.from_channel input in
    Parser_tptp.parse_file Lexer_tptp.token buf
  with _ as e -> close_in input; raise e

(** print stats *)
let print_stats state =
  let print_hashcons_stats what (sz, num, sum_length, small, median, big) =
    Printf.printf "%% hashcons stats for %s: size %d, num %d, sum length %d, buckets: small %d, median %d, big %d\n"
      what sz num sum_length small median big
  and print_state_stats stats =
    Printf.printf "%% proof state stats:\n";
    Printf.printf "%%   active clauses   %d\n" stats.PS.stats_active_clauses;
    Printf.printf "%%   passive clauses  %d\n" stats.PS.stats_passive_clauses
  in
  print_hashcons_stats "terms" (T.H.stats T.terms);
  print_hashcons_stats "clauses" (C.H.stats C.clauses);
  print_state_stats (PS.stats state)

(** setup an alarm for abrupt stop *)
let setup_alarm timeout =
  let handler s = (Printf.printf "%% SZS Status ResourceOut\n"; raise Exit) in
  ignore (Sys.signal Sys.sigalrm (Sys.Signal_handle handler));
  Unix.alarm (max 1 (int_of_float timeout))

let () =
  (* parse arguments *)
  let params = parse_args () in
  let steps = if params.param_steps = 0
    then None else (Format.printf "%% run for %d steps@." params.param_steps;
                    Some params.param_steps)
  and timeout = if params.param_timeout = 0.
    then None else (Format.printf "%% run for %f s@." params.param_timeout;
                    ignore (setup_alarm params.param_timeout);
                    Some (Unix.gettimeofday() +. params.param_timeout)) in
  (* parse file *)
  let f = List.hd params.param_files in
  Printf.printf "%% process file %s\n" f;
  let clauses, _ = parse_file f in
  Printf.printf "%% parsed %d clauses\n" (List.length clauses);
  (* choose an ord now *)
  let ord = params.param_ord () in  (* using current signature *)
  Format.printf "%% signature: %a@." T.pp_signature ord#symbol_ordering#signature;
  let clauses = List.map (C.reord_clause ~ord) clauses in
  (* create a state, with clauses added to passive_set *)
  let state = PS.make_state ord (CQ.default_queues ~ord) in
  let state = {state with PS.passive_set=PS.add_passives state.PS.passive_set clauses} in
  (* saturate *)
  let state, result, num = Sat.given_clause ?steps ?timeout state in
  Printf.printf "%% ===============================================\n";
  Printf.printf "%% done %d iterations\n" num;
  print_stats state;
  match result with
  | Sat.Sat -> Printf.printf "%% SZS status CounterSatisfiable\n"
  | Sat.Unknown | Sat.Timeout -> Printf.printf "%% SZS status ResourceOut\n"
  | Sat.Error s -> Printf.printf "%% error occurred: %s\n" s
  | Sat.Unsat c ->
      (* print status then proof *)
      Printf.printf "%% SZS status Theorem\n";
      Format.printf "proof: @[<v>%a@]@." C.pp_proof_rec c.node
