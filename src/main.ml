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
module Delayed = Delayed

(** special heuristic: an ordering constraint that makes symbols
    occurring in negative equations bigger than symbols in
    positive equations in the given list of clauses *)
let heuristic_constraint clauses : ordering_constraint =
  let _, _, signature = O.current_signature () in
  let table = Hashtbl.create 23 in (* symbol -> (neg occurrences - pos occurences) *)
  (* update counts with term *)
  let rec update_with_term sign t = match t.node.term with
    | Var _ -> ()
    | Leaf s ->
        let count = try Hashtbl.find table s with Not_found -> 0 in
        Hashtbl.replace table s (if sign then count-1 else count+1)
    | Node l -> List.iter (update_with_term sign) l
  (* update counts with clause *)
  and update_with_clause clause =
    List.iter
      (fun (Equation (l, r, sign, _)) ->
        update_with_term sign l;
        update_with_term sign r)
    clause.clits
  in 
  List.iter update_with_clause clauses;
  (* sort symbols by decreasing (neg occurences - pos occurences) *)
  let ordered_symbols = List.sort
    (fun a b ->
      let count_a = try Hashtbl.find table a with Not_found -> 0
      and count_b = try Hashtbl.find table b with Not_found -> 0 in
      count_b - count_a)
    signature
  in
  (* make a constraint out of the ordered signature *)
  O.list_constraint ordered_symbols

(** create an ordering from the clauses *)
let heuristic_ordering clauses =
  let constr = heuristic_constraint clauses in
  let constr = O.compose_constraints constr O.consts_constraint in
  O.make_ordering constr

(** parameters for the main procedure *)
type parameters = {
  param_ord : string;
  param_steps : int;
  param_calculus : string;
  param_timeout : float;
  param_files : string list;
  param_proof : bool;
  param_output_syntax : string;   (** syntax for output *)
  param_print_sort : bool;        (** print sorts of terms *)
  param_print_all : bool;         (** print desugarized lambda / DB symbols *)
}

(** parse_args returns parameters *)
let parse_args () =
  (* parameters *)
  let ord = ref "rpo"
  and steps = ref 0
  and timeout = ref 0.
  and proof = ref true
  and output = ref "debug"
  and calculus = ref "delayed"
  and print_sort = ref false
  and print_all = ref false
  and file = ref "stdin" in
  (* options list (TODO parse something about heuristics) *) 
  let options =
    [ ("-ord", Arg.Set_string ord, "choose ordering (rpo,kbo)");
      ("-debug", Arg.Int Utils.set_debug, "debug level");
      ("-steps", Arg.Set_int steps, "verbose mode");
      ("-profile", Arg.Set HExtlib.profiling_enabled, "enable profile");
      ("-calculus", Arg.Set_string calculus, "set calculus ('superposition' or 'delayed')");
      ("-timeout", Arg.Set_float timeout, "verbose mode");
      ("-noproof", Arg.Clear proof, "disable proof printing");
      ("-output", Arg.Set_string output, "output syntax ('debug', 'tstp')");
      ("-print-sort", Arg.Set print_sort, "print sorts of terms");
      ("-print-all", Arg.Set print_sort, "print desugarized terms (lambdas, De Bruijn terms)");
    ]
  in
  Arg.parse options (fun f -> file := f) "solve problem in first file";
  (* return parameter structure *)
  { param_ord = !ord; param_steps = !steps; param_calculus = !calculus;
    param_timeout = !timeout; param_files = [!file];
    param_proof = !proof; param_output_syntax = !output;
    param_print_sort = !print_sort; param_print_all = !print_all; }

(** parse given tptp file *)
let parse_file ~recursive f =
  let dir = Filename.dirname f in
  (* [aux files clauses] parses all files in files and add
     the resulting clauses to clauses *)
  let rec aux files clauses = match files with
  | [] -> clauses
  | f::tail ->
    (* if relative, append prefix, else keep absolute name *)
    let f = if Filename.is_relative f then Filename.concat dir f else f in
    let new_clauses, new_includes = parse_this f in
    if recursive
      then aux (List.rev_append new_includes tail) (List.rev_append new_clauses clauses)
      else (List.rev_append new_clauses clauses)
  (* parse the given file, raise exception in case of error *)
  and parse_this f =
    let input = match f with
    | "stdin" -> stdin
    | _ -> open_in f in
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
  let handler s =
    begin
      Printf.printf "%% SZS Status ResourceOut\n";
      Unix.kill (Unix.getpid ()) Sys.sigterm
    end
  in
  ignore (Sys.signal Sys.sigalrm (Sys.Signal_handle handler));
  Unix.alarm (max 1 (int_of_float timeout))

(** setup output format and details *)
let setup_output params =
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
  (if params.param_print_sort && params.param_output_syntax = "debug"
    then T.pp_term_debug#sort true);
  (if params.param_print_all && params.param_output_syntax = "debug"
    then T.pp_term_debug#skip_lambdas false; T.pp_term_debug#skip_db false)

let () =
  (* parse arguments *)
  let params = parse_args () in
  let steps = if params.param_steps = 0
    then None else (Format.printf "%% run for %d steps@." params.param_steps;
                    Some params.param_steps)
  and timeout = if params.param_timeout = 0.
    then None else (Format.printf "%% run for %f s@." params.param_timeout;
                    ignore (setup_alarm params.param_timeout);
                    Some (Unix.gettimeofday() +. params.param_timeout -. 0.25)) in
  (* setup printing *)
  setup_output params;
  (* parse file *)
  let f = List.hd params.param_files in
  Printf.printf "%% process file %s\n" f;
  let clauses = parse_file ~recursive:true f in
  Printf.printf "%% parsed %d clauses\n" (List.length clauses);
  (* find the calculus *)
  let calculus = match params.param_calculus with
    | "superposition" -> Sup.superposition
    | "delayed" -> Delayed.delayed
    | x -> failwith ("unknown calculus "^x)
  in
  (* choose an ord now *)
  let constr = O.compose_constraints
    (heuristic_constraint clauses) calculus#constr in
  let so = O.make_ordering constr in
  let ord = match params.param_ord with
    | "rpo" -> new O.rpo so
    | "kbo" -> new O.kbo so
    | x -> failwith ("unknown ordering " ^ x)
  in
  Format.printf "%% signature: %a@." T.pp_signature ord#symbol_ordering#signature;
  (* preprocess clauses *)
  let num_clauses = List.length clauses in
  let clauses = calculus#preprocess ~ord clauses in
  Utils.debug 2 (lazy (Utils.sprintf "%% %d clauses processed into: @[<v>%a@]@."
                 num_clauses (Utils.pp_list ~sep:"" !C.pp_clause#pp) clauses));
  (* create a state, with clauses added to passive_set and axioms to set of support *)
  let state = PS.make_state ord (CQ.default_queues ~ord) in
  let state = {state with PS.passive_set=PS.add_passives state.PS.passive_set clauses} in
  let state = Sat.set_of_support ~calculus state calculus#axioms in
  (* saturate *)
  let state, result, num = Sat.given_clause ?steps ?timeout ~calculus state
  in
  Printf.printf "%% ===============================================\n";
  Printf.printf "%% done %d iterations\n" num;
  print_stats state;
  match result with
  | Sat.Unknown | Sat.Timeout -> Printf.printf "%% SZS status ResourceOut\n"
  | Sat.Error s -> Printf.printf "%% error occurred: %s\n" s
  | Sat.Sat ->
      Printf.printf "%% SZS status CounterSatisfiable\n";
      Utils.debug 1 (lazy (Utils.sprintf "%% saturated set: @[<v>%a@]@."
                     C.pp_bag state.PS.active_set.PS.active_clauses))
  | Sat.Unsat c ->
      (* print status then proof *)
      Printf.printf "%% SZS status Theorem\n";
      if params.param_proof
        then Format.printf "@.%% SZS output start CNFRefutation@.@[<v>%a@]@." !C.pp_proof#pp c.node
        else ()
