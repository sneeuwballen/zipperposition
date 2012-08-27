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
  (* function used to compute a symbol ordering *)
  let get_so clauses = heuristic_ordering clauses in
  Hashtbl.add ords "rpo" (fun clauses -> new O.rpo (get_so clauses));
  Hashtbl.add ords "kbo" (fun clauses -> new O.kbo (get_so clauses));

(** parameters for the main procedure *)
type parameters = {
  param_ord : clause list -> ordering;
  param_steps : int;
  param_timeout : float;
  param_files : string list;
  param_proof : bool;
  param_debug_proof : bool;
}

(** parse_args returns parameters *)
let parse_args () =
  (* parameters *)
  let ord = ref (fun clauses -> new O.rpo (heuristic_ordering clauses))
  and steps = ref 0
  and timeout = ref 0.
  and proof = ref true
  and debug_proof = ref false
  and file = ref "stdin" in
  (* argument functions *)
  let set_ord s = (* select ordering *)
    try ord := Hashtbl.find ords s
    with Not_found as e -> (Printf.printf "unknown ordering: %s\n" s; raise e)
  and set_file s = file := s in
  (* options list (TODO parse something about heuristics) *) 
  let options =
    [ ("-ord", Arg.String set_ord, "choose ordering (rpo,kbo)");
      ("-debug", Arg.Int Utils.set_debug, "debug level");
      ("-steps", Arg.Set_int steps, "verbose mode");
      ("-profile", Arg.Set HExtlib.profiling_enabled, "enable profile");
      ("-timeout", Arg.Set_float timeout, "verbose mode");
      ("-noproof", Arg.Clear proof, "disable proof printing");
      ("-debug_proof", Arg.Set debug_proof, "print a debug (detailed) proof");
    ]
  in
  Arg.parse options set_file "solve problem in first file";
  (* return parameter structure *)
  { param_ord = !ord; param_steps = !steps; param_timeout = !timeout; param_files = [!file];
    param_proof = !proof; param_debug_proof = !debug_proof; }

(** parse given tptp file (TODO also parse include()s *)
let parse_file ~recursive f =
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
    | _ -> open_in f in
    try
      let buf = Lexing.from_channel input in
      Const.cur_filename := f;
      Parser_tptp.parse_file Lexer_tptp.token buf
    with _ as e -> close_in input; raise e
  in aux [f] []

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
  (* parse file *)
  let f = List.hd params.param_files in
  Printf.printf "%% process file %s\n" f;
  let clauses = parse_file ~recursive:true f in
  Printf.printf "%% parsed %d clauses\n" (List.length clauses);
  (* choose an ord now *)
  let ord = params.param_ord clauses in  (* compute precedence using clauses *)
  Format.printf "%% signature: %a@." T.pp_signature ord#symbol_ordering#signature;
  let clauses = List.map (C.reord_clause ~ord) clauses in
  Utils.debug 2 (lazy (Utils.sprintf "clauses: @[<v>%a@]@."
                 (Utils.pp_list ~sep:"" (C.pp_clause ~sort:false)) clauses));
  (* create a state, with clauses added to passive_set *)
  let state = PS.make_state ord (CQ.default_queues ~ord) in
  let state = {state with PS.passive_set=PS.add_passives state.PS.passive_set clauses} in
  (* saturate *)
  let state, result, num = Sat.given_clause ?steps ?timeout state in
  Printf.printf "%% ===============================================\n";
  Printf.printf "%% done %d iterations\n" num;
  print_stats state;
  match result with
  | Sat.Unknown | Sat.Timeout -> Printf.printf "%% SZS status ResourceOut\n"
  | Sat.Error s -> Printf.printf "%% error occurred: %s\n" s
  | Sat.Sat ->
      Printf.printf "%% SZS status CounterSatisfiable\n";
      if Utils.debug_level () > 1 then
        Format.printf "%% saturated set: @[<v>%a@]@." C.pp_bag state.PS.active_set.PS.active_clauses
  | Sat.Unsat c ->
      (* print status then proof *)
      Printf.printf "%% SZS status Theorem\n";
      (if params.param_proof then
        Format.printf "@.%% SZS output start CNFRefutation@.@[<v>%a@]@." C.pp_tstp_proof c.node);
      (if params.param_debug_proof then
        Format.printf "@.%% debug proof: @.@[<v>%a@]@." C.pp_proof_rec c.node)
