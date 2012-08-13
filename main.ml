(* main file *)

open Types

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
  Hashtbl.add ords "lpo" (new Orderings.lpo);
  Hashtbl.add ords "kbo" (new Orderings.kbo);
  Hashtbl.add ords "nrkbo" (new Orderings.nrkbo)

(** parameters for the main procedure *)
type parameters = {
  param_ord : ordering;
  param_verbose : bool;
  param_files : string list;
}

(** parse_args returns parameters *)
let parse_args () =
  (* parameters *)
  let ord = ref Orderings.default
  and verbose = ref false
  and file = ref "stdin" in
  (* argument functions *)
  let set_ord s = (* select ordering *)
    try ord := Hashtbl.find ords s
    with Not_found as e -> (Printf.printf "unknown ordering: %s\n" s; raise e)
  and set_file s = file := s in
  (* options list (TODO parse something about heuristics) *) 
  let options =
    [ ("-ord", Arg.String set_ord, "choose ordering (lpo,kbo,nrkbo)");
      ("-verbose", Arg.Set verbose, "verbose mode") ]
  in
  Arg.parse options set_file "solve problem in first file";
  (* return parameter structure *)
  { param_ord = !ord; param_verbose = !verbose; param_files = [!file] }

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

let () =
  (* parse arguments *)
  let params = parse_args () in
  let verbose = params.param_verbose in
  if verbose then Sat.set_debug true;
  (* parse file *)
  let f = List.hd params.param_files in
  Printf.printf "# process file %s\n" f;
  let clauses, _ = parse_file f in
  Printf.printf "# parsed %d clauses\n" (List.length clauses);
  (* create a state, with clauses added to passive_set *)
  let state = PS.make_state params.param_ord CQ.default_queues in
  let state = {state with PS.passive_set=PS.add_passives state.PS.passive_set clauses} in
  (* saturate *)
  let state, result = Sat.given_clause state in
  match result with
  | Sat.Sat -> Printf.printf "# SZS status CounterSatisfiable\n"
  | Sat.Unsat _ -> Printf.printf "# SZS status Theorem\n"
  | Sat.Unknown | Sat.Timeout -> Printf.printf "# SZS status ResourceOut\n"
  | Sat.Error s -> Printf.printf "error occurred: %s\n" s
