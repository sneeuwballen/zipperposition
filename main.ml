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

(** get first file of command line arguments *)
let get_file () =
  let files = ref [] in
  Arg.parse [] (fun s -> files := s :: !files) "./prover file";
  match !files with
  | [] -> failwith "file required."
  | (x::_) -> x

(** parse given tptp file (TODO also parse include()s *)
let parse_file f =
  let input = match f with
    | "stdin" -> stdin
    | f -> open_in f in
  try
    let buf = Lexing.from_channel input in
    Parser_tptp.parse_file Lexer_tptp.token buf
  with _ as e -> close_in input; raise e
      
(* create a bag from the given clauses *)
let make_initial_bag clauses =
  let b = C.empty_bag in
  List.fold_left (fun b c -> fst (C.add_to_bag b c)) b clauses

let () =
  let f = get_file () in
  Printf.printf "# process file %s\n" f;
  let clauses, _ = parse_file f in
  Printf.printf "# parsed %d clauses\n" (List.length clauses);
  (* create a state *)
  let state = PS.make_state O.default CQ.default_queues in
  let state = {state with PS.passive_set=PS.add_passives state.PS.passive_set clauses} in
  (* print some stuff *)
  Pp.debug_state Format.std_formatter state
