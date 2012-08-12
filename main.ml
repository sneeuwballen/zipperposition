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

let ord = ref Orderings.default
let set_ord s = (* select ordering *)
  try
    ord := Hashtbl.find ords s
  with
    Not_found -> Printf.printf "unknown ordering: %s\n" s

let options =
  [ ("-ord", Arg.String set_ord, "choose ordering (lpo,kbo,nrkbo)") ]
  (* TODO parse something about heuristics *)
let args_fun s = ()

let parse_args () =
  Arg.parse options args_fun "solve problem in first file";
  PS.make_state !ord CQ.default_queues

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
  (* create a state, with clauses added to passive_set and active_set *)
  let state = PS.make_state O.default CQ.default_queues in
  let state = {state with PS.passive_set=PS.add_passives state.PS.passive_set clauses} in
  let state = {state with PS.active_set=PS.add_actives state.PS.active_set clauses} in
  (* print some stuff *)
  Pp.debug_state Format.std_formatter state
