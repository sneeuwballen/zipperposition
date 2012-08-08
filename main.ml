(* main file *)

module Ord = Orderings
module Utils = FoUtils
module Unif = FoUnif

(* get first file of command line arguments *)
let get_file () =
  let files = ref [] in
  Arg.parse [] (fun s -> files := s :: !files) "./prover file";
  match !files with
  | [] -> failwith "file required."
  | (x::_) -> x


(* parse given tptp file *)
let parse_file f =
  let input = match f with
    | "stdin" -> stdin
    | f -> open_in f in
  try
    let buf = Lexing.from_channel input in
    Parser_tptp.parse_file Lexer_tptp.token buf
  with _ as e -> close_in input; raise e

let () =
  let f = get_file () in
  Printf.printf "# process file %s\n" f;
  let clauses, _ = parse_file f in
  let bag = List.fold_left (fun bag c -> fst (Terms.add_to_bag c bag))
    Terms.empty_bag clauses in
  Printf.printf "# parsed %d clauses\n" (List.length clauses);
  Format.printf "@[<v>%a@]@." Pp.pp_bag bag
