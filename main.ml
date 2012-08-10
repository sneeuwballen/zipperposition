(* main file *)

module Utils = FoUtils
module Unif = FoUnif
module CQ = ClauseQueue
module PState = ProofState
module I = Index

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

(* parse a string in a clause list option *)
let parse_string s =
  let buf = Lexing.from_string s in
  try
    Some (fst (Parser_tptp.parse_file Lexer_tptp.token buf))
  with _ -> None

(* parse a string into a term option *)
let parse_term_str s =
  let buf = Lexing.from_string s in
  try
    Some (Parser_tptp.term Lexer_tptp.token buf)
  with _ -> None

(* gather all terms in list *)
let all_terms () =
  let l = ref [] in
  Terms.iter_terms (fun t -> l := t :: !l);
  !l

(* try to pairwise unify all terms *)
let unify_all_terms all_terms =
  let max_var = List.fold_left
    (fun max_var t -> max max_var (FoUtils.max_var (Terms.vars_of_term t)))
    0 all_terms in
  let alt_terms = List.map  (* renamed terms *)
    (fun t -> FoUtils.fresh_foterm max_var t)
    all_terms in
  let pairs = List.fold_left (* all combinations *)
    (fun pairs_ t ->
      List.append (List.map (fun t' -> (t, t')) alt_terms) pairs_)
    [] all_terms
  in
  List.iter
    (fun (t, t') ->
      Format.printf "  unify %a %a ... " Pp.pp_foterm t Pp.pp_foterm t';
      try
        let subst = FoUnif.unification t t' in
        Format.printf "success, %a@." Pp.pp_substitution subst
      with FoUnif.UnificationFailure _ ->
        Format.printf "failure.@.")
    pairs
      
(* create a bag from the given clauses *)
let make_initial_bag clauses =
  let b = Terms.empty_bag in
  List.fold_left (fun b c -> fst (Terms.add_to_bag c b)) b clauses

(* print an index *)
let print_index idx = 
  let print_dt_path path set =
    let l = I.ClauseSet.elements set in
    Format.printf "%s : @[<hov>%a@]@;"
      (I.FotermIndexable.string_of_path path)
      (Pp.pp_list ~sep:", " Pp.pp_clause_pos) l
  in
  Format.printf "index:@.root_index=@[<v 2>";
  I.DT.iter idx.I.root_index print_dt_path;
  Format.printf "@]@;subterm_index=@[<v 2>";
  I.DT.iter idx.I.subterm_index print_dt_path;
  Format.printf "@]@;"

let () =
  let f = get_file () in
  Printf.printf "# process file %s\n" f;
  let clauses, _ = parse_file f in
  let bag = List.fold_left (fun bag c -> fst (Terms.add_to_bag c bag))
    Terms.empty_bag clauses in
  Printf.printf "# parsed %d clauses\n" (List.length clauses);
  Format.printf "@[<v>%a@]@." Pp.pp_bag bag;
  let terms = all_terms () in
  Format.printf "@[<h>terms: %a@]@." (Pp.pp_list Pp.pp_foterm) terms;
  (* now for bag testing *)
  let bag = make_initial_bag clauses in
  Format.printf "clauses: %a@." Pp.pp_bag bag;
  (* and indexing *)
  let index = List.fold_left (fun i c -> I.index_clause i c)
    I.empty clauses in
  print_index index
