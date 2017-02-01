
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Tool to orient a serie of rewriting rules} *)

open Libzipperposition
open Libzipperposition_solving

module PT = STerm
module E = CCResult
module T = TypedSTerm
module Loc = ParseLocation

let files = ref []
let flag_print_signature = ref false
let flag_print_rules = ref false
let num_solutions = ref 1
let add_file f = files := f :: !files

let options =
  [ "--print-signature", Arg.Set flag_print_signature, "print the signature of the theory"
  ; "--print-rules", Arg.Set flag_print_rules, "print the rewrite rules"
  ; "--num", Arg.Set_int num_solutions, "number of solutions to print"
  ] @ Options.make ()
  |> List.sort Pervasives.compare
  |> Arg.align

type statement = RewriteRules.statement

let print_solution solution =
  Format.printf "solution: %a@." Lpo.Solution.pp solution

let print_signature signature =
  Format.printf "@[<2>signature: @[%a@]@]@."
    (CCFormat.seq ~sep:" "
       (fun out (a,b) -> Format.fprintf out "@[%a :@ %a@]" ID.pp a T.pp b))
    (ID.Map.to_seq signature)

(* given a list of files, parse them into pairs of terms, and
   make typed rules from those. returns the signature and
   the typed rules *)
let parse_files_into_rules files =
  let open E.Infix in
  E.fold_l
    (fun rules file ->
       begin match file with
         | "stdin" -> E.return stdin
         | _ ->
           begin try E.return (open_in file)
             with Sys_error msg ->
               let msg = Printf.sprintf "could not open file %s: %s" file msg in
               E.fail msg
           end
       end >>=
       RewriteRules.parse_file file >>=
       RewriteRules.rules_of_pairs >|= fun rules' ->
       List.rev_append rules' rules)
    []
    files

let parse_args () =
  let help_msg = "orient: finds orderings for rewriting rules" in
  Arg.parse options add_file help_msg;
  ()

let () =
  let open E.Infix in
  parse_args ();
  let res =
    (* parse rules *)
    parse_files_into_rules !files >|= fun rules ->
    if !flag_print_signature
    then (
      let signature = RewriteRules.signature (Sequence.of_list rules) in
      print_signature signature;
    );
    if !flag_print_rules
    then Format.printf "@[%a@]@." RewriteRules.print_rules rules;
    (* orient rules *)
    let constraints = Lpo.TypedSTerm.orient_lpo_list rules in
    let solutions = Lpo.solve_multiple constraints in
    (* get at most !num_solutions solutions *)
    let rec get_solutions solutions n = match solutions with
      | _ when n = 0 -> []
      | lazy LazyList.Nil -> []
      | lazy (LazyList.Cons (s,tl)) ->
        let tl = get_solutions tl (n-1) in
        s::tl
    in
    get_solutions solutions !num_solutions
  in
  match res with
    | E.Error msg ->
      print_endline msg
    | E.Ok [] ->
      print_endline "no solution for this set of rules"
    | E.Ok solutions ->
      List.iter print_solution solutions
