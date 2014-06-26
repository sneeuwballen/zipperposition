
(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Tool to orient a serie of rewriting rules} *)

open Logtk
open Logtk_solving

module PT = PrologTerm
module E = Monad.Err
module FOT = FOTerm
module Loc = ParseLocation

let files = ref []
let flag_print_signature = ref false
let flag_print_rules = ref false
let num_solutions = ref 1
let add_file f = files := f :: !files

let options =
  [ "-print-signature", Arg.Set flag_print_signature, "print the signature of the theory"
  ; "-print-rules", Arg.Set flag_print_rules, "print the rewrite rules"
  ; "-num", Arg.Set_int num_solutions, "number of solutions to print"
  ] @ Options.global_opts

type statement = RewriteRules.statement

let print_solution solution =
  Util.printf "solution: %a\n" Lpo.Solution.pp solution

let print_signature signature =
  Util.printf "signature:\n  %a\n"
    (Util.pp_seq ~sep:"\n  " (Util.pp_pair ~sep:" : " Symbol.pp Type.pp))
    (Signature.Seq.to_seq signature)

(* given a list of files, parse them into pairs of terms, and
   make typed rules from those. returns the signature and
   the typed rules *)
let parse_files_into_rules files =
  E.(fold
      (Sequence.of_list files)
      (E.return (Signature.empty, []))
      (fun (signature,rules) file ->
        begin match file with
          | "stdin" -> E.return stdin
          | _ ->
              begin try E.return (open_in file)
              with Sys_error msg ->
                let msg = Printf.sprintf "could not open file %s: %s" file msg in
                Monad.Err.fail msg
              end
        end >>=
        RewriteRules.parse_file file >>=
        RewriteRules.rules_of_pairs signature >>= fun (signature,rules') ->
        E.return (signature, List.rev_append rules' rules)
      ))

let parse_args () =
  let help_msg = "orient: finds orderings for rewriting rules" in
  Arg.parse options add_file help_msg;
  ()

let () =
  parse_args ();
  let res = E.(
    (* parse rules *)
    parse_files_into_rules !files >>= fun (signature,rules) ->
    if !flag_print_signature
      then print_signature signature;
    if !flag_print_rules
      then RewriteRules.print_rules stdout rules;
    (* orient rules *)
    let constraints = Lpo.FO.orient_lpo_list rules in
    let solutions = Lpo.solve_multiple constraints in
    (* get at most !num_solutions solutions *)
    let rec get_solutions solutions n = match solutions with
      | _ when n = 0 -> E.return []
      | lazy LazyList.Nil -> E.return []
      | lazy (LazyList.Cons (s,tl)) ->
          get_solutions tl (n-1) >>= fun tl ->
          E.return (s::tl)
    in
    get_solutions solutions !num_solutions
  ) in
  match res with
  | E.Error msg ->
      print_endline msg
  | E.Ok [] ->
      print_endline "no solution for this set of rules"
  | E.Ok solutions ->
      List.iter print_solution solutions
