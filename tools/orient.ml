
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
open Logtk_parsers
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

let rule_arrow = Symbol.of_string "-->"

type statement =
  | Rule of PT.t * PT.t
  | Type of string * PT.t

(* parse file into rewrite rules *)
let parse_file filename ic =
  let lexbuf = Lexing.from_channel ic in
  Loc.set_file lexbuf filename;
  try
    (* parse declarations *)
    let terms = Parse_ho.parse_decls Lex_ho.token lexbuf in
    (* keep the facts of the form  a --> b *)
    let rules = Util.list_fmap
      (function
        | Ast_ho.Clause (t, []) ->
            begin match PT.view t with
            | PT.App ({PT.term=PT.Const s}, [l;r]) when Symbol.eq s rule_arrow ->
                Some (Rule (l, r))
            | _ -> None
            end
        | Ast_ho.Type (s, ty) -> Some (Type (s,ty))
        | _ -> None)
      terms
    in
    E.return rules
  with
  | Parse_ho.Error ->
    let msg = Util.sprintf "parse error at %a" Loc.pp (Loc.of_lexbuf lexbuf) in
    E.fail msg

let print_rule buf (l,r) =
  Printf.bprintf buf "%a --> %a" FOT.pp l FOT.pp r

let print_rules rules =
  Util.printf "rules:\n  %a\n"
    (Util.pp_list ~sep:"\n  " print_rule) rules

let print_solution solution =
  Util.printf "solution: %a\n" Lpo.Solution.pp solution

let print_signature signature =
  Util.printf "signature:\n  %a\n"
    (Util.pp_seq ~sep:"\n  " (Util.pp_pair ~sep:" : " Symbol.pp Type.pp))
    (Signature.Seq.to_seq signature)

(* list of pairs of terms --> list of pairs of FOTerms *)
let rules_of_pairs signature pairs =
  try
    let ctx = TypeInference.Ctx.create signature in
    (* infer types *)
    let pairs = Util.list_fmap
      (function
        | Rule (l,r) ->
          let ty_l, l' = TypeInference.FO.infer ctx l in
          let ty_r, r' = TypeInference.FO.infer ctx r in
          TypeInference.Ctx.constrain_type_type ctx ty_l ty_r;
          TypeInference.Ctx.exit_scope ctx;
          Some (fun ctx -> l' ctx, r' ctx)
        | Type (s, ty) ->
          (* declare the type *)
          begin match TypeInference.Ctx.ty_of_prolog ctx ty with
          | None -> ()
          | Some ty -> TypeInference.Ctx.declare ctx (Symbol.of_string s) ty
          end; None
      ) pairs
    in
    let pairs = TypeInference.Closure.seq pairs in
    TypeInference.Ctx.generalize ctx;
    let signature = TypeInference.Ctx.to_signature ctx in
    (* extract typed rules and signature *)
    let rules = pairs ctx in
    E.return (signature,rules)
  with Type.Error msg ->
    E.fail msg

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
        parse_file file >>=
        rules_of_pairs signature >>= fun (signature,rules') ->
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
      then print_rules rules;
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
