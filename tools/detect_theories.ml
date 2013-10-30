
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

(** {1 Check presence of theories in files} *)

open Logtk
open Logtk_meta

module HOT = HOTerm
module F = FOFormula
module A = Ast_tptp
module KB = MetaKB

let theory_files = ref []
let kb_files = ref []
let flag_print_kb = ref false
let flag_print_datalog = ref false

let add_theory f =
  theory_files := f :: !theory_files
let add_kb f =
  kb_files := f :: !kb_files

let options =
  [ "-debug", Arg.Int Util.set_debug, "debug level"
  ; "-theory", Arg.String add_theory, "use theory file"
  ; "-kb", Arg.String add_kb, "use KB file"
  ; "-print-kb", Arg.Set flag_print_kb, "print KB"
  ; "-print-datalog", Arg.Set flag_print_datalog, "print Datalog clauses"
  ]

(* parse the given theory files into a KB *)
let parse_kb kb_files theory_files =
  let kb = List.fold_left
    (fun kb file -> 
      Util.debug 3 "parse KB file %s" file;
      match MetaKB.restore file with
      | None -> kb
      | Some kb' -> MetaKB.union kb kb')
    MetaKB.empty kb_files
  in
  List.fold_left
    (fun kb file ->
      Util.debug 3 "parse theory file %s" file;
      let kb' = MetaKB.parse_theory_file file in
      MetaKB.union kb kb')
    kb theory_files

(* conversion to CNF of declarations *)
let to_cnf ?(ctx=Skolem.create ()) decls =
  let tyctx = TypeInference.Ctx.create ~base:true () in
  let seq = Sequence.flatMap
    (function
      | A.FOF(n,role,f,info)
      | A.TFF(n,role,f,info) ->
        (* type formula *)
        let f = TypeInference.FO.convert ~ctx:tyctx f in
        begin match role with
        | A.R_conjecture ->
          (* negate conjecture *)
          let clauses = Cnf.cnf_of ~ctx (F.mk_not f) in
          Sequence.of_list clauses
        | _ ->
          (* translate, keeping the same role *)
          let clauses = Cnf.cnf_of ~ctx f in
          Sequence.of_list clauses
        end
      | A.CNF(_,_,c,_) ->
        let c = TypeInference.FO.convert_clause ~ctx:tyctx c in
        Sequence.singleton c
      | _ -> Sequence.empty)
    decls
  in
  (* iterating again would change skolems, etc, which is bad *)
  Sequence.persistent seq

(* obtain the global CNF of all files *)
let parse_and_cnf files =
  let ctx = Skolem.create () in
  let clauses = List.map
    (fun file ->
      try
        let decls = Util_tptp.parse_file ~recursive:true file in
        let decls = to_cnf ~ctx decls in
        decls
      with Util_tptp.ParseError _ as e ->
        (* syntax error *)
        Printf.printf "%s\n" (Util_tptp.string_of_error e);
        exit 1)
    files
  in
  Sequence.concat (Sequence.of_list clauses)

(* print KB on stdout *)
let print_kb ~kb =
  Util.printf "%a\n" KB.pp kb;
  flush stdout;
  ()

(* detect theories in clauses *)
let detect_theories ~kb clauses =
  let mp = MetaProver.create ~kb () in
  Util.debug 4 "clauses: %a\n"
    (Util.pp_seq ~sep:"\n" MetaReasoner.pp_clause)
    (MetaReasoner.all_clauses (MetaProver.reasoner mp));
  (* event notifiers *)
  Signal.on (MetaProver.on_lemma mp)
    (function
      | KB.NewLemma (f,_) -> Util.printf "lemma: %a\n" F.pp f; true);
  Signal.on (MetaProver.on_theory mp)
    (function
      | KB.NewTheory (s,args,_) ->
        Util.printf "theory: %s(%a)\n" s (Util.pp_list HOT.pp) args;
        true);
  Signal.on (MetaProver.on_axiom mp)
    (function
      | KB.NewAxiom (s,args) ->
        Util.printf "axiom: %s(%a)\n" s (Util.pp_list HOT.pp) args;
        true);
  (* match clauses *)
  Sequence.iter
    (fun c ->
      let facts = MetaProver.match_clause mp c in
      MetaProver.add_literals mp (Sequence.of_list facts))
    clauses;
  mp

let main () =
  let files = ref [] in
  let add_file f = files := f :: !files in
  Arg.parse options add_file "detect_theories [options] [file1|stdin] file2...";
  (* set default *)
  (match !kb_files, !theory_files with
    | [], [] -> theory_files := ["data/builtin.theory"]
    | _ -> ());
  (if !files = [] then files := ["stdin"]);
  (* parse KB *)
  let kb = parse_kb !kb_files !theory_files in
  (if !flag_print_kb then print_kb ~kb);
  (* parse CNF formulas *)
  let clauses = parse_and_cnf !files in
  (* detect theories *)
  let mp = detect_theories ~kb clauses in
  (* print datalog? *)
  if !flag_print_datalog then
    let clauses = MetaReasoner.all_clauses (MetaProver.reasoner mp) in
    Sequence.iter (fun c -> Util.printf "  %a\n" MetaReasoner.pp_clause c) clauses;
  flush stdout;
  Util.debug 1 "success!";
  ()

let _ =
  main ()
