
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
open Logtk_parsers
open Logtk_meta

module HOT = HOTerm
module F = Formula.FO
module A = Ast_ho
module E = Monad.Err

let files = ref []
let theory_files = ref []
let flag_print_kb = ref false
let flag_print_datalog = ref false

let add_file f = files := f :: !files
let add_theory f = theory_files := f :: !theory_files

let options =
  [ "-theory", Arg.String add_theory, "use given theory file"
  ; "-print-theory", Arg.Set flag_print_kb, "print the whole theory"
  ] @ Options.global_opts

(* parse the given theory files into the prover *)
let parse_files prover files =
  E.fold_l files (E.return prover)
    (fun p file ->
      E.map (Prover.parse_file p file) fst)

let to_cnf ~signature decls =
  let signature, decls = Util_tptp.infer_types (`sign signature) decls in
  let _, decls = Util_tptp.to_cnf signature decls in
  fun k ->
    let a = object
      inherit [unit] Ast_tptp.Typed.visitor
      method cnf () c = k c
    end in
    Sequence.fold a#visit () decls

let parse_and_cnf ?(signature=Signature.TPTP.base) files =
  Sequence.flatMap
    (fun file ->
      (* parse *)
      let decls = Util_tptp.parse_file ~recursive:true file in
      (* CNF *)
      let clauses = to_cnf ~signature decls in
      (* convert clauses into Encoding.foclause *)
      let clauses = Sequence.map Encoding.foclause_of_clause clauses in
      let clauses = Sequence.persistent clauses in
      clauses
    ) (Sequence.of_list files)

(* print content of the reasoner *)
let print_theory r =
  Reasoner.Seq.to_seq r
    |> Util.printf "theory: %a\n" (Util.pp_seq ~sep:"\n" Reasoner.Clause.pp);
  ()

(* detect theories in clauses *)
let detect_theories prover clauses =
  let facts = Sequence.map Plugin.holds#to_fact clauses in
  let facts = Sequence.map Reasoner.Clause.fact facts in
  (* add clauses (ignore prover) *)
  let _, consequences = Prover.Seq.of_seq prover facts in
  let consequence_terms = Sequence.map fst consequences in
  (* filter theories, axioms, lemmas... *)
  let theories = Sequence.fmap Plugin.theory#of_fact consequence_terms
  and lemmas = Sequence.fmap Plugin.lemma#of_fact consequence_terms
  and axioms = Sequence.fmap Plugin.axiom#of_fact consequence_terms in
  theories, lemmas, axioms

let main () =
  Arg.parse options add_file "detect_theories [options] (file1|stdin) [file2...]";
  (* set default *)
  begin match !theory_files with
    | [] -> theory_files := ["data/builtin.theory"]
    | _ -> ()
  end;
  if !files = [] then files := ["stdin"];
  (* parse theory files *)
  let prover = Prover.empty in
  let res = E.(
    parse_files prover !theory_files >>= fun prover ->
    if !flag_print_kb then print_theory (Prover.reasoner prover);
    (* parse CNF formulas *)
    E.guard (fun () -> parse_and_cnf !files) >>= fun clauses ->
    let theories, lemmas, axioms = detect_theories prover clauses in
    E.return (theories, lemmas, axioms)
  ) in
  match res with
  | E.Error msg ->
      Util.debug 0 "error: %s" msg; exit 1
  | E.Ok (theories, lemmas, axioms) ->
      Util.debug 1 "success!";
      Util.debug 1 "axioms:\n  %a\n"
        (Util.pp_seq ~sep:"\n  " (Util.pp_pair Symbol.pp HOT.pp)) axioms;
      Util.debug 0 "theories:\n  %a\n"
        (Util.pp_seq ~sep:"\n  " (Util.pp_pair Symbol.pp HOT.pp)) theories;
      Util.debug 0 "lemmas:\n  %a\n"
        (Util.pp_seq ~sep:"\n  " (Encoding.pp_clause FOTerm.pp)) lemmas;
      ()

let _ =
  try
    main ()
  with Type.Error s ->
    Util.printf "%s\n" s;
    exit 1
