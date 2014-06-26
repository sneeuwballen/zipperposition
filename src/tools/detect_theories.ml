
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
module E = CCError

let files = ref []
let theory_files = ref []
let flag_print_theory = ref false
let flag_print_cnf = ref false
let flag_print_signature = ref false

let add_file f = files := f :: !files
let add_theory f = theory_files := f :: !theory_files

let options =
  [ "-theory", Arg.String add_theory, "use given theory file"
  ; "-print-theory", Arg.Set flag_print_theory, "print the whole theory"
  ; "-print-cnf", Arg.Set flag_print_cnf, "print the clausal form of input files"
  ; "-print-signature", Arg.Set flag_print_signature, "print initial theory signature"
  ] @ Options.global_opts

(* parse the given theory files into the prover *)
let parse_files prover files =
  E.fold_l
    (fun p file -> E.map fst (Prover.parse_file p file))
    prover files

let to_cnf ~signature decls =
  E.(
    Util_tptp.infer_types (`sign signature) decls
    >>= fun (signature, decls) ->
    let _sign, clauses = Util_tptp.to_cnf signature decls in
    let seq k =
      let a = object
        inherit [unit] Ast_tptp.Typed.visitor
        method clause () _role c = k c
      end in
      Sequence.fold a#visit () clauses
    in
    return seq
  )

let parse_and_cnf ?(signature=Signature.TPTP.base) files =
  let q = Queue.create () in
  let res = E.(
    fold_l
    (fun () file ->
      Util.debug 1 "parse input file %s" file;
      (* parse *)
      Util_tptp.parse_file ~recursive:true file
      >>= fun decls ->
      Util.debug 3 "parsed %d declarations..." (Sequence.length decls);
      (* CNF *)
      to_cnf ~signature decls
      >>= fun clauses ->
      Util.debug 3 "obtained %d clauses..." (Sequence.length clauses);
      (* convert clauses into Encoding.foclause *)
      let clauses = Sequence.map Encoding.foclause_of_clause clauses in
      Queue.add clauses q;
      return ()
    ) () files
  )
  in
  match res with
  | `Ok () ->
      E.return (Sequence.of_queue q |> Sequence.flatten)
  | `Error msg ->
      E.fail msg

(* print content of the reasoner *)
let print_theory r =
  Reasoner.Seq.to_seq r
    |> Util.printf "theory:\n  %a\n" (Util.pp_seq ~sep:"\n  " Reasoner.Clause.pp);
  ()

let print_clauses c =
  Util.printf "clauses:\n  %a\n"
    (Util.pp_seq ~sep:"\n  " (Encoding.pp_clause FOTerm.pp)) c

let print_signature signature =
  Util.printf "signature:\n  %a\n"
    (Util.pp_seq ~sep:"\n  " (Util.pp_pair ~sep:" : " Symbol.pp Type.pp))
    (Signature.Seq.to_seq signature)

let pp_theory_axiom buf (name, _, t) =
  Printf.bprintf buf "%s %a" name HOT.pp t

let pp_rewrite_system buf l =
  Printf.bprintf buf "rewrite system\n    ";
  Util.pp_list ~sep:"\n    " (Util.pp_pair ~sep:" --> " FOTerm.pp FOTerm.pp) buf l

let pp_pre_rewrite_system buf l =
  HORewriting.pp buf l

type result = {
  lemmas : Plugin.foclause Sequence.t;
  theories : (string * Type.t list * HOT.t) Sequence.t;
  axioms : (string * Type.t list * HOT.t) Sequence.t;
  rewrite : (FOTerm.t * FOTerm.t) list Sequence.t;
  pre_rewrite : HORewriting.t Sequence.t;
}

(* detect theories in clauses *)
let detect_theories prover clauses =
  let facts = clauses
    |> Sequence.map Plugin.holds#to_fact
    |> Sequence.map Reasoner.Clause.fact
  in
  (* add clauses (ignore prover) *)
  let _, consequences = Prover.Seq.of_seq prover facts in
  let consequence_terms = Sequence.map fst consequences in
  (* filter theories, axioms, lemmas... *)
  let theories = Sequence.fmap Plugin.theory#of_fact consequence_terms
  and lemmas = Sequence.fmap Plugin.lemma#of_fact consequence_terms
  and axioms = Sequence.fmap Plugin.axiom#of_fact consequence_terms
  and rewrite = Sequence.fmap Plugin.rewrite#of_fact consequence_terms
  and pre_rewrite = Sequence.fmap Plugin.pre_rewrite#of_fact consequence_terms
  in
  { theories; lemmas; axioms; rewrite; pre_rewrite; }

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
  if !flag_print_signature then print_signature (Prover.signature prover);
  let res = E.(
    parse_files prover !theory_files
    >>= fun prover ->
    Util.debug 3 "theory files parsed";
    if !flag_print_theory then print_theory (Prover.reasoner prover);
    if !flag_print_signature then print_signature (Prover.signature prover);
    (* parse CNF formulas *)
    parse_and_cnf !files
    >>= fun clauses ->
    Util.debug 3 "input files parsed and translated to CNF";
    if !flag_print_cnf then print_clauses clauses;
    let results = detect_theories prover clauses in
    Util.debug 3 "theory detection done";
    E.return results
  ) in
  match res with
  | `Error msg ->
      Util.debug 0 "error: %s" msg; exit 1
  | `Ok {theories; lemmas; axioms; rewrite; pre_rewrite; } ->
      Util.debug 1 "success!";
      Util.printf "axioms:\n  %a\n"
        (Util.pp_seq ~sep:"\n  " pp_theory_axiom) axioms;
      Util.printf "theories:\n  %a\n"
        (Util.pp_seq ~sep:"\n  " pp_theory_axiom) theories;
      Util.printf "lemmas:\n  %a\n"
        (Util.pp_seq ~sep:"\n  " (Encoding.pp_clause FOTerm.pp)) lemmas;
      Util.printf "rewrite systems:\n  %a\n"
        (Util.pp_seq ~sep:"\n  " pp_rewrite_system) rewrite;
      Util.printf "pre-rewrite systems:\n  %a\n"
        (Util.pp_seq ~sep:"\n  " pp_pre_rewrite_system) pre_rewrite;
      ()

let _ =
  try
    main ()
  with Type.Error s ->
    Util.printf "%s\n" s;
    exit 1
