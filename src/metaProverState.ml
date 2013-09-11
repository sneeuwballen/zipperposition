
(*
Zipperposition: a functional superposition prover for prototyping
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

(** {1 Meta Prover for zipperposition} *)

open Logtk

let prof_scan_clause = Util.mk_profiler "meta.scan_clause"
let prof_scan_formula = Util.mk_profiler "meta.scan_formula"
let prof_scan_set = Util.mk_profiler "meta.scan_set"

module T = Term
module F = Formula
module PF = PFormula
module C = Clause
module M = Logtk_meta
module Lit = Literal
module Lits = Literal.Arr

type result =
  | Deduced of PFormula.t * source list
  | Theory of string * Term.t list
  | Expert of Experts.t
  (** Feedback from the meta-prover *)

and source =
  | FromClause of Clause.t
  | FromForm of PFormula.t

module Logic = M.MetaReasoner.Logic

module LitMap = Map.Make(struct
  type t = Logic.literal
  let compare = Pervasives.compare
end)

type t = {
  prover : M.MetaProver.t;    (* real meta-prover *)
  mutable sources : source LitMap.t;     (** for reconstructing proofs *)
  mutable theories : (string * Term.t list) list;
  mutable experts : Experts.t list;
  mutable results : result list;
  mutable new_results : result list;  (* recent results *)
  mutable new_patterns : M.MetaPattern.t list;  (** List of new patterns to match *)
}

(* add a new result *)
let add_new_result p res =
  begin match res with
  | Theory (s, args) -> p.theories <- (s, args) :: p.theories
  | Expert e -> p.experts <- e :: p.experts
  | Deduced _ -> ()
  end;
  p.results <- res :: p.results;
  p.new_results <- res :: p.new_results;
  ()

(* remember that this literal is explained by this clause *)
let map_lit_to_clause p lit c =
  p.sources <- LitMap.add lit (FromClause c) p.sources

let map_lit_to_form p lit f =
  p.sources <- LitMap.add lit (FromForm f) p.sources

(* find which sources "explain" this fact *)
let find_premises p lit =
  let lits = M.MetaReasoner.explain (M.MetaProver.reasoner p.prover) lit in
  Util.list_fmap
    (fun lit ->
      try Some (LitMap.find lit p.sources)
      with Not_found -> None)
    lits

let proof_of_source = function
  | FromClause c -> c.C.hcproof
  | FromForm pf -> pf.PF.proof

(* new results have been handled, clean up *)
let flush_new_results p =
  p.new_results <- []

let create ?(kb=M.MetaKB.empty) () =
  let p = {
    prover = M.MetaProver.create ~kb ();
    sources = LitMap.empty;
    theories = [];
    experts = [];
    results = [];
    new_results = [];
    new_patterns = [];
  } in
  (* hook events to p.results *)
  M.Signal.on (M.MetaProver.on_theory p.prover)
    (function | M.MetaKB.NewTheory (name, args) ->
      add_new_result p (Theory (name, args));
      true);
  M.Signal.on (M.MetaProver.on_lemma p.prover)
    (function | M.MetaKB.NewLemma (f, lit) ->
      let premises = find_premises p lit in
      let proofs = List.map proof_of_source premises in
      let proof = Proof.mk_f_step f ~rule:"lemma" proofs in
      let pf = PF.create f proof in
      add_new_result p (Deduced (pf, premises));
      true);
  p

let has_new_patterns p =
  match p.new_patterns with
  | [] -> false
  | _::_ -> true

(* TODO: handle ground convergent systems in Meta Prover, e.g. using 
    a specific file... *)

let scan_formula p pf =
  Util.debug 3 "meta-prover: scan %a" PF.pp pf;
  let f = pf.PF.form in
  Util.enter_prof prof_scan_formula;
  let lits = M.MetaProver.match_formula p.prover f in
  List.iter (fun lit -> map_lit_to_form p lit pf) lits;
  M.MetaProver.add_literals p.prover (Sequence.of_list lits);
  (* get results *)
  let results = p.new_results in
  flush_new_results p;
  Util.exit_prof prof_scan_formula;
  results

let scan_clause p c =
  Util.enter_prof prof_scan_clause;
  (* match [c] against patterns *)
  let f = Lits.to_form c.C.hclits in
  let lits = M.MetaProver.match_formula p.prover f in
  List.iter (fun lit -> map_lit_to_clause p lit c) lits;
  M.MetaProver.add_literals p.prover (Sequence.of_list lits);
  (* get results *)
  let results = p.new_results in
  flush_new_results p;
  Util.exit_prof prof_scan_clause;
  results

let scan_set p set =
  Util.enter_prof prof_scan_set;
  C.CSet.iter set
    (fun c ->
      (* match [c] against patterns *)
      let f = Lits.to_form c.C.hclits in
      let lits = M.MetaProver.match_formula p.prover f in
      List.iter (fun lit -> map_lit_to_clause p lit c) lits;
      M.MetaProver.add_literals p.prover (Sequence.of_list lits));
  (* get results *)
  let results = p.new_results in
  flush_new_results p;
  Util.exit_prof prof_scan_set;
  results

let theories p = Sequence.of_list p.theories

let experts p = Sequence.of_list p.experts

let results p = Sequence.of_list p.results

let reasoner p = M.MetaProver.reasoner p.prover

let kb p = M.MetaProver.kb p.prover

let add_kb p kb = M.MetaProver.add_kb p.prover kb

let parse_theory_file p filename =
  Util.debug 1 "parse theory file %s" filename;
  M.MetaProver.parse_theory_file p.prover filename

let parse_kb_file p filename =
  Util.debug 1 "read KB file %s" filename;
  M.MetaProver.restore_kb p.prover filename

let save_kb_file p filename =
  Util.debug 1 "save KB to file %s" filename;
  M.MetaProver.save_kb p.prover filename

let pp_result buf r = match r with
  | Deduced (f, _) -> Printf.bprintf buf "deduced %a" PF.pp f
  | Theory (n, args) -> Printf.bprintf buf "theory %s(%a)" n (Util.pp_list T.pp) args
  | Expert e -> Printf.bprintf buf "expert %a" Experts.pp e

let pp_theory buf (name, args) =
  match args with
  | [] -> Printf.bprintf buf "theory %s" name
  | _::_ -> Printf.bprintf buf "theory %s(%a)" name (Util.pp_list T.pp) args
