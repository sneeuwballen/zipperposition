
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
let prof_scan_set = Util.mk_profiler "meta.scan_set"

module T = Term
module C = Clause

type result =
  | Deduced of Term.t * Clause.t list
  | Theory of string * Term.t list
  | Expert of Experts.t
  (** Feedback from the meta-prover *)

module Logic = MetaReasoner.Logic

module LitMap = Map.Make(struct
  type t = Logic.literal
  let compare = Pervasives.compare
end)

type t = {
  prover : MetaProver.t;    (* real meta-prover *)
  mutable ctx : Ctx.t;
  mutable clauses : Clause.t LitMap.t;     (** for reconstructing proofs *)
  mutable theories : (string * Term.t list) list;
  mutable experts : Experts.t list;
  mutable results : result list;
  mutable new_results : result list;  (* recent results *)
  mutable new_patterns : MetaPattern.t list;  (** List of new patterns to match *)
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
  p.clauses <- LitMap.add lit c p.clauses

(* find which clauses "explain" this fact *)
let find_premises p lit =
  let lits = MetaReasoner.explain (MetaProver.reasoner p.prover) lit in
  Util.list_fmap
    (fun lit ->
      try Some (LitMap.find lit p.clauses)
      with Not_found -> None)
    lits

(* new results have been handled, clean up *)
let flush_new_results p =
  p.new_results <- []

let create ~ctx kb =
  let p = {
    prover = MetaProver.create ();
    ctx;
    clauses = LitMap.empty;
    theories = [];
    experts = [];
    results = [];
    new_results = [];
    new_patterns = [];
  } in
  MetaProver.add_kb p.prover kb;
  (* hook events to p.results *)
  Signal.on (MetaProver.on_theory p.prover)
    (function | MetaKB.NewTheory (name, args) ->
      add_new_result p (Theory (name, args));
      true);
  Signal.on (MetaProver.on_lemma p.prover)
    (function | MetaKB.NewLemma (f, lit) ->
      let premises = find_premises p lit in
      add_new_result p (Deduced (f, premises));
      true);
  p

let update_ctx ~ctx prover =
  prover.ctx <- ctx

let has_new_patterns p =
  match p.new_patterns with
  | [] -> false
  | _::_ -> true

(* TODO: handle ground convergent systems in Meta Prover, e.g. using 
    a specific file... *)

let scan_clause p c =
  Util.debug 3 "%% meta-prover: scan %a" C.pp_debug c;
  Util.enter_prof prof_scan_clause;
  (* match [c] against patterns *)
  let f = Literal.term_of_lits c.C.hclits in
  let lits = MetaProver.match_formula p.prover f in
  List.iter (fun lit -> map_lit_to_clause p lit c) lits;
  MetaProver.add_literals p.prover (Sequence.of_list lits);
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
      let f = Literal.term_of_lits c.C.hclits in
      let lits = MetaProver.match_formula p.prover f in
      List.iter (fun lit -> map_lit_to_clause p lit c) lits;
      MetaProver.add_literals p.prover (Sequence.of_list lits));
  (* get results *)
  let results = p.new_results in
  flush_new_results p;
  Util.exit_prof prof_scan_set;
  results

let theories p = Sequence.of_list p.theories

let experts p = Sequence.of_list p.experts

let results p = Sequence.of_list p.results

let reasoner p = MetaProver.reasoner p.prover

let kb p = MetaProver.kb p.prover

let parse_theory_file p filename =
  MetaProver.parse_theory_file p.prover filename

let pp_result buf r = match r with
  | Deduced (f, _) -> Printf.bprintf buf "deduced %a" T.pp f
  | Theory (n, args) -> Printf.bprintf buf "theory %s(%a)" n (Util.pp_list T.pp) args
  | Expert e -> Printf.bprintf buf "expert %a" Experts.pp e
