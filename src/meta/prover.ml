(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** {1 The meta-prover itself} *)

open Types
module T = Terms
module C = Clauses

(** {2 Type definitions} *)

module Logic = KB.Logic
  (** Alias for the datalog module *)

module LitMap = Map.Make(struct
  type t = Logic.literal
  let compare = Logic.compare_literal
end)

type t = {
  kb : KB.t;
  db : KB.Logic.db;
  ctx : context;
  mutable patterns : Pattern.t list;      (** patterns to match *)
  mutable clauses : hclause LitMap.t;     (** for reconstructing proofs *)
  mutable results : result list;
  mutable new_results : result list;      (** new results (transient) *)
  mutable new_patterns : Pattern.t list;  (** List of new patterns to match *)
} (** A meta-prover, reasoning at the theory/lemma level *)
and result =
  | Deduced of literal array * hclause list
  | Theory of string * term list
  | Expert of Experts.expert
  (** Feedback from the meta-prover *)

(** Convert the arguments into terms. Expected sorts are given and
    must match. *)
let extract_terms sorts args =
  assert (List.length sorts = List.length args);
  let terms = List.fold_left2
    (fun acc sort arg -> match arg with
      | `Symbol (KB.MPattern _)
      | `Symbol (KB.MString _) -> assert false
      | `Symbol (KB.MTerm t) ->
        assert (t.sort == sort);
        t::acc
      | `Var i -> (T.mk_var i sort) :: acc)
    [] sorts args in
  List.rev terms

(** Goal handler *)
let goal_handler prover lit =
  match Logic.open_literal lit with
  | KB.MString "pattern", (`Symbol (KB.MPattern p) :: args) ->
    let terms = extract_terms (snd p) args in
    (* partial instantiation (there may be constants in arguments) *)
    let t = Pattern.instantiate p terms in
    let new_pattern, new_args = Pattern.of_term t in
    prover.patterns <- new_pattern :: prover.patterns;
    prover.new_patterns <- new_pattern :: prover.new_patterns;
    ()
  | _ -> ()

(** Handler called on facts *)
let fact_handler prover lit =
  match Logic.open_literal lit with
  | KB.MString "pattern", (`Symbol (KB.MPattern p) :: args) ->
    if LitMap.mem lit prover.clauses
      then ()  (* a clause we already know *)
      else begin
        let terms = extract_terms (snd p) args in
        assert (not (List.exists T.is_var terms));
        (* a formula is true! *)
        let t = Pattern.instantiate p terms in
        let lits = [| Literals.mk_eq ~ord:prover.ctx.ctx_ord t T.true_term |] in
        (* explanations: find the ones which are in fact clauses. *)
        let premises = Logic.db_explain prover.db lit in
        let premises = FoUtils.list_flatmap
          (fun lit -> try [LitMap.find lit prover.clauses]
                      with Not_found -> [])
          premises in
        (* result: "conclusion because of premises" *)
        let result = Deduced (lits, premises) in
        prover.results <- result :: prover.results;
        prover.new_results <- result :: prover.new_results
      end
  | KB.MString "named", (`Symbol (KB.MString name) :: args) ->
    ()  (* log *)
  | _ -> ()  (* nothing to do *) 

(** Fact handler *)

(** Fresh meta-prover, using the given KB *)
let create ~ctx kb =
  let db = Logic.db_create () in
  let prover = { db;
    ctx;
    kb;
    patterns = [];
    clauses = LitMap.empty;
    results = [];
    new_results = [];
    new_patterns = [];
  } in
  (* add handlers *)
  Logic.db_subscribe_goal prover.db (goal_handler prover);
  Logic.db_subscribe_fact prover.db (KB.MString "pattern") (fact_handler prover);
  (* return prover *)
  prover

(** Get the current Knowledge Base of the prover *)
let get_kb prover = prover.kb

(* XXX: call calculus#preprocess on resulting clauses (CNF, etc.)?? *)

(** Match a clause against the patterns of the prover. [k] is called
    with every matching pattern and substitution. *)
let match_patterns prover lits k =
  List.iter
    (fun pattern ->
      let solutions = Pattern.matching pattern lits in
      Sequence.iter
        (fun args -> (* pattern(args) =_AC lits *)
          k pattern args)
        solutions)
    prover.patterns

(** Match the clause against patterns known to the KB. Matches
    are added to the Datalog engine, and if some theories and lemma
    are detected they are returned *)
let scan_clause prover hc =
  (* assert new facts (match [hc] against patterns) *)
  match_patterns prover hc.hclits
    (fun pattern args -> (* pattern(args) = hc *)
      let fact = KB.ThenPattern (pattern, args) in
      let lit = KB.fact_to_datalog fact in
      (* remember that [hc] is the explanation for this fact *)
      prover.clauses <- LitMap.add lit hc prover.clauses;
      (* add fact *)
      Logic.db_add_fact prover.db lit);
  (* get results *)
  let results = prover.new_results in
  prover.new_results <- [];
  results
    

(** Are there some new patterns? *)
let has_new_patterns prover = prover.new_patterns <> []

(** Scan the set of clauses for patterns that are new. This should
    be called on the active set every time [has_new_patterns prover]
    returns true. After this, [has_new_patterns prover] returns false
    at least until the next call to [scan_clause]. *)
let scan_set prover set =
  (* patterns to search for *)
  let patterns = prover.new_patterns in
  prover.new_patterns <- [];
  []  (* TODO: scan each clause in the set for patterns;
        then return new results *)

(** List of theories detected so far *)
let theories prover =
  let results = Sequence.of_list prover.results in
  Sequence.flatMap
    (function
      | Theory (th,args) -> Sequence.singleton (th,args)
      | _ -> Sequence.empty)
    results

(** Current list of experts that can be used *)
let experts prover =
  let results = Sequence.of_list prover.results in
  Sequence.flatMap
    (function
      | Expert ex -> Sequence.singleton ex
      | _ -> Sequence.empty)
    results

