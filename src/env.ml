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

(** {1 Global environment for an instance of the prover} *)

open Basic
open Symbols

module C = Clauses
module Utils = FoUtils

type binary_inf_rule = ProofState.active_set -> clause -> hclause list
  (** binary inferences. An inference returns a list of conclusions *)

type unary_inf_rule = hclause -> hclause list
  (** unary infererences *)

type t = {
  mutable params : Basic.parameters;
  mutable ctx : Basic.context;

  mutable binary_rules : (string * binary_inf_rule) list;
    (** the binary inference rules *)
  
  mutable unary_rules : (string * unary_inf_rule) list;
    (** the unary inference rules *)
  
  mutable basic_simplify : hclause -> hclause;
    (** how to simplify a clause *)
  
  mutable rw_simplify : ProofState.simpl_set -> hclause -> hclause;
    (** how to simplify a clause w.r.t a set of unit clauses *)
  
  mutable active_simplify : ProofState.active_set -> hclause -> hclause;
    (** how to simplify a clause w.r.t an active set of clauses *)

  mutable backward_simplify : ProofState.active_set -> hclause -> Clauses.CSet.t;
    (** backward simplification by a unit clause. It returns a set of
        active clauses that can potentially be simplified by the given clause *)

  mutable redundant : ProofState.active_set -> hclause -> bool;
    (** check whether the clause is redundant w.r.t the set *)

  mutable backward_redundant : ProofState.active_set -> hclause -> hclause list;
    (** find redundant clauses in set w.r.t the clause *)

  mutable list_simplify : hclause -> hclause list;
    (** how to simplify a clause into a (possibly empty) list
        of clauses. This subsumes the notion of trivial clauses (that
        are simplified into the empty list of clauses) *)

  mutable is_trivial : hclause -> bool;
    (** single test to detect trivial clauses *)

  mutable axioms : hclause list;
    (** a list of axioms to add to the problem *)

  mutable mk_constr : (hclause list -> precedence_constraint list) list;
    (** How to build constraints from a list of clauses *)

  mutable constr : precedence_constraint list;
    (** some constraints on the precedence *)

  mutable preprocess : ctx:context -> hclause list -> hclause list;
    (** how to preprocess the initial list of clauses *)

  mutable state : ProofState.state;
    (** Proof state *)

  mutable empty_clauses : Clauses.CSet.t;
    (** Set of empty clauses *)

  mutable on_empty : (hclause -> unit) list;
    (** Callbacks for empty clause detection *)
}

(** {2 Basic operations} *)

let mk_env ?meta ~ctx params signature =
  let state = ProofState.mk_state ~ctx ?meta params signature in
  let env = {
    params;
    ctx;
    binary_rules = [];
    unary_rules = [];
    basic_simplify = (fun hc -> hc);
    rw_simplify = (fun _ hc -> hc);
    active_simplify = (fun _ hc -> hc);
    backward_simplify = (fun _ hc -> C.CSet.empty);
    redundant = (fun _ _ -> false);
    backward_redundant = (fun _ _ -> []);
    list_simplify = (fun hc -> [hc]);
    is_trivial = (fun _ -> false);
    axioms = [];
    mk_constr = [];
    constr = [];
    preprocess = (fun ~ctx l -> l);
    state;
    empty_clauses = C.CSet.empty;
    on_empty = [];
  } in
  env

let add_empty ~env hc =
  assert (C.is_empty hc);
  env.empty_clauses <- C.CSet.add env.empty_clauses hc;
  List.iter (fun h -> h hc) env.on_empty;
  ()

let add_passive ~env hcs =
  env.state#passive_set#add hcs;
  Sequence.iter
    (fun hc -> if C.is_empty hc then add_empty ~env hc) hcs;
  ()

let add_active ~env hcs =
  env.state#active_set#add hcs;
  Sequence.iter
    (fun hc -> if C.is_empty hc then add_empty ~env hc) hcs;
  ()

let add_simpl ~env hcs =
  env.state#simpl_set#add hcs

let remove_active ~env hcs =
  env.state#active_set#remove hcs

let remove_passive ~env hcs =
  let passive_set = env.state#passive_set in
  Sequence.iter
    (fun hc -> passive_set#remove hc.hctag)
    hcs

let remove_passive_id ~env ids =
  let passive_set = env.state#passive_set in
  Sequence.iter
    (fun id -> passive_set#remove id)
    ids

let remove_simpl ~env hcs =
  env.state#simpl_set#remove hcs

let clean_passive ~env =
  env.state#passive_set#clean ()

let add_constrs ~env constrs =
  env.constr <- Sequence.fold (fun cs c -> c::cs) env.constr constrs

let add_mk_constr ~env mk_constr =
  env.mk_constr <- mk_constr :: env.mk_constr

let get_passive ~env =
  C.CSet.to_seq env.state#passive_set#clauses

let get_active ~env =
  C.CSet.to_seq env.state#active_set#clauses

let get_simpl ~env =
  failwith "env.get_simpl: not implemented"

let add_binary_inf ~env name rule =
  env.binary_rules <- (name, rule) :: env.binary_rules

let add_unary_inf ~env name rule =
  env.unary_rules <- (name, rule) :: env.unary_rules

let list_simplify ~env hc =
  env.list_simplify hc

let add_expert ~env expert =
  env.state#add_expert expert

let get_experts ~env =
  env.state#experts

let get_meta ~env =
  env.state#meta_prover

let get_params ~env =
  env.params

let get_empty_clauses ~env =
  env.empty_clauses

let get_some_empty_clause ~env =
  C.CSet.choose (get_empty_clauses ~env)

let add_on_empty ~env h =
  env.on_empty <- h :: env.on_empty

(** Compute all ordering constraints for the given list of clauses *)
let compute_constrs ~env hcs =
  let constrs = env.constr in
  let constrs = List.fold_left
    (fun acc mk_constr -> acc @ mk_constr hcs)
    constrs env.mk_constr
  in constrs

let pp fmt env =
  Format.fprintf fmt "state: @[%a@]@;experts: @[%a@]"
    ProofState.debug_state env.state
    Experts.Set.pp (get_experts ~env)

(** {2 High level operations} *)

let prof_generate = Utils.mk_profiler "generate"
let prof_generate_unary = Utils.mk_profiler "generate_unary"
let prof_generate_binary = Utils.mk_profiler "generate_binary"
let prof_back_simplify = Utils.mk_profiler "back_simplify"
let prof_simplify = Utils.mk_profiler "simplify"
let prof_all_simplify = Utils.mk_profiler "all_simplify"
let prof_is_redundant = Utils.mk_profiler "is_redundant"
let prof_subsumed_by = Utils.mk_profiler "subsumed_by"

let stat_killed_orphans = mk_stat "orphan clauses removed"
let stat_inferred = mk_stat "inferred clauses"

type stats = int * int * int
  (** statistics on clauses : num active, num passive, num simplification *)

let stats ~env =
  ProofState.stats env.state

let next_passive ~env =
  env.state#passive_set#next ()

(** do binary inferences that involve the given clause *)
let do_binary_inferences ~env c =
  Utils.enter_prof prof_generate_binary;
  let active_set = env.state#active_set in
  Utils.debug 3 "do binary inferences with current active set: %a"
                C.pp_set active_set#clauses;
  (* apply every inference rule *)
  let clauses = List.fold_left
    (fun acc (name, rule) ->
      Utils.debug 3 "%%  apply binary rule %s" name;
      let new_clauses = rule active_set c in
      List.rev_append new_clauses acc)
    [] env.binary_rules
  in
  Utils.exit_prof prof_generate_binary;
  Sequence.of_list clauses

(** do unary inferences for the given clause *)
let do_unary_inferences ~env hc =
  Utils.enter_prof prof_generate_unary;
  Utils.debug 3 "do unary inferences";
  (* apply every inference rule *)
  let clauses = List.fold_left
    (fun acc (name, rule) ->
      Utils.debug 3 "%%  apply unary rule %s" name;
      let new_clauses = rule hc in
      List.rev_append new_clauses acc)
    [] env.unary_rules in
  Utils.exit_prof prof_generate_unary;
  Sequence.of_list clauses

(** Check whether the clause is trivial (also with Experts) *)
let is_trivial ~env hc =
  env.is_trivial hc || Experts.Set.is_redundant (get_experts ~env) hc

(** Simplify the hclause. Returns both the hclause and its simplification. *)
let simplify ~env old_hc =
  Utils.enter_prof prof_simplify;
  (* simplify with unit clauses, then all active clauses *)
  let hc = env.rw_simplify env.state#simpl_set old_hc in
  let hc = env.basic_simplify hc in
  let hc = Experts.Set.simplify (get_experts env) hc in
  let hc = env.active_simplify env.state#active_set hc in
  let hc = env.basic_simplify hc in
  (if not (Literals.eq_lits hc.hclits old_hc.hclits)
    then Utils.debug 2 "@[<hov 4>clause @[<h>%a@]@ simplified into @[<h>%a@]@]"
                        !C.pp_clause#pp_h old_hc !C.pp_clause#pp_h hc);
  Utils.exit_prof prof_simplify;
  old_hc, hc

(** Perform backward simplification with the given clause *)
let backward_simplify ~env given =
  Utils.enter_prof prof_back_simplify;
  (* set of candidate clauses, that may be unit-simplifiable *)
  let candidates = env.backward_simplify env.state#active_set given in
  (* try to simplify the candidates. Before is the set of clauses that
     are simplified, after is the list of those clauses after simplification *)
  let simpl_set = env.state#simpl_set in
  let before, after =
    C.CSet.fold
      (fun (before, after) _ hc ->
        let hc' = env.rw_simplify simpl_set hc in
        if not (Literals.eq_lits hc.hclits hc'.hclits)
          (* the active clause has been simplified! *)
          then begin
            Utils.debug 2 "@[<hov 4>active clause @[<h>%a@]@ simplified into @[<h>%a@]@]"
                          !C.pp_clause#pp_h hc !C.pp_clause#pp_h hc';
            C.CSet.add before hc, hc' :: after
          end else before, after)
    (C.CSet.empty, []) candidates
  in
  Utils.exit_prof prof_back_simplify;
  before, Sequence.of_list after

(** Simplify the clause w.r.t to the active set and experts *)
let forward_simplify ~env hc =
  let cs = list_simplify ~env hc in
  let cs = List.map (Experts.Set.simplify (get_experts ~env)) cs in
  let cs = List.map (env.rw_simplify env.state#simpl_set) cs in
  let cs = List.map env.basic_simplify cs in
  Sequence.of_list cs

(** generate all clauses from inferences *)
let generate ~env given =
  Utils.enter_prof prof_generate;
  (* binary clauses *)
  let binary_clauses = do_binary_inferences ~env given in
  (* unary inferences *)
  let unary_clauses = ref []
  and unary_queue = Queue.create () in
  Queue.push (given, 0) unary_queue;
  while not (Queue.is_empty unary_queue) do
    let hc, depth = Queue.pop unary_queue in
    let hc = env.basic_simplify hc in (* simplify a bit the clause *)
    if not (is_trivial ~env hc) then begin
      (* add the clause to set of inferred clauses, if it's not the original clause *)
      (if depth > 0 then unary_clauses := hc :: !unary_clauses);
      if depth < env.params.param_unary_depth
        then begin
          (* infer clauses from c, add them to the queue *)
          let new_clauses = do_unary_inferences ~env hc in
          Sequence.iter
            (fun hc' -> Queue.push (hc', depth+1) unary_queue)
            new_clauses
        end
    end
  done;
  let result = Sequence.append (Sequence.of_list !unary_clauses) binary_clauses in
  add_stat stat_inferred (Sequence.length result);
  Utils.exit_prof prof_generate;
  result

(** remove direct descendants of the clauses from the passive set *)
let remove_orphans ~env removed_clauses =
  (* remove descendants of the clause. If the descendants are redundant
     (cf C.flag_redundant) their descendants are also removed *)
  let rec remove_descendants hc =
    let orphans = hc.hcdescendants in
    (* remove orphans from passive set *)
    SmallSet.iter
      (fun orphan_id ->
        incr_stat stat_killed_orphans;
        env.state#passive_set#remove orphan_id
        (*
        try
          let c = C.CSet.get passive_set#clauses orphan_id in
          if Ptset.is_empty c.hcdescendants then begin
            (* only kill orphans that have never participated in inferences *)
            incr_stat stat_killed_orphans;
            passive_set#remove orphan_id
          end
        with Not_found -> ())
        *)
        )
      orphans
  in
  Sequence.iter remove_descendants removed_clauses

(** check whether the clause is redundant w.r.t the current active_set *)
let is_redundant ~env hc =
  Utils.enter_prof prof_is_redundant;
  let res = env.redundant env.state#active_set hc in
  Utils.exit_prof prof_is_redundant;
  res

(** find redundant clauses in current active_set *)
let subsumed_by ~env hc =
  Utils.enter_prof prof_subsumed_by;
  let res = env.backward_redundant env.state#active_set hc in
  Utils.exit_prof prof_subsumed_by;
  res

(** Use all simplification rules to convert a clause into a list of maximally
    simplified clauses (possibly empty, if trivial). *)
let all_simplify ~env hc =
  Utils.enter_prof prof_all_simplify;
  let clauses = env.list_simplify hc in
  let clauses = Utils.list_flatmap
    (fun hc ->
      (* simplify this clause *)
      let _, hc' = simplify ~env hc in
      if env.is_trivial hc' (* XXX: does not seem very useful? || Sup.is_semantic_tautology hc' *)
        then [] else [hc']) (* TODO CLI flag to enable semantic tauto *)
    clauses
  in
  Utils.exit_prof prof_all_simplify;
  clauses

(** Make a clause out of a 'Deduced' result *)
let clause_of_deduced ~env lits parents = 
  let premises = List.map (fun hc -> hc.hcproof) parents in
  let hc = C.mk_hclause_a ~ctx:env.ctx lits ~parents
    (fun c -> Proof.mk_proof c "lemma" premises) in
  env.basic_simplify (C.clause_of_fof hc)

(** Find the lemmas that can be deduced if we consider this new clause *)
let find_lemmas ~env hc = 
  match (get_meta env) with
  | None -> Sequence.empty (* lemmas detection is disabled *)
  | Some meta ->
    let results = Meta.Prover.scan_clause meta hc in
    let results = Utils.list_flatmap
      (function
      | Meta.Prover.Deduced (lits,parents) ->
        let hc = clause_of_deduced ~env lits parents in
        Utils.debug 1 "%% meta-prover: lemma @[<h>%a@]" !C.pp_clause#pp_h hc;
        [hc]
      | _ -> [])
      results in
    Sequence.of_list results

(** Do one step of the meta-prover. The current given clause and active set
    are provided. This returns a list of new clauses. *)
let meta_step ~env hc =
  let results = match (get_meta env) with
  | None -> []
  | Some prover -> begin
    (* forward scanning *)
    let results = Meta.Prover.scan_clause prover hc in
    (* backward scanning, if needed *)
    let results' =
      if Meta.Prover.has_new_patterns prover
        then Meta.Prover.scan_set prover env.state#active_set#clauses
        else [] in
    let results = List.rev_append results' results in
    (* use results *)
    Utils.list_flatmap
      (fun result -> match result with
        | Meta.Prover.Deduced (lits,parents) ->
          let lemma = clause_of_deduced ~env lits parents in
          Utils.debug 1 "%% meta-prover: lemma @[<h>%a@]" !C.pp_clause#pp lemma;
          [lemma]
        | Meta.Prover.Theory (th_name, th_args) ->
          Utils.debug 1 "%% meta-prover: theory @[<h>%a@]" Meta.Prover.pp_result result;
          []
        | Meta.Prover.Expert expert ->
          Utils.debug 1 "%% meta-prover: expert @[<h>%a@]" Experts.pp_expert expert;
          add_expert env expert;
          [])
      results
    end
  in
  Sequence.of_list results

(** Preprocess clauses *)
let preprocess ~env clauses =
  env.preprocess ~ctx:env.ctx clauses
