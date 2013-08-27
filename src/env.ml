
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

(** {1 Global environment for an instance of the prover} *)

open Logtk

module T = Term
module C = Clause

type binary_inf_rule = ProofState.ActiveSet.t -> Clause.t -> Clause.t list
  (** binary inferences. An inference returns a list of conclusions *)

type unary_inf_rule = Clause.t -> Clause.t list
  (** unary infererences *)

type lit_rewrite_rule = ctx:Ctx.t -> Literal.t -> Literal.t
  (** Rewrite rule on literals *)


type t = {
  mutable params : Params.t;
  mutable ctx : Ctx.t;

  mutable binary_rules : (string * binary_inf_rule) list;
    (** the binary inference rules *)
  
  mutable unary_rules : (string * unary_inf_rule) list;
    (** the unary inference rules *)

  mutable rewrite_rules : (string * (Term.t -> Term.t)) list;
    (** Rules to apply to term *)

  mutable lit_rules : (string * lit_rewrite_rule) list;
    (** Rules to be applied to literals *)
  
  mutable basic_simplify : Clause.t -> Clause.t;
    (** how to simplify a clause *)
  
  mutable rw_simplify : ProofState.SimplSet.t -> Clause.t -> Clause.t;
    (** how to simplify a clause w.r.t a set of unit clauses *)
  
  mutable active_simplify : ProofState.ActiveSet.t -> Clause.t -> Clause.t;
    (** how to simplify a clause w.r.t an active set of clauses *)

  mutable backward_simplify : ProofState.ActiveSet.t -> Clause.t -> Clause.CSet.t;
    (** backward simplification by a unit clause. It returns a set of
        active clauses that can potentially be simplified by the given clause *)

  mutable redundant : ProofState.ActiveSet.t -> Clause.t -> bool;
    (** check whether the clause is redundant w.r.t the set *)

  mutable backward_redundant : ProofState.ActiveSet.t -> Clause.t -> Clause.t list;
    (** find redundant clauses in set w.r.t the clause *)

  mutable list_simplify : Clause.t -> Clause.t list;
    (** how to simplify a clause into a (possibly empty) list
        of clauses. This subsumes the notion of trivial clauses (that
        are simplified into the empty list of clauses) *)

  mutable is_trivial : Clause.t -> bool;
    (** single test to detect trivial clauses *)

  mutable axioms : Clause.t list;
    (** a list of axioms to add to the problem *)

  mutable mk_constr : (Clause.t Sequence.t -> Precedence.constr list) list;
    (** How to build constraints from a list of clauses *)

  mutable constr : Precedence.constr list;
    (** some constraints on the precedence *)

  mutable preprocess : ctx:Ctx.t -> Clause.t list -> Clause.t list;
    (** how to preprocess the initial list of clauses *)

  mutable state : ProofState.t;
    (** Proof state *)

  mutable empty_clauses : Clause.CSet.t;
    (** Set of empty clauses *)

  mutable on_empty : (Clause.t -> unit) list;
    (** Callbacks for empty clause detection *)
}

(** {2 Basic operations} *)

let create ?meta ~ctx params signature =
  let state = ProofState.create ~ctx ?meta params signature in
  let env = {
    params;
    ctx;
    binary_rules = [];
    unary_rules = [];
    rewrite_rules = [];
    lit_rules = [];
    basic_simplify = (fun c -> c);
    rw_simplify = (fun _ c -> c);
    active_simplify = (fun _ c -> c);
    backward_simplify = (fun _ c -> C.CSet.empty);
    redundant = (fun _ _ -> false);
    backward_redundant = (fun _ _ -> []);
    list_simplify = (fun c -> [c]);
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

let add_empty ~env c =
  assert (C.is_empty c);
  env.empty_clauses <- C.CSet.add env.empty_clauses c;
  List.iter (fun h -> h c) env.on_empty;
  ()

let add_passive ~env cs =
  env.state#passive_set#add cs;
  Sequence.iter
    (fun c -> if C.is_empty c then add_empty ~env c) cs;
  ()

let add_active ~env cs =
  env.state#active_set#add cs;
  Sequence.iter
    (fun c -> if C.is_empty c then add_empty ~env c) cs;
  ()

let add_simpl ~env cs =
  env.state#simpl_set#add cs

let remove_active ~env cs =
  env.state#active_set#remove cs

let remove_passive ~env cs =
  let passive_set = env.state#passive_set in
  Sequence.iter
    (fun c -> passive_set#remove c.C.hctag)
    cs

let remove_passive_id ~env ids =
  let passive_set = env.state#passive_set in
  Sequence.iter
    (fun id -> passive_set#remove id)
    ids

let remove_simpl ~env cs =
  env.state#simpl_set#remove cs

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
  if not (List.mem_assoc name env.binary_rules)
    then env.binary_rules <- (name, rule) :: env.binary_rules

let add_unary_inf ~env name rule =
  if not (List.mem_assoc name env.unary_rules)
    then env.unary_rules <- (name, rule) :: env.unary_rules

let list_simplify ~env c =
  env.list_simplify c

let add_expert ~env expert =
  env.state#add_expert expert

let add_rewrite_rule ~env name rule =
  env.rewrite_rules <- (name, rule) :: env.rewrite_rules

let add_lit_rule ~env name rule =
  env.lit_rules <- (name, rule) :: env.lit_rules

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
let compute_constrs ~env cs =
  let constrs = env.constr in
  let constrs = List.fold_left
    (fun acc mk_constr -> acc @ mk_constr cs)
    constrs env.mk_constr
  in constrs

let ord env = Ctx.ord env.ctx

let pp buf env = 
  Printf.bprintf buf "env(state: %a, experts: %a)"
    ProofState.debug env.state
    Experts.Set.pp (get_experts ~env)

let fmt fmt env =
  Format.pp_print_string fmt (Util.on_buffer pp env)

(** {2 High level operations} *)

let prof_generate = Util.mk_profiler "generate"
let prof_generate_unary = Util.mk_profiler "generate_unary"
let prof_generate_binary = Util.mk_profiler "generate_binary"
let prof_back_simplify = Util.mk_profiler "back_simplify"
let prof_simplify = Util.mk_profiler "simplify"
let prof_all_simplify = Util.mk_profiler "all_simplify"
let prof_is_redundant = Util.mk_profiler "is_redundant"
let prof_subsumed_by = Util.mk_profiler "subsumed_by"

let stat_killed_orphans = Util.mk_stat "orphan clauses removed"
let stat_inferred = Util.mk_stat "inferred clauses"

type stats = int * int * int
  (** statistics on clauses : num active, num passive, num simplification *)

let stats ~env =
  ProofState.stats env.state

let next_passive ~env =
  env.state#passive_set#next ()

(** do binary inferences that involve the given clause *)
let do_binary_inferences ~env c =
  Util.enter_prof prof_generate_binary;
  let active_set = env.state#active_set in
  Util.debug 3 "do binary inferences with current active set: %a"
                C.pp_set_debug active_set#clauses;
  (* apply every inference rule *)
  let clauses = List.fold_left
    (fun acc (name, rule) ->
      Util.debug 3 "%%  apply binary rule %s" name;
      let new_clauses = rule active_set c in
      List.rev_append new_clauses acc)
    [] env.binary_rules
  in
  Util.exit_prof prof_generate_binary;
  Sequence.of_list clauses

(** do unary inferences for the given clause *)
let do_unary_inferences ~env c =
  Util.enter_prof prof_generate_unary;
  Util.debug 3 "do unary inferences";
  (* apply every inference rule *)
  let clauses = List.fold_left
    (fun acc (name, rule) ->
      Util.debug 3 "%%  apply unary rule %s" name;
      let new_clauses = rule c in
      List.rev_append new_clauses acc)
    [] env.unary_rules in
  Util.exit_prof prof_generate_unary;
  Sequence.of_list clauses

(** Check whether the clause is trivial (also with Experts) *)
let is_trivial ~env c =
  env.is_trivial c || Experts.Set.is_redundant (get_experts ~env) c

(** Apply rewrite rules *)
let rewrite ~env c =
  let applied_rules = ref (SmallSet.empty ~cmp:String.compare) in
  let rec reduce_term rules t =
    match rules with
    | [] -> t
    | (name, r)::rules' ->
      let t' = r t in
      if t != t'
        then begin
          applied_rules := SmallSet.add !applied_rules name;
          reduce_term env.rewrite_rules t'  (* re-apply all rules *)
        end else reduce_term rules' t  (* try next rule *)
  in
  (* reduce every literal *)
  let lits' = Array.map
    (function (Literal.Equation (l, r, sign, _) as lit) ->
      let l' = reduce_term env.rewrite_rules l
      and r' = reduce_term env.rewrite_rules r in
      if l == l' && r == r'
        then lit  (* same lit *)
        else Literal.mk_lit ~ord:(Ctx.ord env.ctx) l' r' sign)
    c.C.hclits
  in
  if SmallSet.is_empty !applied_rules
    then c (* no simplification *)
    else begin
      let rule = "rw_" ^ (String.concat "_" (SmallSet.to_list !applied_rules)) in
      let proof c' = Proof.mk_infer c' rule [c.C.hcproof] in
      let parents = [c] in
      let new_clause = C.create_a ~parents ~ctx:env.ctx lits' proof in
      Util.debug 3 "rewritten %a into %a" C.pp_debug c C.pp_debug new_clause;
      new_clause
    end

(** Apply literal rewrite rules *)
let rewrite_lits ~env c =
  let ctx = env.ctx in
  let applied_rules = ref (SmallSet.empty ~cmp:String.compare) in
  let rec rewrite_lit rules lit = match rules with
  | [] -> lit
  | (name,r)::rules' ->
    let lit' = r ~ctx lit in  (* apply the rule *)
    if Literal.eq lit lit'
      then rewrite_lit rules' lit
      else begin
        applied_rules := SmallSet.add !applied_rules name;
        rewrite_lit env.lit_rules lit'
      end
  in
  (* apply lit rules *)
  let lits = Array.map (fun lit -> rewrite_lit env.lit_rules lit) c.C.hclits in
  if SmallSet.is_empty !applied_rules then c
  else begin  (* simplifications occurred! *)
    let rule = "lit_rw_" ^ (String.concat "_" (SmallSet.to_list !applied_rules)) in
    let proof c' = Proof.mk_infer c' rule [c.C.hcproof] in
    let parents = [c] in
    let new_clause = C.create_a ~parents ~ctx:env.ctx lits proof in
    Util.debug 3 "lit rewritten %a into %a" C.pp_debug c C.pp_debug new_clause;
    new_clause
  end

(** All basic simplification of the clause itself *)
let basic_simplify ~env c =
  if env.lit_rules = []
    then env.basic_simplify c
    else  (* rewrite lits, then simplify *)
      let c' = rewrite_lits ~env c in
      env.basic_simplify c'

(** Simplify the hclause. Returns both the hclause and its simplification. *)
let simplify ~env old_hc =
  Util.enter_prof prof_simplify;
  let c = old_hc in
  (* simplify with unit clauses, then all active clauses *)
  let c = rewrite ~env c in
  let c = env.rw_simplify env.state#simpl_set c in
  let c = basic_simplify ~env c in
  let c = Experts.Set.simplify (get_experts env) c in
  let c = env.active_simplify env.state#active_set c in
  let c = basic_simplify ~env c in
  (if not (Literal.eq_lits c.C.hclits old_hc.C.hclits)
    then Util.debug 2 "clause %a simplified into %a" C.pp_debug old_hc C.pp_debug c);
  Util.exit_prof prof_simplify;
  old_hc, c

(** Perform backward simplification with the given clause *)
let backward_simplify ~env given =
  Util.enter_prof prof_back_simplify;
  (* set of candidate clauses, that may be unit-simplifiable *)
  let candidates = env.backward_simplify env.state#active_set given in
  (* try to simplify the candidates. Before is the set of clauses that
     are simplified, after is the list of those clauses after simplification *)
  let simpl_set = env.state#simpl_set in
  let before, after =
    C.CSet.fold candidates (C.CSet.empty, [])
      (fun (before, after) _ c ->
        let c' = env.rw_simplify simpl_set c in
        if not (Literal.eq_lits c.C.hclits c'.C.hclits)
          (* the active clause has been simplified! *)
          then begin
            Util.debug 2 "active clause %a simplified into %a" C.pp_debug c C.pp_debug c';
            C.CSet.add before c, c' :: after
          end else before, after)
  in
  Util.exit_prof prof_back_simplify;
  before, Sequence.of_list after

(** Simplify the clause w.r.t to the active set and experts *)
let forward_simplify ~env c =
  let c = rewrite ~env c in
  let cs = list_simplify ~env c in
  let cs = List.map (Experts.Set.simplify (get_experts ~env)) cs in
  let cs = List.map (env.rw_simplify env.state#simpl_set) cs in
  let cs = List.map (basic_simplify ~env) cs in
  Sequence.of_list cs

(** generate all clauses from inferences *)
let generate ~env given =
  Util.enter_prof prof_generate;
  (* binary clauses *)
  let binary_clauses = do_binary_inferences ~env given in
  (* unary inferences *)
  let unary_clauses = ref []
  and unary_queue = Queue.create () in
  Queue.push (given, 0) unary_queue;
  while not (Queue.is_empty unary_queue) do
    let c, depth = Queue.pop unary_queue in
    let c = (basic_simplify ~env) c in (* simplify a bit the clause *)
    if not (is_trivial ~env c) then begin
      (* add the clause to set of inferred clauses, if it's not the original clause *)
      (if depth > 0 then unary_clauses := c :: !unary_clauses);
      if depth < env.params.Params.param_unary_depth
        then begin
          (* infer clauses from c, add them to the queue *)
          let new_clauses = do_unary_inferences ~env c in
          Sequence.iter
            (fun c' -> Queue.push (c', depth+1) unary_queue)
            new_clauses
        end
    end
  done;
  let result = Sequence.append (Sequence.of_list !unary_clauses) binary_clauses in
  Util.add_stat stat_inferred (Sequence.length result);
  Util.exit_prof prof_generate;
  result

(** remove direct descendants of the clauses from the passive set *)
let remove_orphans ~env removed_clauses =
  (* remove descendants of the clause. If the descendants are redundant
     (cf C.flag_redundant) their descendants are also removed *)
  let rec remove_descendants c =
    let orphans = c.C.hcdescendants in
    (* remove orphans from passive set *)
    SmallSet.iter
      (fun orphan_id ->
        Util.incr_stat stat_killed_orphans;
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
let is_redundant ~env c =
  Util.enter_prof prof_is_redundant;
  let res = env.redundant env.state#active_set c in
  Util.exit_prof prof_is_redundant;
  res

(** find redundant clauses in current active_set *)
let subsumed_by ~env c =
  Util.enter_prof prof_subsumed_by;
  let res = env.backward_redundant env.state#active_set c in
  Util.exit_prof prof_subsumed_by;
  res

(** Use all simplification rules to convert a clause into a list of maximally
    simplified clauses (possibly empty, if trivial). *)
let all_simplify ~env c =
  Util.enter_prof prof_all_simplify;
  let clauses = env.list_simplify c in
  let clauses = Util.list_flatmap
    (fun c ->
      (* simplify this clause *)
      let _, c' = simplify ~env c in
      if env.is_trivial c' (* XXX: does not seem very useful? || Sup.is_semantic_tautology c' *)
        then [] else [c']) (* TODO CLI flag to enable semantic tauto *)
    clauses
  in
  Util.exit_prof prof_all_simplify;
  clauses

(** Make a clause out of a 'Deduced' result *)
let clause_of_deduced ~env lits parents = 
  let premises = List.map (fun c -> c.C.hcproof) parents in
  let c = C.create_a ~ctx:env.ctx lits ~parents
    (fun c -> Proof.mk_infer c "lemma" premises) in
  basic_simplify ~env (C.clause_of_fof c)

(** Find the lemmas that can be deduced if we consider this new clause *)
let find_lemmas ~env c = 
  match (get_meta env) with
  | None -> Sequence.empty (* lemmas detection is disabled *)
  | Some meta ->
    let results = MetaProverState.scan_clause meta c in
    let results = Util.list_fmap
      (function
      | MetaProverState.Deduced (f,parents) ->
        (* TODO: CNF reduction now? *)
        let lits = [| Literal.mk_eq ~ord:(Ctx.ord env.ctx) f T.true_term |] in
        let c = clause_of_deduced ~env lits parents in
        Util.debug 1 "%% meta-prover: lemma %a" C.pp_debug c;
        Some c
      | _ -> None)
      results in
    Sequence.of_list results

(** Do one step of the meta-prover. The current given clause and active set
    are provided. This returns a list of new clauses. *)
let meta_step ~env c =
  let results = match (get_meta env) with
  | None -> []
  | Some prover -> begin
    (* forward scanning *)
    let results = MetaProverState.scan_clause prover c in
    (* backward scanning, if needed *)
    let results' =
      if MetaProverState.has_new_patterns prover
        then MetaProverState.scan_set prover env.state#active_set#clauses
        else [] in
    let results = List.rev_append results' results in
    (* use results *)
    Util.list_flatmap
      (fun result -> match result with
        | MetaProverState.Deduced (f,parents) ->
          (* TODO: CNF reduction now? *)
          let lits = [| Literal.mk_eq ~ord:(Ctx.ord env.ctx) f T.true_term |] in
          let lemma = clause_of_deduced ~env lits parents in
          Util.debug 1 "%% meta-prover: lemma %a" C.pp_debug lemma;
          [lemma]
        | MetaProverState.Theory (th_name, th_args) ->
          Util.debug 1 "%% meta-prover: theory %a" MetaProverState.pp_result result;
          []
        | MetaProverState.Expert expert ->
          Util.debug 1 "%% meta-prover: expert %a" Experts.pp expert;
          add_expert env expert;
          [])
      results
    end
  in
  Sequence.of_list results

(** Preprocess clauses *)
let preprocess ~env clauses =
  env.preprocess ~ctx:env.ctx clauses
