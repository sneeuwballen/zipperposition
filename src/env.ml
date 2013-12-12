
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

module T = FOTerm
module C = Clause
module F = FOFormula
module PF = PFormula
module Lit = Literal
module Lits = Literal.Arr

type binary_inf_rule = ProofState.ActiveSet.t -> Clause.t -> Clause.t list
  (** binary inferences. An inference returns a list of conclusions *)

type unary_inf_rule = Clause.t -> Clause.t list
  (** unary infererences *)

type rw_simplify_rule = ProofState.SimplSet.t -> Clause.t -> Clause.t 
  (** Simplify a clause w.r.t. a simplification set *)

type active_simplify_rule = ProofState.ActiveSet.t -> Clause.t -> Clause.t
  (** Simplify the given clause using clauses from the active set. *)

type backward_simplify_rule = ProofState.ActiveSet.t -> Clause.t -> Clause.CSet.t
  (** backward simplification by a unit clause. It returns a set of
      active clauses that can potentially be simplified by the given clause.
      [backward_simplify active c] therefore returns a subset of [active]. *)

type redundant_rule = ProofState.ActiveSet.t -> Clause.t -> bool
  (** check whether the clause is redundant w.r.t the set *)

type backward_redundant_rule = ProofState.ActiveSet.t -> Clause.t -> Clause.CSet.t
  (** find redundant clauses in set w.r.t the clause *)

type simplify_rule = Clause.t -> Clause.t
  (** Simplify the clause structurally (basic simplifications) *)

type is_trivial_rule = Clause.t -> bool
  (** Rule that checks whether the clause is trivial (a tautology) *)

type term_rewrite_rule = FOTerm.t -> FOTerm.t
  (** Rewrite rule on terms *)

type lit_rewrite_rule = ctx:Ctx.t -> Lit.t -> Lit.t
  (** Rewrite rule on literals *)

type t = {
  mutable params : Params.t;
  mutable ctx : Ctx.t;

  mutable binary_rules : (string * binary_inf_rule) list;
    (** the binary inference rules *)
  
  mutable unary_rules : (string * unary_inf_rule) list;
    (** the unary inference rules *)

  mutable rewrite_rules : (string * (T.t -> T.t)) list;
    (** Rules to apply to term *)

  mutable lit_rules : (string * lit_rewrite_rule) list;
    (** Rules to be applied to literals *)
  
  mutable basic_simplify : simplify_rule list;
    (** how to simplify a clause *)
  
  mutable rw_simplify : rw_simplify_rule list;
    (** how to simplify a clause w.r.t a set of unit clauses *)
  
  mutable active_simplify : active_simplify_rule list;
    (** how to simplify a clause w.r.t an active set of clauses *)

  mutable backward_simplify : backward_simplify_rule list;
    (** backward simplification by a unit clause. It returns a set of
        active clauses that can potentially be simplified by the given clause *)

  mutable redundant : redundant_rule list;
    (** check whether the clause is redundant w.r.t the set *)

  mutable backward_redundant : backward_redundant_rule list;
    (** find redundant clauses in set w.r.t the clause *)

  mutable is_trivial : is_trivial_rule list;
    (** single test to detect trivial clauses *)

  mutable state : ProofState.t;
    (** Proof state *)

  mutable empty_clauses : Clause.CSet.t;
    (** Set of empty clauses *)

  mutable on_empty : (Clause.t -> unit) list;
    (** Callbacks for empty clause detection *)

  mutable evaluator : Evaluator.FO.t;
    (** Evaluator *)
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
    basic_simplify = [];
    rw_simplify = [];
    active_simplify = [];
    backward_simplify = [];
    redundant = [];
    backward_redundant = [];
    is_trivial = [];
    state;
    empty_clauses = C.CSet.empty;
    on_empty = [];
    evaluator = Evaluator.FO.create ();
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

let add_rw_simplify ~env r =
  env.rw_simplify <- r :: env.rw_simplify

let add_active_simplify ~env r =
  env.active_simplify <- r :: env.active_simplify

let add_backward_simplify ~env r =
  env.backward_simplify <- r :: env.backward_simplify

let add_redundant ~env r =
  env.redundant <- r :: env.redundant

let add_backward_redundant ~env r =
  env.backward_redundant <- r :: env.backward_redundant

let add_simplify ~env r =
  env.basic_simplify <- r :: env.basic_simplify

let add_is_trivial ~env r =
  env.is_trivial <- r :: env.is_trivial

let add_rewrite_rule ~env name rule =
  env.rewrite_rules <- (name, rule) :: env.rewrite_rules

let add_lit_rule ~env name rule =
  env.lit_rules <- (name, rule) :: env.lit_rules

let interpret_symbol ~env s rule =
  Evaluator.FO.register env.evaluator s rule

let interpret_symbols ~env l =
  List.iter (fun (s,rule) -> interpret_symbol ~env s rule) l

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

let ctx env = env.ctx
let ord env = Ctx.ord env.ctx
let precedence env = Ordering.precedence (ord env)
let signature env = Ctx.signature env.ctx

let state env = env.state

let pp buf env = 
  Printf.bprintf buf "env(state: %a)"
    ProofState.debug env.state

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

let cnf ~env set =
  let ctx = env.ctx in
  let clauses = Sequence.fold
    (fun cset pf ->
      let f = pf.PF.form in
      Util.debug 3 "reduce %a to CNF..." F.pp f;
      (* reduce to CNF this clause *)
      let clauses = Cnf.cnf_of ~ctx:ctx.Ctx.skolem f in
      (* now build "proper" clauses, with proof and all *)
      let proof cc =
        match clauses with
        | [[f']] when F.eq f f' -> Proof.adapt_c pf.PF.proof cc  (* keep proof *)
        | _ -> Proof.mk_c_step ~esa:true cc ~rule:"cnf" [pf.PF.proof]
      in
      let clauses = List.map (fun c -> C.create_forms ~ctx c proof) clauses in
      C.CSet.add_list cset clauses)
    C.CSet.empty (PF.Set.to_seq set)
  in
  (* declaration of new skolem symbols *)
  Ctx.add_signature ~ctx (Skolem.to_signature ctx.Ctx.skolem);
  clauses

let next_passive ~env =
  env.state#passive_set#next ()

(** do binary inferences that involve the given clause *)
let do_binary_inferences ~env c =
  Util.enter_prof prof_generate_binary;
  let active_set = env.state#active_set in
  Util.debug 3 "do binary inferences with current active set: %a"
                C.pp_set active_set#clauses;
  (* apply every inference rule *)
  let clauses = List.fold_left
    (fun acc (name, rule) ->
      Util.debug 3 "apply binary rule %s" name;
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
      Util.debug 3 "apply unary rule %s" name;
      let new_clauses = rule c in
      List.rev_append new_clauses acc)
    [] env.unary_rules in
  Util.exit_prof prof_generate_unary;
  Sequence.of_list clauses

let is_trivial ~env c =
  if C.get_flag C.flag_persistent c then false else
  match env.is_trivial with
  | [] -> false
  | [f] -> f c
  | [f;g] -> f c || g c
  | l -> List.exists (fun f -> f c) l

let is_active ~env c =
  C.CSet.mem env.state#active_set#clauses c

let is_passive ~env c =
  C.CSet.mem env.state#passive_set#clauses c

(** Apply rewrite rules AND evaluation functions *)
let rewrite ~env c =
  Util.debug 5 "rewrite clause %a..." C.pp c;
  let applied_rules = ref (SmallSet.empty ~cmp:String.compare) in
  let rec reduce_term rules t =
    match rules with
    | [] ->
      (* all rules tried, now evaluate *)
      let t' = Evaluator.FO.eval env.evaluator t in
      if T.eq t t'
        then t
        else begin
          applied_rules := SmallSet.add !applied_rules "evaluation";
          Util.debug 5 "Env: rewrite %a into %a" T.pp t T.pp t';
          reduce_term env.rewrite_rules t'  (* re-apply rules *)
        end
    | (name, r)::rules' ->
      let t' = r t in
      if t != t'
        then begin
          applied_rules := SmallSet.add !applied_rules name;
          Util.debug 5 "Env: rewrite %a into %a" T.pp t T.pp t';
          reduce_term env.rewrite_rules t'  (* re-apply all rules *)
        end else reduce_term rules' t  (* try next rule *)
  in
  (* reduce every literal *)
  let lits' = Array.map
    (fun lit -> match lit with
      | Lit.Equation (l, r, sign, _) ->
        let l' = reduce_term env.rewrite_rules l
        and r' = reduce_term env.rewrite_rules r in
        if l == l' && r == r'
          then lit  (* same lit *)
          else Lit.mk_lit ~ord:(Ctx.ord env.ctx) l' r' sign
      | Lit.Prop (p, sign) ->
        let p' = reduce_term env.rewrite_rules p in
        if p == p'
          then lit
          else Lit.mk_prop p' sign
      | Lit.True
      | Lit.False -> lit)
    c.C.hclits
  in
  if SmallSet.is_empty !applied_rules
    then c (* no simplification *)
    else begin
      let rule = "rw_" ^ (String.concat "_" (SmallSet.to_list !applied_rules)) in
      let proof c' = Proof.mk_c_step c' rule [c.C.hcproof] in
      let parents = [c] in
      let new_clause = C.create_a ~parents ~ctx:env.ctx lits' proof in
      Util.debug 3 "Env: term rewritten clause %a into %a" C.pp c C.pp new_clause;
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
    if Lit.eq_com lit lit'
      then rewrite_lit rules' lit
      else begin
        applied_rules := SmallSet.add !applied_rules name;
        Util.debug 5 "Env: rewritten lit %a into %a" Lit.pp lit Lit.pp lit';
        rewrite_lit env.lit_rules lit'
      end
  in
  (* apply lit rules *)
  let c = C.follow_simpl c in
  let lits = Array.map (fun lit -> rewrite_lit env.lit_rules lit) c.C.hclits in
  if Lits.eq_com lits c.C.hclits then c
  else begin  (* simplifications occurred! *)
    let rule = "lit_rw_" ^ (String.concat "_" (SmallSet.to_list !applied_rules)) in
    let proof c' = Proof.mk_c_step c' rule [c.C.hcproof] in
    let parents = [c] in
    let new_clause = C.create_a ~parents ~ctx:env.ctx lits proof in
    Util.debug 3 "Env: lit rewritten %a into %a" C.pp c C.pp new_clause;
    new_clause
  end

(** All basic simplification of the clause itself *)
let rec basic_simplify ~env c =
  (* first, rewrite terms *)
  let c = rewrite ~env c in
  let c = C.follow_simpl c in
  (* rewrite literals (if needed) *)
  let c = match env.lit_rules with
  | [] -> c
  | l -> rewrite_lits ~env c
  in
  let c = C.follow_simpl c in
  (* apply simplifications *)
  let c' = match env.basic_simplify with
  | [] -> c
  | [f] -> f c
  | [f;g] -> g (f c)
  | l -> List.fold_left (fun c f -> f c) c l
  in
  (* fixpoint *)
  if C.eq c c'
    then c'
    else begin
      C.simpl_to ~from:c ~into:c';
      basic_simplify ~env c'  (* fixpoint *)
    end

(* rewrite clause with simpl_set *)
let rec rw_simplify ~env c =
  if C.get_flag C.flag_persistent c then c else
  let simpl_set = env.state#simpl_set in
  let c = C.follow_simpl c in
  let c' = match env.rw_simplify with
  | [] -> c
  | [f] -> f simpl_set c
  | [f;g] -> g simpl_set (f simpl_set c)
  | l -> List.fold_left (fun c f -> f simpl_set c) c l
  in
  if C.eq c c'
    then c'
    else 
      let _ = C.simpl_to ~from:c ~into:c' in
      rw_simplify ~env c'

(* simplify clause w.r.t. active set *)
let rec active_simplify ~env c =
  if C.get_flag C.flag_persistent c then c else
  let active = env.state#active_set in
  let c' = match env.active_simplify with
  | [] -> c
  | [f] -> f active c
  | [f;g] -> f active (g active c)
  | l -> List.fold_left (fun c f -> f active c) c l
  in
  if C.eq c c'
    then c'
    else
      let _ = C.simpl_to ~from:c ~into:c' in
      active_simplify ~env c'

(** Simplify the hclause. Returns both the hclause and its simplification. *)
let simplify ~env old_c =
  (* fixpoint *)
  let rec fix ~env old_c =
    let c = old_c in
    let c = basic_simplify ~env c in
    (* simplify with unit clauses, then all active clauses *)
    let c = rewrite ~env c in
    let c = rw_simplify ~env c in
    let c = basic_simplify ~env c in
    let c = active_simplify ~env c in
    if not (Lits.eq_com c.C.hclits old_c.C.hclits)
      then begin
        Util.debug 2 "clause %a simplified into %a" C.pp old_c C.pp c;
        C.simpl_to old_c c;
        fix ~env c
      end else
        c
  in
  Util.enter_prof prof_simplify;
  let c = fix ~env old_c in
  Util.exit_prof prof_simplify;
  old_c, c

(* find candidates for backward simplification in active set *)
let backward_simplify ~env given =
  let active = env.state#active_set in
  match env.backward_simplify with
  | [] -> C.CSet.empty
  | [f] -> f active given
  | [f;g] -> C.CSet.union (f active given) (g active given)
  | l -> List.fold_left (fun set f -> C.CSet.union set (f active given)) C.CSet.empty l

(** Perform backward simplification with the given clause *)
let backward_simplify ~env given =
  Util.enter_prof prof_back_simplify;
  (* set of candidate clauses, that may be unit-simplifiable *)
  let candidates = backward_simplify ~env given in
  (* try to simplify the candidates. Before is the set of clauses that
     are simplified, after is the list of those clauses after simplification *)
  let before, after =
    C.CSet.fold candidates (C.CSet.empty, [])
      (fun (before, after) _ c ->
        let c' = rw_simplify ~env c in
        if not (Lit.Arr.eq c.C.hclits c'.C.hclits)
          (* the active clause has been simplified! *)
          then begin
            Util.debug 2 "active clause %a simplified into %a" C.pp c C.pp c';
            C.CSet.add before c, c' :: after
          end else before, after)
  in
  Util.exit_prof prof_back_simplify;
  before, Sequence.of_list after

(** Simplify the clause w.r.t to the active set *)
let forward_simplify ~env c =
  let c = C.follow_simpl c in
  let c = rewrite ~env c in
  let c = rw_simplify ~env c in
  let c = basic_simplify ~env c in
  c

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
        env.state#passive_set#remove orphan_id)
      orphans
  in
  Sequence.iter remove_descendants removed_clauses

(** check whether the clause is redundant w.r.t the current active_set *)
let is_redundant ~env c =
  Util.enter_prof prof_is_redundant;
  let active = env.state#active_set in
  let res = match env.redundant with
  | [] -> false
  | [f] -> f active c
  | [f;g] -> f active c || g active c
  | l -> List.exists (fun f -> f active c) l
  in
  Util.exit_prof prof_is_redundant;
  res

(** find redundant clauses in current active_set *)
let subsumed_by ~env c =
  Util.enter_prof prof_subsumed_by;
  let active = env.state#active_set in
  let res = match env.backward_redundant with
  | [] -> C.CSet.empty
  | [f] -> f active c
  | [f;g] -> C.CSet.union (f active c) (g active c)
  | l -> List.fold_left (fun set f -> C.CSet.union set (f active c)) C.CSet.empty l
  in
  Util.exit_prof prof_subsumed_by;
  res

(** Use all simplification rules to convert a clause into a maximally
    simplified clause, or None *)
let all_simplify ~env c =
  Util.enter_prof prof_all_simplify;
  let _, c' = simplify ~env c in
  let res = if is_trivial ~env c' || is_redundant ~env c'
    then None
    else Some c'
  in
  Util.exit_prof prof_all_simplify;
  res

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
      begin fun result -> match result with
        | MetaProverState.Deduced (f,parents) ->
          (* reduce result in CNF *)
          let cset = cnf ~env (PF.Set.singleton f) in
          C.CSet.to_list cset
        | MetaProverState.Theory (th_name, th_args, lit) ->
          []
      end
      results
    end
  in
  Sequence.of_list results
