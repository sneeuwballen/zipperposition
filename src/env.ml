
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
module F = Formula.FO
module PF = PFormula
module Lit = Literal
module Lits = Literals

module type S = Env_intf.S

module Make(X : sig
  module Ctx : Ctx.S
  val params : Params.t
end) : S with module Ctx = X.Ctx = struct

  module Ctx = X.Ctx
  module C = Clause.Make(Ctx)
  module ProofState = ProofState.Make(C)

  type inf_rule = C.t -> C.t list
  (** An inference returns a list of conclusions *)

  type generate_rule = unit -> C.t list

  type binary_inf_rule = inf_rule
  type unary_inf_rule = inf_rule

  type simplify_rule = C.t -> C.t
  (** Simplify the clause structurally (basic simplifications) *)

  type active_simplify_rule = simplify_rule
  type rw_simplify_rule = simplify_rule

  type backward_simplify_rule = C.t -> C.CSet.t
    (** backward simplification by a unit clause. It returns a set of
        active clauses that can potentially be simplified by the given clause.
        [backward_simplify c] therefore returns a subset of
        [ProofState.ActiveSet.clauses ()] *)

  type redundant_rule = C.t -> bool
    (** check whether the clause is redundant w.r.t the set *)

  type backward_redundant_rule = C.t -> C.CSet.t
  (** find redundant clauses in [ProofState.ActiveSet] w.r.t the clause *)

  type is_trivial_rule = C.t -> bool
    (** Rule that checks whether the clause is trivial (a tautology) *)

  type term_rewrite_rule = FOTerm.t -> FOTerm.t
    (** Rewrite rule on terms *)

  type lit_rewrite_rule = Literal.t -> Literal.t
    (** Rewrite rule on literals *)

  type multi_simpl_rule = C.t -> C.t list option
    (** (maybe) rewrite a clause to a set of clauses.
        Must return [None] if the clause is unmodified *)

  let _binary_rules = ref []
  let _unary_rules = ref []
  let _rewrite_rules = ref []
  let _lit_rules = ref []
  let _basic_simplify = ref []
  let _rw_simplify = ref []
  let _active_simplify = ref []
  let _backward_simplify = ref []
  let _redundant = ref []
  let _backward_redundant = ref []
  let _is_trivial = ref []
  let _empty_clauses = ref C.CSet.empty
  let _multi_simpl_rule = ref []
  let _generate_rules = ref []

  let on_start = Signal.create()
  let on_empty_clause = Signal.create ()

  (** {2 Basic operations} *)

  let add_empty c =
    assert (C.is_empty c);
    _empty_clauses := C.CSet.add !_empty_clauses c;
    Signal.send on_empty_clause c;
    ()

  let add_passive cs =
    ProofState.PassiveSet.add cs;
    Sequence.iter
      (fun c -> if C.is_empty c then add_empty c) cs;
    ()

  let add_active cs =
    ProofState.ActiveSet.add cs;
    Sequence.iter
      (fun c -> if C.is_empty c then add_empty c) cs;
    ()

  let add_simpl cs =
    ProofState.SimplSet.add cs

  let remove_active cs =
    ProofState.ActiveSet.remove cs

  let remove_passive cs =
    ProofState.PassiveSet.remove cs

  let remove_passive_id ids =
    ProofState.PassiveSet.remove_by_id ids

  let remove_simpl cs =
    ProofState.SimplSet.remove cs

  let clean_passive () =
    ProofState.PassiveSet.clean ()

  let get_passive () =
    ProofState.PassiveSet.clauses () |> C.CSet.to_seq

  let get_active () =
    ProofState.ActiveSet.clauses () |> C.CSet.to_seq

  let add_binary_inf name rule =
    if not (List.mem_assoc name !_binary_rules)
      then _binary_rules := (name, rule) :: !_binary_rules

  let add_unary_inf name rule =
    if not (List.mem_assoc name !_unary_rules)
      then _unary_rules := (name, rule) :: !_unary_rules

  let add_generate name rule =
    if not (List.mem_assoc name !_generate_rules)
      then _generate_rules := (name, rule) :: !_generate_rules

  let add_rw_simplify r =
    _rw_simplify := r :: !_rw_simplify

  let add_active_simplify r =
    _active_simplify := r :: !_active_simplify

  let add_backward_simplify r =
    _backward_simplify := r :: !_backward_simplify

  let add_redundant r =
    _redundant := r :: !_redundant

  let add_backward_redundant r =
    _backward_redundant := r :: !_backward_redundant

  let add_simplify r =
    _basic_simplify := r :: !_basic_simplify

  let add_is_trivial r =
    _is_trivial := r :: !_is_trivial

  let add_rewrite_rule name rule =
    _rewrite_rules := (name, rule) :: !_rewrite_rules

  let add_lit_rule name rule =
    _lit_rules := (name, rule) :: !_lit_rules

  let add_multi_simpl_rule rule =
    _multi_simpl_rule := rule :: !_multi_simpl_rule

  let params = X.params

  let get_empty_clauses () =
    !_empty_clauses

  let get_some_empty_clause () =
    C.CSet.choose !_empty_clauses

  let has_empty_clause () =
    not (C.CSet.is_empty !_empty_clauses)

  let ord () = Ctx.ord ()
  let precedence () = Ordering.precedence (ord ())
  let signature () = Ctx.signature ()

  let pp buf () = Printf.bprintf buf "env"

  let fmt out () =
    Format.fprintf out "env(state:@ %a)" ProofState.debug ()

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

  let stats () = ProofState.stats ()

  let _cnf_options = ref []
  let add_cnf_option o = _cnf_options := o :: !_cnf_options

  let cnf set =
    let clauses = Sequence.fold
      (fun cset pf ->
        let f = PF.form pf in
        Util.debug 3 "reduce %a to CNF..." F.pp f;
        (* reduce to CNF this clause *)
        let clauses = Cnf.cnf_of ~opts:!_cnf_options ~ctx:Ctx.skolem f in
        (* now build "proper" clauses, with proof and all *)
        let proof cc =
          match clauses with
          | [[f']] when F.eq f f' -> Proof.adapt_c (PF.proof pf) cc (* keep proof *)
          | _ -> Proof.mk_c_esa ~rule:"cnf" cc [PF.proof pf]
        in
        let clauses = List.map (fun c -> C.of_forms c proof) clauses in
        C.CSet.add_list cset clauses)
      C.CSet.empty (PF.Set.to_seq set)
    in
    (* declaration of new skolem symbols *)
    Util.debug 5 "declare skolem symbols";
    Ctx.add_signature (Skolem.to_signature Ctx.skolem);
    clauses

  let next_passive () =
    ProofState.PassiveSet.next ()

  (** do binary inferences that involve the given clause *)
  let do_binary_inferences c =
    Util.enter_prof prof_generate_binary;
    Util.debug 3 "do binary inferences with current active set: %a"
                C.pp_set (ProofState.ActiveSet.clauses ());
    (* apply every inference rule *)
    let clauses = List.fold_left
      (fun acc (name, rule) ->
        Util.debug 3 "apply binary rule %s" name;
        let new_clauses = rule c in
        List.rev_append new_clauses acc)
      [] !_binary_rules
    in
    Util.exit_prof prof_generate_binary;
    Sequence.of_list clauses

  (** do unary inferences for the given clause *)
  let do_unary_inferences c =
    Util.enter_prof prof_generate_unary;
    Util.debug 3 "do unary inferences";
    (* apply every inference rule *)
    let clauses = List.fold_left
      (fun acc (name, rule) ->
        Util.debug 3 "apply unary rule %s" name;
        let new_clauses = rule c in
        List.rev_append new_clauses acc)
      [] !_unary_rules in
    Util.exit_prof prof_generate_unary;
    Sequence.of_list clauses

  let do_generate () =
    let clauses = List.fold_left
      (fun acc (name,g) ->
        Util.debug 3 "apply generating rule %s" name;
        List.rev_append (g()) acc
      ) [] !_generate_rules
    in
    Sequence.of_list clauses

  let is_trivial c =
    if C.get_flag C.flag_persistent c then false else
    match !_is_trivial with
    | [] -> false
    | [f] -> f c
    | [f;g] -> f c || g c
    | l -> List.exists (fun f -> f c) l

  let is_active c =
    C.CSet.mem (ProofState.ActiveSet.clauses ()) c

  let is_passive c =
    C.CSet.mem (ProofState.PassiveSet.clauses ()) c

  (** Apply rewrite rules AND evaluation functions *)
  let rewrite c =
    Util.debug 5 "rewrite clause %a..." C.pp c;
    let applied_rules = ref (SmallSet.empty ~cmp:String.compare) in
    let rec reduce_term rules t =
      match rules with
      | [] -> t
      | (name, r)::rules' ->
        let t' = r t in
        if t != t'
          then begin
            applied_rules := SmallSet.add !applied_rules name;
            Util.debug 5 "Env: rewrite %a into %a" T.pp t T.pp t';
            reduce_term !_rewrite_rules t'  (* re-apply all rules *)
          end else reduce_term rules' t  (* try next rule *)
    in
    (* reduce every literal *)
    let lits' = Array.map
      (fun lit -> Lit.map (reduce_term !_rewrite_rules) lit)
      (C.lits c)
    in
    if SmallSet.is_empty !applied_rules
      then c (* no simplification *)
      else begin
        let rule = "rw_" ^ (String.concat "_" (SmallSet.to_list !applied_rules)) in
        let proof c' = Proof.mk_c_simp ~rule c' [C.proof c] in
        let parents = [c] in
        let new_clause = C.create_a ~parents lits' proof in
        Util.debug 3 "Env: term rewritten clause %a into %a" C.pp c C.pp new_clause;
        new_clause
      end

  (** Apply literal rewrite rules *)
  let rewrite_lits c =
    let applied_rules = ref (SmallSet.empty ~cmp:String.compare) in
    let rec rewrite_lit rules lit = match rules with
    | [] -> lit
    | (name,r)::rules' ->
      let lit' = r lit in  (* apply the rule *)
      if Lit.eq_com lit lit'
        then rewrite_lit rules' lit
        else begin
          applied_rules := SmallSet.add !applied_rules name;
          Util.debug 5 "Env: rewritten lit %a into %a (using %s)"
            Lit.pp lit Lit.pp lit' name;
          rewrite_lit !_lit_rules lit'
        end
    in
    (* apply lit rules *)
    let c = C.follow_simpl c in
    let lits = Array.map (fun lit -> rewrite_lit !_lit_rules lit) (C.lits c) in
    if Lits.eq_com lits (C.lits c) then c
    else begin  (* simplifications occurred! *)
      let rule = "lit_rw_" ^ (String.concat "_" (SmallSet.to_list !applied_rules)) in
      let proof c' = Proof.mk_c_simp ~rule c' [C.proof c]  in
      let parents = [c] in
      let new_clause = C.create_a ~parents lits proof in
      Util.debug 3 "Env: lit rewritten %a into %a" C.pp c C.pp new_clause;
      new_clause
    end

  (** All basic simplification of the clause itself *)
  let rec basic_simplify c =
    (* first, rewrite terms *)
    let c = rewrite c in
    let c = C.follow_simpl c in
    (* rewrite literals (if needed) *)
    let c = match !_lit_rules with
    | [] -> c
    | l -> rewrite_lits c
    in
    let c = C.follow_simpl c in
    (* apply simplifications *)
    let c' = match !_basic_simplify with
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
        basic_simplify c'  (* fixpoint *)
      end

  (* rewrite clause with simpl_set *)
  let rec rw_simplify c =
    if C.get_flag C.flag_persistent c then c else
    let c = C.follow_simpl c in
    let c' = match !_rw_simplify with
    | [] -> c
    | [f] -> f c
    | [f;g] -> g (f c)
    | l -> List.fold_left (fun c f -> f c) c l
    in
    if C.eq c c'
      then c'
      else
        let _ = C.simpl_to ~from:c ~into:c' in
        rw_simplify c'

  (* simplify clause w.r.t. active set *)
  let rec active_simplify c =
    if C.get_flag C.flag_persistent c then c else
    let c' = match !_active_simplify with
    | [] -> c
    | [f] -> f c
    | [f;g] -> f (g c)
    | l -> List.fold_left (fun c f -> f c) c l
    in
    if C.eq c c'
      then c'
      else
        let _ = C.simpl_to ~from:c ~into:c' in
        active_simplify c'

  (** Simplify the hclause. Returns both the hclause and its simplification. *)
  let simplify old_c =
    (* fixpoint *)
    let rec fix old_c =
      let c = old_c in
      let c = basic_simplify c in
      (* simplify with unit clauses, then all active clauses *)
      let c = rewrite c in
      let c = rw_simplify c in
      let c = basic_simplify c in
      let c = active_simplify c in
      if not (Lits.eq_com (C.lits c) (C.lits old_c))
        then begin
          Util.debug 2 "clause %a simplified into %a" C.pp old_c C.pp c;
          C.simpl_to old_c c;
          fix c
        end else
          c
    in
    Util.enter_prof prof_simplify;
    let c = fix old_c in
    Util.exit_prof prof_simplify;
    old_c, c

  let multi_simplify c =
    let did_something = ref false in
    (* try rules one by one until some of them succeeds *)
    let rec try_next c rules = match rules with
      | [] -> None
      | r::rules' ->
          match r c with
          | Some l -> Some l
          | None -> try_next c rules'
    in
    (* fixpoint of [try_next] *)
    let set = ref C.CSet.empty in
    let q = Queue.create () in
    Queue.push c q;
    while not (Queue.is_empty q) do
      let c = Queue.pop q in
      if not (C.CSet.mem !set c) then begin
        let c = basic_simplify c in
        match try_next c !_multi_simpl_rule with
        | None ->
            (* keep the clause! *)
            set := C.CSet.add !set c;
        | Some l ->
            did_something := true;
            List.iter (fun c -> Queue.push c q) l;
      end
    done;
    if !did_something
      then Some (C.CSet.to_list !set)
      else None

  (* find candidates for backward simplification in active set *)
  let backward_simplify_find_candidates given =
    match !_backward_simplify with
    | [] -> C.CSet.empty
    | [f] -> f given
    | [f;g] -> C.CSet.union (f given) (g given)
    | l -> List.fold_left (fun set f -> C.CSet.union set (f given)) C.CSet.empty l

  (** Perform backward simplification with the given clause *)
  let backward_simplify given =
    Util.enter_prof prof_back_simplify;
    (* set of candidate clauses, that may be unit-simplifiable *)
    let candidates = backward_simplify_find_candidates given in
    (* try to simplify the candidates. Before is the set of clauses that
       are simplified, after is the list of those clauses after simplification *)
    let before, after =
      C.CSet.fold candidates (C.CSet.empty, [])
        (fun (before, after) _ c ->
          let c' = rw_simplify c in
          if not (Lits.eq (C.lits c) (C.lits c'))
            (* the active clause has been simplified! *)
            then begin
              Util.debug 2 "active clause %a simplified into %a" C.pp c C.pp c';
              C.CSet.add before c, c' :: after
            end else before, after)
    in
    Util.exit_prof prof_back_simplify;
    before, Sequence.of_list after

  let simplify_active_with f =
    let set = C.CSet.fold
      (ProofState.ActiveSet.clauses ()) []
      (fun set _id c ->
        match f c with
        | None -> set
        | Some c'_list ->
            let c''_list = List.map basic_simplify c'_list in
            Util.debug 3 "active clause %a simplified into clauses %a"
              C.pp c (CCList.pp C.pp) c''_list;
            (c, c''_list) :: set
      )
    in
    (* remove clauses from active set, put their simplified version into
        the passive set for further processing *)
    ProofState.ActiveSet.remove (Sequence.of_list set |> Sequence.map fst);
    Sequence.of_list set
      |> Sequence.map snd
      |> Sequence.map Sequence.of_list
      |> Sequence.flatten
      |> ProofState.PassiveSet.add;
    ()

  (** Simplify the clause w.r.t to the active set *)
  let forward_simplify c =
    let c = C.follow_simpl c in
    let c = rewrite c in
    let c = rw_simplify c in
    let c = basic_simplify c in
    c

  (** generate all clauses from inferences *)
  let generate given =
    Util.enter_prof prof_generate;
    (* binary clauses *)
    let binary_clauses = do_binary_inferences given in
    (* unary inferences *)
    let unary_clauses = ref []
    and unary_queue = Queue.create () in
    Queue.push (given, 0) unary_queue;
    while not (Queue.is_empty unary_queue) do
      let c, depth = Queue.pop unary_queue in
      let c = basic_simplify c in (* simplify a bit the clause *)
      if not (is_trivial c) then begin
        (* add the clause to set of inferred clauses, if it's not the original clause *)
        (if depth > 0 then unary_clauses := c :: !unary_clauses);
        if depth < params.Params.param_unary_depth
          then begin
            (* infer clauses from c, add them to the queue *)
            let new_clauses = do_unary_inferences c in
            Sequence.iter
              (fun c' -> Queue.push (c', depth+1) unary_queue)
              new_clauses
          end
      end
    done;
    (* generating rules *)
    let other_clauses = do_generate () in
    (* combine all clauses *)
    let result = Sequence.(
      append
        (of_list !unary_clauses)
        (append binary_clauses other_clauses))
    in
    Util.add_stat stat_inferred (Sequence.length result);
    Util.exit_prof prof_generate;
    result

  (** remove direct descendants of the clauses from the passive set *)
  let remove_orphans removed_clauses =
    (* remove descendants of the clause. If the descendants are redundant
       (cf C.flag_redundant) their descendants are also removed *)
    let remove_descendants c =
      let orphans = C.descendants c in
      (* remove orphans from passive set *)
      SmallSet.to_seq orphans
        |> ProofState.PassiveSet.remove_by_id;
        for _i=1 to SmallSet.size orphans do
          Util.incr_stat stat_killed_orphans
        done;
    in
    Sequence.iter remove_descendants removed_clauses

  (** check whether the clause is redundant w.r.t the current active_set *)
  let is_redundant c =
    Util.enter_prof prof_is_redundant;
    let res = match !_redundant with
    | [] -> false
    | [f] -> f c
    | [f;g] -> f c || g c
    | l -> List.exists (fun f -> f c) l
    in
    Util.exit_prof prof_is_redundant;
    res

  (** find redundant clauses in current active_set *)
  let subsumed_by c =
    Util.enter_prof prof_subsumed_by;
    let res = match !_backward_redundant with
    | [] -> C.CSet.empty
    | [f] -> f c
    | [f;g] -> C.CSet.union (f c) (g c)
    | l -> List.fold_left (fun set f -> C.CSet.union set (f c)) C.CSet.empty l
    in
    Util.exit_prof prof_subsumed_by;
    res

  (** Use all simplification rules to convert a clause into a list of
      maximally simplified clauses *)
  let all_simplify c =
    Util.enter_prof prof_all_simplify;
    let set = ref C.CSet.empty in
    let q = Queue.create () in
    Queue.push c q;
    while not (Queue.is_empty q) do
      let c = Queue.pop q in
      let _, c' = simplify c in
      if is_trivial c' || is_redundant c'
        then ()
        else match multi_simplify c' with
        | None ->
            (* clause has reached fixpoint *)
            set := C.CSet.add !set c'
        | Some l ->
            List.iter (fun c -> Queue.push c q) l
    done;
    let res = C.CSet.to_list !set in
    Util.exit_prof prof_all_simplify;
    res

  (** {2 Misc} *)

  let mixtbl = Mixtbl.create 15
end
