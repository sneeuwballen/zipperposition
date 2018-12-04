
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Global environment for an instance of the prover} *)

open Logtk

module T = Term
module Lit = Literal
module Lits = Literals
module P = Proof

let section = Util.Section.make ~parent:Const.section "env"

let stat_inferred = Util.mk_stat "env.inferred clauses"

let prof_generate = Util.mk_profiler "env.generate"
let prof_generate_unary = Util.mk_profiler "env.generate_unary"
let prof_generate_binary = Util.mk_profiler "env.generate_binary"
let prof_back_simplify = Util.mk_profiler "env.back_simplify"
let prof_simplify = Util.mk_profiler "env.simplify"
let prof_all_simplify = Util.mk_profiler "env.all_simplify"
let prof_is_redundant = Util.mk_profiler "env.is_redundant"
let prof_subsumed_by = Util.mk_profiler "env.subsumed_by"

(** {2 Signature} *)
module type S = Env_intf.S

type 'a packed = (module S with type C.t = 'a)

module Make(X : sig
    module Ctx : Ctx.S
    val params : Params.t
    val flex_state : Flex_state.t
  end)
  : S with module Ctx = X.Ctx
= struct
  module Ctx = X.Ctx
  module C = Clause.Make(Ctx)
  module ProofState = ProofState.Make(C)


  type inf_rule = C.t -> C.t list
  (** An inference returns a list of conclusions *)

  type generate_rule = full:bool -> unit -> C.t list
  (** Generation of clauses regardless of current clause *)

  type binary_inf_rule = inf_rule
  type unary_inf_rule = inf_rule

  type simplify_rule = C.t -> C.t SimplM.t
  (** Simplify the clause structurally (basic simplifications),
      in the simplification monad.
      [(c, `Same)] means the clause has not been simplified;
      [(c, `New)] means the clause has been simplified at least once *)

  type active_simplify_rule = simplify_rule
  type rw_simplify_rule = simplify_rule

  type backward_simplify_rule = C.t -> C.ClauseSet.t
  (** backward simplification by a unit clause. It returns a set of
      active clauses that can potentially be simplified by the given clause.
      [backward_simplify c] therefore returns a subset of
      [ProofState.ActiveSet.clauses ()] *)

  type redundant_rule = C.t -> bool
  (** check whether the clause is redundant w.r.t the set *)

  type backward_redundant_rule = C.ClauseSet.t -> C.t -> C.ClauseSet.t
  (** find redundant clauses in [ProofState.ActiveSet] w.r.t the clause.
       first param is the set of already known redundant clause, the rule
       should add clauses to it *)

  type is_trivial_trail_rule = Trail.t -> bool
  (** Rule that checks whether the trail is trivial (a tautology) *)

  type is_trivial_rule = C.t -> bool
  (** Rule that checks whether the clause is trivial (a tautology) *)

  type term_rewrite_rule = Term.t -> (Term.t * Proof.parent list) option
  (** Rewrite rule on terms *)

  type lit_rewrite_rule = Literal.t -> (Literal.t * Proof.parent list * Proof.tag list) option
  (** Rewrite rule on literals *)

  type multi_simpl_rule = C.t -> C.t list option
  (** (maybe) rewrite a clause to a set of clauses.
      Must return [None] if the clause is unmodified *)

  type 'a conversion_result =
    | CR_skip (** rule didn't fire *)
    | CR_add of 'a (** add this to the result *)
    | CR_return of 'a (** shortcut the remaining rules, return this *)

  type clause_conversion_rule = Statement.clause_t -> C.t list conversion_result
  (** A hook to convert a particular statement into a list
      of clauses *)

  let _binary_rules : (string * binary_inf_rule) list ref = ref []
  let _unary_rules : (string * unary_inf_rule) list ref = ref []
  let _rewrite_rules : (string * term_rewrite_rule) list ref = ref []
  let _lit_rules : (string * lit_rewrite_rule) list ref = ref []
  let _basic_simplify : simplify_rule list ref = ref []
  let _unary_simplify : simplify_rule list ref = ref []
  let _rw_simplify = ref []
  let _active_simplify = ref []
  let _backward_simplify = ref []
  let _redundant = ref []
  let _backward_redundant : backward_redundant_rule list ref = ref []
  let _is_trivial_trail : is_trivial_trail_rule list ref = ref []
  let _is_trivial : is_trivial_rule list ref = ref []
  let _empty_clauses = ref C.ClauseSet.empty
  let _multi_simpl_rule : multi_simpl_rule list ref = ref []
  let _generate_rules : (string * generate_rule) list ref = ref []
  let _clause_conversion_rules : clause_conversion_rule list ref = ref []
  let _step_init = ref []

  let on_start = Signal.create()
  let on_input_statement = Signal.create()
  let on_empty_clause = Signal.create ()

  (** {2 Basic operations} *)

  let add_empty c =
    assert (C.is_empty c);
    _empty_clauses := C.ClauseSet.add c !_empty_clauses;
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

  let remove_simpl cs =
    ProofState.SimplSet.remove cs

  let get_passive () =
    ProofState.PassiveSet.clauses () |> C.ClauseSet.to_seq

  let get_active () =
    ProofState.ActiveSet.clauses () |> C.ClauseSet.to_seq

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

  let add_basic_simplify r =
    _basic_simplify := r :: !_basic_simplify

  let add_unary_simplify r =
    _unary_simplify := r :: !_unary_simplify

  let add_is_trivial_trail r =
    _is_trivial_trail := r :: !_is_trivial_trail

  let add_is_trivial r =
    _is_trivial := r :: !_is_trivial

  let add_rewrite_rule name rule =
    _rewrite_rules := (name, rule) :: !_rewrite_rules

  let add_lit_rule name rule =
    _lit_rules := (name, rule) :: !_lit_rules

  let add_multi_simpl_rule rule =
    _multi_simpl_rule := rule :: !_multi_simpl_rule

  let cr_skip = CR_skip
  let cr_add x = CR_add x
  let cr_return x = CR_return x

  let add_clause_conversion r =
    _clause_conversion_rules := r :: !_clause_conversion_rules

  let add_step_init f = _step_init := f :: !_step_init

  let params = X.params

  let get_empty_clauses () =
    !_empty_clauses

  let get_some_empty_clause () =
    try Some (C.ClauseSet.choose !_empty_clauses)
    with Not_found -> None

  let has_empty_clause () =
    not (C.ClauseSet.is_empty !_empty_clauses)

  let ord () = Ctx.ord ()
  let precedence () = Ordering.precedence (ord ())
  let signature () = Ctx.signature ()

  let pp out () = CCFormat.string out "env"

  let pp_full out () =
    Format.fprintf out "@[<hv2>env(state:@ %a@,)@]" ProofState.debug ()

  (** {2 High level operations} *)

  type stats = int * int * int

  let stats () = ProofState.stats ()

  let next_passive () =
    ProofState.PassiveSet.next ()

  (** do binary inferences that involve the given clause *)
  let do_binary_inferences c =
    Util.enter_prof prof_generate_binary;
    Util.debugf ~section 5 "@[<2>do binary inferences with current active set:@ `@[%a@]`@]"
      (fun k->k C.pp_set (ProofState.ActiveSet.clauses ()));
    (* apply every inference rule *)
    let clauses =
      List.fold_left
        (fun acc (name, rule) ->
           Util.debugf ~section 3 "apply binary rule %s" (fun k->k name);
           let new_clauses = rule c in
           List.rev_append new_clauses acc)
        [] !_binary_rules
    in
    Util.exit_prof prof_generate_binary;
    Sequence.of_list clauses

  (** do unary inferences for the given clause *)
  let do_unary_inferences c =
    Util.enter_prof prof_generate_unary;
    Util.debug ~section 3 "do unary inferences";
    (* apply every inference rule *)
    let clauses = List.fold_left
        (fun acc (name, rule) ->
           Util.debugf ~section 3 "apply unary rule %s" (fun k->k name);
           let new_clauses = rule c in
           List.rev_append new_clauses acc)
        [] !_unary_rules in
    Util.exit_prof prof_generate_unary;
    Sequence.of_list clauses

  let do_generate ~full () =
    let clauses =
      List.fold_left
        (fun acc (name,g) ->
           Util.debugf ~section 3 "apply generating rule %s" (fun k->k name);
           List.rev_append (g ~full ()) acc)
        []
        !_generate_rules
    in
    Sequence.of_list clauses

  let is_trivial_trail trail = match !_is_trivial_trail with
    | [] -> false
    | [f] -> f trail
    | [f1;f2] -> f1 trail || f2 trail
    | l -> List.exists (fun f -> f trail) l

  let is_trivial c =
    if C.get_flag SClause.flag_persistent c then false
    else (
      let res =
        C.is_redundant c
        || is_trivial_trail (C.trail c)
        || begin match !_is_trivial with
          | [] -> false
          | [f] -> f c
          | [f;g] -> f c || g c
          | l -> List.exists (fun f -> f c) l
        end
      in
      if res then C.mark_redundant c;
      res
    )

  let is_active c =
    C.ClauseSet.mem c (ProofState.ActiveSet.clauses ())

  let is_passive c =
    C.ClauseSet.mem c (ProofState.PassiveSet.clauses ())

  module StrSet = CCSet.Make(String)

  (** Apply rewrite rules AND evaluation functions *)
  let rewrite c =
    Util.debugf ~section 5 "@[<2>rewrite clause@ `@[%a@]`...@]" (fun k->k C.pp c);
    let applied_rules = ref StrSet.empty in
    let proofs : Proof.parent list ref = ref [] in
    let rec reduce_term rules t =
      match rules with
        | [] -> t
        | (name, r)::rules' ->
          begin match r t with
            | None -> reduce_term rules' t (* try next rules *)
            | Some (t',proof) ->
              applied_rules := StrSet.add name !applied_rules;
              proofs := List.rev_append proof !proofs;
              Util.debugf ~section 5
                "@[<2>rewrite `@[%a@]`@ into `@[%a@]`@ :proof (@[%a@])@]"
                (fun k->k T.pp t T.pp t' (Util.pp_list Proof.pp_parent) proof);
              reduce_term !_rewrite_rules t'  (* re-apply all rules *)
          end
    in
    (* reduce every literal *)
    let lits' =
      Array.map
        (fun lit -> Lit.map (reduce_term !_rewrite_rules) lit)
        (C.lits c)
    in
    if StrSet.is_empty !applied_rules
    then SimplM.return_same c (* no simplification *)
    else (
      C.mark_redundant c;
      (* FIXME: put the rules as parameters *)
      let rule = Proof.Rule.mk "rw" in
      let proof =
        Proof.Step.simp ~rule
          (C.proof_parent c :: !proofs)
      in
      let c' = C.create_a ~trail:(C.trail c) ~penalty:(C.penalty c) lits' proof in
      assert (not (C.equal c c'));
      Util.debugf ~section 3 "@[term rewritten clause `@[%a@]`@ into `@[%a@]`"
        (fun k->k C.pp c C.pp c');
      SimplM.return_new c'
    )

  (** Apply literal rewrite rules *)
  let rewrite_lits c =
    let applied_rules = ref StrSet.empty in
    let proofs : Proof.parent list ref = ref [] in
    let tags : Proof.tag list ref = ref [] in
    let rec rewrite_lit rules lit = match rules with
      | [] -> lit
      | (name,r)::rules' ->
        match r lit with
          | None -> rewrite_lit rules' lit
          | Some (lit',proof,tgs) ->
            applied_rules := StrSet.add name !applied_rules;
            proofs := List.rev_append proof !proofs;
            tags := List.rev_append tgs !tags;
            Util.debugf ~section 5
              "@[rewritten lit `@[%a@]`@ into `@[%a@]`@ (using %s)@ \
               :proof (@[%a@]) :tags %a@]"
              (fun k->k Lit.pp lit Lit.pp lit' name
                  (Util.pp_list Proof.pp_parent) proof Proof.pp_tags tgs);
            rewrite_lit !_lit_rules lit'
    in
    (* apply lit rules *)
    let lits = Array.map (fun lit -> rewrite_lit !_lit_rules lit) (C.lits c) in
    if StrSet.is_empty !applied_rules
    then SimplM.return_same c
    else (
      (* simplifications occurred! *)
      C.mark_redundant c;
      (* FIXME: put the rules as parameters *)
      let rule = Proof.Rule.mk "rw_lit" in
      let proof =
        Proof.Step.simp ~rule ~tags:!tags
          (C.proof_parent c :: !proofs)
      in
      let c' = C.create_a ~trail:(C.trail c) ~penalty:(C.penalty c) lits proof in
      assert (not (C.equal c c'));
      Util.debugf ~section 3 "@[lit rewritten `@[%a@]`@ into `@[%a@]`@]"
        (fun k->k C.pp c C.pp c');
      SimplM.return_new c'
    )

  (* apply simplification in a fixpoint *)
  let rec fix_simpl
    : f:(C.t -> C.t SimplM.t) -> C.t -> C.t SimplM.t
    = fun ~f c ->
      let open SimplM.Infix in
      let new_c = f c in
      if C.equal c (SimplM.get new_c)
      then new_c (* fixpoint reached *)
      else (
        (* some progress was made *)
        C.mark_redundant c;
        new_c >>= fix_simpl ~f
      )

  let basic_simplify c =
    let open SimplM.Infix in
    begin match !_basic_simplify with
      | [] -> SimplM.return_same c
      | [f] -> f c
      | [f;g] -> f c >>= g
      | l -> SimplM.app_list l c
    end

  (* All basic simplification of the clause itself *)
  let unary_simplify c =
    let open SimplM.Infix in
    fix_simpl c
      ~f:(fun c ->
        basic_simplify c >>= fun c ->
        (* first, rewrite terms *)
        rewrite c >>= fun c ->
        (* rewrite literals (if needed) *)
        begin match !_lit_rules with
          | [] -> SimplM.return_same c
          | _::_ -> rewrite_lits c
        end
        >>= fun c ->
        (* apply simplifications *)
        begin match !_unary_simplify with
          | [] -> SimplM.return_same c
          | [f] -> f c
          | [f;g] -> f c >>= g
          | l -> SimplM.app_list l c
        end)

  (* rewrite clause with simpl_set *)
  let rw_simplify c =
    let open SimplM.Infix in
    fix_simpl c
      ~f:(fun c ->
        if C.get_flag SClause.flag_persistent c
        then SimplM.return_same c
        else match !_rw_simplify with
          | [] -> SimplM.return_same c
          | [f] -> f c
          | [f;g] -> f c >>= g
          | l -> SimplM.app_list l c)

  (* simplify clause w.r.t. active set *)
  let active_simplify c =
    let open SimplM.Infix in
    fix_simpl c
      ~f:(fun c ->
        if C.get_flag SClause.flag_persistent c
        then SimplM.return_same c
        else match !_active_simplify with
          | [] -> SimplM.return_same c
          | [f] -> f c
          | [f;g] -> f c >>= g
          | l -> SimplM.app_list l c)

  let simplify c =
    let open SimplM.Infix in
    Util.enter_prof prof_simplify;
    let res = fix_simpl c
        ~f:(fun c ->
          let old_c = c in
          basic_simplify c >>=
          (* simplify with unit clauses, then all active clauses *)
          rewrite >>=
          rw_simplify >>=
          unary_simplify >>=
          active_simplify >|= fun c ->
          if not (Lits.equal_com (C.lits c) (C.lits old_c))
          then
            Util.debugf ~section 2 "@[clause `@[%a@]`@ simplified into `@[%a@]`@]"
              (fun k->k C.pp old_c C.pp c);
          c)
    in
    Util.exit_prof prof_simplify;
    res

  let multi_simplify c : C.t list option =
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
    let set = ref C.ClauseSet.empty in
    let q = Queue.create () in
    Queue.push c q;
    while not (Queue.is_empty q) do
      let c = Queue.pop q in
      if not (C.ClauseSet.mem c !set) then (
        let c, st = unary_simplify c in
        if st = `New then did_something := true;
        match try_next c !_multi_simpl_rule with
          | None ->
            (* keep the clause! *)
            set := C.ClauseSet.add c !set;
          | Some l ->
            did_something := true;
            List.iter (fun c -> Queue.push c q) l;
      )
    done;
    if !did_something
    then (
      C.mark_redundant c;
      Some (C.ClauseSet.to_list !set)
    )
    else None

  (* find candidates for backward simplification in active set *)
  let backward_simplify_find_candidates given =
    match !_backward_simplify with
      | [] -> C.ClauseSet.empty
      | [f] -> f given
      | [f;g] -> C.ClauseSet.union (f given) (g given)
      | l ->
        List.fold_left
          (fun set f -> C.ClauseSet.union set (f given))
          C.ClauseSet.empty l

  (* Perform backward simplification with the given clause *)
  let backward_simplify given =
    Util.enter_prof prof_back_simplify;
    (* set of candidate clauses, that may be unit-simplifiable *)
    let candidates = backward_simplify_find_candidates given in
    let back_simplify c =
      let open SimplM.Infix in
      fix_simpl c
        ~f:(fun c ->
          let old_c = c in
          basic_simplify c >>=
          (* simplify with unit clauses, then all active clauses *)
          rewrite >>=
          rw_simplify >>=
          unary_simplify >|= fun c ->
          if not (Lits.equal_com (C.lits c) (C.lits old_c)) then (
            Util.debugf ~section 2 "@[clause `@[%a@]`@ simplified into `@[%a@]`@]"
              (fun k->k C.pp old_c C.pp c);
          );
          c)
    in
    (* try to simplify the candidates. Before is the set of clauses that
       are simplified, after is the list of those clauses after simplification *)
    let before, after =
      C.ClauseSet.fold
        (fun c (before, after) ->
           let c', is_new = back_simplify c in
           begin match is_new with
             | `Same ->
               if is_trivial c'
               then C.ClauseSet.add c before, after (* just remove the clause *)
               else before, after
             | `New ->
               (* the active clause has been backward simplified! *)
               C.mark_redundant c;
               C.mark_backward_simplified c;
               Util.debugf ~section 2
                 "@[active clause `@[%a@]@ simplified into `@[%a@]`@]"
                 (fun k->k C.pp c C.pp c');
               C.ClauseSet.add c before, c' :: after
           end)
        candidates (C.ClauseSet.empty, [])
    in
    Util.exit_prof prof_back_simplify;
    before, Sequence.of_list after

  let simplify_active_with f =
    let set =
      C.ClauseSet.fold
        (fun c set ->
           match f c with
             | None -> set
             | Some clauses ->
               let redundant, clauses =
                 CCList.fold_map
                   (fun red c ->
                      let c', is_new = unary_simplify c in
                      (red || is_new=`New), c')
                   false clauses
               in
               if redundant then C.mark_redundant c;
               Util.debugf ~section 3
                 "@[active clause `@[%a@]`@ simplified into clauses `@[%a@]`@]"
                 (fun k->k C.pp c (CCFormat.list C.pp) clauses);
               (c, clauses) :: set)
        (ProofState.ActiveSet.clauses ()) []
    in
    (* remove clauses from active set, put their simplified version into
        the passive set for further processing *)
    ProofState.ActiveSet.remove (Sequence.of_list set |> Sequence.map fst);
    Sequence.of_list set
    |> Sequence.map snd
    |> Sequence.flat_map Sequence.of_list
    |> ProofState.PassiveSet.add;
    ()

  (** Simplify the clause w.r.t to the active set *)
  let forward_simplify c =
    let open SimplM.Infix in
    rewrite c >>= rw_simplify >>= unary_simplify

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
      let c, _ = unary_simplify c in (* simplify a bit the clause *)
      if not (is_trivial c) then (
        (* add the clause to set of inferred clauses, if it's not the original clause *)
        if depth > 0 then unary_clauses := c :: !unary_clauses;
        if depth < params.Params.unary_depth
        then (
          (* infer clauses from c, add them to the queue *)
          let new_clauses = do_unary_inferences c in
          Sequence.iter
            (fun c' -> Queue.push (c', depth+1) unary_queue)
            new_clauses
        )
      )
    done;
    (* generating rules *)
    let other_clauses = do_generate ~full:false () in
    (* combine all clauses *)
    let result = Sequence.(
        append
          (of_list !unary_clauses)
          (append binary_clauses other_clauses))
    in
    Util.add_stat stat_inferred (Sequence.length result);
    Util.exit_prof prof_generate;
    result

  (* check whether the clause is redundant w.r.t the current active_set *)
  let is_redundant_ c =
    let res = match !_redundant with
      | [] -> false
      | [f] -> f c
      | [f;g] -> f c || g c
      | l -> List.exists (fun f -> f c) l
    in
    if res then C.mark_redundant c;
    res

  let is_redundant c =
    C.is_redundant c
    || Util.with_prof prof_is_redundant is_redundant_ c

  (** find redundant clauses in current active_set *)
  let subsumed_by c =
    Util.enter_prof prof_subsumed_by;
    let res =
      List.fold_left
        (fun set rule -> rule set c)
        C.ClauseSet.empty
        !_backward_redundant
    in
    (* all those clauses are redundant *)
    C.ClauseSet.iter C.mark_redundant res;
    Util.exit_prof prof_subsumed_by;
    res

  (** Use all simplification rules to convert a clause into a list of
      maximally simplified clauses *)
  let all_simplify c =
    Util.enter_prof prof_all_simplify;
    let did_simplify = ref false in
    let set = ref C.ClauseSet.empty in
    let q = Queue.create () in
    Queue.push c q;
    while not (Queue.is_empty q) do
      let c = Queue.pop q in
      let c, st = simplify c in
      if st=`New then did_simplify := true;
      if is_trivial c || is_redundant c
      then ()
      else match multi_simplify c with
        | None ->
          (* clause has reached fixpoint *)
          set := C.ClauseSet.add c !set
        | Some l ->
          (* continue processing *)
          did_simplify := true;
          List.iter (fun c -> Queue.push c q) l
    done;
    let res = C.ClauseSet.to_list !set in
    Util.exit_prof prof_all_simplify;
    if !did_simplify
    then SimplM.return_new res
    else SimplM.return_same res

  let step_init () = List.iter (fun f -> f()) !_step_init

  let is_lemma_ st = match Statement.view st with
    | Statement.Lemma _ -> true
    | _ -> false

  let has_sos_attr st =
    CCList.exists
      (function Statement.A_sos -> true | _ -> false)
      (Statement.attrs st)

  let convert_input_statements stmts : C.t Clause.sets =
    Util.debug ~section 2 "trigger on_input_statement";
    CCVector.iter (Signal.send on_input_statement) stmts;
    (* sets of clauses *)
    let c_set = CCVector.create() in
    let c_sos = CCVector.create() in
    (* convert clauses, applying hooks when possible *)
    let rec conv_clause_ rules st = match rules with
      | [] when is_lemma_ st ->
        Util.warnf "@[drop lemma `%a`@]" Statement.pp_clause st;
        []
      | [] -> C.of_statement st
      | r :: rules' ->
        begin match r st with
          | CR_skip -> conv_clause_ rules' st
          | CR_return l -> l
          | CR_add l -> List.rev_append l (conv_clause_ rules' st)
        end
    in
    CCVector.iter
      (fun st ->
         let cs = conv_clause_ !_clause_conversion_rules st in
         begin match Statement.view st with
           | Statement.Assert _ when has_sos_attr st ->
             CCVector.append_list c_sos cs
           | _ -> CCVector.append_list c_set cs
         end)
      stmts;
    Util.debugf ~section 1
      "@[<v>@[<2>clauses:@ @[<v>%a@]@]@ @[<2>sos:@ @[<v>%a@]@]@]"
      (fun k->k
          (Util.pp_seq ~sep:" " C.pp) (CCVector.to_seq c_set)
          (Util.pp_seq ~sep:" " C.pp) (CCVector.to_seq c_sos));
    let c_set = CCVector.freeze c_set in
    let c_sos = CCVector.freeze c_sos in
    { Clause.c_set; c_sos; }

  (** {2 Misc} *)

  let flex_state_ = ref X.flex_state
  let flex_state () = !flex_state_
  let update_flex_state f = CCRef.update f flex_state_
  let flex_add k v = flex_state_ := Flex_state.add k v !flex_state_
  let flex_get k = Flex_state.get_exn k !flex_state_
end

