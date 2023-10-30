
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Global environment for an instance of the prover} *)

open Logtk

module T = Term
module Lit = Literal
module Lits = Literals
module P = Proof
module IntSet = Set.Make(CCInt)

let section = Util.Section.make ~parent:Const.section "env"

let stat_inferred = Util.mk_stat "env.inferred clauses"

let prof_generate = ZProf.make "env.generate"
let prof_generate_unary = ZProf.make "env.generate_unary"
let prof_generate_binary = ZProf.make "env.generate_binary"
let prof_back_simplify = ZProf.make "env.back_simplify"
let prof_simplify = ZProf.make "env.simplify"
let prof_multi_simplify = ZProf.make "env.multi_simplify"
let prof_fwd_simplify = ZProf.make "env.fwd_simplify"
let prof_all_simplify = ZProf.make "env.all_simplify"
let prof_is_redundant = ZProf.make "env.is_redundant"
let prof_subsumed_by = ZProf.make "env.subsumed_by"
let prof_cheap_multi = ZProf.make "env.cheap_multi_simp"

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
  module FormRename = FormulaRename.Make(C)

  let flex_state_ = ref X.flex_state
  let flex_state () = !flex_state_

  let k_max_multi_simpl_depth = Flex_state.create_key ()

  module Stm = Stream.Make(struct
      module Ctx = Ctx
      module C = C
    end)
  module StmQ = StreamQueue.Make(struct
      module Stm = Stm
      let state = fun () ->  !flex_state_
  end)

  type inf_rule = C.t -> C.t list
  (** An inference returns a list of conclusions *)

  type generate_rule = full:bool -> unit -> C.t list
  (** Generation of clauses regardless of current clause *)

  type clause_elim_rule = unit -> unit
  (** Eliminates clauses from the proof state using algorithms
      like blocked clause elimination and similar *)

  type binary_inf_rule = inf_rule
  type unary_inf_rule = inf_rule

  type simplify_rule = C.t -> C.t SimplM.t
  (** Simplify the clause structurally (basic simplifications),
      in the simplification monad.
      [(c, `Same)] means the clause has not been simplified;
      [(c, `New)] means the clause has been simplified at least once *)


  type term_norm_rule = Term.t -> Term.t option
  (** Normalization rule on terms *)

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

  type immediate_simplification_rule = C.t -> C.t Iter.t -> C.t Iter.t option
  type 'a conversion_result =
    | CR_skip (** rule didn't fire *)
    | CR_drop (** remove the clause from proof state *)
    | CR_add of 'a (** add this to the result *)
    | CR_return of 'a (** shortcut the remaining rules, return this *)

  type clause_conversion_rule = Statement.clause_t -> C.t list conversion_result
  (** A hook to convert a particular statement into a list
      of clauses *)

  let _binary_rules : (string * binary_inf_rule) list ref = ref []
  let _unary_rules : (string * unary_inf_rule) list ref = ref []
  let _rewrite_rules : (string * term_rewrite_rule) list ref = ref []
  let _norm_rule : term_norm_rule ref = ref (fun _ -> None)
  let _norm_name : string ref = ref "lambda normalize"
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
  let _multi_simpl_rule : (int * multi_simpl_rule) list ref = ref []
  let _cheap_msr : multi_simpl_rule list ref = ref []
  let _generate_rules : (int * string * generate_rule) list ref = ref []
  let _cl_elim_rules : (int * string * clause_elim_rule) list ref = ref []
  let _clause_conversion_rules : clause_conversion_rule list ref = ref []
  let _step_init = ref []
  let _fragment_checks = ref []
  let _immediate_simpl : immediate_simplification_rule list ref = ref []


  let on_start = Signal.create()
  let on_input_statement = Signal.create()
  let on_empty_clause = Signal.create ()
  let on_forward_simplified = Signal.create()

  (** {2 Basic operations} *)

  let _queue = ref None

  let get_stm_queue () =
    match !_queue with
    | None ->
      _queue := Some (StmQ.default ());
      CCOpt.get_exn (!_queue);
    | Some q -> q 

  let add_empty c =
    assert (C.is_empty c);
    _empty_clauses := C.ClauseSet.add c !_empty_clauses;
    Signal.send on_empty_clause c;
    ()

  let add_passive cs =
    ProofState.PassiveSet.add cs;
    Iter.iter
      (fun c -> if C.is_empty c then add_empty c) cs;
    ()

  let add_active cs =
    ProofState.ActiveSet.add cs;
    Iter.iter
      (fun c -> if C.is_empty c then add_empty c) cs;
    ()

  let add_simpl = ProofState.SimplSet.add

  let remove_active = ProofState.ActiveSet.remove

  let remove_passive = ProofState.PassiveSet.remove

  let remove_simpl = ProofState.SimplSet.remove

  let get_passive () =
    ProofState.PassiveSet.clauses () |> C.ClauseSet.to_iter

  let get_active () =
    ProofState.ActiveSet.clauses () |> C.ClauseSet.to_iter

  let add_binary_inf name rule =
    if not (List.mem_assoc name !_binary_rules)
    then _binary_rules := (name, rule) :: !_binary_rules

  let add_unary_inf name rule =
    if not (List.mem_assoc name !_unary_rules)
    then _unary_rules := (name, rule) :: !_unary_rules

  let _add_prioritized ~store ~priority name rule = 
    if not (List.mem name (List.map (fun (_,n,_) -> n) !store))
    then (
      let cmp (p1,n1,r1) (p2,n2,r2) =
        let open CCOrd in
        CCInt.compare p2 p1
        <?> (CCString.compare, n2, n1) in

      store := CCList.sorted_insert ~cmp (priority,name,rule) !store)

  let add_generate ~priority name rule =
    _add_prioritized ~store:_generate_rules ~priority name rule

  let add_clause_elimination_rule ~priority name rule =
    _add_prioritized ~store:_cl_elim_rules ~priority name rule


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
    Util.debugf ~section 1 "[ Adding rule %s to env ]" (fun k-> k name);
    _rewrite_rules := (name, rule) :: !_rewrite_rules

  let add_immediate_simpl_rule rule =
    _immediate_simpl := rule :: !_immediate_simpl

  let set_ho_normalization_rule name rule =
    _norm_name := name;
    _norm_rule := rule

  let get_ho_normalization_rule () =
    !_norm_rule

  let add_lit_rule name rule =
    _lit_rules := (name, rule) :: !_lit_rules

  let add_multi_simpl_rule ~priority rule =
    _multi_simpl_rule := 
      CCList.sorted_insert ~cmp:(fun (p1,_) (p2,_) -> CCInt.compare p1 p2) 
        (priority,rule) !_multi_simpl_rule

  let multi_simpl_rules () =
    List.map snd !_multi_simpl_rule
  
  let add_cheap_multi_simpl_rule rule =
    _cheap_msr := rule :: !_cheap_msr

  let cr_skip = CR_skip
  let cr_add x = CR_add x
  let cr_return x = CR_return x

  let add_clause_conversion r =
    _clause_conversion_rules := r :: !_clause_conversion_rules

  let add_step_init f = _step_init := f :: !_step_init

  let add_fragment_check f = _fragment_checks := f :: !_fragment_checks

  let check_fragment c = CCList.for_all (fun f -> f c) !_fragment_checks

  let params = X.params

  let[@inline] get_empty_clauses () =
    !_empty_clauses

  let get_some_empty_clause () =
    try Some (C.ClauseSet.choose !_empty_clauses)
    with Not_found -> None

  let has_empty_clause () =
    not (C.ClauseSet.is_empty !_empty_clauses)

  let ord = Ctx.ord
  let precedence () = Ordering.precedence (ord ())
  let signature = Ctx.signature

  let pp out () = CCFormat.string out "env"

  let pp_full out () =
    Format.fprintf out "@[<hv2>env(state:@ %a@,)@]" ProofState.debug ()

  (** {2 Misc} *)
  let update_flex_state f = CCRef.update f flex_state_
  let flex_add k v = flex_state_ := Flex_state.add k v !flex_state_
  let flex_get k = Flex_state.get_exn k !flex_state_

  (** {2 High level operations} *)

  type stats = int * int * int

  let stats = ProofState.stats

  let next_passive = ProofState.PassiveSet.next

  let should_force_stream_eval () =
    flex_get PragUnifParams.k_unif_alg_is_terminating &&
    not (flex_get PragUnifParams.k_schedule_inferences) &&
    flex_get PragUnifParams.k_max_inferences > 0


  let get_finite_infs streams =
    assert(flex_get PragUnifParams.k_unif_alg_is_terminating);


    CCList.flat_map (fun s -> 
      OSeq.to_rev_list @@ OSeq.filter_map CCFun.id s
    ) streams


  (** do binary inferences that involve the given clause *)
  let do_binary_inferences c =
    let _span = ZProf.enter_prof prof_generate_binary in
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
    ZProf.exit_prof _span;
    Iter.of_list clauses

  (** do unary inferences for the given clause *)
  let do_unary_inferences c =
    let _span = ZProf.enter_prof prof_generate_unary in
    Util.debug ~section 3 "do unary inferences";
    (* apply every inference rule *)
    let clauses = List.fold_left
        (fun acc (name, rule) ->
           Util.debugf ~section 3 "apply unary rule %s" (fun k->k name);
           let new_clauses = rule c in
           List.rev_append new_clauses acc)
        [] !_unary_rules in
    ZProf.exit_prof _span;
    Iter.of_list clauses

  let do_generate ~full () =
    let clauses =
      CCList.fold_while
        (fun acc (_,name,g) ->
           Util.debugf ~section 3 "apply generating rule %s (full: %b)" (fun k->k name full);
           (* We are trying low effort generating functions first.
              If they find an empty clause -- then we do not go on to
              full effort generating functions *)
           let from_g  = g ~full () in
           let status = if List.exists C.is_empty from_g then `Stop else `Continue in
           List.rev_append (from_g) acc, status)
        []
        !_generate_rules
    in
    Iter.of_list clauses
  
  let do_clause_eliminate () =
    List.iter (fun (_, _, elim_procedure) -> elim_procedure ()) !_cl_elim_rules

  let is_trivial_trail trail = match !_is_trivial_trail with
    | [] -> false
    | [f] -> f trail
    | f1 :: f2 :: tl -> f1 trail || f2 trail || List.exists (fun f -> f trail) tl

  let is_trivial c =
    if C.get_flag SClause.flag_persistent c then false
    else (
      let res =
        C.is_redundant c
        || is_trivial_trail (C.trail c)
        || begin match !_is_trivial with
          | [] -> false
          | [f] -> f c
          | f :: g :: tl -> f c || g c || List.exists (fun f -> f c) tl
        end
      in
      if res then C.mark_redundant c;
      res
    )

  let immediate_simplify given immediate =
    let rec aux = function 
    | [] -> immediate
    | f :: fs ->
      match f given immediate with 
      | Some res -> res
      | None -> aux fs in
    aux !_immediate_simpl

  let is_active c =
    C.ClauseSet.mem c (ProofState.ActiveSet.clauses ())

  let is_passive =  ProofState.PassiveSet.is_passive

  let on_pred_var_elimination = Signal.create ()
  
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
            let new_t = match !_norm_rule t' with 
              | None -> t'
              | Some tt -> tt in
            Util.debugf ~section 5
              "@[<2>rewrite `@[%a@]`@ into `@[%a@]`@ :proof (@[%a@])@]"
              (fun k->k T.pp t T.pp new_t (Util.pp_list Proof.pp_parent) proof);
            reduce_term !_rewrite_rules new_t  (* re-apply all rules *)
        end
    in 
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

  let ho_normalize c =
    let did_reduce = ref false in
    let lits' =
      Array.map
        (fun lit -> Lit.map (fun t -> match !_norm_rule t with 
             | None -> t
             | Some t' -> did_reduce := true; t' ) lit)
        (C.lits c)
    in
    if not !did_reduce
    then SimplM.return_same c (* no simplification *)
    else (
      C.mark_redundant c;
      (* FIXME: put the rules as parameters *)
      let rule = Proof.Rule.mk !_norm_name in
      let proof =
        Proof.Step.simp ~rule  ~tags:[Proof.Tag.T_ho_norm]
          ([C.proof_parent c])
      in
      let c' = C.create_a ~trail:(C.trail c) ~penalty:(C.penalty c) lits' proof in
      assert (not (C.equal c c'));
      Util.debugf ~section 3 "@[lambda rewritten clause `@[%a@]`@ into `@[%a@]`"
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
          ho_normalize c >>= fun c ->
          basic_simplify c >>= fun c ->
          (* first, rewrite terms *)
          ho_normalize c >>= fun c ->
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
    let _span = ZProf.enter_prof prof_simplify in
    let res = fix_simpl c
      ~f:(fun c ->
          let old_c = c in
          ho_normalize c >>=
          basic_simplify >>=
          (* simplify with unit clauses, then all active clauses *)
          ho_normalize >>=
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
    ZProf.exit_prof _span;
    res

  let multi_simplify ~depth c : (C.t * int) list option =
    let _span = ZProf.enter_prof prof_multi_simplify in
    let depth_map = 
      ref (Util.Int_map.singleton (C.id c) depth) in
    let [@inline] get_depth c =
      CCOpt.get_exn @@ Util.Int_map.get (C.id c) !depth_map in
    let [@inline] update_map c c' = 
      let d = get_depth c in
      depth_map := 
        Util.Int_map.add (C.id c') d 
          (Util.Int_map.remove (C.id c) !depth_map)
    in
    let set_children c children =
      let d' = (get_depth c) + 1 in
      depth_map := 
        List.fold_left (fun map child -> 
          Util.Int_map.add (C.id child) d' map
        ) !depth_map children
    in
    let init_cl = c in

    let did_something = ref false in
    (* try rules one by one until some of them succeeds *)
    let rec try_next ~depth c rules = 
      if flex_get k_max_multi_simpl_depth != -1 &&
         depth > flex_get k_max_multi_simpl_depth 
      then None
      else (
        match rules with
        | [] -> None
        | r::rules' ->
          match r c with
          | Some l -> Some l
          | None -> try_next ~depth c rules'
      )
    in
    (* fixpoint of [try_next] *)
    let set = ref C.ClauseSet.empty in
    let q = Queue.create () in
    Queue.push c q;
    while not (Queue.is_empty q) do
      let c = Queue.pop q in
      let depth = get_depth c in
      if not (C.ClauseSet.mem c !set) then (
        let orig_c = c in
        let c, st = if C.equal c init_cl then SimplM.return_same c else simplify c in
        update_map orig_c c;
        if st = `New then did_something := true;
        match try_next ~depth c (multi_simpl_rules ()) with
        | None ->
          (* keep the clause! *)
          set := C.ClauseSet.add c !set;
        | Some l ->
          did_something := true;
          set_children c l;
          List.iter (fun c -> Queue.push c q) l;
      )
    done;
    ZProf.exit_prof _span;
    if !did_something
    then (
      C.mark_redundant c;
      Some (List.map (fun c -> (c, get_depth c)) (C.ClauseSet.to_list !set))
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
    let _span = ZProf.enter_prof prof_back_simplify in
    (* set of candidate clauses, that may be unit-simplifiable *)
    let candidates = backward_simplify_find_candidates given in
    let back_simplify c =
      let open SimplM.Infix in
      fix_simpl c
        ~f:(fun c ->
            let old_c = c in
            ho_normalize c >>=
            basic_simplify >>=
            (* simplify with unit clauses, then all active clauses *)
            ho_normalize >>=
            rewrite >>=
            rw_simplify >>=
            unary_simplify >|= fun c ->
            if not (Lits.equal_com (C.lits c) (C.lits old_c)) then (
              Util.debugf ~section 1 "@[clause `@[%a@]`@ simplified into `@[%a@]`@]"
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
    ZProf.exit_prof _span;
    before, Iter.of_list after

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
    ProofState.ActiveSet.remove (Iter.of_list set |> Iter.map fst);
    Iter.of_list set
    |> Iter.map snd
    |> Iter.flat_map Iter.of_list
    |> ProofState.PassiveSet.add;
    ()

  (** Simplify the clause w.r.t to the active set *)
  let forward_simplify c =
    let open SimplM.Infix in
    let _span = ZProf.enter_prof prof_fwd_simplify in
    let res = ho_normalize c >>= rewrite >>= rw_simplify >>= unary_simplify in
    ZProf.exit_prof _span;
    res

  let _apply_multi_rules ~rule_list c = 
    let rec apply_rules ~rules c =
      match rules with
      | [] -> None
      | r :: rs ->
        CCOpt.or_lazy ~else_:(fun () -> apply_rules ~rules:rs c) (r c) in
    
    let q = Queue.create () in
    Queue.add c q;
    let res = ref [] in
    let any_simplified = ref false in

    while not (Queue.is_empty q) do
      let c = Queue.pop q in
      match apply_rules ~rules:rule_list c with
      | None -> res := c :: !res
      | Some simplified ->
        any_simplified := true;
        List.iter (fun c -> Queue.add c q) simplified;
    done;

    (!res, !any_simplified)

  let cheap_multi_simplify c = 
    let _span = ZProf.enter_prof prof_cheap_multi in
    let res,any_simplified = _apply_multi_rules ~rule_list:!_cheap_msr c in
    ZProf.exit_prof _span;

    if any_simplified then Some res else None

  (** generate all clauses from inferences *)
  let generate given =
    let _span = ZProf.enter_prof prof_generate in
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
          Iter.iter
            (fun c' -> Queue.push (c', depth+1) unary_queue)
            new_clauses
        )
      )
    done;
    (* generating rules *)
    let other_clauses = do_generate ~full:false () in
    (* combine all clauses *)
    let result = Iter.(
        append
          (of_list !unary_clauses)
          (append binary_clauses other_clauses))
    in
    Util.add_stat stat_inferred (Iter.length result);
    ZProf.exit_prof _span;
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
    || ZProf.with_prof prof_is_redundant is_redundant_ c

  (** find redundant clauses in current active_set *)
  let subsumed_by c =
    let _span = ZProf.enter_prof prof_subsumed_by in
    let res =
      List.fold_left
        (fun set rule -> rule set c)
        C.ClauseSet.empty
        !_backward_redundant
    in
    (* all those clauses are redundant *)
    C.ClauseSet.iter C.mark_redundant res;
    ZProf.exit_prof _span;
    res

  (** Use all simplification rules to convert a clause into a list of
      maximally simplified clauses.
      
      Stop applying mutlti_simpl rules after a certain depth.
      Especially dangerous rules are the ones that do boolean hoisting
      as simplification
  *)
  let all_simplify c =
    let _span = ZProf.enter_prof prof_all_simplify in
    let did_simplify = ref false in
    let set = ref C.ClauseSet.empty in
    let q = Queue.create () in
    Queue.push (c,0) q;

    while not (Queue.is_empty q) do
      let c, depth = Queue.pop q in
      let c, st = if depth == 0 then simplify c else SimplM.return_same c in
      if st=`New then did_simplify := true;
      if is_trivial c || is_redundant c
      then ()
      else (
        (* A list of single step rules works like this:
           Each rule can be applied to a clause which is not
           a descendent of a clause to which the rule has already been applied!

           For each rule, we keep the set of clauses that the rule has been applied on
           an their descendents. Then, we apply a rule on a clause not in this set.
         *)
          match multi_simplify ~depth c with
          | Some l ->
            (* continue processing *)
            did_simplify := true;
            List.iter (fun (c,d) -> Queue.push (c,d) q) l
          | None ->
            (* clause has reached fixpoint *)
            set := C.ClauseSet.add c !set;
    );
    done;
    let res = C.ClauseSet.to_list !set in
    ZProf.exit_prof _span;
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
          | CR_drop -> []
          | CR_return l -> l
          | CR_add l -> List.rev_append l (conv_clause_ rules' st)
        end
    in
    CCVector.iter
      (fun st ->
         let cs = conv_clause_ !_clause_conversion_rules st in
         List.iter (fun c -> C.set_flag (SClause.flag_initial) c true ) cs;
         begin match Statement.view st with
           | Statement.Assert _ when has_sos_attr st ->
             CCVector.append_list c_sos cs
           | _ -> CCVector.append_list c_set cs
         end)
      stmts;
    Util.debugf ~section 1
      "@[<v>@[<2>clauses:@ @[<v>%a@]@]@ @[<2>sos:@ @[<v>%a@]@]@]"
      (fun k->k
          (Util.pp_iter ~sep:" " C.pp) (CCVector.to_iter c_set)
          (Util.pp_iter ~sep:" " C.pp) (CCVector.to_iter c_sos));
    Util.debugf ~section 1 "end@." CCFun.id;
    let c_set = CCVector.freeze c_set in
    let c_sos = CCVector.freeze c_sos in
    { Clause.c_set; c_sos; }
end

