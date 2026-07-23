(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Global environment for an instance of the prover} *)

open Logtk
module T = Term
module Lit = Literal
module Lits = Literals
module P = Proof
module IntSet = Set.Make (CCInt)

let section = Util.Section.make ~parent:Const.section "env"
let stat_inferred = Util.mk_stat "env.inferred clauses"

module type S = Env_intf.S

module Ctx = Ctx
module C = Clause
module ProofState = ProofState
module Stm = Stream
module StmQ = StreamQueue
module FormRename = FormulaRename.Make (Clause)

type 'a packed = unit

let k_max_multi_simpl_depth = Flex_state.create_key ()

(** {2 Record type for the concrete environment (new-style)} *)

type t = {
  ctx: Ctx.t;
  mutable flex_state: Flex_state.t;
  mutable binary_rules: (string * binary_inf_rule) list;
  mutable unary_rules: (string * unary_inf_rule) list;
  mutable rewrite_rules: (string * term_rewrite_rule) list;
  mutable norm_rule: term_norm_rule;
  mutable norm_name: string;
  mutable lit_rules: (string * lit_rewrite_rule) list;
  mutable basic_simplify: simplify_rule list;
  mutable unary_simplify: simplify_rule list;
  mutable rw_simplify: rw_simplify_rule list;
  mutable active_simplify: active_simplify_rule list;
  mutable backward_simplify: backward_simplify_rule list;
  mutable redundant: redundant_rule list;
  mutable backward_redundant: backward_redundant_rule list;
  mutable is_trivial_trail: is_trivial_trail_rule list;
  mutable is_trivial: is_trivial_rule list;
  mutable empty_clauses: C.ClauseSet.t;
  mutable multi_simpl_rule: (int * multi_simpl_rule) list;
  mutable cheap_msr: multi_simpl_rule list;
  mutable generate_rules: (int * string * generate_rule) list;
  mutable cl_elim_rules: (int * string * clause_elim_rule) list;
  mutable clause_conversion_rules: clause_conversion_rule list;
  mutable step_init: (t -> unit) list;
  mutable fragment_checks: (t -> C.t -> bool) list;
  mutable immediate_simpl: immediate_simplification_rule list;
  queue: StmQ.t option ref;
  params: Params.t;
  on_start: unit Signal.t;
  on_input_statement: Statement.clause_t Signal.t;
  on_empty_clause: C.t Signal.t;
  on_forward_simplified: (C.t * C.t option) Signal.t;
  on_pred_var_elimination: (C.t * Term.t) Signal.t;
}

and inf_rule = t -> C.t -> C.t list
and generate_rule = t -> full:bool -> unit -> C.t list
and clause_elim_rule = t -> unit
and binary_inf_rule = inf_rule
and unary_inf_rule = inf_rule
and simplify_rule = t -> C.t -> C.t SimplM.t
and active_simplify_rule = simplify_rule
and rw_simplify_rule = simplify_rule
and backward_simplify_rule = t -> C.t -> C.ClauseSet.t
and redundant_rule = t -> C.t -> bool
and backward_redundant_rule = t -> C.ClauseSet.t -> C.t -> C.ClauseSet.t
and is_trivial_trail_rule = t -> Trail.t -> bool
and is_trivial_rule = t -> C.t -> bool
and term_rewrite_rule = t -> Term.t -> (Term.t * Proof.parent list) option
and term_norm_rule = t -> Term.t -> Term.t option

and lit_rewrite_rule =
  t -> Literal.t -> (Literal.t * Proof.parent list * Proof.tag list) option

and multi_simpl_rule = t -> C.t -> C.t list option
and immediate_simplification_rule = t -> C.t -> C.t Iter.t -> C.t Iter.t option

and 'a conversion_result =
  | CR_skip
  | CR_drop
  | CR_add of 'a
  | CR_return of 'a

and clause_conversion_rule =
  t -> Statement.clause_t -> C.t list conversion_result

and stats = int * int * int

(** {2 Build a new Environment (new-style)} *)

let create ~params ~flex_state ~ctx () =
  {
    ctx;
    flex_state;
    binary_rules = [];
    unary_rules = [];
    rewrite_rules = [];
    norm_rule = (fun _ _ -> None);
    norm_name = "lambda normalize";
    lit_rules = [];
    basic_simplify = [];
    unary_simplify = [];
    rw_simplify = [];
    active_simplify = [];
    backward_simplify = [];
    redundant = [];
    backward_redundant = [];
    is_trivial_trail = [];
    is_trivial = [];
    empty_clauses = C.ClauseSet.empty;
    multi_simpl_rule = [];
    cheap_msr = [];
    generate_rules = [];
    cl_elim_rules = [];
    clause_conversion_rules = [];
    step_init = [];
    fragment_checks = [];
    immediate_simpl = [];
    queue = ref None;
    params;
    on_start = Signal.create ();
    on_input_statement = Signal.create ();
    on_empty_clause = Signal.create ();
    on_forward_simplified = Signal.create ();
    on_pred_var_elimination = Signal.create ();
  }

(** {2 New-style functions (take [t] explicitly)} *)

let ctx_of env = env.ctx
let get_ctx env = env.ctx
let flex_state_of env = env.flex_state
let flex_get_of env k = Flex_state.get_exn k env.flex_state

let flex_get_or_create ~init env k =
  try Flex_state.get_exn k env.flex_state
  with Not_found ->
    let x = init () in
    env.flex_state <- Flex_state.add k x env.flex_state;
    x

let flex_add_of env k v = env.flex_state <- Flex_state.add k v env.flex_state
let params_of env = env.params
let update_flex_state env f = env.flex_state <- f env.flex_state

(** {2 Core operations — internal helpers} *)

module StrSet = CCSet.Make (String)

let get_stm_queue env =
  match !(env.queue) with
  | None ->
    env.queue := Some (StmQ.default ());
    CCOpt.get_exn !(env.queue)
  | Some q -> q

let add_empty env c =
  assert (C.is_empty c);
  env.empty_clauses <- C.ClauseSet.add c env.empty_clauses;
  Signal.send env.on_empty_clause c

let add_passive env cs =
  ProofState.PassiveSet.add cs;
  Iter.iter (fun c -> if C.is_empty c then add_empty env c) cs

let add_active env cs =
  ProofState.ActiveSet.add cs;
  Iter.iter (fun c -> if C.is_empty c then add_empty env c) cs

let add_simpl _env = ProofState.SimplSet.add
let remove_active _env = ProofState.ActiveSet.remove
let remove_passive _env = ProofState.PassiveSet.remove
let remove_simpl _env = ProofState.SimplSet.remove

let get_passive _env () =
  ProofState.PassiveSet.clauses () |> C.ClauseSet.to_seq |> Iter.of_seq

let get_active _env () =
  ProofState.ActiveSet.clauses () |> C.ClauseSet.to_seq |> Iter.of_seq

let add_binary_inf env name rule =
  if not (List.mem_assoc name env.binary_rules) then
    env.binary_rules <- (name, rule) :: env.binary_rules

let add_unary_inf env name rule =
  if not (List.mem_assoc name env.unary_rules) then
    env.unary_rules <- (name, rule) :: env.unary_rules

let add_generate env ~priority name rule =
  if not (List.mem name (List.map (fun (_, n, _) -> n) env.generate_rules)) then (
    let cmp (p1, n1, _) (p2, n2, _) =
      let open CCOrd in
      CCInt.compare p2 p1 <?> (CCString.compare, n2, n1)
    in
    env.generate_rules <-
      CCList.sorted_insert ~cmp (priority, name, rule) env.generate_rules
  )

let add_clause_elimination_rule env ~priority name rule =
  if not (List.mem name (List.map (fun (_, n, _) -> n) env.cl_elim_rules)) then (
    let cmp (p1, n1, _) (p2, n2, _) =
      let open CCOrd in
      CCInt.compare p2 p1 <?> (CCString.compare, n2, n1)
    in
    env.cl_elim_rules <-
      CCList.sorted_insert ~cmp (priority, name, rule) env.cl_elim_rules
  )

let add_rw_simplify env r = env.rw_simplify <- r :: env.rw_simplify
let add_active_simplify env r = env.active_simplify <- r :: env.active_simplify

let add_backward_simplify env r =
  env.backward_simplify <- r :: env.backward_simplify

let add_redundant env r = env.redundant <- r :: env.redundant

let add_backward_redundant env r =
  env.backward_redundant <- r :: env.backward_redundant

let add_basic_simplify env r = env.basic_simplify <- r :: env.basic_simplify
let add_unary_simplify env r = env.unary_simplify <- r :: env.unary_simplify

let add_is_trivial_trail env r =
  env.is_trivial_trail <- r :: env.is_trivial_trail

let add_is_trivial env r = env.is_trivial <- r :: env.is_trivial

let add_rewrite_rule env name rule =
  Util.debugf ~section 1 "[ Adding rule %s to env ]" (fun k -> k name);
  env.rewrite_rules <- (name, rule) :: env.rewrite_rules

let add_immediate_simpl_rule env rule =
  env.immediate_simpl <- rule :: env.immediate_simpl

let set_ho_normalization_rule env name rule =
  env.norm_name <- name;
  env.norm_rule <- rule

let get_ho_normalization_rule env = env.norm_rule
let add_lit_rule env name rule = env.lit_rules <- (name, rule) :: env.lit_rules

let add_multi_simpl_rule env ~priority rule =
  env.multi_simpl_rule <-
    CCList.sorted_insert
      ~cmp:(fun (p1, _) (p2, _) -> CCInt.compare p1 p2)
      (priority, rule) env.multi_simpl_rule

let multi_simpl_rules env = List.map snd env.multi_simpl_rule
let add_cheap_multi_simpl_rule env rule = env.cheap_msr <- rule :: env.cheap_msr
let cr_skip = CR_skip
let cr_add x = CR_add x
let cr_return x = CR_return x

let add_clause_conversion env r =
  env.clause_conversion_rules <- r :: env.clause_conversion_rules

let add_step_init env f = env.step_init <- f :: env.step_init
let add_fragment_check env f = env.fragment_checks <- f :: env.fragment_checks
let check_fragment env c = CCList.for_all (fun f -> f env c) env.fragment_checks
let get_empty_clauses env = env.empty_clauses

let get_some_empty_clause env =
  try Some (C.ClauseSet.choose env.empty_clauses) with Not_found -> None

let has_empty_clause env = not (C.ClauseSet.is_empty env.empty_clauses)

let rec fix_simpl ~f c =
  let open SimplM.Infix in
  let new_c = f c in
  if C.equal c (SimplM.get new_c) then
    new_c
  else (
    C.mark_redundant c;
    new_c >>= fix_simpl ~f
  )

let ho_normalize env c =
  let did_reduce = ref false in
  let lits' =
    Array.map
      (fun lit ->
        Lit.map
          (fun t ->
            match env.norm_rule env t with
            | None -> t
            | Some t' ->
              did_reduce := true;
              t')
          lit)
      (C.lits c)
  in
  if not !did_reduce then
    SimplM.return_same c
  else (
    C.mark_redundant c;
    let rule = Proof.Rule.mk env.norm_name in
    let proof =
      Proof.Step.simp ~rule ~tags:[ Proof.Tag.T_ho_norm ] [ C.proof_parent c ]
    in
    let c' =
      C.create_a ~ctx:(C.ctx_of c) ~trail:(C.trail c) ~penalty:(C.penalty c)
        lits' proof
    in
    SimplM.return_new c'
  )

let rewrite env c =
  let applied_rules = ref StrSet.empty in
  let proofs : Proof.parent list ref = ref [] in
  let rec reduce_term rules t =
    match rules with
    | [] -> t
    | (name, r) :: rules' ->
      (match r env t with
      | None -> reduce_term rules' t
      | Some (t', proof) ->
        applied_rules := StrSet.add name !applied_rules;
        proofs := List.rev_append proof !proofs;
        let new_t =
          match env.norm_rule env t' with
          | None -> t'
          | Some tt -> tt
        in
        reduce_term env.rewrite_rules new_t)
  in
  let lits' =
    Array.map
      (fun lit -> Lit.map (reduce_term env.rewrite_rules) lit)
      (C.lits c)
  in
  if StrSet.is_empty !applied_rules then
    SimplM.return_same c
  else (
    C.mark_redundant c;
    let rule = Proof.Rule.mk "rw" in
    let proof = Proof.Step.simp ~rule (C.proof_parent c :: !proofs) in
    let c' =
      C.create_a ~ctx:(C.ctx_of c) ~trail:(C.trail c) ~penalty:(C.penalty c)
        lits' proof
    in
    SimplM.return_new c'
  )

let rewrite_lits env c =
  let applied_rules = ref StrSet.empty in
  let proofs : Proof.parent list ref = ref [] in
  let tags : Proof.tag list ref = ref [] in
  let rec rewrite_lit rules lit =
    match rules with
    | [] -> lit
    | (name, r) :: rules' ->
      (match r env lit with
      | None -> rewrite_lit rules' lit
      | Some (lit', proof, tgs) ->
        applied_rules := StrSet.add name !applied_rules;
        proofs := List.rev_append proof !proofs;
        tags := List.rev_append tgs !tags;
        rewrite_lit env.lit_rules lit')
  in
  let lits = Array.map (fun lit -> rewrite_lit env.lit_rules lit) (C.lits c) in
  if StrSet.is_empty !applied_rules then
    SimplM.return_same c
  else (
    C.mark_redundant c;
    let rule = Proof.Rule.mk "rw_lit" in
    let proof =
      Proof.Step.simp ~rule ~tags:!tags (C.proof_parent c :: !proofs)
    in
    let c' =
      C.create_a ~ctx:(C.ctx_of c) ~trail:(C.trail c) ~penalty:(C.penalty c)
        lits proof
    in
    SimplM.return_new c'
  )

let basic_simplify env c =
  let open SimplM.Infix in
  match env.basic_simplify with
  | [] -> SimplM.return_same c
  | [ f ] -> f env c
  | [ f; g ] -> f env c >>= g env
  | l -> SimplM.app_list (List.map (fun f -> f env) l) c

let unary_simplify env c =
  let open SimplM.Infix in
  fix_simpl c ~f:(fun c ->
      ho_normalize env c >>= fun c ->
      basic_simplify env c >>= fun c ->
      ho_normalize env c >>= fun c ->
      rewrite env c >>= fun c ->
      (match env.lit_rules with
      | [] -> SimplM.return_same c
      | _ -> rewrite_lits env c)
      >>= fun c ->
      match env.unary_simplify with
      | [] -> SimplM.return_same c
      | [ f ] -> f env c
      | [ f; g ] -> f env c >>= g env
      | l -> SimplM.app_list (List.map (fun f -> f env) l) c)

let rw_simplify env c =
  let open SimplM.Infix in
  fix_simpl c ~f:(fun c ->
      if C.get_flag SClause.flag_persistent c then
        SimplM.return_same c
      else (
        match env.rw_simplify with
        | [] -> SimplM.return_same c
        | [ f ] -> f env c
        | [ f; g ] -> f env c >>= g env
        | l -> SimplM.app_list (List.map (fun f -> f env) l) c
      ))

let active_simplify env c =
  let open SimplM.Infix in
  fix_simpl c ~f:(fun c ->
      if C.get_flag SClause.flag_persistent c then
        SimplM.return_same c
      else (
        match env.active_simplify with
        | [] -> SimplM.return_same c
        | [ f ] -> f env c
        | [ f; g ] -> f env c >>= g env
        | l -> SimplM.app_list (List.map (fun f -> f env) l) c
      ))

let simplify env c =
  let open SimplM.Infix in
  fix_simpl c ~f:(fun c ->
      let old_c = c in
      ho_normalize env c >>= basic_simplify env >>= ho_normalize env
      >>= rewrite env >>= rw_simplify env >>= unary_simplify env
      >>= active_simplify env
      >|= fun c -> c)

let multi_simplify env ~depth c =
  let depth_map = ref (Util.Int_map.singleton (C.id c) depth) in
  let get_depth c = CCOpt.get_exn @@ Util.Int_map.get (C.id c) !depth_map in
  let update_map c c' =
    let d = get_depth c in
    depth_map :=
      Util.Int_map.add (C.id c') d (Util.Int_map.remove (C.id c) !depth_map)
  in
  let set_children c children =
    let d' = get_depth c + 1 in
    depth_map :=
      List.fold_left
        (fun map child -> Util.Int_map.add (C.id child) d' map)
        !depth_map children
  in
  let init_cl = c in
  let did_something = ref false in
  let rec try_next ~depth c rules =
    if
      flex_get_of env k_max_multi_simpl_depth != -1
      && depth > flex_get_of env k_max_multi_simpl_depth
    then
      None
    else (
      match rules with
      | [] -> None
      | r :: rules' ->
        (match r env c with
        | Some l -> Some l
        | None -> try_next ~depth c rules')
    )
  in
  let set = ref C.ClauseSet.empty in
  let q = Queue.create () in
  Queue.push c q;
  while not (Queue.is_empty q) do
    let c = Queue.pop q in
    let depth = get_depth c in
    if not (C.ClauseSet.mem c !set) then (
      let orig_c = c in
      let c, st =
        if C.equal c init_cl then
          SimplM.return_same c
        else
          simplify env c
      in
      update_map orig_c c;
      if st = `New then did_something := true;
      match try_next ~depth c (multi_simpl_rules env) with
      | None -> set := C.ClauseSet.add c !set
      | Some l ->
        did_something := true;
        set_children c l;
        List.iter (fun c -> Queue.push c q) l
    )
  done;
  if !did_something then (
    C.mark_redundant c;
    Some (List.map (fun c -> c, get_depth c) (C.ClauseSet.to_list !set))
  ) else
    None

let backward_simplify_find_candidates env given =
  match env.backward_simplify with
  | [] -> C.ClauseSet.empty
  | [ f ] -> f env given
  | [ f; g ] -> C.ClauseSet.union (f env given) (g env given)
  | l ->
    List.fold_left
      (fun set f -> C.ClauseSet.union set (f env given))
      C.ClauseSet.empty l

let is_trivial_trail env trail =
  match env.is_trivial_trail with
  | [] -> false
  | [ f ] -> f env trail
  | f1 :: f2 :: tl ->
    f1 env trail || f2 env trail || List.exists (fun f -> f env trail) tl

let is_trivial env c =
  if C.get_flag SClause.flag_persistent c then
    false
  else (
    let res =
      C.is_redundant c
      || is_trivial_trail env (C.trail c)
      ||
      match env.is_trivial with
      | [] -> false
      | [ f ] -> f env c
      | f :: g :: tl -> f env c || g env c || List.exists (fun f -> f env c) tl
    in
    if res then C.mark_redundant c;
    res
  )

let is_active _env c = C.ClauseSet.mem c (ProofState.ActiveSet.clauses ())
let is_passive _env = ProofState.PassiveSet.is_passive

let backward_simplify env given =
  let candidates = backward_simplify_find_candidates env given in
  let back_simplify c =
    let open SimplM.Infix in
    fix_simpl c ~f:(fun c ->
        let old_c = c in
        ho_normalize env c >>= basic_simplify env >>= ho_normalize env
        >>= rewrite env >>= rw_simplify env >>= unary_simplify env
        >|= fun c -> c)
  in
  let before, after =
    C.ClauseSet.fold
      (fun c (before, after) ->
        let c', is_new = back_simplify c in
        match is_new with
        | `Same ->
          if is_trivial env c' then
            C.ClauseSet.add c before, after
          else
            before, after
        | `New ->
          C.mark_redundant c;
          C.mark_backward_simplified c;
          C.ClauseSet.add c before, c' :: after)
      candidates (C.ClauseSet.empty, [])
  in
  before, Iter.of_list after

let backward_simplify env given =
  let candidates = backward_simplify_find_candidates env given in
  let back_simplify c =
    let open SimplM.Infix in
    fix_simpl c ~f:(fun c ->
        let old_c = c in
        ho_normalize env c >>= basic_simplify env >>= ho_normalize env
        >>= rewrite env >>= rw_simplify env >>= unary_simplify env
        >|= fun c -> c)
  in
  let before, after =
    C.ClauseSet.fold
      (fun c (before, after) ->
        let c', is_new = back_simplify c in
        match is_new with
        | `Same ->
          if is_trivial env c' then
            C.ClauseSet.add c before, after
          else
            before, after
        | `New ->
          C.mark_redundant c;
          C.mark_backward_simplified c;
          C.ClauseSet.add c before, c' :: after)
      candidates (C.ClauseSet.empty, [])
  in
  before, Iter.of_list after

let immediate_simplify env given immediate =
  let rec aux = function
    | [] -> immediate
    | f :: fs ->
      (match f env given immediate with
      | Some res -> res
      | None -> aux fs)
  in
  aux env.immediate_simpl

let do_binary_inferences env c =
  List.fold_left
    (fun acc (name, rule) ->
      Util.debugf ~section 3 "apply binary rule %s" (fun k -> k name);
      List.rev_append (rule env c) acc)
    [] env.binary_rules
  |> Iter.of_list

let do_unary_inferences env c =
  List.fold_left
    (fun acc (name, rule) ->
      Util.debugf ~section 3 "apply unary rule %s" (fun k -> k name);
      List.rev_append (rule env c) acc)
    [] env.unary_rules
  |> Iter.of_list

let do_generate env ~full () =
  CCList.fold_while
    (fun acc (_, name, g) ->
      Util.debugf ~section 3 "apply generating rule %s (full: %b)" (fun k ->
          k name full);
      let from_g = g env ~full () in
      let status =
        if List.exists C.is_empty from_g then
          `Stop
        else
          `Continue
      in
      List.rev_append from_g acc, status)
    [] env.generate_rules
  |> Iter.of_list

let generate env given =
  let binary_clauses = do_binary_inferences env given in
  let unary_clauses = ref [] and unary_queue = Queue.create () in
  Queue.push (given, 0) unary_queue;
  while not (Queue.is_empty unary_queue) do
    let c, depth = Queue.pop unary_queue in
    let c, _ = unary_simplify env c in
    if not (is_trivial env c) then (
      if depth > 0 then unary_clauses := c :: !unary_clauses;
      if depth < (params_of env).Params.unary_depth then
        Iter.iter
          (fun c' -> Queue.push (c', depth + 1) unary_queue)
          (do_unary_inferences env c)
    )
  done;
  let other_clauses = do_generate env ~full:false () in
  let result =
    Iter.(append (of_list !unary_clauses) (append binary_clauses other_clauses))
  in
  Util.add_stat stat_inferred (Iter.length result);
  result

let do_clause_eliminate env =
  List.iter (fun (_, _, elim) -> elim env) env.cl_elim_rules

let is_redundant__env env c =
  let res =
    match env.redundant with
    | [] -> false
    | [ f ] -> f env c
    | [ f; g ] -> f env c || g env c
    | l -> List.exists (fun f -> f env c) l
  in
  if res then C.mark_redundant c;
  res

let is_redundant env c = C.is_redundant c || is_redundant__env env c

let subsumed_by env c =
  let res =
    List.fold_left
      (fun set rule -> rule env set c)
      C.ClauseSet.empty env.backward_redundant
  in
  C.ClauseSet.iter C.mark_redundant res;
  res

let all_simplify env c =
  let did_simplify = ref false in
  let set = ref C.ClauseSet.empty in
  let q = Queue.create () in
  Queue.push (c, 0) q;
  while not (Queue.is_empty q) do
    let c, depth = Queue.pop q in
    let c, st =
      if depth == 0 then
        simplify env c
      else
        SimplM.return_same c
    in
    if st = `New then did_simplify := true;
    if is_trivial env c || is_redundant env c then
      ()
    else (
      match multi_simplify env ~depth c with
      | Some l ->
        did_simplify := true;
        List.iter (fun (c, d) -> Queue.push (c, d) q) l
      | None -> set := C.ClauseSet.add c !set
    )
  done;
  let res = C.ClauseSet.to_list !set in
  if !did_simplify then
    SimplM.return_new res
  else
    SimplM.return_same res

let step_init env = List.iter (fun f -> f env) env.step_init

(** {2 Forward simplify and cheap multi simplify — new-style} *)

let forward_simplify env c =
  let open SimplM.Infix in
  ho_normalize env c >>= rewrite env >>= rw_simplify env >>= unary_simplify env

let _apply_multi_rules_env env ~rule_list c =
  let rec apply_rules ~rules c =
    match rules with
    | [] -> None
    | r :: rs ->
      CCOpt.or_lazy ~else_:(fun () -> apply_rules ~rules:rs c) (r env c)
  in
  let q = Queue.create () in
  Queue.add c q;
  let res = ref [] in
  let any = ref false in
  while not (Queue.is_empty q) do
    let c = Queue.pop q in
    match apply_rules ~rules:rule_list c with
    | None -> res := c :: !res
    | Some simplified ->
      any := true;
      List.iter (fun c -> Queue.add c q) simplified
  done;
  !res, !any

let cheap_multi_simplify env c =
  let res, any = _apply_multi_rules_env env ~rule_list:env.cheap_msr c in
  if any then
    Some res
  else
    None

let simplify_active_with_env env f =
  let set =
    C.ClauseSet.fold
      (fun c set ->
        match f c with
        | None -> set
        | Some clauses ->
          let redundant, clauses =
            CCList.fold_map
              (fun red c ->
                let c', is_new = unary_simplify env c in
                red || is_new = `New, c')
              false clauses
          in
          if redundant then C.mark_redundant c;
          (c, clauses) :: set)
      (ProofState.ActiveSet.clauses ())
      []
  in
  ProofState.ActiveSet.remove (Iter.of_list set |> Iter.map fst);
  Iter.of_list set |> Iter.map snd |> Iter.flat_map Iter.of_list
  |> ProofState.PassiveSet.add;
  ()

let convert_input_statements_env env stmts : C.t Clause.sets =
  let is_lemma_ st =
    match Statement.view st with
    | Statement.Lemma _ -> true
    | _ -> false
  in
  let has_sos_attr st =
    CCList.exists
      (function
        | Statement.A_sos -> true
        | _ -> false)
      (Statement.attrs st)
  in
  CCVector.iter (Signal.send env.on_input_statement) stmts;
  let c_set = CCVector.create () in
  let c_sos = CCVector.create () in
  let rec conv_clause_ rules st =
    match rules with
    | [] when is_lemma_ st ->
      Util.warnf "@[drop lemma `%a`@]" Statement.pp_clause st;
      []
    | [] ->
      let clauses = C.of_statement ~ctx:env.ctx st in
      let c_sos_list = [] in
      List.iter (fun cl -> CCVector.push c_sos cl) c_sos_list;
      clauses
    | r :: rules' ->
      (match r env st with
      | CR_skip -> conv_clause_ rules' st
      | CR_drop -> []
      | CR_add l -> l
      | CR_return l -> l)
  in
  CCVector.iter
    (fun st ->
      let l = conv_clause_ env.clause_conversion_rules st in
      List.iter (fun cl -> CCVector.push c_set cl) l)
    stmts;
  Util.debugf ~section 1 "cnf: got %d + %d clauses" (fun k ->
      k (CCVector.length c_set) (CCVector.length c_sos));
  { Clause.c_set = CCVector.freeze c_set; c_sos = CCVector.freeze c_sos }

let stats _env = ProofState.stats ()
let next_passive _env () = ProofState.PassiveSet.next ()

let should_force_stream_eval env () =
  flex_get_of env PragUnifParams.k_unif_alg_is_terminating
  && (not (flex_get_of env PragUnifParams.k_schedule_inferences))
  && flex_get_of env PragUnifParams.k_max_inferences > 0

let get_finite_infs env streams =
  assert (flex_get_of env PragUnifParams.k_unif_alg_is_terminating);
  CCList.flat_map
    (fun s -> OSeq.to_rev_list @@ OSeq.filter_map CCFun.id s)
    streams

let simplify_active_with env f = simplify_active_with_env env f
let on_start env = env.on_start
let on_input_statement env = env.on_input_statement
let on_empty_clause env = env.on_empty_clause
let on_forward_simplified env = env.on_forward_simplified
let on_pred_var_elimination env = env.on_pred_var_elimination
