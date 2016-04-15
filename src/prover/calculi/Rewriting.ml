
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Rewriting} *)

open Libzipperposition

module T = FOTerm
module RR = Rewrite_rule

let section = RR.section

let stat_narrowing_lit = Util.mk_stat "narrow.clause_steps"
let stat_narrowing_term = Util.mk_stat "narrow.term_steps"

module Make(E : Env_intf.S) = struct
  module Env = E
  module C = E.C

  (* simplification rule *)
  let simpl_term rules t =
    let t' = RR.normalize_term rules t in
    if T.equal t t' then None
    else (
      Util.debugf ~section 2
        "@[<2>@{<green>rewrite@} `@[%a@]`@ into `@[%a@]`@]" (fun k->k T.pp t T.pp t');
      Some t'
    )

  (* TODO: term narrowing + contextual extended narrowing *)

  (* XXX: for now, we only do one step, and let Env.multi_simplify
     manage the fixpoint *)
  let simpl_clause rules c =
    let lits = C.lits c |> Array.to_list in
    match RR.normalize_clause rules lits with
      | None -> None
      | Some clauses ->
        let proof = ProofStep.mk_simp ~rule:(ProofStep.mk_rule "rw_clause") [C.proof c] in
        let clauses =
          List.map
            (fun c' -> C.create ~trail:(C.trail c) c' proof)
            clauses
        in
        Util.debugf ~section 2
          "@[<2>@{<green>rewrite@} `@[%a@]`@ into `@[<v>%a@]`@]"
          (fun k->k C.pp c CCFormat.(list C.pp) clauses);
        Some clauses

  (* narrowing on literals of given clause, using lits rewrite rules *)
  let narrow_lits rules c =
    let eligible = C.Eligible.res c in
    let lits = C.lits c in
    Literals.fold_lits ~eligible lits
    |> Sequence.fold
      (fun acc (lit,i) ->
         RR.narrow_lit (rules,1) (lit,0)
         |> Sequence.fold
           (fun acc (rule,subst) ->
              let proof =
                ProofStep.mk_inference [C.proof c]
                  ~rule:(ProofStep.mk_rule "narrow_clause") in
              let lits' = CCArray.except_idx lits i in
              (* create new clauses that correspond to replacing [lit]
                 by [rule.rhs] *)
              let clauses =
                List.map
                  (fun c' ->
                     let renaming = E.Ctx.renaming_clear () in
                     let new_lits =
                       (Literal.apply_subst_list ~renaming subst (lits',0)) @
                       (Literal.apply_subst_list ~renaming subst (c',1))
                     in
                     C.create ~trail:(C.trail c) new_lits proof)
                  (RR.rhs_clause rule)
              in
              Util.debugf ~section 3
                "@[<2>narrowing of `@[%a@]`@ using `@[%a@]`@ with @[%a@]@ yields @[%a@]@]"
                (fun k->k C.pp c RR.pp_rule_clause rule Substs.pp subst
                    CCFormat.(list (hovbox C.pp)) clauses);
              Util.incr_stat stat_narrowing_lit;
              List.rev_append clauses acc)
           acc)
      []

  let setup rules =
    Util.debug ~section 1 "register Rewriting to Env...";
    if not (RR.Set.is_empty rules) then (
      Util.debugf ~section 2 "@[<v2>rewrite rules:@ %a@]" (fun k->k RR.Set.pp rules);
      E.Ctx.lost_completeness ();
      E.add_rewrite_rule "rewrite_defs" (simpl_term rules);
      E.add_multi_simpl_rule (simpl_clause rules);
      E.add_unary_inf "narrow_defs" (narrow_lits rules);
    )
end

module Key = struct
  let rules = Flex_state.create_key()
end

let post_cnf stmts st =
  CCVector.iter Statement.scan_stmt_for_defined_cst stmts;
  (* add set of rules to [st] *)
  let rules =
    CCVector.fold
      (fun set s -> RR.Set.add_stmt s set)
      RR.Set.empty stmts
  in
  Flex_state.add Key.rules rules st

(* add a term simplification that normalizes terms w.r.t the set of rules *)
let normalize_simpl (module E : Env_intf.S) =
  let module M = Make(E) in
  let rules = E.flex_get Key.rules in
  M.setup rules

let extension =
  let open Extensions in
  { default with
    name = "rewriting";
    post_cnf_actions=[post_cnf];
    env_actions=[normalize_simpl];
  }

