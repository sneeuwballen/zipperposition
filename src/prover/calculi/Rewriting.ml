
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Rewriting} *)

open Libzipperposition

module T = FOTerm
module RR = Rewrite_rule

let section = RR.section

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

  (* TODO: narrowing, on terms and clauses *)

  let setup rules =
    Util.debug ~section 1 "register Rewriting to Env...";
    if not (RR.Set.is_empty rules) then (
      Util.debugf ~section 2 "@[<v2>rewrite rules:@ %a@]" (fun k->k RR.Set.pp rules);
      E.Ctx.lost_completeness ();
      E.add_rewrite_rule "rewrite_defs" (simpl_term rules);
      E.add_multi_simpl_rule (simpl_clause rules);
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

