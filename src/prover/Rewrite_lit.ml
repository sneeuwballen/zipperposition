
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Logtk

(** {1 Rewrite Rules} *)
module T = FOTerm
module Stmt = Statement
module Su = Subst

let section = Util.Section.make "rewriting"
let stats_rw = Util.mk_stat "rw.steps_clause"
let prof_rw = Util.mk_profiler "rw.clause"

type rule = {
  c_lhs: Literal.t;
  c_rhs: Literal.t list list; (* list of clauses *)
}
(* invariant: all variables in [c_rhs] also occur in [c_lhs] *)

let make_c c_lhs c_rhs = {c_lhs; c_rhs}

let lhs c = c.c_lhs
let rhs c = c.c_rhs

let pp_rule out r =
  let pp_c = CCFormat.hvbox (Util.pp_list ~sep:" ∨ " Literal.pp) in
  Format.fprintf out "@[<2>@[%a@] ==>@ @[<v>%a@]@]"
    Literal.pp r.c_lhs (Util.pp_list pp_c) r.c_rhs

module Set = struct
  type t = {
    clauses: rule list;
  }
  (* head symbol -> set of rules *)

  let empty = {
    clauses=[];
  }

  let is_empty t: bool = t.clauses=[]

  let add_clause r s =
    Util.debugf ~section 5 "@[<2>add rewrite rule@ `@[%a@]`@]" (fun k->k pp_rule r);
    {clauses=r :: s.clauses}

  let add_stmt stmt t = match Stmt.view stmt with
    | Stmt.Def l ->
      Sequence.of_list l
      |> Sequence.flat_map
        (fun {Stmt.def_ty=ty; def_rules; def_rewrite=b; _} ->
           if b || Type.is_const ty
           then Sequence.of_list def_rules
           else Sequence.empty)
      |> Sequence.fold
        (fun t rule -> match rule with
           | Stmt.Def_term _ -> t
           | Stmt.Def_form (_,lhs,rhs) ->
             let lhs = Literal.Conv.of_form lhs in
             let rhs = List.map (List.map Literal.Conv.of_form) rhs in
             let r = make_c lhs rhs in
             add_clause r t)
        t
    | Stmt.RewriteTerm _ -> t
    | Stmt.RewriteForm (_, lhs, rhs) ->
      let lhs = Literal.Conv.of_form lhs in
      let rhs = List.map (List.map Literal.Conv.of_form) rhs in
      let r = make_c lhs rhs in
      add_clause r t
    | Stmt.TyDecl _
    | Stmt.Data _
    | Stmt.Assert _
    | Stmt.Lemma _
    | Stmt.Goal _
    | Stmt.NegatedGoal _
      -> t

  let to_seq t = Sequence.of_list t.clauses

  let pp out t =
    Format.fprintf out "{@[<hv>%a@]}"
      (Util.pp_seq pp_rule) (to_seq t)
end

(* try to rewrite this literal, returning a list of list of lits instead *)
let step_lit rules lit =
  CCList.find_map
    (fun r ->
       let substs = Literal.matching ~pattern:(r.c_lhs,1) (lit,0) in
       match Sequence.head substs with
         | None -> None
         | Some subst -> Some (r, subst))
    rules

let normalize_clause_ rules lits =
  let eval_ll ~renaming subst (l,sc) =
    List.map
      (List.map
         (fun lit -> Literal.apply_subst ~renaming subst (lit,sc)))
      l
  in
  let step =
    CCList.find_mapi
      (fun i lit -> match step_lit rules.Set.clauses lit with
         | None -> None
         | Some (rule,subst) ->
           let clauses = rule.c_rhs in
           Util.debugf ~section 5
             "@[<2>rewrite `@[%a@]`@ :into `@[<v>%a@]`@ :with @[%a@]@ :rule `%a`@]"
             (fun k->k Literal.pp lit
                 (Util.pp_list (CCFormat.hvbox (Util.pp_list ~sep:" ∨ " Literal.pp)))
                 clauses Subst.pp subst pp_rule rule);
           Util.incr_stat stats_rw;
           Some (i, clauses, subst))
      lits
  in
  begin match step with
    | None -> None
    | Some (i, clause_chunks, subst) ->
      let renaming = Subst.Renaming.create () in
      (* remove rewritten literal, replace by [clause_chunks], apply
         substitution (clause_chunks might contain other variables!),
         distribute to get a CNF again *)
      let lits = CCList.remove_at_idx i lits in
      let lits = Literal.apply_subst_list ~renaming subst (lits,0) in
      let clause_chunks = eval_ll ~renaming subst (clause_chunks,1) in
      let clauses = List.map (fun new_lits -> new_lits @ lits) clause_chunks in
      Some clauses
  end

let normalize_clause rules lits =
  Util.with_prof prof_rw (normalize_clause_ rules) lits

let narrow_lit ?(subst=Subst.empty) (rules,sc_r) (lit,sc_lit) =
  Sequence.of_list rules.Set.clauses
  |> Sequence.flat_map
    (fun r ->
       Literal.unify ~subst (r.c_lhs,sc_r) (lit,sc_lit)
       |> Sequence.map (fun subst -> r, subst))
