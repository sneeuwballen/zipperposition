
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition

(** {1 Rewrite Rules} *)
module T = FOTerm
module Stmt = Statement
module Su = Substs

let section = Util.Section.(make ~parent:zip "rewriting")

let stat_term_rw = Util.mk_stat "rw.term steps"
let stat_clause_rw = Util.mk_stat "rw.clause steps"

let prof_term_rw = Util.mk_profiler "rw.term"
let prof_clause_rw = Util.mk_profiler "rw.clause"

type rule_term = {
  lhs: T.t;
  rhs: T.t;
  lhs_id: ID.t; (* head of lhs *)
}

(* constant rule [id := rhs] *)
let make_t_const id ty rhs =
  { lhs_id=id; lhs=T.const ~ty id; rhs; }

(* [id args := rhs] *)
let make_t id ty args rhs =
  let lhs = T.app (T.const ~ty id) args in
  { lhs_id=id; lhs; rhs; }

let rhs_term r = r.rhs

let pp_rule_term out r =
  Format.fprintf out "@[<2>@[%a@] -->@ @[%a@]@]" T.pp r.lhs T.pp r.rhs

type rule_clause = {
  c_lhs: Literal.t;
  c_rhs: Literal.t list list; (* list of clauses *)
}
(* invariant: all variables in [c_rhs] also occur in [c_lhs] *)

let make_c c_lhs c_rhs = {c_lhs; c_rhs}

let rhs_clause c = c.c_rhs

let pp_rule_clause out r =
  let pp_c = CCFormat.hvbox (Util.pp_list ~sep:" ∨ " Literal.pp) in
  Format.fprintf out "@[<2>@[%a@] ==>@ @[<v>%a@]@]"
    Literal.pp r.c_lhs (Util.pp_list pp_c) r.c_rhs

let compare_rule_term r1 r2 =
  CCOrd.(T.compare r1.lhs r2.lhs <?> (T.compare, r1.rhs, r2.rhs))

module Set = struct
  module S = CCMultiMap.Make(ID)(struct
      type t = rule_term
      let compare = compare_rule_term
    end)

  type t = {
    terms: S.t;
    clauses: rule_clause list;
  }
  (* head symbol -> set of rules *)

  let empty = {
    terms=S.empty;
    clauses=[];
  }

  let is_empty t = S.is_empty t.terms && t.clauses=[]

  let add_term r s = {s with terms=S.add s.terms r.lhs_id r}
  let add_clause r s = {s with clauses=r :: s.clauses}

  let find_iter s id = S.find_iter s.terms id

  let add_stmt stmt t = match Stmt.view stmt with
    | Stmt.Def (id, ty, rhs) ->
      (* simple constant *)
      let r = make_t_const id ty rhs in
      Util.debugf ~section 5 "@[<2>add rewrite rule@ `@[%a@]`@]" (fun k->k pp_rule_term r);
      add_term r t
    | Stmt.RewriteTerm (id, ty, args, rhs) ->
      let r = make_t id ty args rhs in
      Util.debugf ~section 5 "@[<2>add rewrite rule@ `@[%a@]`@]" (fun k->k pp_rule_term r);
      add_term r t
    | Stmt.RewriteForm (lhs, rhs) ->
      let lhs = Literal.Conv.of_form lhs in
      let rhs = List.map (List.map Literal.Conv.of_form) rhs in
      let r = make_c lhs rhs in
      Util.debugf ~section 5 "@[<2>add rewrite rule@ `@[%a@]`@]" (fun k->k pp_rule_clause r);
      add_clause r t
    | Stmt.TyDecl _
    | Stmt.Data _
    | Stmt.Assert _
    | Stmt.Goal _
    | Stmt.NegatedGoal _ -> t

  let to_seq_t t = S.to_seq t.terms |> Sequence.map snd
  let to_seq_c t = Sequence.of_list t.clauses

  let pp out t =
    Format.fprintf out "{@[<hv>%a@,%a@]}"
      (CCFormat.seq ~start:"" ~stop:"" pp_rule_term) (to_seq_t t)
      (CCFormat.seq ~start:"" ~stop:"" pp_rule_clause) (to_seq_c t)
end

(* TODO: {b long term}

   use De Bruijn  indices for rewrite rules, with RuleSet.t being a
   decision tree (similar to pattern-matching compilation) on head symbols
   + equality contraints for non-linear rules.

   Use the FOTerm.DB case extensively... *)

let normalize_term_ rules t =
  (* compute normal form of subterm
     @return [t'] where [t'] is the normal form of [t] *)
  let rec reduce ~subst sc t = match T.view t with
    | T.Const id ->
      (* pick a constant rule *)
      begin match Set.find_iter rules id |> Sequence.head with
        | None -> t
        | Some r ->
          assert (T.is_const r.lhs);
          (* reduce [rhs], but no variable can be bound *)
          reduce ~subst:Substs.empty 0 r.rhs
      end
    | T.App (f, l) ->
      (* first, reduce subterms *)
      let l' = reduce_l ~subst sc l in
      let t' = if T.same_l l l' then t else T.app f l' in
      begin match T.view f with
        | T.Const id ->
          let sc' = sc+1 in
          let find_rule =
            Set.find_iter rules id
            |> Sequence.find
              (fun r ->
                 try Some (r, Unif.FO.matching ~subst ~pattern:(r.lhs,sc') (t',sc))
                 with Unif.Fail -> None)
          in
          begin match find_rule with
            | None -> t'
            | Some (r, subst) ->
              (* rewrite [t = r.lhs\sigma] into [rhs] (and normalize [rhs],
                 which contain variables bound by [subst]) *)
              Util.debugf ~section 5 "@[<2>rewrite `@[%a@]`@ using `@[%a@]`@ with `@[%a@]`@]"
                (fun k->k T.pp t' pp_rule_term r Su.pp subst);
              Util.incr_stat stat_term_rw;
              reduce ~subst sc' r.rhs
          end
        | _ -> t'
      end
    | T.DB _ -> t
    | T.Var _ ->
      (* dereference, or return [t]. Careful not to traverse the already-
          evaluated value! *)
      fst (Su.FO.deref subst (t,sc))
    | T.AppBuiltin (_,[]) -> t
    | T.AppBuiltin (b,l) ->
      let l' = reduce_l ~subst sc l in
      if T.same_l l l' then t else T.app_builtin ~ty:(T.ty t) b l'
  (* reduce list *)
  and reduce_l ~subst sc l = match l with
    | [] -> []
    | t :: tail ->
      reduce ~subst sc t :: reduce_l ~subst sc tail
  in
  reduce ~subst:Su.empty 0 t

let normalize_term rules t =
  Util.with_prof prof_term_rw (normalize_term_ rules) t

let narrow_term ?(subst=Substs.empty) (rules,sc_r) (t,sc_t) = match T.view t with
  | T.Const _ -> Sequence.empty (* already normal form *)
  | T.App (f, _) ->
    begin match T.view f with
      | T.Const id ->
        Set.find_iter rules id
        |> Sequence.filter_map
          (fun r ->
             try Some (r, Unif.FO.unification ~subst (r.lhs,sc_r) (t,sc_t))
             with Unif.Fail -> None)
      | _ -> Sequence.empty
    end
  | T.Var _
  | T.DB _
  | T.AppBuiltin _ -> Sequence.empty

(* try to rewrite this literal, returning a list of list of lits instead *)
let step_lit rules lit =
  CCList.find_map
    (fun r ->
       let substs = Literal.matching ~pattern:(r.c_lhs,1) (lit,0) in
       match Sequence.head substs with
       | None -> None
       | Some subst -> Some (r.c_rhs, subst))
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
         | Some (clauses,subst) ->
           Util.debugf ~section 5
             "@[<2>rewrite `@[%a@]`@ into `@[<v>%a@]`@ with @[%a@]@]"
             (fun k->k Literal.pp lit
                 CCFormat.(list (hvbox (Util.pp_list ~sep:" ∨ " Literal.pp))) clauses
                Substs.pp subst);
           Util.incr_stat stat_clause_rw;
           Some (i, clauses, subst))
      lits
  in
  match step with
    | None -> None
    | Some (i, clause_chunks, subst) ->
      let renaming = Substs.Renaming.create () in
      (* remove rewritten literal, replace by [clause_chunks], apply
         substitution (clause_chunks might contain other variables!),
         distribute to get a CNF again *)
      let lits = CCList.Idx.remove lits i in
      let lits = Literal.apply_subst_list ~renaming subst (lits,0) in
      let clause_chunks = eval_ll ~renaming subst (clause_chunks,1) in
      let clauses = List.map (fun new_lits -> new_lits @ lits) clause_chunks in
      Some clauses

let normalize_clause rules lits =
  Util.with_prof prof_clause_rw (normalize_clause_ rules) lits

let narrow_lit ?(subst=Substs.empty) (rules,sc_r) (lit,sc_lit) =
  Sequence.of_list rules.Set.clauses
  |> Sequence.flat_map
    (fun r ->
       Literal.unify ~subst (r.c_lhs,sc_r) (lit,sc_lit)
       |> Sequence.map (fun subst -> r, subst))
