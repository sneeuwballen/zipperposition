
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

open Libzipperposition

(** {1 Rewrite Rules} *)
module T = FOTerm
module Stmt = Statement
module Su = Substs

let section = Util.Section.(make ~parent:zip "rewriting")

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

let pp_rule_term out r =
  Format.fprintf out "@[<2>@[%a@] -->@ @[%a@]@]" T.pp r.lhs T.pp r.rhs

type rule_clause = {
  c_lhs: Literal.t;
  c_rhs: Literal.t list list; (* list of clauses *)
}
(* invariant: all variables in [c_rhs] also occur in [c_lhs] *)

let make_c c_lhs c_rhs = {c_lhs; c_rhs}

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
      Util.debugf ~section 5 "@[<2>add rewrite rule (c)@ `@[%a@]`@]" (fun k->k pp_rule_clause r);
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

let normalize_term rules t =
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
      let t' = if List.for_all2 T.equal l l' then t else T.app f l' in
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
              reduce ~subst sc' r.rhs
          end
        | _ -> t'
      end
    | T.DB _
    | T.Var _ ->
      (* dereference, or return [t] *)
      Su.FO.apply_no_renaming subst (t,sc)
    | T.AppBuiltin (_,[]) -> t
    | T.AppBuiltin (b,l) ->
      let l' = reduce_l ~subst sc l in
      if List.for_all2 T.equal l l' then t else T.app_builtin ~ty:(T.ty t) b l'
  (* reduce list *)
  and reduce_l ~subst sc l =
    List.map (reduce ~subst sc) l
  in
  reduce ~subst:Su.empty 0 t

(* try to rewrite this literal, returning a list of list of lits instead *)
let step_lit rules lit =
  let eval_ll subst (l,sc) =
    List.map
      (List.map
         (fun lit -> Literal.apply_subst_no_renaming subst (lit,sc)))
      l
  in
  CCList.find_map
    (fun r ->
       let substs = Literal.matching ~pattern:(r.c_lhs,1) (lit,0) in
       match Sequence.head substs with
       | None -> None
       | Some subst ->
         (* win! *)
         let clauses = eval_ll subst (r.c_rhs,1) in
         Some clauses)
    rules

let normalize_clause rules lits =
  let step =
    CCList.find_mapi
      (fun i lit -> match step_lit rules.Set.clauses lit with
         | None -> None
         | Some clauses ->
           Util.debugf ~section 5
             "@[<2>rewrote `@[%a@]`@ into `@[<v>%a@]`@]"
             (fun k->k Literal.pp lit
                 CCFormat.(list (hvbox (Util.pp_list ~sep:" ∨ " Literal.pp))) clauses);
           Some (i, clauses))
      lits
  in
  match step with
    | None -> None
    | Some (i, clause_chunks) ->
      (* remove rewritten literal, replace by [clause_chunks], distribute to
         get a CNF again *)
      let lits = CCList.Idx.remove lits i in
      let clauses =
        Util.map_product clause_chunks ~f:(fun new_lits -> [new_lits @ lits])
      in
      Some clauses
