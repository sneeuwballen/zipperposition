
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Rewriting} *)

open Libzipperposition

module T = FOTerm
module Stmt = Statement
module Su = Substs

let section = Util.Section.zip

type rule = {
  lhs: T.t;
  rhs: T.t;
  lhs_id: ID.t; (* head of lhs *)
}

module Rule = struct
  type t = rule

  (* constant rule [id := rhs] *)
  let make_const id rhs =
    { lhs_id=id; lhs=T.const ~ty:(T.ty rhs) id; rhs; }

  let pp out r =
    Format.fprintf out "@[<2>@[%a@] -->@ @[%a@]@]" T.pp r.lhs T.pp r.rhs

  let compare r1 r2 =
    CCOrd.(T.compare r1.lhs r2.lhs <?> (T.compare, r1.rhs, r2.rhs))
end

let pp_rule = Rule.pp

module RuleSet = struct
  module S = CCMultiMap.Make(ID)(Rule)

  type t = S.t
  (* head symbol -> set of rules *)

  let empty = S.empty
  let add r s = S.add s r.lhs_id r
  let add_list l s = List.fold_right add l s

  let find_iter = S.find_iter
  let find = S.find

  let add_stmt stmt t = match Stmt.view stmt with
    | Stmt.Def (id, _, rhs) ->
      (* simple constant *)
      let r = Rule.make_const id rhs in
      add r t
    | _ -> t

  let to_seq t = S.to_seq t |> Sequence.map snd
  let to_list t = to_seq t |> Sequence.to_rev_list

  let pp out t =
    let pp_pair out (_,r) = pp_rule out r in
    Format.fprintf out "{@[<hv>%a@]}"
      (CCFormat.seq ~start:"" ~stop:"" pp_pair) (S.to_seq t)
end

(** {2 Normalization} *)

(* TODO: {b long term}

   use De Bruijn  indices for rewrite rules, with RuleSet.t being a
   decision tree (similar to pattern-matching compilation) on head symbols
   + equality contraints for non-linear rules.

   Use the FOTerm.DB case extensively... *)

let normalize rules t =
  (* compute normal form of subterm
     @return [t'] where [t'] is the normal form of [t] *)
  let rec reduce ~subst sc t = match T.view t with
    | T.Const id ->
      (* pick a constant rule *)
      begin match RuleSet.find_iter rules id |> Sequence.head with
        | None -> t
        | Some r ->
          assert (T.is_const r.lhs);
          r.rhs
      end
    | T.App (f, l) ->
      (* first, reduce subterms *)
      let l' = reduce_l ~subst sc l in
      let t' = if List.for_all2 T.equal l l' then t else T.app f l' in
      begin match T.view f with
        | T.Const id ->
          let find_rule =
            RuleSet.find_iter rules id
            |> Sequence.find
              (fun r ->
                 try Some (r, Unif.FO.matching ~subst ~pattern:(r.lhs,sc) (t',0))
                 with Unif.Fail -> None)
          in
          begin match find_rule with
            | None -> t'
            | Some (r, subst) ->
              (* rewrite [t = r.lhs\sigma] into [rhs] (and evaluate [rhs],
                 which contain variables bound by [subst]) *)
              Util.debugf ~section 5 "@[<2>rewrite `@[%a@]`@ using `@[%a@]`@ with `@[%a@]`@]"
                (fun k->k T.pp t' pp_rule r Su.pp subst);
              Su.FO.apply_no_renaming subst (r.rhs,sc)
          end
        | _ -> t'
      end
    | T.DB _
    | T.Var _ ->
      (* dereference, or return [t] *)
      Su.FO.apply_no_renaming subst (t,0)
    | T.AppBuiltin (_,[]) -> t
    | T.AppBuiltin (b,l) ->
      let l' = reduce_l ~subst sc l in
      if List.for_all2 T.equal l l' then t else T.app_builtin ~ty:(T.ty t) b l'
  (* reduce list *)
  and reduce_l ~subst sc l =
    List.map (reduce ~subst sc) l
  in
  reduce ~subst:Su.empty 0 t

(** {2 Pre-processing Rules} *)

module Key = struct
  let set = Flex_state.create_key()
end

(* TODO: somewhere, remove definitions that have been successfully converted
   into rules, from the list of statements (before conversion to list of clauses) *)

let post_cnf stmts st =
  CCVector.iter Statement.scan_stmt_for_defined_cst stmts;
  (* add set of rules to [st] *)
  let rules =
    CCVector.fold (fun set s -> RuleSet.add_stmt s set) RuleSet.empty stmts
  in
  st
    |> Flex_state.add Key.set rules

(* add a term simplification that normalizes terms w.r.t the set of rules *)
let normalize_simpl (module E : Env_intf.S) =
  let rules = E.flex_get Key.set in
  (* simplification rule *)
  let simpl t =
    let t' = normalize rules t in
    if T.equal t t' then None
    else (
      Util.debugf ~section 4
        "@[<2>rewrite `@[%a@]`@ into `@[%a@]`@]" (fun k->k T.pp t T.pp t');
      Some t'
    )
  in
  E.add_rewrite_rule "rewrite_defs" simpl

let extension =
  let open Extensions in
  { default with
    name = "rewriting";
    post_cnf_actions=[post_cnf];
    env_actions=[normalize_simpl];
  }

