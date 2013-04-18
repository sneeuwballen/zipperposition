(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** {1 Term rewriting} *)

open Basic

module T = Terms
module C = Clauses
module S = FoSubst
module DT = Dtree
module Unif = FoUnif
module Utils = FoUtils

let prof_ordered_rewriting = Utils.mk_profiler "rewriting.ordered"

let stat_ordered_rewriting = mk_stat "rewriting.ordered.steps"

(** {2 Ordered rewriting} *)

module TermHASH = struct
  type t = term
  let equal = (==)
  let hash t = t.tag
end

(** Memoization cache for rewriting *)
module TCache = Cache.Replacing(TermHASH)

module OrderedTRS = struct
  type t = {
    ord : ordering;
    mutable rules : rule Dtree.dtree;
  } (** Ordered rewriting system *)
  and rule = {
    rule_clause : hclause;
    rule_left : term;       (** Pattern *)
    rule_right : term;      (** Result *)
    rule_oriented : bool;
  } (** A rule, oriented or not *)

  let eq_rule r1 r2 =
    C.eq_hclause r1.rule_clause r2.rule_clause &&
    r1.rule_oriented = r2.rule_oriented &&
    r1.rule_left == r2.rule_left &&
    r1.rule_right == r2.rule_right

  let rule_priority rule =
    (* better priority for oriented rules *)
    if rule.rule_oriented then 1 else 2

  let create ~ord =
    { ord;
      rules = Dtree.empty eq_rule;
    }

  let mk_rule hc l r oriented =
    { rule_clause=hc; rule_left=l; rule_right=r; rule_oriented=oriented; }

  (** Extract a list of rules from the clause *)
  let rules_of_hc hc =
    match hc.hclits with
    | [| Equation (l,r,true,Gt) |] ->
      [mk_rule hc l r true]
    | [| Equation (l,r,true,Lt) |] ->
      [mk_rule hc r l true]
    | [| Equation (l,r,true,Incomparable) |] ->
      [mk_rule hc l r false; mk_rule hc r l false]
    | _ -> []

  let add_clause trs hc =
    assert (hc.hcctx.ctx_ord == trs.ord);
    let rules = rules_of_hc hc in
    trs.rules <- List.fold_left
      (fun rules rule ->
        (* add the rule to the list of rules *)
        let priority = rule_priority rule in
        let rules = Dtree.add rules ~priority rule.rule_left rule in
        rules)
      trs.rules rules

  let add_seq trs seq =
    Sequence.iter (add_clause trs) seq

  let to_seq trs =
    let rules = trs.rules in
    Sequence.from_iter
      (fun k ->
        Dtree.iter rules (fun _ rule -> k rule.rule_clause))

  let size trs =
    let r = ref 0 in
    Dtree.iter trs.rules (fun _ _ -> incr r);
    !r
  
  exception RewrittenInto of term

  (** Given a TRS and a cache size, build a memoized function that
      performs term rewriting *)
  let mk_rewrite trs ~size =
    (* reduce to normal form. [reduce'] is the memoized version of reduce. *)
    let rec reduce reduce' t =
      match t.term with
      | Var _ | BoundVar _ -> t
      | Bind (s, a_sort, t') ->
        let t' = reduce' t' in
        T.mk_bind ~old:t s t.sort a_sort t'
      | Node (s, l) ->
        let l' = List.map reduce' l in
        let t' = if List.for_all2 (==) l l'
          then t
          else T.mk_node s t.sort l' in
        (* now rewrite the term itself *)
        rewrite_here reduce' t'
    (* rewrite once at this position. If it succeeds,
       yields back to [reduce]. *)
    and rewrite_here reduce' t =
      try
        Dtree.iter_match (trs.rules,1) (t,0)
          (fun _ rule subst ->
            (* right-hand part *)
            let r = rule.rule_right in
            let r' = S.apply_subst subst (r,1) in
            if rule.rule_oriented
              then raise (RewrittenInto r')  (* we know that t > r' *)
              else (
                assert (t == S.apply_subst subst (rule.rule_left,1));
                if trs.ord#compare t r' = Gt
                  then raise (RewrittenInto r')
                  else ()));
        t (* could not rewrite t *)
      with RewrittenInto t' ->
        Utils.debug 3 "%% rewrite @[<h>%a into %a@]" !T.pp_term#pp t !T.pp_term#pp t';
        incr_stat stat_ordered_rewriting;
        assert (trs.ord#compare t t' = Gt);
        reduce reduce' t'  (* term is rewritten, reduce it again *)
    in
    let cache = TCache.create size in
    let reduce = TCache.with_cache_rec cache reduce in
    (* The main rewriting function *)
    let rewrite t =
      Utils.enter_prof prof_ordered_rewriting;
      let t' = reduce t in
      Utils.exit_prof prof_ordered_rewriting;
      t'
    in
    rewrite

  let pp formatter trs =
    Format.fprintf formatter "@[<hov2>%a@]"
      (Sequence.pp_seq ~sep:" and " !C.pp_clause#pp_h)
      (to_seq trs)
end

(** {2 Regular rewriting} *)

module TRS = struct
  type rule = (term * term)

  type t = {
    mutable index : term DT.dtree;    (** index of rules *)
    mutable nbr_rules : int;          (** number of rules *)
  } (** Term Rewriting System *)

  let create () = {
    index = DT.empty T.eq_term;
    nbr_rules = 0;
  }

  let add_rule trs (l, r) =
    (* check that the rule does not introduce variables *)
    assert (List.for_all
      (fun v -> T.member_term v l)
      (T.vars r));
    assert (not (T.is_var l));
    assert (l.sort = r.sort);
    (* add rule to the discrimination tree *)
    (* now add the rule to the index *)
    trs.index <- DT.add trs.index l r;
    trs.nbr_rules <- trs.nbr_rules + 1

  let add_rules trs l = List.iter (add_rule trs) l

  let from_list l =
    let trs = create () in
    add_rules trs l;
    trs

  let size trs = trs.nbr_rules

  let iter trs k =
    DT.iter trs.index (fun l r -> k (l, r))

  let pp_rule formatter (l, r) =
    Format.fprintf formatter "@[<h>%a → %a@]" !T.pp_term#pp l !T.pp_term#pp r

  let pp_trs formatter trs =
    let rules = ref [] in
    iter trs (fun rule -> rules := rule :: !rules);
    Format.fprintf formatter "{@[<hv>%a@]}"
      (Utils.pp_list ~sep:";" pp_rule) !rules

  (* ----------------------------------------------------------------------
   * Computation of normal forms
   * ---------------------------------------------------------------------- *)

  exception RewrittenIn of term

  (** Compute normal form of the term, and set its binding to the normal form *)
  let rewrite trs t = 
    (* compute normal form of this term *)
    let rec compute_nf offset trs t =
      match t.term with
      | Bind (s, a_sort, t') ->
        let t'' = compute_nf offset trs t' in
        let new_t = T.mk_bind ~old:t s t.sort a_sort t'' in
        reduce_at_root offset trs new_t
      | Node (hd, l) ->
        (* rewrite subterms first *)
        let l' = List.map (compute_nf offset trs) l in
        let t' = T.mk_node ~old:t hd t.sort l' in
        (* rewrite at root *)
        reduce_at_root offset trs t'
      | Var _ | BoundVar _ -> assert false
    (* assuming subterms are in normal form, reduce the term *)
    and reduce_at_root offset trs t =
      try
        DT.iter_match (trs.index,offset) (t,0) rewrite_handler;
        t  (* normal form *)
      with (RewrittenIn t') ->
        compute_nf offset trs t' (* rewritten in t', continue *)
    (* attempt to use one of the rules to rewrite t *)
    and rewrite_handler (l,o) r subst =
      (* all vars in [r] are bound in [subst] *)
      let t' = S.apply_subst subst (r,o) in
      raise (RewrittenIn t')
    in
    (* any offset will do, as long as it's <> 0, because no variable of the TRS
       should remain free during instantiation (vars(r) \subset vars(l) for all rules) *)
    compute_nf 1 trs t

  let pp_trs_index formatter trs = DT.pp_term_tree formatter trs.index
end
