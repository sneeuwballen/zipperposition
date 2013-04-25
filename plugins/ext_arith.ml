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

(** {1 Plugin for TSTP arithmetic} *)

(** See http://www.cs.miami.edu/~tptp/TPTP/TR/TPTPTR.shtml#TypeSystem *)

open Basic
open Symbols

module T = Terms
module C = Clauses
module S = FoSubst

(** simplification function for arithmetic *)
let rec arith_canonize t =
  match t.term with
  | Var _
  | BoundVar _ -> t
  | Bind (s, a_sort, t') ->
    let new_t' = arith_canonize t' in
    T.mk_bind s t.sort a_sort new_t'
  | Node (s, [a]) ->
    let a' = arith_canonize a in
    try_reduce_unary s t.sort a'
  | Node (s, [a; b]) ->
    let a' = arith_canonize a
    and b' = arith_canonize b in
    try_reduce_binary s t.sort a' b'
  (*
  | Node (s, l) when Symbols.symbol_val s == Const "$distinct" ->
    try_reduce_distinct l
  *)
  | Node (s, l) ->
    let l' = List.map arith_canonize l in
    T.mk_node s t.sort l'
(** unary builtins *)
and try_reduce_unary s sort a =
  match Symbols.get_val s, a.term with
  | Const "$uminus", Node (n, []) when is_numeric n ->
    T.mk_const (Arith.uminus n) a.sort
  | Const "$floor", Node (n, []) when is_numeric n ->
    T.mk_const (Arith.floor n) int_
  | Const "$ceiling", Node (n, []) when is_numeric n ->
    T.mk_const (Arith.ceiling n) int_
  | Const "$round", Node (n, []) when is_numeric n ->
    T.mk_const (Arith.round n) int_
  | Const "$truncate", Node (n, []) when is_numeric n ->
    T.mk_const (Arith.truncate n) int_
  | Const "$is_int", Node (n, []) when is_numeric n ->
    if is_int n then T.true_term else T.false_term
  | Const "$is_rat", Node (n, []) when is_numeric n ->
    if is_rat n then T.true_term else T.false_term
  | Const "$is_real", Node (n, []) when is_numeric n ->
    if is_real n then T.true_term else T.false_term
  | _ ->
    T.mk_node s sort [a]  (* default case *)
(** binary builtins *)
and try_reduce_binary s sort a b =
  match Symbols.get_val s, a.term, b.term with
  | Const "$sum", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.sum na nb) sort
  | Const "$difference", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.difference na nb) sort
  | Const "$product", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.product na nb) sort
  | Const "$quotient", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.quotient na nb) sort
  | Const "$quotient_e", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.quotient_e na nb) sort
  | Const "$quotient_t", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.quotient_t na nb) sort
  | Const "$quotient_f", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.quotient_f na nb) sort
  | Const "$remainder_e", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.remainder_e na nb) sort
  | Const "$remainder_t", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.remainder_t na nb) sort
  | Const "$remainder_f", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    T.mk_const (Arith.remainder_f na nb) sort
  | Const "$less", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    if Arith.less na nb then T.true_term else T.false_term
  | Const "$lesseq", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    if Arith.lesseq na nb then T.true_term else T.false_term
  | Const "$greater", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    if Arith.greater na nb then T.true_term else T.false_term
  | Const "$greatereq", Node (na, []), Node (nb, []) when is_numeric na && is_numeric nb ->
    if Arith.greatereq na nb then T.true_term else T.false_term
  | _ ->
    T.mk_node s sort [a; b]  (* default case *)
  
(** Expert that evaluates arithmetic expressions *)
let rec expert ~ctx =
  let open Experts in
  let signature = SSet.empty in
  { expert_name = "arith";
    expert_descr = "evaluation for TSTP arithmetic";
    expert_equal = (fun t1 t2 -> arith_canonize t1 == arith_canonize t2);
    expert_sig = signature;
    expert_clauses = [];
    expert_canonize = arith_canonize;
    expert_ord = (fun _ -> true);
    expert_ctx = ctx;
    expert_update_ctx = (fun ctx -> [expert ~ctx]);
    expert_solve = None;
  }

(** Instantiate [hc], minus its i-th component, with given bindings *)
let rebuild ?(rule="arith") hc i bindings =
  let ctx = hc.hcctx in
  let subst = List.fold_left
    (fun s (x,t) -> S.bind s (x,0) (t,0)) S.id_subst bindings in
  let lits = FoUtils.array_except_idx hc.hclits i in
  let lits = Literals.apply_subst_list ~ord:ctx.ctx_ord subst (lits,0) in
  let proof c = Proof (c, rule, [hc.hcproof]) in
  let parents = [hc] in
  let new_clause = C.mk_hclause ~parents ~ctx lits proof in
  FoUtils.debug 3 "%% arith deduction (%s with @[<h>%a@]): @[<h>%a@]"
    rule S.pp_substitution subst !C.pp_clause#pp_h new_clause;
  new_clause

let plus a b = T.mk_node (mk_symbol "$sum") a.sort [a; b]
let minus a b = T.mk_node (mk_symbol "$difference") a.sort [a; b]

let succ a =
  if a.sort == real_
    then plus a (T.mk_const Arith.one_f real_)
    else plus a (T.mk_const Arith.one_i a.sort)

let pred a =
  if a.sort == real_
    then minus a (T.mk_const Arith.one_f real_)
    else minus a (T.mk_const Arith.one_i a.sort)

(** Propose some clauses, derived from [hc], where the [i-th] literal is
    removed (and proposition [t] is satisfied) *)
let try_satisfy hc i t =
  match t.term with
  | Node ({symb_val=Const "$less"}, [{term=Var _} as v; b])
  | Node ({symb_val=Const "$lesseq"}, [{term=Var _} as v; b]) 
  | Node ({symb_val=Const "$greater"}, [b; {term=Var _} as v]) 
  | Node ({symb_val=Const "$greatereqeq"}, [b; {term=Var _} as v]) when not (T.var_occurs v b) ->
    [rebuild hc i [v, pred b]]
  | Node ({symb_val=Const "$less"}, [b; {term=Var _} as v])
  | Node ({symb_val=Const "$lesseq"}, [b; {term=Var _} as v])
  | Node ({symb_val=Const "$greater"}, [{term=Var _} as v; b])
  | Node ({symb_val=Const "$greatereq"}, [{term=Var _} as v; b]) when not (T.var_occurs v b) ->
    [rebuild hc i [v, succ b]]
  | _ -> []

let try_contradict hc i t =
  []  (* TODO *)

let try_make_eq hc i l r =
  []  (* TODO *)

let try_make_neq hc i l r =
  [] (* TODO *)

(** inference rule that tries some basic hacks *)
let unary_inf_rule hc =
  (* try to eliminate one literal *)
  let basic_solve_lit acc i lit = match lit with
  | Equation (t, true_, false, _) when true_ == T.true_term ->
    try_satisfy hc i t @ acc
  | Equation (true_, t, false, _) when true_ == T.true_term ->
    try_satisfy hc i t @ acc
  | Equation (t, true_, true, _) when true_ == T.true_term ->
    try_contradict hc i t @ acc
  | Equation (true_, t, true, _) when true_ == T.true_term ->
    try_contradict hc i t @ acc
  | Equation (l, r, false, _) when l.sort != bool_ ->
    try_make_eq hc i l r
  | Equation (l, r, true, _) when l.sort != bool_ ->
    try_make_neq hc i l r @ acc
  | _ -> acc
  in
  FoUtils.array_foldi basic_solve_lit [] hc.hclits

(** The extension itself *)
let ext =
  let open Extensions in
  let actions =
    [ Ext_expert expert;
      Ext_unary_inf_rule ("arith_inst", unary_inf_rule);
      Ext_term_rewrite ("arith_eval", arith_canonize);
      Ext_signal_incompleteness]
  in
  { name = "arith";
    actions;
  }

let _ =
  Extensions.register ext

