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

(** Reduction to CNF, and simplifications. See "computing small normal forms",
    in "handbook of automated reasoning". *)

open Types
open Symbols

module T = Terms
module C = Clauses
module S = FoSubst
module Lits = Literals
module Utils = FoUtils

(* ----------------------------------------------------------------------
 * Recognize clauses
 * ---------------------------------------------------------------------- *)

(** check whether the clause is already in CNF *)
let is_cnf lits =
  Utils.array_forall
    (fun (Equation (l, r, sign, _)) ->
      T.atomic_rec l && T.atomic_rec r && (l.sort != bool_ ||
                                          (l == T.true_term || r == T.true_term)))
    lits

(** Is the clause almost in CNF (i.e. some equivalences between
    an atomic prop and a prop remain)? *)
let is_quasi_cnf lits =
  let is_equiv = function
    | Equation (l, r, true, Gt) ->
      r != T.true_term && r != T.false_term && T.atomic_rec l && l.sort == bool_
    | Equation (l, r, true, Lt) ->
      l != T.true_term && l != T.false_term && T.atomic_rec r && r.sort == bool_
    | _ -> false
  and is_atomic = function
    | Equation (l, r, _, _) ->
      l.sort != bool_ || (T.atomic_rec l && T.atomic_rec r)
  in
  Utils.array_forall
    (fun lit -> is_atomic lit || is_equiv lit)
    lits

(* ----------------------------------------------------------------------
 * syntactic simplification
 * ---------------------------------------------------------------------- *)

(** Simplify a boolean term (a formula) *)
let rec simplify_term t =
  let mark_simplified t = T.set_flag T.flag_simplified t true in
  if T.get_flag T.flag_simplified t then t else  (* maybe it's already simplified *)
  match t.term with
  | Var _ | Node (_, []) | BoundVar _ -> (mark_simplified t; t)
  | Bind (f, t') when not (T.db_contains t' 0) ->
    simplify_term t'  (* eta-reduction: binder binds nothing, remove it *)
  | Bind (f, t') -> T.mk_bind ~old:t f t.sort (simplify_term t')
  | Node (s, [{term=Bind (s', t')}]) when s == not_symbol && s' == forall_symbol ->
    simplify_term (T.mk_exists (T.mk_not t'))  (* not forall t -> exists (not t) *)
  | Node (s, [{term=Bind (s', t')}]) when s == not_symbol && s' == exists_symbol ->
    simplify_term (T.mk_forall (T.mk_not t'))  (* not exists t -> forall (not t) *)
  | Node (s, [{term=Node (s', [t'])}]) when s == not_symbol && s' == not_symbol ->
    simplify_term t'  (* double negation *)
  | Node (s, [t']) when s == not_symbol && t' == T.true_term ->
    T.false_term  (* not true -> false *)
  | Node (s, [t']) when s == not_symbol && t' == T.false_term ->
    T.true_term   (* not false -> true *)
  | Node (s, [a; b]) when s == and_symbol && (a == T.false_term || b == T.false_term) ->
    T.false_term  (* a and false -> false *)
  | Node (s, [a; b]) when s == or_symbol && (a == T.true_term || b == T.true_term) ->
    T.true_term  (* a or true -> true *)
  | Node (s, [a; b]) when s == or_symbol && a == T.false_term ->
    simplify_term b  (* b or false -> b *)
  | Node (s, [a; b]) when s == or_symbol && b == T.false_term ->
    simplify_term a  (* a or false -> a *)
  | Node (s, [a; b]) when s == and_symbol && a == T.true_term ->
    simplify_term b  (* b and true -> b *)
  | Node (s, [a; b]) when s == and_symbol && b == T.true_term ->
    simplify_term a  (* a and true -> a *)
  | Node (s, [a; b]) when s == imply_symbol && (a == T.false_term || b == T.true_term) ->
    T.true_term  (* (false => a) or (a => true) -> true *)
  | Node (s, [a; b]) when s == imply_symbol && a == T.true_term ->
    simplify_term b  (* (true => a) -> a *)
  | Node (s, [a; b]) when s == eq_symbol && a == b ->
    T.true_term  (* a = a -> true *)
  | Node (s, [a; b]) when s == eq_symbol && 
    ((a == T.true_term && b == T.false_term) ||
     (b == T.true_term && a == T.false_term)) ->
    T.false_term (* true = false -> false *)
  | Node (s, [a; b]) when s == eq_symbol && b == T.true_term ->
    simplify_term a  (* a = true -> a *)
  | Node (s, [a; b]) when s == eq_symbol && a == T.true_term ->
    simplify_term b  (* b = true -> b *)
  | Node (s, [a; b]) when s == eq_symbol && b == T.false_term ->
    simplify_term (T.mk_not a)  (* a = false -> not a *)
  | Node (s, [a; b]) when s == eq_symbol && a == T.false_term ->
    simplify_term (T.mk_not b)  (* b = false -> not b *)
  | Node (s, l) ->
    let l' = List.map simplify_term l in
    if List.for_all2 (==) l l'
      then (mark_simplified t; t)
      else 
        let new_t = T.mk_node s t.sort (List.map simplify_term l) in
        simplify_term new_t

(** Simplify the inner formula (double negation, trivial equalities...) *)
let simplify hc =
  let ctx = hc.hcctx in
  let simplified = ref false in
  (* simplify a lit *)
  let simp_lit (Equation (l,r,sign,_) as lit) =
    let lit' = Lits.mk_lit ~ord:ctx.ctx_ord (simplify_term l) (simplify_term r) sign in
    (if not (Lits.eq lit lit') then simplified := true);
    lit'
  in
  let lits = Array.map simp_lit hc.hclits in
  if !simplified
    then C.mk_hclause_a ~ctx lits (C.adapt_proof hc.hcproof)
    else hc  (* no simplification *)

(* ----------------------------------------------------------------------
 * reduction to CNF
 * ---------------------------------------------------------------------- *)

(** Apply miniscoping (push quantifiers as deep as possible in the formula) to the term *)
let rec miniscope_term t =
  (* build a n-ary and/or *)
  let rec mk_n_ary s l = match l with
  | [] -> assert false
  | x::[] -> x
  | x::y::l' ->  (* pop x, y from stack *)
    let t = T.mk_node s bool_ [x;y] in
    mk_n_ary s (t::l')  (* push back (x op y) on stack *)
  in
  (* simplify the term *)
  let t = simplify_term t in
  (* recursive miniscoping *)
  match t.term with
  | Bind (s, {term=Node (s', l)})
    when (s == forall_symbol || s == exists_symbol) && (s' == and_symbol || s' == or_symbol) ->
    (* Q x. a and/or b -> (Q x. a) and/or b  if x \not\in vars(b) *)
    let a, b = List.partition (fun f -> T.db_contains f 0) l in
    assert (a <> []);  (* eta-reduction should have worked! *)
    if b <> []
      then
        (* distribute forall over and, or exists over or; otherwise keep it outside *)
        let a =
          if ((s == forall_symbol && s' == and_symbol)
            || (s == exists_symbol && s' == or_symbol))
          then mk_n_ary s' (List.map (fun t -> miniscope_term (T.mk_bind s bool_ t)) a)
          else T.mk_bind s bool_ (mk_n_ary s' a)
        in
        (* some subformulas do not contain x, put them outside of quantifier *)
        let b = mk_n_ary s' (List.map (fun t -> miniscope_term (T.db_unlift t)) b) in
        simplify_term (T.mk_node s' bool_ [a; b])
      else t
  | Bind (_, _) -> t
  | BoundVar _ | Var _ | Node _ -> t

(** Apply miniscoping transformation to the clause *)
let miniscope hc =
  let ctx = hc.hcctx in
  let simplified = ref false in
  (* simplify a lit *)
  let miniscope_lit (Equation (l,r,sign,_) as lit) =
    let lit' = Lits.mk_lit ~ord:ctx.ctx_ord (miniscope_term l) (miniscope_term r) sign in
    (if not (Lits.eq lit lit') then simplified := true);
    lit'
  in
  let lits = Array.map miniscope_lit hc.hclits in
  if !simplified
    then (* mark the miniscoping as a proof step, and produce a new clause *)
      let proof c = Proof (c, "miniscope", [hc.hcproof]) in
      let hc' = C.mk_hclause_a ~parents:[hc] ~ctx lits proof in
      Utils.debug 3 (lazy (Utils.sprintf "miniscoped @[<h>%a@] into @[<h>%a@]"
                    !C.pp_clause#pp_h hc !C.pp_clause#pp_h hc'));
      hc'
    else hc  (* no miniscoping done *)

(** negation normal form (also remove equivalence and implications) *) 
let rec nnf t =
  if t.sort != bool_ then t else
  match t.term with
  | Var _ | Node (_, []) | BoundVar _ -> t
  | Bind (f, t') -> T.mk_bind f t.sort (nnf t')
  | Node (s, [{term=Node (s', [a; b])}]) when s = not_symbol && s' = and_symbol ->
    nnf (T.mk_or (T.mk_not a) (T.mk_not b))  (* de morgan *)
  | Node (s, [{term=Node (s', [a; b])}]) when s = not_symbol && s' = or_symbol ->
    nnf (T.mk_and (T.mk_not a) (T.mk_not b)) (* de morgan *)
  | Node (s, [a; b]) when s = imply_symbol ->
    nnf (T.mk_or (T.mk_not a) b) (* (a => b) -> (not a or b) *)
  | Node (s, [a; b]) when s = eq_symbol && a.sort == bool_ ->
    (* (a <=> b) -> (not a or b) and (not b or a) *)
    nnf (T.mk_and
      (T.mk_or (T.mk_not a) b)
      (T.mk_or (T.mk_not b) a))
  | Node (s, [{term=Node (s', [a; b])}]) when s = not_symbol && s' = imply_symbol ->
    nnf (T.mk_and a (T.mk_not b)) (* not (a => b) -> (a and not b) *)
  | Node (s, [{term=Node (s', [a; b])}])
    when s = not_symbol && s' = eq_symbol && a.sort == bool_ ->
    (* not (a <=> b) -> (a <=> (not b)) *)
    nnf (T.mk_or
      (T.mk_and a (T.mk_not b))
      (T.mk_and b (T.mk_not a)))
  | Node (s, [{term=Bind (s', t')}]) when s = not_symbol && s' = forall_symbol ->
    nnf (T.mk_exists (T.mk_not t')) (* not forall -> exists not *)
  | Node (s, [{term=Bind (s', t')}]) when s = not_symbol && s' = exists_symbol ->
    nnf (T.mk_forall (T.mk_not t')) (* not exists -> forall not *)
  | Node (s, [{term=Node (s', [t])}]) when s = not_symbol && s' = not_symbol ->
    nnf t (* double negation *)
  | Node (s, l) ->
    let t' = T.mk_node s t.sort (List.map nnf l) in
    if T.eq_term t t' then t' else nnf t'

(** skolemization of existentials, removal of forall *)
let rec skolemize ~ord ~var_index t = match t.term with
  | Var _ | Node (_, []) | BoundVar _ -> t
  | Node (s, [{term=Node (s', [t])}]) when s = not_symbol && s' = not_symbol ->
    skolemize ~ord ~var_index t (* double negation *)
  | Bind (s, t') when s = forall_symbol ->
    (* a fresh variable *)
    let sort = match T.look_db_sort 0 t with
      | None -> univ_
      | Some s -> s in
    let v = T.mk_var (!var_index) sort in
    incr var_index;
    let new_t' = T.db_unlift (T.db_replace t' v) in
    skolemize ~ord ~var_index new_t' (* remove forall *)
  | Bind (s, t') when s = exists_symbol ->
    (* make a skolem symbol *)
    let sort = match T.look_db_sort 0 t with
      | None -> univ_
      | Some s -> s in
    let new_t' = !T.skolem ~ord t' sort in
    skolemize ~ord ~var_index new_t' (* remove forall *)
  | Bind (s, t') -> T.mk_bind s t.sort (skolemize ~ord ~var_index t')
  | Node (s, l) -> T.mk_node s t.sort (List.map (skolemize ~ord ~var_index) l)

(** reduction to cnf using De Morgan laws. Returns a list of list of terms *)
let rec to_cnf t =
  if t.sort != bool_ then [[t, true]]
  else match t.term with
  | Var _ | Node (_, []) | BoundVar _ -> [[t, true]]
  | Node (s, [t']) when s = not_symbol ->
    assert (T.atomic_rec t' ||
            match t'.term with Node (s', _) when s' = eq_symbol -> true | _ -> false);
    [[t', false]]
  | Node (s, [a; b]) when s = and_symbol ->
    let ca = to_cnf a
    and cb = to_cnf b in
    List.rev_append ca cb
  | Node (s, [a; b]) when s = or_symbol ->
    product (to_cnf a) (to_cnf b)
  | Node _ | Bind _ -> [[t, true]]
(* cartesian product of lists of lists *)
and product a b =
  List.fold_left
    (fun acc litsa -> List.fold_left
      (fun acc' litsb -> (litsa @ litsb) :: acc')
      acc b)
    [] a

(** Transform the clause into proper CNF; returns a list of clauses *)
let cnf_of hc =
  let ctx = hc.hcctx in
  let ord = ctx.ctx_ord in
  let var_index = ref 0 in
  (* unique counter for variable indexes *)
  Utils.debug 3 (lazy (Utils.sprintf "input clause %a@." !C.pp_clause#pp_h hc));
  if is_cnf hc.hclits
    then begin
      Utils.debug 3 (lazy (Utils.sprintf "clause @[<h>%a@] is cnf" !C.pp_clause#pp_h hc));
      [hc] (* already cnf, perfect *)
    end else
      (* simplify clause *)
      let hc = simplify hc in
      (* steps of CNF reduction *)
      let lits = Array.to_list hc.hclits in
      let nnf_lits = List.map (fun lit -> nnf (Lits.term_of_lit lit)) lits in
      let miniscoped_lits = List.map miniscope_term nnf_lits in
      let skolem_lits = List.map (fun t -> skolemize ~ord ~var_index t) miniscoped_lits in
      let clauses_of_lits = List.map to_cnf skolem_lits in
      (* list of list of literals, by or-product *)
      let lit_list_list = match clauses_of_lits with
        | [] -> assert false  (* is in cnf ;) *)
        | hd::tl -> List.fold_left product hd tl in
      (* build clauses from lits *)
      let proof c = Proof (c, "to_cnf", [hc.hcproof]) in
      let clauses = List.map
        (fun lits ->
          let lits = List.map (fun (t, sign) -> Lits.mk_lit ~ord t T.true_term sign) lits in
          let new_hc = C.mk_hclause ~parents:[hc] ~ctx lits proof in
          Utils.debug 4 (lazy (Utils.sprintf "mk_clause %a@." !C.pp_clause#pp_h new_hc));
          C.clause_of_fof new_hc)
        lit_list_list
      in
      Utils.debug 3 (lazy (Utils.sprintf "%% clause @[<h>%a@] to_cnf -> @[<h>%a@]"
                    !C.pp_clause#pp_h hc (Utils.pp_list !C.pp_clause#pp_h) clauses));
      List.iter (fun hc -> assert (is_cnf hc.hclits)) clauses;
      clauses
