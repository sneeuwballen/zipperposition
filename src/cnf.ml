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
module Utils = FoUtils

(* ----------------------------------------------------------------------
 * reduction to CNF
 * ---------------------------------------------------------------------- *)

(** negation normal form (also remove equivalence and implications) *) 
let rec nnf t =
  if t.sort <> bool_sort then t else
  match t.term with
  | Var _ | Node (_, []) | BoundVar _ -> t
  | Bind (f, t') -> T.mk_bind f (nnf t')
  | Node (s, [{term=Node (s', [a; b])}]) when s = not_symbol && s' = and_symbol ->
    nnf (T.mk_or (T.mk_not a) (T.mk_not b))  (* de morgan *)
  | Node (s, [{term=Node (s', [a; b])}]) when s = not_symbol && s' = or_symbol ->
    nnf (T.mk_and (T.mk_not a) (T.mk_not b)) (* de morgan *)
  | Node (s, [a; b]) when s = imply_symbol ->
    nnf (T.mk_or (T.mk_not a) b) (* (a => b) -> (not a or b) *)
  | Node (s, [a; b]) when s = eq_symbol && a.sort = bool_sort ->
    (* (a <=> b) -> (not a or b) and (not b or a) *)
    nnf (T.mk_and
      (T.mk_or (T.mk_not a) b)
      (T.mk_or (T.mk_not b) a))
  | Node (s, [{term=Node (s', [a; b])}]) when s = not_symbol && s' = imply_symbol ->
    nnf (T.mk_and a (T.mk_not b)) (* not (a => b) -> (a and not b) *)
  | Node (s, [{term=Node (s', [a; b])}])
    when s = not_symbol && s' = eq_symbol && a.sort = bool_sort ->
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
      | None -> univ_sort
      | Some s -> s in
    let v = T.mk_var (!var_index) sort in
    incr var_index;
    let new_t' = T.db_unlift (T.db_replace t' v) in
    skolemize ~ord ~var_index new_t' (* remove forall *)
  | Bind (s, t') when s = exists_symbol ->
    (* make a skolem symbol *)
    let sort = match T.look_db_sort 0 t with
      | None -> univ_sort
      | Some s -> s in
    let new_t' = !T.skolem ~ord t' sort in
    skolemize ~ord ~var_index new_t' (* remove forall *)
  | Bind (s, t') -> T.mk_bind s (skolemize ~ord ~var_index t')
  | Node (s, l) -> T.mk_node s t.sort (List.map (skolemize ~ord ~var_index) l)

(** reduction to cnf using De Morgan laws. Returns a list of list of terms *)
let rec to_cnf t =
  if t.sort <> bool_sort then [[t, true]]
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
let cnf_of ~ord hc =
  let var_index = ref 0 in
  (* unique counter for variable indexes *)
  Utils.debug 3 (lazy (Utils.sprintf "input clause %a@." !C.pp_clause#pp_h hc));
  if C.is_cnf hc
    then begin
      Utils.debug 3 (lazy (Utils.sprintf "clause @[<h>%a@] is cnf" !C.pp_clause#pp_h hc));
      [hc] (* already cnf, perfect *)
    end else
      let lits = Array.to_list hc.hclits in
      let nnf_lits = List.map (fun lit -> nnf (C.term_of_lit lit)) lits in
      let skolem_lits = List.map (fun t -> skolemize ~ord ~var_index t) nnf_lits in
      let clauses_of_lits = List.map to_cnf skolem_lits in
      (* list of list of literals, by or-product *)
      let lit_list_list = match clauses_of_lits with
        | [] -> assert false  (* is in cnf ;) *)
        | hd::tl -> List.fold_left product hd tl in
      (* build clauses from lits *)
      let proof = Proof ("to_cnf", [C.base_clause hc, [], S.id_subst]) in
      let clauses = List.map
        (fun lits ->
          let lits = List.map (fun (t, sign) -> C.mk_lit ~ord t T.true_term sign) lits in
          let new_hc = C.mk_hclause ~ord lits proof [hc] in
          Utils.debug 4 (lazy (Utils.sprintf "mk_clause %a@." !C.pp_clause#pp_h new_hc));
          C.clause_of_fof ~ord new_hc)
        lit_list_list
      in
      Utils.debug 3 (lazy (Utils.sprintf "%% clause @[<h>%a@] to_cnf -> @[<h>%a@]"
                    !C.pp_clause#pp_h hc (Utils.pp_list !C.pp_clause#pp_h) clauses));
      List.iter (fun hc -> assert (C.is_cnf hc)) clauses;
      clauses
