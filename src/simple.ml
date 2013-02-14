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

(** Simple representation of terms and formula *)

open Symbols

type term =
  | Var of int * sort
  | Node of symbol * sort * term list

let compare_terms = Pervasives.compare

let eq_terms = (=)

let term_sort = function | Var (_,s) -> s | Node (_,s,_) -> s

let cast t sort = match t with
  | Var (i, _) -> Var (i, sort)
  | Node (f, _, l) -> Node (f, sort, l)

let is_var = function | Var _ -> true | Node _ -> false

let is_node = function | Var _ -> false | Node _ -> true

let mk_var i sort = Var (i, sort)
let mk_node f sort l = Node (f, sort, l)
let mk_const f sort = Node (f, sort, [])

type formula =
  | True | False
  | Atom of term
  | Eq of term * term
  | Or of formula list
  | And of formula list
  | Not of formula
  | Equiv of formula * formula
  | Forall of term * formula
  | Exists of term * formula
and sourced_formula = formula * source
and source = Axiom of string * string | Derived of string * formula list

let compare_formulas = Pervasives.compare

let eq_formulas f1 f2 = f1 = f2

let mk_true = True
let mk_false = False
let mk_atom t = assert (term_sort t == bool_); Atom t
let mk_eq t1 t2 = assert (term_sort t1 == term_sort t2); Eq (t1, t2)
let mk_neq t1 t2 = Not (mk_eq t1 t2)
let mk_lit t1 t2 sign = if sign then mk_eq t1 t2 else mk_neq t1 t2
let mk_or fs = Or fs
let mk_and fs = And fs
let mk_not f = Not f
let mk_imply f1 f2 = mk_or [mk_not f1; f2]
let mk_equiv f1 f2 = Equiv (f1, f2)
let mk_xor f1 f2 = Not (mk_equiv f1 f2)
let mk_forall v f = Forall (v, f)
let mk_exists v f = Exists (v, f)

module TMap = Map.Make(struct type t = term let compare = compare_terms end)

(** Signature (map symbol -> sort) for this set of formulas *)
let signature formulas =
  let rec explore signature f = match f with
  | True | False -> signature
  | Atom t -> explore_term signature t
  | Eq (t1, t2) -> explore_term (explore_term signature t1) t2
  | Or fs | And fs -> List.fold_left explore signature fs
  | Equiv (f1, f2) -> explore (explore signature f1) f2
  | Not f -> explore signature f
  | Forall (v, f) | Exists (v, f) -> assert (is_var v); explore signature f
  and explore_term signature t = match t with
  | Var _ -> signature
  | Node (f, sort, l) ->
    begin
      (* build a function sort *)
      let sort = sort <== (List.map term_sort l) in
      (try (* check consistency with signature *)
        let sort' = SMap.find f signature in
        assert (sort == sort');
      with Not_found -> ());
      let signature' = SMap.add f sort signature in
      List.fold_left explore_term signature' l
    end
  in
  List.fold_left explore empty_signature formulas

(** get a list of symbols from a list of (sourced) formulas *)
let symbols formulas =
  let m = signature formulas in
  SMap.fold (fun s _ acc -> s :: acc) m []
