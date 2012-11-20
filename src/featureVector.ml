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

(** Feature Vector indexing (see Schulz 2004) for efficient forward
    and backward subsumption *)

open Types

module T = Terms
module C = Clauses
module Utils = FoUtils

(* ----------------------------------------------------------------------
 * features
 * ---------------------------------------------------------------------- *)

(** a vector of feature *)
type feature_vector = int list

(** a function that computes a feature *)
type feature = clause -> int

let compute_fv features clause =
  List.map (fun feat -> feat clause) features

let feat_size_plus clause =
  let rec aux lits count = match lits with
  | [] -> count
  | (Equation (_,_,true,_))::lits' -> aux lits' (count+1)
  | _::lits' -> aux lits' count
  in aux clause.clits 0

let feat_size_minus clause =
  let rec aux lits count = match lits with
  | [] -> count
  | (Equation (_,_,false,_))::lits' -> aux lits' (count+1)
  | _::lits' -> aux lits' count
  in aux clause.clits 0

(* number of occurrences of symbol in literal *)
let count_symb_lit symb lit =
  let rec count_symb_term t = match t.term with
  | Var _ -> 0
  | Node (s, l) ->
    let cnt = if s = symb then 1 else 0 in
    List.fold_left
      (fun sum subterm -> sum + (count_symb_term subterm))
      cnt l
  in match lit with
  | Equation (l, r, _, _) -> count_symb_term l + count_symb_term r

let count_symb_plus symb clause =
  List.fold_left
    (fun sum lit -> if C.pos_lit lit
      then (count_symb_lit symb lit) + sum else sum)
    0 clause.clits

let count_symb_minus symb clause =
  List.fold_left
    (fun sum lit -> if C.neg_lit lit
      then (count_symb_lit symb lit) + sum else sum)
    0 clause.clits

(* max depth of the symbol in the literal, or -1 *)
let max_depth_lit symb lit =
  let rec max_depth_term t depth = match t.term with
  | Var _ -> -1
  | Node (s, l) ->
    let depth = if s = symb then depth else -1 in
    List.fold_left
      (fun maxdepth subterm -> max maxdepth (max_depth_term subterm (depth+1)))
      depth l
  in match lit with
  | Equation (l, r, _, _) -> max (max_depth_term l 0) (max_depth_term r 0)

let max_depth_plus symb clause =
  List.fold_left
    (fun maxdepth lit -> if C.pos_lit lit
      then max maxdepth (max_depth_lit symb lit) else maxdepth)
    0 clause.clits

let max_depth_minus symb clause =
  List.fold_left
    (fun maxdepth lit -> if C.neg_lit lit
      then max maxdepth (max_depth_lit symb lit) else maxdepth)
    0 clause.clits

(* ----------------------------------------------------------------------
 * FV index
 * ---------------------------------------------------------------------- *)

module S = Set.Make(
  struct
    type t = hclause
    let compare t1 t2 = t1.ctag - t2.ctag
  end)

(** a set of hclause *)
type hclauses = S.t

(** a trie of ints *)
module FVTrie = Trie.Make(Ptmap)

(** a feature vector index, based on a trie that contains sets of hclauses *)
type fv_index = feature list * hclauses FVTrie.t

let mk_fv_index features = (features, FVTrie.empty)

let max_symbols = 30    (** maximum number of symbols considered for indexing *)

let mk_fv_index_signature signature =
  (* only consider a bounded number of symbols *)
  let bounded_signature = Utils.list_take max_symbols signature in
  let features = [feat_size_plus; feat_size_minus] @
    List.flatten
      (List.map (fun symb ->
        (* for each symbol, use 4 features *)
        [count_symb_plus symb; count_symb_minus symb])
        bounded_signature)
  in
  (* build an index with those features *)
  mk_fv_index features

let index_clause (features, trie) hc =
  (* feature vector of the clause *)
  let fv = compute_fv features hc in
  (* set for this feature vector *)
  let set = try FVTrie.find fv trie with Not_found -> S.empty in
  (* add the set+clause to the trie *)
  let new_trie = FVTrie.add fv (S.add hc set) trie in
  (features, new_trie)

let remove_clause (features, trie) hc =
  (* feature vector of the clause *)
  let fv = compute_fv features hc in
  (* set for this feature vector *)
  try
    let set = FVTrie.find fv trie in
    let set = S.remove hc set in
    if S.is_empty set
      then
        (* remove the (now empty) set from the trie *)
        let new_trie = FVTrie.remove fv trie in
        (features, new_trie)
      else
        (* put the new set (without hc) in the trie *)
        let new_trie = FVTrie.add fv set trie in
        (features, new_trie)
  with Not_found -> (features, trie)  (* the clause cannot be present *)


(** clauses that subsume (potentially) the given clause *)
let retrieve_subsuming (features, trie) clause f =
  (* feature vector of the clause *)
  let fv = compute_fv features clause in
  let rec iter_lower fv node = match fv, node with
  | [], FVTrie.Node (None, _) -> ()
  | [], FVTrie.Node (Some hclauses, _) ->
      S.iter f hclauses
  | i::fv', FVTrie.Node (_, map) ->
    Ptmap.iter
      (fun j subnode -> if j <= i
        then iter_lower fv' subnode  (* go in the branch *)
        else () (* do not go in the branch *)
      )
      map
  in
  iter_lower fv trie

(** clauses that are subsumed (potentially) by the given clause *)
let retrieve_subsumed (features, trie) clause f =
  (* feature vector of the clause *)
  let fv = compute_fv features clause in
  let rec iter_higher fv node = match fv, node with
  | [], FVTrie.Node (None, _) -> ()
  | [], FVTrie.Node (Some hclauses, _) -> S.iter f hclauses
  | i::fv', FVTrie.Node (_, map) ->
    Ptmap.iter
      (fun j subnode -> if j >= i
        then iter_higher fv' subnode)  (* go in the branch *)
      map
  in
  iter_higher fv trie

