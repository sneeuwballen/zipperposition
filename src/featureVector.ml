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

open Basic
open Symbols

module T = Terms
module C = Clauses
module Lits = Literals
module Utils = FoUtils

(* ----------------------------------------------------------------------
 * features
 * ---------------------------------------------------------------------- *)

(** a vector of feature *)
type feature_vector = int list

(** a function that computes a feature *)
type feature = literal array -> int

let compute_fv features hc =
  List.map (fun feat -> feat hc) features

let feat_size_plus lits =
  let cnt = ref 0 in
  Array.iter (fun (Equation (_,_,sign,_)) -> if sign then incr cnt) lits;
  !cnt

let feat_size_minus lits =
  let cnt = ref 0 in
  Array.iter (fun (Equation (_,_,sign,_)) -> if not sign then incr cnt) lits;
  !cnt

(* sum of depths at which symbols occur. Eg f(a, g(b)) will yield 4 (f
   is at depth 0) *)
let sum_of_depths_lit lit =
  let rec sum depth acc t = match t.term with
  | Var _ | BoundVar _ -> acc
  | Bind (_, _, t') -> sum (depth+1) (acc+depth) t'
  | Node (s, l) -> List.fold_left (sum (depth+1)) (acc+depth) l
  in
  match lit with
  | Equation (l, r, _, _) -> sum 0 (sum 0 0 l) r

let sum_of_depths lits =
  Array.fold_left (fun acc lit -> acc + sum_of_depths_lit lit) 0 lits

(* number of occurrences of symbol in literal *)
let count_symb_lit symb lit =
  let cnt = ref 0 in
  let rec count_symb_term t = match t.term with
  | Var _ | BoundVar _ -> ()
  | Bind (s, _, t') -> 
    (if s = symb then incr cnt);
    count_symb_term t'
  | Node (s, l) ->
    (if s = symb then incr cnt);
    List.iter count_symb_term l
  in
  match lit with
  | Equation (l, r, _, _) ->
    count_symb_term l; count_symb_term r; !cnt

(** Count the number of distinct split symbols *)
let count_split_symb lits =
  let table = SHashtbl.create 3 in
  let rec gather t = match t.term with
  | Node (s, l) ->
    (if has_attr attr_split s then SHashtbl.replace table s ());
    List.iter gather l
  | BoundVar _ | Var _ -> ()
  | Bind (s, _, t') ->
    (if has_attr attr_split s then SHashtbl.replace table s ());
    gather t'
  in
  Array.iter (function | Equation (l, r,_,_) -> gather l; gather r) lits;
  SHashtbl.length table

(** Count the number of distinct skolem symbols *)
let count_skolem_symb lits =
  let table = SHashtbl.create 3 in
  let rec gather t = match t.term with
  | Node (s, l) ->
    (if has_attr attr_skolem s then SHashtbl.replace table s ());
    List.iter gather l
  | BoundVar _ | Var _ -> ()
  | Bind (s, _, t') ->
    (if has_attr attr_skolem s then SHashtbl.replace table s ());
    gather t'
  in
  Array.iter (function | Equation (l, r,_,_) -> gather l; gather r) lits;
  SHashtbl.length table

let count_symb_plus symb lits =
  let cnt = ref 0 in
  Array.iter
    (fun lit -> if Lits.is_pos lit
      then cnt := !cnt + count_symb_lit symb lit) lits;
  !cnt

let count_symb_minus symb lits =
  let cnt = ref 0 in
  Array.iter
    (fun lit -> if Lits.is_neg lit
      then cnt := !cnt + count_symb_lit symb lit) lits;
  !cnt

(* max depth of the symbol in the literal, or -1 *)
let max_depth_lit symb lit =
  let rec max_depth_term t depth =
    match t.term with
    | Var _ | BoundVar _ -> -1
    | Bind (s, _, t') ->
      let cur_depth = if s = symb then depth else -1 in
      max cur_depth (max_depth_term t' (depth+1))
    | Node (s, l) ->
      let cur_depth = if s = symb then depth else -1 in
      List.fold_left
        (fun maxdepth subterm -> max maxdepth (max_depth_term subterm (depth+1)))
        cur_depth l
  in
  match lit with
  | Equation (l, r, _, _) -> max (max_depth_term l 0) (max_depth_term r 0)

let max_depth_plus symb lits =
  let depth = ref 0 in
  Array.iter
    (fun lit -> if Lits.is_pos lit
      then depth := max !depth (max_depth_lit symb lit)) lits;
  !depth

let max_depth_minus symb lits =
  let depth = ref 0 in
  Array.iter
    (fun lit -> if Lits.is_neg lit
      then depth := max !depth (max_depth_lit symb lit)) lits;
  !depth

(* ----------------------------------------------------------------------
 * FV index
 * ---------------------------------------------------------------------- *)

type trie =
  | TrieNode of trie Ptmap.t  (** map feature -> trie *)
  | TrieLeaf of C.CSet.t      (** leaf with a set of hcs *)

let empty_trie n = match n with
  | TrieNode m when Ptmap.is_empty m -> true
  | TrieLeaf set when C.CSet.is_empty set -> true
  | _ -> false

(** get/add/remove the leaf for the given list of ints. The
    continuation k takes the leaf, and returns a leaf
    that replaces the old leaf. 
    This function returns the new trie. *)
let goto_leaf trie t k =
  (* the root of the tree *)
  let root = trie in
  (* function to go to the given leaf, building it if needed *)
  let rec goto trie t rebuild =
    match trie, t with
    | (TrieLeaf set) as leaf, [] -> (* found leaf *)
      (match k set with
      | new_leaf when leaf == new_leaf -> root  (* no change, return same tree *)
      | new_leaf -> rebuild new_leaf)           (* replace by new leaf *)
    | TrieNode m, c::t' ->
      (try  (* insert in subtrie *)
        let subtrie = Ptmap.find c m in
        let rebuild' subtrie = match subtrie with
          | _ when empty_trie subtrie -> rebuild (TrieNode (Ptmap.remove c m))
          | _ -> rebuild (TrieNode (Ptmap.add c subtrie m))
        in
        goto subtrie t' rebuild'
      with Not_found -> (* no subtrie found *)
        let subtrie = if t' = [] then TrieLeaf C.CSet.empty else TrieNode Ptmap.empty
        and rebuild' subtrie = match subtrie with
          | _ when empty_trie subtrie -> rebuild (TrieNode (Ptmap.remove c m))
          | _ -> rebuild (TrieNode (Ptmap.add c subtrie m))
        in
        goto subtrie t' rebuild')
    | TrieNode _, [] -> assert false (* ill-formed term *)
    | TrieLeaf _, _ -> assert false  (* wrong arity *)
  in
  goto trie t (fun t -> t)

(** a trie of ints *)
module FVTrie = Trie.Make(Ptmap)

(** a feature vector index, based on a trie that contains sets of hcs *)
type fv_index = feature list * trie

let mk_fv_index features = (features, TrieNode Ptmap.empty)

(** maximam number of features in addition to basic ones *)
let max_features = 25

let pp_feat_triple formatter (_,_,name) = Format.pp_print_string formatter name

let mk_fv_index_signature signature =
  (* list of (salience: float, feature, repr: string) *)
  let features = ref [] in
  let pp name s = Utils.sprintf "%s(%s)" name (name_symbol s) in
  (* create features for the symbols *)
  SMap.iter
    (fun s sort ->
      let arity = arity sort in
      if sort == bool_
        then features := [1 + arity, count_symb_plus s, pp "count+" s;
                          1 + arity, count_symb_minus s, pp "count-" s]
                          @ !features
      else
        features := [0, max_depth_plus s, pp "max_depth+" s;
                     0, max_depth_minus s, pp "max_depth-" s;
                     1 + arity, count_symb_plus s, pp "count+" s;
                     1 + arity, count_symb_minus s, pp "count-" s]
                    @ !features)
    signature;
  (* only take a limited number of features *)
  let features = List.sort (fun (s1,_,_) (s2, _,_) -> s2 - s1) !features in
  let features = Utils.list_take max_features features in
  Utils.debug 2 "%% FV index use features @[<h>%a@]"
                 (Utils.pp_list pp_feat_triple) features;
  let features = List.map (fun (_, f,_) -> f) features in
  let features = [feat_size_plus; feat_size_minus; count_skolem_symb;
                  count_split_symb; sum_of_depths] @ features in
  (* build an index with those features *)
  mk_fv_index features

let index_clause (features, trie) hc =
  (* feature vector of the hc *)
  let fv = compute_fv features hc.hclits in
  (* add the hc to the trie *)
  let k set = TrieLeaf (C.CSet.add set hc) in
  let new_trie = goto_leaf trie fv k in
  (features, new_trie)

let index_clauses fv hcs = List.fold_left index_clause fv hcs

let remove_clause (features, trie) hc =
  (* feature vector of the hc *)
  let fv = compute_fv features hc.hclits in
  (* add the hc to the trie *)
  let k set = TrieLeaf (C.CSet.remove set hc) in
  let new_trie = goto_leaf trie fv k in
  (features, new_trie)

let remove_clauses fv hcs = List.fold_left remove_clause fv hcs

(** hcs that subsume (potentially) the given literals *)
let retrieve_subsuming (features, trie) lits f =
  (* feature vector of the hc *)
  let fv = compute_fv features lits in
  let rec iter_lower fv node = match fv, node with
  | [], TrieLeaf set -> C.CSet.iter set f
  | i::fv', TrieNode map ->
    Ptmap.iter
      (fun j subnode -> if j <= i
        then iter_lower fv' subnode)  (* go in the branch *)
      map
  | _ -> failwith "number of features in feature vector changed"
  in
  iter_lower fv trie

(** hcs that are subsumed (potentially) by the given literals *)
let retrieve_subsumed (features, trie) lits f =
  (* feature vector of the hc *)
  let fv = compute_fv features lits in
  let rec iter_higher fv node = match fv, node with
  | [], TrieLeaf set -> C.CSet.iter set f
  | i::fv', TrieNode map ->
    Ptmap.iter
      (fun j subnode -> if j >= i
        then iter_higher fv' subnode)  (* go in the branch *)
      map
  | _ -> failwith "number of features in feature vector changed"
  in
  iter_higher fv trie
