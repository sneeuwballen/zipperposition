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

(** fingerprint term indexing *)

open Types
open Symbols

module Utils = FoUtils
module T = Terms
module C = Clauses
module I = Index

type data = I.data

(** a feature *)
type feature = A | B | N | S of symbol

(** a fingerprint function, it computes several features of a term *)
type fingerprint_fun = term -> feature list

(** compute a feature for a given position *)
let rec gfpf pos t = match pos, t.term with
  | [], Var _ -> A
  | [], BoundVar _ -> S db_symbol
  | [], Bind (s, _, _) -> S s
  | [], Node (s, _) -> S s
  | 0::pos', Bind (_, _, t') -> gfpf pos' t'  (* recurse in subterm *)
  | i::pos', Node (_, l) ->
    (try gfpf pos' (Utils.list_get l i)  (* recurse in subterm *)
    with Not_found -> N)  (* not a position in t *)
  | _::_, Bind _ | _::_, BoundVar _ -> N
  | _::_, Var _ -> B  (* under variable *)

(** compute a feature vector for some positions *)
let fp positions =
  (* list of fingerprint feature functions *)
  let fpfs = List.map (fun pos -> gfpf pos) positions in
  fun t ->
    List.map (fun fpf -> fpf t) fpfs

(* ----------------------------------------------------------------------
 * fingerprint functions
 * ---------------------------------------------------------------------- *)

let fp3d = fp [[]; [1]; [1;1]]
let fp3w = fp [[]; [1]; [2]]
let fp4d = fp [[]; [1]; [1;1;]; [1;1;1]]
let fp4m = fp [[]; [1]; [2]; [1;1]]
let fp4w = fp [[]; [1]; [2]; [3]]
let fp5m = fp [[]; [1]; [2]; [3]; [1;1]]
let fp6m = fp [[]; [1]; [2]; [3]; [1;1]; [1;2]]
let fp7  = fp [[]; [1]; [2]; [1;1]; [1;2]; [2;1] ; [2;2]]
let fp7m = fp [[]; [1]; [2]; [3]; [1;1]; [4]; [1;2]]
let fp16 = fp [[]; [1]; [2]; [3]; [4]; [1;1]; [1;2]; [1;3]; [2;1];
               [2;2]; [2;3]; [3;1]; [3;2]; [3;3]; [1;1;1]; [2;1;1]]

(* ----------------------------------------------------------------------
 * index
 * ---------------------------------------------------------------------- *)

(** check whether two features are compatible for unification. TODO union find
    for compatibility modulo theories *)
let compatible_features_unif f1 f2 =
  match f1, f2 with
  | S s1, S s2 -> s1 = s2
  | B, _ | _, B -> true
  | A, N | N, A -> false
  | A, _ | _, A -> true
  | N, S _ | S _, N -> false
  | N, N -> true

(** check whether two features are compatible for matching. TODO union find
    for compatibility modulo theories *)
let compatible_features_match f1 f2 =
  match f1, f2 with
  | S s1, S s2 -> s1 == s2
  | B, _ -> true
  | N, N -> true
  | N, _ -> false
  | _, N -> false
  | A, B -> false
  | A, _ -> true
  | S _, _ -> false

(** Map whose keys are features *)
module FeatureMap = Map.Make(
  struct
    type t = feature
    let compare f1 f2 = match f1, f2 with
      (* N < B < A < S, S are ordered by symbols *)
      | _, _ when f1 = f2 -> 0
      | N, _ -> -1
      | _, N -> 1
      | B, _ -> -1
      | _, B -> 1
      | A, _ -> -1
      | _, A -> 1
      | S s1, S s2 -> compare_symbols s1 s2
  end)

(** the fingerprint trie, of constant length *)
type feature_trie =
  | Empty
  | Node of feature_trie FeatureMap.t
  | Leaf of I.index_leaf

(** add t -> data to the trie *)
let add fp trie t data =
  (* recursive insertion *)
  let rec recurse trie features =
    match trie, features with
    | Empty, [] ->
      let leaf = I.add_leaf I.empty_leaf t data in
      Leaf leaf (* creation of new leaf *)
    | Empty, f::features' ->
      let subtrie = recurse Empty features' in
      let map = FeatureMap.add f subtrie FeatureMap.empty in
      Node map  (* index new subtrie by feature *)
    | Node map, f::features' ->
      let subtrie =
        try FeatureMap.find f map
        with Not_found -> Empty in
      (* insert in subtrie *)
      let subtrie = recurse subtrie features' in
      let map = FeatureMap.add f subtrie map in
      Node map  (* point to new subtrie *)
    | Leaf leaf, [] ->
      let leaf = I.add_leaf leaf t data in
      Leaf leaf (* addition to set *)
    | Node _, [] | Leaf _, _::_ ->
      failwith "different feature length in fingerprint trie"
  in
  let features = fp t in  (* features of term *)
  recurse trie features

(** remove t -> data from the trie *)
let remove fp trie t data =
  (* recursive deletion *)
  let rec recurse trie features =
    match trie, features with
    | Empty, [] | Empty, _::_ ->
      Empty (* keep it empty *)
    | Node map, f::features' ->
      let map =
        (* delete from subtrie, if there is a subtrie *)
        try
          let subtrie = FeatureMap.find f map in
          let subtrie = recurse subtrie features' in
          if subtrie = Empty
            then FeatureMap.remove f map
            else FeatureMap.add f subtrie map
        with Not_found -> map
      in
      (* if the map is empty, use Empty *)
      if FeatureMap.is_empty map
        then Empty
        else Node map
    | Leaf leaf, [] ->
      let leaf = I.remove_leaf leaf t data in
      if I.is_empty_leaf leaf
        then Empty
        else Leaf leaf
    | Node _, [] | Leaf _, _::_ ->
      failwith "different feature length in fingerprint trie"
  in
  let features = fp t in  (* features of term *)
  recurse trie features

let rec iter trie f = match trie with
  | Empty -> ()
  | Node map -> FeatureMap.iter (fun _ subtrie -> iter subtrie f) map
  | Leaf leaf -> I.iter_leaf leaf f

let fold trie f acc =
  let acc = ref acc in
  iter trie (fun t set -> acc := f !acc t set);
  !acc

(** number of indexed terms *)
let count trie =
  let n = ref 0 in
  iter trie (fun _ _ -> incr n);
  !n

(** fold on parts of the trie that are compatible with features *)
let traverse ~compatible trie features f acc =
  (* fold on the trie *)
  let rec recurse trie features acc =
    match trie, features with
    | Empty, _ -> acc
    | Leaf leaf, [] ->
      (* fold on the set of data *)
      I.fold_leaf leaf f acc
    | Node map, f::features' ->
      (* fold on any subtrie that is compatible with current feature *)
      FeatureMap.fold
        (fun f' subtrie acc ->
          if compatible f f'
            then recurse subtrie features' acc 
            else acc)
        map acc
    | Node _, [] | Leaf _, _::_ ->
      failwith "different feature length in fingerprint trie"
  in
  recurse trie features acc

let mk_index fp =
  object (_: 'self)
    val trie = Empty    (** the trie of features *)
    val fp = fp         (** fingerprint function used *)

    method name = "fingerprint_index"

    method add t data =
      let new_trie = add fp trie t data in
      ({< trie = new_trie >} :> 'self)

    method remove t data =
      let new_trie = remove fp trie t data in
      ({< trie = new_trie >} :> 'self)

    method iter f = iter trie f

    method fold : 'a. ('a -> term -> I.ClauseSet.t -> 'a) -> 'a -> 'a =
      fun f acc -> fold trie f acc

    method retrieve_unifiables: 'a. term -> 'a -> ('a -> term -> I.ClauseSet.t -> 'a) -> 'a  =
      fun t acc f ->
        let features = fp t in
        traverse ~compatible:compatible_features_unif trie features f acc

    method retrieve_generalizations: 'a. term -> 'a -> ('a -> term -> I.ClauseSet.t -> 'a) -> 'a =
      fun t acc f ->
        let features = fp t in
        (* compatible t1 t2 if t2 can match t1 *)
        let compatible f1 f2 = compatible_features_match f2 f1 in
        traverse ~compatible trie features f acc

    method retrieve_specializations: 'a. term -> 'a -> ('a -> term -> I.ClauseSet.t -> 'a) -> 'a = 
      fun t acc f ->
        let features = fp t in
        traverse ~compatible:compatible_features_match trie features f acc

    method pp ~all_clauses formatter () =
      if not all_clauses
        then Format.fprintf formatter "fingerprint for %d clauses" (count trie)
        else
          let print_elt (hc, pos, t) = Format.fprintf formatter "%a@;" !C.pp_clause#pp_h_pos (hc, pos, t) in
          let print t set =
            begin
              Format.fprintf formatter "%a -> @[<v>" !T.pp_term#pp t;
              I.ClauseSet.iter print_elt set;
              Format.fprintf formatter "@]@;"
            end
          in begin
            Format.fprintf formatter "fingerprint for %d clauses: @[<v>"
              (count trie);
            iter trie print;
            Format.fprintf formatter "@]"
          end
  end
