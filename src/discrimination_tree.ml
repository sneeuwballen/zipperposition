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

open Types
open Hashcons

module T = Terms
module C = Clauses
module I = Index
module Unif = FoUnif
module Utils = FoUtils

type path_string_elem = 
  | Constant of symbol * int (* name, arity *)
  | Bound of int * int (* rel, arity *)
  | Variable (* arity is 0 *)
  | Proposition (* arity is 0 *) 
  | Datatype (* arity is 0 *) 
  | Dead (* arity is 0 *) 

type path = path_string_elem list

let arity_of = function
  | Constant (_,a)
  | Bound (_,a) -> a
  | _ -> 0

let prof_dt_generalization = HExtlib.profile ~enable:true "discr_tree.retrieve_generalizations"
let prof_dt_unifiables = HExtlib.profile ~enable:true "discr_tree.retrieve_unifiables"
let prof_dt_specializations = HExtlib.profile ~enable:true "discr_tree.retrieve_specializations"

module OrderedPathStringElement = struct
  type t = path_string_elem
  (** compare two path string elements *)
  let compare e1 e2 = 
    match e1,e2 with 
    | Constant (a1,ar1), Constant (a2,ar2) ->
        let c = compare a1 a2 in
        if c <> 0 then c else Pervasives.compare ar1 ar2
    | Variable, Variable -> 0
    | Constant _, Variable -> ~-1
    | Variable, Constant _ -> 1
    | Proposition, _ | _, Proposition
    | Datatype, _ | _, Datatype
    | Dead, _ | _, Dead
    | Bound _, _ | _, Bound _ -> assert false
end

(* convert into a path string *)
let path_string_of t =
  let rec aux arity t acc = match t.term with
    | Leaf a -> (Constant (a, arity)) :: acc
    | Var i -> (* assert (arity = 0); *) Variable :: acc
    | Node ([] | [ _ ] )
    (* FIXME : should this be allowed or not ? *)
    | Node ({term=Var _}::_)
    | Node ({term=Node _}::_) ->
      failwith (Utils.sprintf "linearizing %a failed." !T.pp_term#pp t)
    | Node (hd::tl) ->
        let acc = aux (List.length tl) hd acc in
        List.fold_left (fun acc t -> aux 0 t acc) acc tl
  in 
  (* we build the path in reverse order because it should be faster,
     only two lists have to be built, and rev is tailrec *)
  List.rev (aux 0 t [])

(* print path into string *)
let string_of_path l =
  let str_of_elem = function
  | Variable -> "*"
  | Constant (a, ar) -> Utils.on_buffer !T.pp_symbol#pp a
  | _ -> "?"
  in String.concat "." (List.map str_of_elem l)

(* map of string elements *)
module PSMap = Map.Make(OrderedPathStringElement)

type key = PSMap.key

(** build a trie of such string elements *)
module DiscriminationTree = Trie.Make(PSMap)

type tree = I.index_leaf DiscriminationTree.t

let empty = DiscriminationTree.empty

let iter dt f = DiscriminationTree.iter (fun p x -> f p x) dt

let fold dt f = DiscriminationTree.fold (fun p x -> f p x) dt

let index tree term data =
  let ps = path_string_of term in
  let leaf = 
    try DiscriminationTree.find ps tree
    with Not_found -> I.empty_leaf
  in
  DiscriminationTree.add ps (I.add_leaf leaf term data) tree

let remove_index tree term data =
  let ps = path_string_of term in
  try
    let leaf = DiscriminationTree.find ps tree in
    let leaf = I.remove_leaf leaf term data in
    if I.is_empty_leaf leaf
      then DiscriminationTree.remove ps tree
      else DiscriminationTree.add ps leaf tree
  with Not_found -> tree

(* You have h(f(x,g(y,z)),t) whose path_string_of_term_with_jl is
   (h,2).(f,2).(x,0).(g,2).(y,0).(z,0).(t,0) and you are at f and want to
   skip all its progeny, thus you want to reach t.

   You need to skip as many elements as the sum of all arities contained
    in the progeny of f.

   The input ariety is the one of f while the path is x.g....t
   Should be the equivalent of after_t in the literature (handbook A.R.)
 *)
let rec skip arity path =
  if arity = 0 then path else match path with
  | [] -> assert false
  | m::tl -> skip (arity-1+arity_of m) tl

(* the equivalent of skip, but on the index, thus the list of trees
   that are rooted just after the term represented by the tree root
   are returned (we are skipping the root) *)
let skip_root = function DiscriminationTree.Node (value, map) ->
  let rec get n = function DiscriminationTree.Node (v, m) as tree ->
     if n = 0 then [tree] else
     PSMap.fold (fun k v res -> (get (n-1 + arity_of k) v) @ res) m []
  in
    PSMap.fold (fun k v res -> (get (arity_of k) v) @ res) map []

let retrieve ~unify_query ~unify_indexed tree term acc f =
  let path = path_string_of term in
  let rec retrieve path acc tree =
    match tree, path with
    | DiscriminationTree.Node (Some leaf, _), [] ->
      I.fold_leaf leaf f acc
    | DiscriminationTree.Node (None, _), [] -> acc
    | DiscriminationTree.Node (_, map), Variable::path when unify_query ->
        List.fold_left (retrieve path) acc (skip_root tree)
    | DiscriminationTree.Node (_, map), node::path ->
        let acc = 
          if not unify_query && node = Variable then acc else
          (* follow the branch of the trie that corresponds to the query symbol *)
          try retrieve path acc (PSMap.find node map)
          with Not_found -> acc
        in
        if not unify_indexed then acc else
        (* follow a 'variable' branch of the trie *)
        try match PSMap.find Variable map,skip (arity_of node) path with
          | DiscriminationTree.Node (Some leaf, _), [] ->
            I.fold_leaf leaf f acc 
          | n, path -> retrieve path acc n
        with Not_found -> acc
 in
 retrieve path acc tree

let num_keys tree =
  let num = ref 0 in
  iter tree (fun _ _ -> incr num);
  !num

let index : Index.index =
  object (_ : 'self)
    val tree : tree = empty   (* the discrimination tree *)

    method name = "discrimination_tree_index"

    method add t data = ({< tree = index tree t data >} : 'self)

    method remove t data = ({< tree = remove_index tree t data >} : 'self)

    method iter f = iter tree (fun p leaf -> I.iter_leaf leaf (fun t set -> f t set))

    method fold : 'a. ('a -> term -> I.ClauseSet.t -> 'a) -> 'a -> 'a =
      fun f acc ->
        let acc = ref acc in
        iter tree (fun p leaf -> I.iter_leaf leaf (fun t set -> acc := f !acc t set));
        !acc

    method retrieve_generalizations: 'a. term -> 'a ->
      ('a -> term -> I.ClauseSet.t -> 'a) -> 'a =
      fun t acc f ->
        prof_dt_generalization.HExtlib.profile
        (retrieve ~unify_query:false ~unify_indexed:true tree t acc) f

    method retrieve_unifiables : 'a. term -> 'a ->
      ('a -> term -> I.ClauseSet.t -> 'a) -> 'a =
      fun t acc f ->
        prof_dt_unifiables.HExtlib.profile
        (retrieve ~unify_query:true ~unify_indexed:true tree t acc) f

    method retrieve_specializations : 'a. term -> 'a ->
      ('a -> term -> I.ClauseSet.t -> 'a) -> 'a =
      fun t acc f ->
        prof_dt_specializations.HExtlib.profile
        (retrieve ~unify_query:true ~unify_indexed:false tree t acc) f

    method pp ~all_clauses formatter () =
      let rec print_elt (hc, pos, t) = Format.fprintf formatter "%a@;"
        !C.pp_clause#pp_h_pos (hc, pos, t)
      and print t set =
        begin
          Format.fprintf formatter "%a -> @[<v>" !T.pp_term#pp t;
          I.ClauseSet.iter print_elt set;
          Format.fprintf formatter "@]@;"
        end
      and print_dt_path path leaf =
        if all_clauses
          then begin
            Format.fprintf formatter "%s : @[<hov>" (string_of_path path);
            I.iter_leaf leaf print;
            Format.fprintf formatter "@]@;"
          end else
            Format.fprintf formatter "@[<h>%s : %d terms@]@;"
              (string_of_path path) (I.size_leaf leaf)
      in
      iter tree print_dt_path
  end

