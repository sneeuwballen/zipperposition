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

(* a set of (hashconsed clause, position in clause). A position is a
 * list, that, once reversed, is [lit index, 1|2 (left or right), ...]
 * where ... is the path in the term *)
module ClauseSet : Set.S with 
  type elt = hclause * position * foterm
  = Set.Make(
      struct 
      type t = hclause * position * foterm

      let compare (c1, p1, t1) (c2, p2, t2) = 
        let c = Pervasives.compare p1 p2 in
        if c <> 0 then c else
        let c = C.compare_hclause c1 c2 in
        if c <> 0 then c else
        (assert (T.eq_foterm t1 t2); 0)
    end)

type input = foterm                         (** indexed type *)
type data = ClauseSet.elt                   (** value associated with input *)
type dataset = ClauseSet.t                  (** set of values *)
type constant_name = symbol                 (** constant terms (leaves) *)

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
  let rec aux arity t acc = match t.node.term with
    | Leaf a -> (Constant (a, arity)) :: acc
    | Var i -> (* assert (arity = 0); *) Variable :: acc
    | Node ([] | [ _ ] )
    (* FIXME : should this be allowed or not ? *)
    | Node ({node={term=Var _}}::_)
    | Node ({node={term=Node _}}::_) ->
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

type tree = dataset DiscriminationTree.t

let empty = DiscriminationTree.empty

let iter dt f = DiscriminationTree.iter (fun p x -> f p x) dt

let fold dt f = DiscriminationTree.fold (fun p x -> f p x) dt

let index tree term info =
  let ps = path_string_of term in
  let ps_set =
    try DiscriminationTree.find ps tree with Not_found -> ClauseSet.empty
  in
  DiscriminationTree.add ps (ClauseSet.add info ps_set) tree

let remove_index tree term info =
  let ps = path_string_of term in
  try
    let ps_set = ClauseSet.remove info (DiscriminationTree.find ps tree) in
    if ClauseSet.is_empty ps_set then DiscriminationTree.remove ps tree
    else DiscriminationTree.add ps ps_set tree
  with Not_found -> tree

let in_index tree term test =
  let ps = path_string_of term in
  try
    let ps_set = DiscriminationTree.find ps tree in
    ClauseSet.exists test ps_set
  with Not_found -> false

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
    | DiscriminationTree.Node (Some s, _), [] -> ClauseSet.fold (fun elt acc -> f acc elt) s acc
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
          | DiscriminationTree.Node (Some s, _), [] ->
              ClauseSet.fold (fun elt acc -> f acc elt) s acc
          | n, path -> retrieve path acc n
        with Not_found -> acc
 in
 retrieve path acc tree

let num_keys tree =
  let num = ref 0 in
  iter tree (fun _ _ -> incr num);
  !num

let num_elems tree =
  let num = ref 0 in
  iter tree (fun _ elems -> num := !num + (ClauseSet.cardinal elems));
  !num

let index : Index.index =
  object (_ : 'self)
    val tree : tree = empty   (* the discrimination tree *)

    method add t data = ({< tree = index tree t data >} : 'self)

    method remove t data = ({< tree = remove_index tree t data >} : 'self)

    method iter f = iter tree (fun p data -> ClauseSet.iter f data)

    method fold : 'a. ('a -> ClauseSet.elt -> 'a) -> 'a -> 'a =
      fun f acc ->
        let acc = ref acc in
        iter tree (fun p data -> acc :=
          ClauseSet.fold (fun elt acc -> f acc elt) data !acc);
        !acc

    method retrieve_generalizations: 'a. foterm -> 'a -> ('a -> data -> 'a) -> 'a =
      fun t acc f ->
        prof_dt_generalization.HExtlib.profile
        (retrieve ~unify_query:false ~unify_indexed:true tree t acc) f

    method retrieve_unifiables : 'a. foterm -> 'a -> ('a -> data -> 'a) -> 'a =
      fun t acc f ->
        prof_dt_unifiables.HExtlib.profile
        (retrieve ~unify_query:true ~unify_indexed:true tree t acc) f

    method retrieve_specializations : 'a. foterm -> 'a -> ('a -> data -> 'a) -> 'a =
      fun t acc f ->
        prof_dt_specializations.HExtlib.profile
        (retrieve ~unify_query:true ~unify_indexed:false tree t acc) f

    method pp ~all_clauses formatter () =
      let print_dt_path path set =
        if all_clauses
          then let l = ClauseSet.elements set in
          Format.fprintf formatter "%s : @[<hov>%a@]@;" (string_of_path path)
            (Utils.pp_list ~sep:", " !C.pp_clause#pp_h_pos) l
          else Format.fprintf formatter "@[<h>%s : %d clauses/pos@]@;"
            (string_of_path path) (ClauseSet.cardinal set)
      in
      iter tree print_dt_path
  end

(** process the literal (only its maximal side(s)) *)
let process_lit op c tree (lit, pos) =
  match lit with
  | Equation (l,_,_,Gt) -> 
      op tree l (c, [C.left_pos; pos])
  | Equation (_,r,_,Lt) -> 
      op tree r (c, [C.right_pos; pos])
  | Equation (l,r,_,Incomparable)
  | Equation (l,r,_,Invertible) ->
      let tmp_tree = op tree l (c, [C.left_pos; pos]) in
      op tmp_tree r (c, [C.right_pos; pos])
  | Equation (l,r,_,Eq) ->
    Utils.debug 4 (lazy (Utils.sprintf "add %a = %a to index"
                   !T.pp_term#pp l !T.pp_term#pp r));
    op tree l (c, [C.left_pos; pos])  (* only index one side *)

(** apply op to the maximal literals of the clause, and only to
    the maximal side(s) of those. *)
let process_clause op tree c =
  (* index literals with their position *)
  let lits_pos = Utils.list_pos c.node.clits in
  let new_tree = List.fold_left (process_lit op c) tree lits_pos in
  new_tree

(** apply (op tree) to all subterms, folding the resulting tree *)
let rec fold_subterms op tree t (c, path) = match t.node.term with
  | Var _ -> tree  (* variables are not indexed *)
  | Leaf _ -> op tree t (c, List.rev path, t) (* reverse path now *)
  | Node (_::l) ->
      (* apply the operation on the term itself *)
      let tmp_tree = op tree t (c, List.rev path, t) in
      let _, new_tree = List.fold_left
        (* apply the operation on each i-th subterm with i::path
           as position. i starts at 1 and the function symbol is ignored. *)
        (fun (idx, tree) t -> idx+1, fold_subterms op tree t (c, idx::path))
        (1, tmp_tree) l
      in new_tree
  | _ -> assert false

(** apply (op tree) to the root term, after reversing the path *)
let apply_root_term op tree t (c, path) = op tree t (c, List.rev path, t)

let clause_index =
  object (_: 'self)
    val _root_index = index
    val _subterm_index = index
    val _unit_root_index = index 

    method name = "discrimination_tree_index"

    (** add root terms and subterms to respective indexes *)
    method index_clause hc =
      let op tree = tree#add in
      let new_subterm_index = process_clause (fold_subterms op) _subterm_index hc
      and new_unit_root_index = match hc.node.clits with
          | [(Equation (_,_,true,_)) as lit] ->
              process_lit (apply_root_term op) hc _unit_root_index (lit, 0)
          | _ -> _unit_root_index
      and new_root_index = process_clause (apply_root_term op) _root_index hc
      in ({< _root_index=new_root_index;
            _unit_root_index=new_unit_root_index;
            _subterm_index=new_subterm_index >} :> 'self)

    (** remove root terms and subterms from respective indexes *)
    method remove_clause hc =
      let op tree = tree#remove in
      let new_subterm_index = process_clause (fold_subterms op) _subterm_index hc
      and new_unit_root_index = match hc.node.clits with
          | [(Equation (_,_,true,_)) as lit] ->
              process_lit (apply_root_term op) hc _unit_root_index (lit, 0)
          | _ -> _unit_root_index
      and new_root_index = process_clause (apply_root_term op) _root_index hc
      in ({< _root_index=new_root_index;
            _unit_root_index=new_unit_root_index;
            _subterm_index=new_subterm_index >} :> 'self)

    method root_index = _root_index
    method unit_root_index = _unit_root_index
    method subterm_index = _subterm_index

    method pp ~all_clauses formatter () =
      Format.fprintf formatter
        "clause_index:@.root_index=@[<v>%a@]@.unit_root_index=@[<v>%a@]@.subterm_index=@[<v>%a@]@."
        (_root_index#pp ~all_clauses) ()
        (_unit_root_index#pp ~all_clauses) ()
        (_subterm_index#pp ~all_clauses) ()
  end
