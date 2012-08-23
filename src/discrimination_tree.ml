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


type 'a path_string_elem =
  | Constant of 'a * int (* name, arity *)
  | Bound of int * int (* rel, arity *)
  | Variable (* arity is 0 *)
  | Proposition (* arity is 0 *)
  | Datatype (* arity is 0 *)
  | Dead (* arity is 0 *)

type 'a path = ('a path_string_elem) list

module type Indexable = sig
  type input
  type constant_name
  val compare:
    constant_name path_string_elem ->
    constant_name path_string_elem -> int
  val string_of_path : constant_name path -> string
  val path_string_of : input -> constant_name path
end

let arity_of = function
  | Constant (_,a)
  | Bound (_,a) -> a
  | _ -> 0

module type DiscriminationTree =
  sig
    type input    (* indexed type *)
    type data     (* value associated with input *)
    type dataset  (* set of such values *)
    type constant_name  (* constant terms (leaves) *)
    type t        (* the tree itself *)

    val iter : t -> (constant_name path -> dataset -> unit) -> unit
    val fold : t -> (constant_name path -> dataset -> 'b -> 'b) -> 'b -> 'b

    val empty : t
    val index : t -> input -> data -> t
    val remove_index : t -> input -> data -> t
    val in_index : t -> input -> (data -> bool) -> bool
    val retrieve_generalizations : t -> input -> dataset
    val retrieve_unifiables : t -> input -> dataset
    val retrieve_specializations: t -> input -> dataset
    val num_keys : t -> int                       (** number of indexed keys (paths) *)
    val num_elems : t -> int                      (** number of elements for any key *)

    (* the int is the number of symbols that matched, note that
     * Collector.to_list returns a sorted list, biggest matches first. *)
    module type Collector = sig
      type t
      val empty : t
      val union : t -> t -> t
      val inter : t -> t -> data list
      val to_list : t -> data list
    end
    module Collector : Collector
    val retrieve_generalizations_sorted : t -> input -> Collector.t
    val retrieve_unifiables_sorted : t -> input -> Collector.t
  end

let prof_dt_generalization = HExtlib.profile ~enable:true "discr_tree.retrieve_generalizations"
let prof_dt_unifiables = HExtlib.profile ~enable:true "discr_tree.retrieve_unifiables"
let prof_dt_specializations = HExtlib.profile ~enable:true "discr_tree.retrieve_specializations"

module Make (I:Indexable) (A:Set.S) : DiscriminationTree
  with type constant_name = I.constant_name and type input = I.input
   and type data = A.elt and type dataset = A.t =

  struct
    module OrderedPathStringElement = struct
      type t = I.constant_name path_string_elem
      let compare = I.compare
    end

    type constant_name = I.constant_name
    type data = A.elt
    type dataset = A.t
    type input = I.input

    (* map of string elements *)
    module PSMap = Map.Make(OrderedPathStringElement)

    type key = PSMap.key

    (* build a trie of such string elements *)
    module DiscriminationTree = Trie.Make(PSMap)

    type t = A.t DiscriminationTree.t

    let empty = DiscriminationTree.empty

    let iter dt f = DiscriminationTree.iter (fun p x -> f p x) dt

    let fold dt f = DiscriminationTree.fold (fun p x -> f p x) dt

    let index tree term info =
      let ps = I.path_string_of term in
      let ps_set =
        try DiscriminationTree.find ps tree with Not_found -> A.empty
      in
      DiscriminationTree.add ps (A.add info ps_set) tree

    let remove_index tree term info =
      let ps = I.path_string_of term in
      try
        let ps_set = A.remove info (DiscriminationTree.find ps tree) in
        if A.is_empty ps_set then DiscriminationTree.remove ps tree
        else DiscriminationTree.add ps ps_set tree
      with Not_found -> tree

    let in_index tree term test =
      let ps = I.path_string_of term in
      try
        let ps_set = DiscriminationTree.find ps tree in
        A.exists test ps_set
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

    let retrieve ~unify_query ~unify_indexed tree term =
      let path = I.path_string_of term in
      let rec retrieve path tree =
        match tree, path with
        | DiscriminationTree.Node (Some s, _), [] -> s
        | DiscriminationTree.Node (None, _), [] -> A.empty
        | DiscriminationTree.Node (_, map), Variable::path when unify_query ->
            List.fold_left A.union A.empty
              (List.map (retrieve path) (skip_root tree))
        | DiscriminationTree.Node (_, map), node::path ->
            A.union
               (if not unify_query && node = Variable then A.empty else
               (* follow the branch of the trie that corresponds to the query symbol *)
                try retrieve path (PSMap.find node map)
                with Not_found -> A.empty)
               (if not unify_indexed then A.empty else
               (* follow a 'variable' branch of the trie *)
               try match PSMap.find Variable map,skip (arity_of node) path with
                  | DiscriminationTree.Node (Some s, _), [] -> s
                  | n, path -> retrieve path n
                with Not_found -> A.empty)
     in
     retrieve path tree

    let retrieve_generalizations tree term =
      prof_dt_generalization.HExtlib.profile
      (retrieve ~unify_query:false ~unify_indexed:true tree) term

    let retrieve_unifiables tree term =
      prof_dt_unifiables.HExtlib.profile
      (retrieve ~unify_query:true ~unify_indexed:true tree) term

    let retrieve_specializations tree term =
      prof_dt_specializations.HExtlib.profile
      (retrieve ~unify_query:true ~unify_indexed:false tree) term

    let num_keys tree =
      let num = ref 0 in
      iter tree (fun _ _ -> incr num);
      !num

    let num_elems tree =
      let num = ref 0 in
      iter tree (fun _ elems -> num := !num + (A.cardinal elems));
      !num

    module O = struct
      type t = A.t * int
      let compare (sa,wa) (sb,wb) =
        let c = compare wb wa in
        if c <> 0 then c else A.compare sb sa
    end
    module S = Set.Make(O)

    (* TASSI: here we should think of a smarted data structure *)
    module type Collector = sig
      type t
      val empty : t
      val union : t -> t -> t
      val inter : t -> t -> data list
      val to_list : t -> data list
    end
    module Collector : Collector with type t = S.t = struct
      type t = S.t
      let union = S.union
      let empty = S.empty

      let merge l =
        let rec aux s w = function
          | [] -> [s,w]
          | (t, wt)::tl when w = wt -> aux (A.union s t) w tl
          | (t, wt)::tl -> (s, w) :: aux t wt tl
        in
        match l with
        | [] -> []
        | (s, w) :: l -> aux s w l

      let rec undup ~eq = function
        | [] -> []
        | x :: tl -> x :: undup ~eq (List.filter (fun y -> not(eq x y)) tl)

      let to_list t =
        undup ~eq:(fun x y -> A.equal (A.singleton x) (A.singleton y))
          (List.flatten (List.map
            (fun (x,_) -> A.elements x) (merge (S.elements t))))

      (* TODO why not use Set.inter? *)
      let inter t1 t2 =
        let l1 = merge (S.elements t1) in
        let l2 = merge (S.elements t2) in
        let res =
         List.flatten
          (List.map
            (fun (s, w) ->
               HExtlib.filter_map (fun x ->
                 try Some (x, w + snd (List.find (fun (s,w) -> A.mem x s) l2))
                 with Not_found -> None)
                 (A.elements s))
            l1)
        in
        undup ~eq:(fun x y -> A.equal (A.singleton x) (A.singleton y))
          (List.map fst (List.sort (fun (_,x) (_,y) -> y - x) res))
    end

    let retrieve_sorted unif tree term =
      let path = I.path_string_of term in
      let rec retrieve n path tree =
        match tree, path with
        | DiscriminationTree.Node (Some s, _), [] -> S.singleton (s, n)
        | DiscriminationTree.Node (None, _), [] -> S.empty
        | DiscriminationTree.Node (_, map), Variable::path when unif ->
            List.fold_left S.union S.empty
              (List.map (retrieve n path) (skip_root tree))
        | DiscriminationTree.Node (_, map), node::path ->
            S.union
               (if not unif && node = Variable then S.empty else
                try retrieve (n+1) path (PSMap.find node map)
                with Not_found -> S.empty)
               (try
                  match PSMap.find Variable map,skip (arity_of node) path with
                  | DiscriminationTree.Node (Some s, _), [] ->
                     S.singleton (s, n)
                  | no, path -> retrieve n path no
                with Not_found -> S.empty)
     in
      retrieve 0 path tree

    let retrieve_generalizations_sorted tree term =
      retrieve_sorted false tree term
    let retrieve_unifiables_sorted tree term =
      retrieve_sorted true tree term
  end

