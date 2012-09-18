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

(** E-graph imperative data structure *)

open Types

module T = Terms
module Utils = FoUtils

(** A node that represents a term in the E-graph *)
type egraph_node = {
  node_term: foterm;                        (** term this node represents *)
  node_children: egraph_node list;          (** children of the node *)
  mutable node_representative: egraph_node; (** representative of the term *)
  mutable node_parents: egraph_node list;   (** parents terms of the node *)
  mutable node_class: egraph_node list;     (** equivalence class, only meaningful when the
                                                node is the representative of its class *)
}

(** hashtable with terms as keys *)
module THashtbl = Hashtbl.Make(
  struct
    type t = foterm
    let equal = T.eq_foterm
    let hash x = x.hkey
  end)

(** Actions to perform to backtrack *)
type action =
  | StopBacktracking  (** we reached the last backtracking point *)
  | Delete of egraph_node
  | SetParents of egraph_node * egraph_node list
  | SetClass of egraph_node * egraph_node list
  | SetRepresentative of egraph_node * egraph_node

(** The E-graph structure *)
type egraph = {
  graph_nodes: egraph_node THashtbl.t;      (** term -> node *)
  graph_stack: action Stack.t;
}

(** create an empty E-graph *)
let empty () =
  let nodes = THashtbl.create 1621
  and stack = Stack.create () in
  Stack.push StopBacktracking stack;
  { graph_nodes = nodes; graph_stack = stack }

(** push a backtracking point on the stack *)
let push egraph =
  Stack.push StopBacktracking egraph.graph_stack

(** pop to the last backtracking point, cancelling all actions performed since *)
let pop egraph =
  let do_action = function
  | StopBacktracking -> assert false
  | Delete node ->
    THashtbl.remove egraph.graph_nodes node.node_term;
    List.iter
      (fun child ->  (* remove node from the parents of all its children *)
        child.node_parents <- List.filter (fun parent -> parent != node) child.node_parents)
      node.node_children
  | SetParents (node, parents) -> node.node_parents <- parents
  | SetClass (node, class_) -> node.node_class <- class_
  | SetRepresentative (node, representative) -> node.node_representative <- representative
  in
  (* unwind the stack down to the last backtracking point *)
  let rec unwind () =
    let action =
      try Stack.pop egraph.graph_stack
      with Stack.Empty -> raise (Invalid_argument "no backtracking point left")
    in
    match action with
    | StopBacktracking -> ()  (* reached the last backtracking point *)
    | _ -> do_action action; unwind ()
  in
  unwind ()

(** find the representative of a node *)
let rec find node =
  let repr = node.node_representative in
  if node == repr
    then node  (* its own representative *)
    else begin
      assert (not (T.eq_foterm node.node_term repr.node_term));
      (* recurse to find the actual representative *)
      let root = find repr in
      (* path compression *)
      node.node_representative <- root;
      root
    end

(** union of two nodes *)
let union egraph n1 n2 =
  (* merge the first into the second. The first is no more a representative. *)
  let merge_into from into =
    assert (from.node_representative == from);
    assert (into.node_representative == into);
    Stack.push (SetRepresentative (from, from)) egraph.graph_stack;
    Stack.push (SetClass (into, into.node_class)) egraph.graph_stack;
    from.node_representative <- into;
    into.node_class <- List.rev_append from.node_class into.node_class
  in
  (* merge one parent into the other *)
  let n1 = find n1
  and n2 = find n2 in
  if n1 == n2
    then ()  (* already merged *)
    else if List.length n1.node_class > List.length n2.node_class
      then merge_into n2 n1 (* n1 bigger than n2, merge n2 into n1 *)
      else merge_into n1 n2

(** get term from node *)
let term_of_node node = node.node_term

(** creation of a node *)
let rec node_of_term egraph t =
  try THashtbl.find egraph.graph_nodes t
  with Not_found ->
    (* find subnodes *)
    let subterms = match t.term with
    | Var _ | Leaf _ -> []
    | Node (hd::tl) -> List.map (node_of_term egraph) tl
    | Node [] -> assert false
    in
    (* create a node *)
    let rec node = {
      node_term = t;
      node_children = subterms;
      node_representative = node;
      node_class = [node];
      node_parents = [];
    }
    in
    (* add node as a parent of all its children *)
    List.iter (fun child -> child.node_parents <- node :: child.node_parents) subterms;
    THashtbl.add egraph.graph_nodes t node;
    Stack.push (Delete node) egraph.graph_stack; (* delete node when backtracking *)
    node

let are_equal n1 n2 = n1 == n2 || find n1 == find n2

let equiv_class node = node.node_class

let representative node = find node

type label = NodeVar of int | NodeSymbol of string

(** label of a node, i.e. the root term *)
let label node =
  let rec seek t = match t.term with
    | Var i -> NodeVar i
    | Leaf s -> NodeSymbol s
    | Node (hd::_) -> seek hd
    | Node [] -> assert false
  in
  seek node.node_term

(** check whether two nodes are congruent. They are if they
    are currently equal (same representative), or if they have the
    same label and all their children are congruent. *)
let congruent n1 n2 =
  are_equal n1 n2 ||
    (label n1 = label n2 &&
     (try List.for_all2 are_equal n1.node_children n2.node_children
      with Invalid_argument _ -> false))

(** merge two terms (assert a = b). This follows the Nelson-Oppen
    algorithm for computing congruence closure, 1980. *)
let rec merge egraph n1 n2 =
  let n1 = find n1
  and n2 = find n2 in
  if n1 == n2
    then ()  (* trivial *)
    else begin
      let c1 = n1.node_class
      and c2 = n2.node_class in
      (* union of classes *)
      union egraph n1 n2;
      (* check parents of equivalence classes for new congruences *)
      let rec check_congruent l1 l2 = match l1 with
      | [] -> ()
      | t1::l1' ->
        List.iter
          (fun t2 -> (* those two parents are congruent, merge them *)
            if find t1 != find t2 && congruent t1 t2 then merge egraph t1 t2)
          l2;
        check_congruent l1' l2
      (* check whether some parents of those two terms, that are congruent
         respectively to n1 and n2, are also congruent *)
      and check_parents c1 c2 = match c1 with
      | [] -> ()
      | t1::c1' ->
        (* check congruence of parents of t1 with parents of any term of c2 *)
        List.iter (fun t2 -> check_parents t1.node_parents t2.node_parents) c2;
        check_parents c1' c2
      in check_parents c1 c2
    end


module Graph =
  struct
    type vertex = egraph_node
    type edge = EdgeSubterm | EdgeCongruent

    let equal n1 n2 = n1 == n2
    let hash n = n.node_term.hkey
    
    let print_vertex node =
      match label node with
      | NodeVar i -> Utils.sprintf "X%d" i
      | NodeSymbol s -> s

    let print_edge _ = ""
  end

(** module to print E-graphs to DOT *)
module D = Dot.Make(Graph)

(** Print the E-graph in DOT format *)
let to_dot ~name egraph =
  let graph = D.mk_graph ~name in
  (* map the node to the DOT graph *)
  let on_node node = 
    let n = D.get_node graph node in
    D.add_node_attribute n (D.Shape "box");
    (* add links to children *)
    List.iter
      (fun child ->
        let c = D.get_node graph child in
        let e = D.add_edge graph n c Graph.EdgeSubterm in
        D.add_edge_attribute e (D.Weight 10);
        D.add_edge_attribute e (D.Style "filled");
        D.add_edge_attribute e (D.Style "bold"))
      node.node_children;
    (* add links to congruent terms *)
    List.iter
      (fun congruent_node ->
        if congruent_node.node_term.tag <= node.node_term.tag
          then ()  (* print the link only once *)
          else begin
            let c = D.get_node graph congruent_node in
            let e = D.add_edge graph n c Graph.EdgeCongruent in
            D.add_edge_attribute e (D.Weight 1);
            D.add_edge_attribute e (D.Style "dotted");
            D.add_edge_attribute e (D.Other ("arrowhead", "none"))
          end)
      node.node_class;
  in
  THashtbl.iter (fun _ node -> on_node node) egraph.graph_nodes;
  (* print the graph into a string *)
  D.print_graph graph
