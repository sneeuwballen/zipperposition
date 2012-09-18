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

(** DOT format generator. I don't want to depend on gtk. *)

module Utils = FoUtils

module type Graph =
  sig
    type vertex
    type edge

    val equal: vertex -> vertex -> bool
    val hash: vertex -> int

    val print_vertex: vertex -> string
    val print_edge: edge -> string
  end

module Make(G: Graph) =
  struct

    (** hashtable of vertices *)
    module VertexTable = Hashtbl.Make(
      struct
        type t = G.vertex
        let equal = G.equal
        let hash = G.hash
      end)

    type attribute =
      | Color of string
      | Shape of string
      | Weight of int
      | Style of string
      | Label of string

    (** a node of the DOT graph *)
    type dot_node = {
      node_label: G.vertex;
      node_count: int;
      mutable node_edges: G.edge list;
      mutable node_attributes: attribute list;
    }

    (** an edge of the DOT graph *)
    type dot_edge = {
      edge_label: G.edge;
      edge_from: dot_node;
      edge_to: dot_node;
      mutable edge_attributes: attribute list;
    }

    (** A (directed) graph, parametrized by the types of vertices and edges *)
    type graph = {
      name: string;
      mutable count: int;
      nodes: node VertexTable.t;
    }

    let mk_graph ~name =
      { name = name;
        count = 0;
        nodes = VertexTable.create 23;
        attributes = []; }

    (** get the node associated with the given vertex (v) *)
    let get_node graph v =
      try VertexTable.find graph.nodes v
      with Not_found ->
        let node = {
          node_label = v;
          node_count = graph.count;
          node_edges = [];
          node_attributes = [];
        } in
        graph.count <- graph.count + 1;  (* keep count unique *)
        VertexTable.add graph.nodes v node;
        node

    let add_node_attribute node attr =
      node.node_attributes <- attr :: node.node_attributes

    (** create a new edge *)
    let add_edge graph from dest label =
      let edge = {
        edge_label = label;
        edge_from = from;
        edge_to = dest;
      } in
      from.node_edges <- edge :: from.node_edges;
      edge

    let add_edge_attribute edge attr =
      edge.edge_attributes <- attr :: edge.edge_attributes

    (** print graph in DOT format *)
    let print_graph graph =
      let buf = Buffer.create 200 in
      Buffer.add_string (Utils.sprintf "digraph %s {\n" graph.name);
      (* the name of a node *)
      let rec node_name node = Utils.sprintf "node_%d" node.node_count
      (* print a node *)
      and print_node _ node =
        (* add a 'label' attribute *)
        let label = G.print_vertex node.node_label in
        let attributes = (Label label) :: node.node_attributes in
        let str = Utils.sprintf "  %s [%a];\n" (node_name node)
          (Utils.pp_list ~sep:"," print_attribute) attributes in
        Buffer.add_string str;
        (* print all outgoing edges *)
        List.iter print_edge node.node_edges
      (* print an edge *)
      and print_edge edge =
        (* add a 'label' attribute *)
        let label = G.print_edge edge.edge_label in
        let attributes = (Label label) :: edge.edge_attributes in
        let str = Utils.sprintf "  %s -> %s [%a];\n"
          (node_name edge.edge_from) (node_name edge.edge_to)
          (Utils.pp_list ~sep:"," print_attribute) attributes in
      (* print an attribute *)
      and print_attribute = function
      | Color c -> "color=" ^ c
      | Shape s -> "shape=" ^ s
      | Weight w -> Utils.sprintf "weight=%f" w
      | Style s -> "style=" ^ s
      | Label l -> Utils.sprintf "label=\"%s\"" l
      in
      VertexTable.iter print_node graph.nodes;
      Buffer.add_string "}\n"
  end
