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

(** {1 A simple persistent directed graph.} *)

module type S = sig
  type vertex

  module M : Map.S with type key = vertex

  type 'e t
    (** Graph parametrized by a type for edges *)

  val empty : 'e t
    (** Create an empty graph. *)

  val add : 'e t -> vertex -> 'e -> vertex -> 'e t
    (** Add an edge between two vertices *)

  val add_seq : 'e t -> (vertex * 'e * vertex) Sequence.t -> 'e t

  val next : 'e t -> vertex -> ('e * vertex) Sequence.t
  val prev : 'e t -> vertex -> ('e * vertex) Sequence.t

  val between : 'e t -> vertex -> vertex -> 'e Sequence.t

  val iter_vertices : 'e t -> (vertex -> unit) -> unit
  val vertices : 'e t -> vertex Sequence.t
      (** Iterate on vertices *)

  val iter : 'e t -> (vertex * 'e * vertex -> unit) -> unit 
  val to_seq : 'e t -> (vertex * 'e * vertex) Sequence.t
    (** Dump the graph as a sequence of vertices *)
end

module Make(V : Map.OrderedType) = struct
  module M = Map.Make(V)

  type vertex = V.t

  type 'e t = 'e node M.t
    (** Graph parametrized by a type for edges *)
  and 'e node = {
    n_vertex : vertex;
    n_next : ('e * vertex) list;
    n_prev : ('e * vertex) list;
  } (** A node of the graph *)

  let empty = M.empty

  let empty_node v = {
    n_vertex = v;
    n_next = [];
    n_prev = [];
  }

  let add t v1 e v2 =
    let n1 = try M.find v1 t with Not_found -> empty_node v1
    and n2 = try M.find v2 t with Not_found -> empty_node v2 in
    let n1 = { n1 with n_next = (e,v2) :: n1.n_next; }
    and n2 = { n2 with n_prev = (e,v1) :: n2.n_prev; } in
    M.add v1 n1 (M.add v2 n2 t)

  let add_seq t seq = Sequence.fold (fun t (v1,e,v2) -> add t v1 e v2) t seq

  let next t v = Sequence.List.to_seq (M.find v t).n_next

  let prev t v = Sequence.List.to_seq (M.find v t).n_prev

  let between t v1 v2 =
    let edges = Sequence.List.to_seq (M.find v1 t).n_prev in
    let edges = Sequence.filter (fun (e, v2') -> V.compare v2 v2' = 0) edges in
    Sequence.map fst edges

  (** Call [k] on every vertex *)
  let iter_vertices t k = M.iter (fun v _ -> k v) t

  let vertices t = Sequence.from_iter (iter_vertices t)

  (** Call [k] on every edge *)
  let iter t k =
    M.iter
      (fun v1 node -> List.iter (fun (e, v2) -> k (v1, e, v2)) node.n_next)
      t

  let to_seq t = Sequence.from_iter (iter t)
end

(** Signature of a module designed to print graphs into DOT *)
module type Dot = sig
  module G : S
    (** A graph module *)

  type attribute = [ `Color of string
  | `Shape of string
  | `Weight of int
  | `Style of string
  | `Label of string
  | `Other of string * string
  ] (** Dot attribute *)

  type 'e t
    (** Dot printer for graphs of type ['e G.t] *)

  val make : name:string ->
            print_edge:(G.vertex -> 'e -> G.vertex -> attribute list) ->
             print_vertex:(G.vertex -> attribute list) -> 'e t
    (** Create a Dot graph printer. Functions to convert edges and vertices
        to Dot attributes must be provided. *)

  val add : 'e t -> 'e G.t -> unit
    (** Add the content of the graph to the Dot printer *)

  val pp : Format.formatter -> 'e t -> unit
    (** Print the content of the graph printer on the formatter. *)
end

(** Create a Dot printing module from a Graph structure *)
module DotMake(G : S) = struct
  module G = G

  type attribute = [ `Color of string
  | `Shape of string
  | `Weight of int
  | `Style of string
  | `Label of string
  | `Other of string * string
  ] (** Dot attribute *)

  type 'e t = {
    name : string;
    print_edge : G.vertex -> 'e -> G.vertex -> attribute list;
    print_vertex : G.vertex -> attribute list;
    mutable count_map : int G.M.t;
    mutable count : int;
    mutable edges : (int * int * attribute list) list;
    mutable vertices : (int * attribute list) list;
  } (** Dot printer for graphs of type ['e G.t] *)

  (** Create a Dot graph printer. Functions to convert edges and vertices
      to Dot attributes must be provided. *)
  let make ~name ~print_edge ~print_vertex = {
    name;
    print_vertex;
    print_edge;
    count_map = G.M.empty;
    count = 0;
    edges = [];
    vertices = [];
  }

  (** Add the content of the graph to the Dot printer *)
  let add dot graph =
    (* map from vertices to integers *)
    let get_id vertex =
      try G.M.find vertex dot.count_map
      with Not_found ->
        let n = dot.count in
        dot.count <- dot.count + 1;
        dot.count_map <- G.M.add vertex n dot.count_map;
        n
    in
    (* add vertices *)
    Sequence.iter
      (fun v ->
        let attributes = dot.print_vertex v in
        dot.vertices <- (get_id v, attributes) :: dot.vertices)
      (G.vertices graph);
    (* add edges *)
    Sequence.iter
      (fun (v1, e, v2) ->
        let attributes = dot.print_edge v1 e v2 in
        dot.edges <- (get_id v1, get_id v2, attributes) :: dot.edges)
      (G.to_seq graph);
    ()


  (** Print the given graph on the formatter, using the graph printer.
      All vertices and nodes of the graph are iterated on. *)
  let pp formatter dot = 
    (* print an attribute *)
    let print_attribute formatter attr =
      match attr with
      | `Color c -> Format.fprintf formatter "color=%s" c
      | `Shape s -> Format.fprintf formatter "shape=%s" s
      | `Weight w -> Format.fprintf formatter "weight=%d" w
      | `Style s -> Format.fprintf formatter "style=%s" s
      | `Label l -> Format.fprintf formatter "label=\"%s\"" l
      | `Other (name, value) -> Format.fprintf formatter "%s=\"%s\"" name value
    in
    (* the name of a vertex *)
    let pp_vertex formatter i = Format.fprintf formatter "vertex_%d" i in
    (* print preamble *)
    Format.fprintf formatter "@[<v2>digraph %s {@;" dot.name;
    (* print vertices *)
    List.iter
      (fun (i, attributes) ->
        Format.fprintf formatter "  @[<h>%a [%a];@]@." pp_vertex i
          (FoUtils.pp_list ~sep:"," print_attribute) attributes)
      dot.vertices;
    (* print edges *)
    List.iter
      (fun (i1, i2, attributes) ->
        Format.fprintf formatter "  @[<h>%a -> %a [%a];@]@."
          pp_vertex i1 pp_vertex i2
          (FoUtils.pp_list ~sep:"," print_attribute) attributes)
      dot.edges;
    (* close *)
    Format.fprintf formatter "}@]@;"
end
