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
  (** {2 Basics} *)

  type vertex

  module M : Map.S with type key = vertex
  module S : Set.S with type elt = vertex

  type 'e t
    (** Graph parametrized by a type for edges *)

  val empty : 'e t
    (** Create an empty graph. *)

  val add : 'e t -> vertex -> 'e -> vertex -> 'e t
    (** Add an edge between two vertices *)

  val add_seq : 'e t -> (vertex * 'e * vertex) Sequence.t -> 'e t
    (** Add the vertices to the graph *)

  val next : 'e t -> vertex -> ('e * vertex) Sequence.t
    (** Outgoing edges *)

  val prev : 'e t -> vertex -> ('e * vertex) Sequence.t
    (** Incoming edges *)

  val between : 'e t -> vertex -> vertex -> 'e Sequence.t

  val iter_vertices : 'e t -> (vertex -> unit) -> unit
  val vertices : 'e t -> vertex Sequence.t
      (** Iterate on vertices *)

  val iter : 'e t -> (vertex * 'e * vertex -> unit) -> unit 
  val to_seq : 'e t -> (vertex * 'e * vertex) Sequence.t
    (** Dump the graph as a sequence of vertices *)

  (** {2 Global operations} *)

  val roots : 'e t -> vertex Sequence.t
    (** Roots, ie vertices with no incoming edges *)

  val leaves : 'e t -> vertex Sequence.t
    (** Leaves, ie vertices with no outgoing edges *)

  val rev : 'e t -> 'e t
    (** Reverse all edges *)

  (** {2 Print to DOT} *)

  type attribute = [
  | `Color of string
  | `Shape of string
  | `Weight of int
  | `Style of string
  | `Label of string
  | `Other of string * string
  ] (** Dot attribute *)

  type 'e dot_printer
    (** Helper to print a graph to DOT *)

  val mk_dot_printer : 
     print_edge:(vertex -> 'e -> vertex -> attribute list) ->
     print_vertex:(vertex -> attribute list) ->
     'e dot_printer
    (** Create a Dot graph printer. Functions to convert edges and vertices
        to Dot attributes must be provided. *)

  val pp : 'e dot_printer -> name:string ->
            Format.formatter ->
            (vertex Sequence.t * (vertex * 'e * vertex) Sequence.t) -> unit
    (** Pretty print the graph in DOT, on given formatter. Using a sequence
        allows to easily select which edges are important, or to combine
        several graphs with [Sequence.append] *)
end

module Make(V : Map.OrderedType) = struct
  module M = Map.Make(V)
  module S = Set.Make(V)

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

  (** {2 Global operations} *)

  (** Roots, ie vertices with no incoming edges *)
  let roots g =
    let vertices = vertices g in
    Sequence.filter (fun v -> Sequence.is_empty (prev g v)) vertices

  (** Leaves, ie vertices with no outgoing edges *)
  let leaves g =
    let vertices = vertices g in
    Sequence.filter (fun v -> Sequence.is_empty (next g v)) vertices

  (** Reverse all edges *)
  let rev g =
    let edges = to_seq g in
    let edges = Sequence.map (fun (v1, e, v2) -> (v2, e, v1)) edges in
    add_seq empty edges

  (** Is the graph acyclic? *)
  let is_dag g = false (* TODO *)

  (** {2 Path operations} *)

  type 'e path = (vertex * 'e * vertex) list

  (** Minimal path from first vertex to second, given the cost function *)
  let min_path graph ~cost v1 v2 = failwith "not implemented"

  (** [paths g v1 v2] iterates on all paths from [v1] to [v2] *)
  let paths graph v1 v2 = []  (* TODO *)

  (** {2 Print to DOT} *)

  type attribute = [
  | `Color of string
  | `Shape of string
  | `Weight of int
  | `Style of string
  | `Label of string
  | `Other of string * string
  ] (** Dot attribute *)

  type 'e dot_printer = {
    print_edge : vertex -> 'e -> vertex -> attribute list;
    print_vertex : vertex -> attribute list;
  } (** Dot printer for graphs of type ['e G.t] *)

  (** Create a Dot graph printer. Functions to convert edges and vertices
      to Dot attributes must be provided. *)
  let mk_dot_printer ~print_edge ~print_vertex = {
    print_vertex;
    print_edge;
  }

  (** Pretty print the graph in DOT, on given formatter. Using sequences
      allows to easily select which edges and vertices are important,
      or to combine several graphs with [Sequence.append].
      All vertices used in edges must appear in the vertices sequence. *)
  let pp printer ~name formatter (vertices,edges) =
    (* map from vertices to integers *)
    let get_id =
      let count_map = ref M.empty
      and count = ref 0 in
      fun vertex ->
        try M.find vertex !count_map
        with Not_found ->
          let n = !count in
          incr count;
          count_map := M.add vertex n !count_map;
          n
    (* print an attribute *)
    and print_attribute formatter attr =
      match attr with
      | `Color c -> Format.fprintf formatter "color=%s" c
      | `Shape s -> Format.fprintf formatter "shape=%s" s
      | `Weight w -> Format.fprintf formatter "weight=%d" w
      | `Style s -> Format.fprintf formatter "style=%s" s
      | `Label l -> Format.fprintf formatter "label=\"%s\"" l
      | `Other (name, value) -> Format.fprintf formatter "%s=\"%s\"" name value
    in
    (* the name of a vertex *)
    let pp_vertex formatter v = Format.fprintf formatter "vertex_%d" (get_id v) in
    (* print preamble *)
    Format.fprintf formatter "@[<v2>digraph %s {@;" name;
    (* print vertices *)
    Sequence.iter
      (fun v ->
        let attributes = printer.print_vertex v in
        Format.fprintf formatter "  @[<h>%a [%a];@]@." pp_vertex v
          (FoUtils.pp_list ~sep:"," print_attribute) attributes)
      vertices;
    (* print edges *)
    Sequence.iter
      (fun (v1, e, v2) ->
        let attributes = printer.print_edge v1 e v2 in
        Format.fprintf formatter "  @[<h>%a -> %a [%a];@]@."
          pp_vertex v1 pp_vertex v2
          (FoUtils.pp_list ~sep:"," print_attribute) attributes)
      edges;
    (* close *)
    Format.fprintf formatter "}@]@;";
    ()
end
