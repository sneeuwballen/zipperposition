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

  val is_dag : 'e t -> bool
    (** Is the graph acyclic? *)

  (** {2 Path operations} *)

  type 'e path = (vertex * 'e * vertex) list

  val min_path : 'e t -> cost:('e -> int) -> vertex -> vertex -> 'e path option
    (** Minimal path from first vertex to second, given the cost function *)

  val paths : 'e t -> vertex -> vertex -> 'e path Sequence.t
    (** [paths g v1 v2] iterates on all paths from [v1] to [v2] *)

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

  val pp : 'e dot_printer -> ?vertices:S.t -> name:string ->
            Format.formatter ->
            (vertex * 'e * vertex) Sequence.t -> unit
    (** Pretty print the graph in DOT, on given formatter. Using a sequence
        allows to easily select which edges are important,
        or to combine several graphs with [Sequence.append].
        An optional set of additional vertices to print can be given. *)
end

module Make(V : Map.OrderedType) : S with type vertex = V.t
