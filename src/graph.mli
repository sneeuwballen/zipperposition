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

  val mk_dot_printer : 
     print_edge:(vertex -> 'e -> vertex -> attribute list) ->
     print_vertex:(vertex -> attribute list) ->
     'e dot_printer
    (** Create a Dot graph printer. Functions to convert edges and vertices
        to Dot attributes must be provided. *)

  val pp : 'e dot_printer -> name:string ->
            Format.formatter ->
            (vertex Sequence.t * (vertex * 'e * vertex) Sequence.t) -> unit
    (** Pretty print the graph in DOT, on given formatter. Using sequences
        allows to easily select which edges and vertices are important,
        or to combine several graphs with [Sequence.append].
        All vertices used in edges must appear in the vertices sequence. *)
end

module Make(V : Map.OrderedType) : S with type vertex = V.t
