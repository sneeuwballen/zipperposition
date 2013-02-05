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

module Make(V : Map.OrderedType) : S with type vertex = V.t

(** Signature of a module designed to print graphs into DOT *)
module type Dot = sig
  module G : S
    (** A graph module *)

  type attribute = [
  | `Color of string
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
module DotMake(G : S) : Dot with module G = G
