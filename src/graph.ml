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

  type 'e t
    (** Graph parametrized by a type for edges *)

  val empty : 'e t
    (** Create an empty graph. *)

  val add : 'e t -> vertex -> 'e -> vertex -> 'e t
    (** Add an edge between two vertices *)

  val add_seq : 'e t -> (vertex * 'e * vertex) Sequence.t -> 'e t

  val next : 'e t -> vertex -> ('e * vertex) Sequence.t
  val prev : 'e t -> vertex -> ('e * vertex) Sequence.t

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
