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

module Graph(V : Map.OrderedType) = struct
  module M = Map.Make(V)

  type 'e t = 'e node M.t
  and 'e node = {
    n_vertex : vertices;
    n_next : ('e * vertex) list;
    n_prev : ('e * vertex) list;
  } (** Graph parametrized by a type for edges *)

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
    and n2 = { n2 with n_prev = (e,v1) :: n2.n_prev; }
    M.add v1 n1 (M.add v2 n2 t)

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
