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

(** {1 Small set structure} *)

type 'a t = {
  cmp : 'a -> 'a -> int;
  nodes : 'a node;
} (** Set of elements of type 'a *)
and 'a node =
  | Empty
  | Node of 'a * 'a node
  (** Sorted list of 'a *)

let empty ~cmp =
  { cmp;
    nodes = Empty;
  }

let is_empty set =
  match set.nodes with
  | Empty -> true
  | Node _ -> false

let mem set x =
  let cmp = set.cmp in
  let rec explore node = match node with
    | Empty -> false
    | Node (y, node') ->
      let c = cmp x y in
      if c = 0 then true
      else if c > 0 then explore node'
      else false
  in
  explore set.nodes

let add set x =
  let cmp = set.cmp in
  let rec insert node = match node with
    | Empty -> Node (x, Empty)  (* insert here *)
    | Node (y, node') ->
      let c = cmp x y in
      if c = 0 then node  (* already there *)
      else if c > 0
        then
          let node'' = insert node' in
          if node' == node'' then node else Node (y, node'')
      else Node (x, node) (* insert before y *)
  in
  let nodes = insert set.nodes in
  if nodes == set.nodes
    then set
    else { set with nodes; }

let rec remove set x =
  let cmp = set.cmp in
  let rec remove node = match node with
    | Empty -> Empty
    | Node (y, node') ->
      let c = cmp x y in
      if c = 0 then node'
      else if c > 0
        then
          let node'' = remove node' in
          if node' == node'' then node else Node (y, node'')
      else node (* not present *)
  in
  let nodes = remove set.nodes in
  if nodes == set.nodes
    then set
    else { set with nodes; }

let choose set =
  match set.nodes with
  | Empty -> raise Not_found
  | Node (x, _) -> x

let fold f acc set =
  let rec fold f acc node = match node with
    | Empty -> acc
    | Node (x, node') ->
      let acc' = f acc x in
      fold f acc' node'
  in fold f acc set.nodes

let iter f set =
  let rec iter f node = match node with
    | Empty -> ()
    | Node (x, node') ->
      f x;
      iter f node'
  in iter f set.nodes

let size set =
  let r = ref 0 in
  iter (fun _ -> incr r) set;
  !r

let to_seq set =
  fun k ->
    iter k set

let of_seq set seq =
  Sequence.fold add set seq


