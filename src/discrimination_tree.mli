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



type 'a path_string_elem = 
  | Constant of 'a * int (* name, arity *)
  | Bound of int * int (* rel, arity *)
  | Variable (* arity is 0 *)
  | Proposition (* arity is 0 *) 
  | Datatype (* arity is 0 *) 
  | Dead (* arity is 0 *) 


type 'a path = ('a path_string_elem) list

module type Indexable = sig
  type input
  type constant_name
  val compare: 
    constant_name path_string_elem -> 
    constant_name path_string_elem -> int
  val string_of_path : constant_name path -> string
  val path_string_of : input -> constant_name path
end

module type DiscriminationTree  =
  sig
    type input    (* indexed type *)
    type data     (* value associated with input *)
    type dataset  (* set of such values *)
    type constant_name  (* constant terms (leaves) *)
    type t        (* the tree itself *)

    val iter : t -> (constant_name path -> dataset -> unit) -> unit
    val fold : t -> (constant_name path -> dataset -> 'b -> 'b) -> 'b -> 'b

    val empty : t
    val index : t -> input -> data -> t
    val remove_index : t -> input -> data -> t
    val in_index : t -> input -> (data -> bool) -> bool
    val retrieve_generalizations : t -> input -> dataset
    val retrieve_unifiables : t -> input -> dataset
    val num_keys : t -> int                       (** number of indexed keys (paths) *)
    val num_elems : t -> int                      (** number of elements for any key *)

    (* the int is the number of symbols that matched, note that
     * Collector.to_list returns a sorted list, biggest matches first. *)
    module type Collector = sig
      type t
      val empty : t
      val union : t -> t -> t
      val inter : t -> t -> data list
      val to_list : t -> data list
    end
    module Collector : Collector
    val retrieve_generalizations_sorted : t -> input -> Collector.t
    val retrieve_unifiables_sorted : t -> input -> Collector.t
  end


module Make (I : Indexable) (A : Set.S) : DiscriminationTree 
  with  type constant_name = I.constant_name and type input = I.input
    and type data = A.elt and type dataset = A.t

