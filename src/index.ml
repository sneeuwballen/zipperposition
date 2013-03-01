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

(** Generic term indexing *)

open Types

module T = Terms
module C = Clauses
module Utils = FoUtils

type data = hclause * position * term

(** a set of (hashconsed clause, position in clause, term). *)
module ClauseSet : Set.S with type elt = data
  = Set.Make(
      struct 
      type t = data

      let compare (c1, p1, t1) (c2, p2, t2) = 
        let c = Pervasives.compare p1 p2 in
        if c <> 0 then c else
        let c = C.compare_hclause c1 c2 in
        if c <> 0 then c else
        (assert (T.eq_term t1 t2); 0)
    end)

(** a leaf of an index is generally a map of terms to data *)
type index_leaf = (term * ClauseSet.t) Ptmap.t

let empty_leaf = Ptmap.empty

let add_leaf leaf t data =
  let set =
    try snd (Ptmap.find t.tag leaf)
    with Not_found -> ClauseSet.empty in
  let set = ClauseSet.add data set in
  Ptmap.add t.tag (t, set) leaf

let remove_leaf leaf t data =
  try
    let t', set = Ptmap.find t.tag leaf in
    assert (T.eq_term t t');
    let set = ClauseSet.remove data set in
    if ClauseSet.is_empty set
      then Ptmap.remove t.tag leaf
      else Ptmap.add t.tag (t, set) leaf
  with Not_found -> leaf

let is_empty_leaf leaf = Ptmap.is_empty leaf

let iter_leaf leaf f =
  Ptmap.iter (fun _ (t, set) -> f t set) leaf

let fold_leaf leaf f acc =
  Ptmap.fold (fun _ (t, set) acc -> f acc t set) leaf acc

let size_leaf leaf =
  let cnt = ref 0 in
  Ptmap.iter (fun _ _ -> incr cnt) leaf;
  !cnt

(** A term index *)
class type index =
  object ('b)
    method name : string
    method add : term -> data -> 'b
    method remove: term -> data -> 'b

    method iter : (term -> ClauseSet.t -> unit) -> unit
    method fold : 'a. ('a -> term -> ClauseSet.t -> 'a) -> 'a -> 'a

    method retrieve_unifiables : 'a. term -> 'a -> ('a -> term -> ClauseSet.t -> 'a) -> 'a
    method retrieve_generalizations : 'a. term -> 'a -> ('a -> term -> ClauseSet.t -> 'a) -> 'a
    method retrieve_specializations : 'a. term -> 'a -> ('a -> term -> ClauseSet.t -> 'a) -> 'a

    method pp : all_clauses:bool -> Format.formatter -> unit -> unit
  end

(** A simplification index *)
class type unit_index = 
  object ('b)
    method name : string
    method maxvar : int
    method is_empty : bool
    method add_clause : hclause -> 'b
    method remove_clause : hclause -> 'b
    method add : term -> term -> bool -> hclause -> 'b
    method remove : term -> term -> bool -> hclause ->'b
    method size : int
    method retrieve : sign:bool -> int -> term bind ->
                      (term bind -> term bind -> substitution -> hclause -> unit) ->
                      unit        (** iter on (in)equations of given sign l=r
                                      where subst(l) = query term *)
    method pp : Format.formatter -> unit -> unit
  end
