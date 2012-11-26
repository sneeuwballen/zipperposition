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

(** DAG of the 'descendant' relation between clauses *)

open Types

module C = Clauses
module Utils = FoUtils

(** a DAG of clauses, in which links represent the 'descendant of'
    and 'descends from' relations *)
type clause_dag = {
  dag_down : hclause list Ptmap.t;    (** c -> descendants of c *)
  dag_up : hclause list Ptmap.t;      (** c -> parents of c *)
}

(** empty dag *)
let empty = {
  dag_down = Ptmap.empty;
  dag_up = Ptmap.empty;
}

(** search the clause in the map; if not found returns default *)
let find_default ~default map hc =
  try Ptmap.find hc.ctag map
  with Not_found -> default

(** set-like insertion of a clause in a clause list *)
let insert_hclause l hc =
  if List.exists (fun hc' -> hc'.ctag = hc.ctag) l
    then l
    else hc :: l

(** [parent_of dag parent child] means that child descends from parent *)
let parent_of ~ord dag parent child =
  let parent = C.hashcons_clause_noselect (C.normalize_clause ~ord parent)
  and child = C.hashcons_clause_noselect (C.normalize_clause ~ord child) in
  if C.eq_hclause parent child
    then dag
    else begin
      Utils.debug 4 (lazy (Utils.sprintf "  @[<h>%a parent of %a@]"
                           !C.pp_clause#pp_h parent !C.pp_clause#pp_h child));
      (* descendants of parent, parents of child *)
      let descendants = find_default ~default:[] dag.dag_down parent
      and parents = find_default ~default:[] dag.dag_up child in
      (* update lists *)
      let descendants = insert_hclause descendants child
      and parents = insert_hclause parents parent in
      {dag_down=Ptmap.add parent.ctag descendants dag.dag_down;
       dag_up=Ptmap.add child.ctag parents dag.dag_up; }
    end

(** update the DAG using the list of parents of the clause *)
let update ~ord dag c =
  let parents = C.parents c in
  List.fold_left (fun dag parent -> parent_of ~ord dag parent c) dag parents

let updates ~ord dag l =
  List.fold_left (update ~ord) dag l

(** get the list of descendants of clause *)
let descendants ~ord dag parent =
  let parent = C.hashcons_clause_noselect (C.normalize_clause ~ord parent) in
  let descendants = find_default ~default:[] dag.dag_down parent in
  Utils.list_uniq C.eq_hclause descendants
