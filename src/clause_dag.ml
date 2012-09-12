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

(** a DAG of clauses, in which links represent the 'descendant of'
    and 'descends from' relations *)
type clause_dag = {
  dag_down : hclause list Ptmap.t;    (** c -> descendants of c *)
  dag_up : hclause list Ptmap.t;      (** c -> parents of c *)
  dag_simplify_down : hclause Ptmap.t;(** c -> simplification of c *)
  dag_simplify_up : hclause Ptmap.t;  (** c -> d such that c is simplification of c *)
}

(** empty dag *)
let empty = {
  dag_down: Ptmap.empty;
  dag_up: Ptmap.empty;
  dag_simplify_down: Ptmap.empty;
  dag_simplify_up: Ptmap.empty;
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

(** [descends_from dag parent child] means that child descends from parent *)
let descends_from dag parent child =
  (* descendants of parent, parents of child *)
  let descendants = find_default ~default:[] dag.dag_down parent
  and parents = find_default ~default:[] dag.dag_up child in
  (* update lists *)
  let descendants = insert_hclause descendants child
  and parents = insert_hclause parents parent in
  {dag with
    dag_down=Ptmap.add parent.ctag descendants dag.dag_down;
    dag_up=Ptmap.add child.ctag parents dag.dag_up; }

(** [simplification_of dag parent child] means that child is a
    simplification of parent, and thus occupies the same place in the DAG *)
let simplification_of dag parent child =
  {dag with
    dag_simplify_down = Ptmap.add parent.ctag child dag.dag_simplify_down;
    dag_simplify_up = Ptmap.add child.ctag parent dag.dag_simplify_up; }

(** find the oldest clause that simplifies to c *)
let rec origin dag c =
  try origin (Ptmap.find c.ctag dag.dag_simplify_up)
  with Not_found -> c (* c is not the simplification of any clause *)

(** get the list of descendants of clause *)
let descendants dag parent =
  (* start from the original clause *)
  let origin = origin dag parent in
  (* descends the path of simplifications, gathering descendants all along *)
  let rec gather_down acc current =
    let current_descendants = find_default ~default:[] dag.dag_down current in
    let acc = Utils.list_merge C.eq_hclause acc current_descendants in
    try 
      (* current simplifies to next, gather also descendants of next *)
      let next = Ptmap.find current.ctag dag.dag_simplify_down in
      gather_down acc next
    with Not_found -> acc (* bottom of 'simplify_to' path for clause *)
  in
  gather_down [] parent

(** [simplified_to c d] checks whether b is obtained by simplifications on a *)
let rec simplified_to dag c d =
  if C.eq_hclause c d then true
    else
      try simplified_to dag (Ptmap.find c.ctag dag.dag_simplify_down)
      with Not_found -> false  (* does not simpligy *)
