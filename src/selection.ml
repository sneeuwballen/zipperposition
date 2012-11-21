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

(** Selection functions *)

open Types

module T = Terms
module C = Clauses
module Utils = FoUtils

let select_max_goal lits =
  try
    Array.iter (fun lit ->
      if lit.lit_maximal && C.neg_eqn lit.lit_eqn then (lit.lit_selected <- true; raise Exit))
      lits
  with Exit -> ()

let select_nothing _ = ()

let select_diff_neg_lit ~ord lits =
  (* find a negative literal with maximal difference between
     the weights of the sides of the equation *)
  let idx = ref (-1) and max_diff = ref 0 in
  Array.iteri
    (fun i lit -> match lit.lit_eqn with
     | Equation (l, r, false) ->
       let weightdiff = abs (ord#compute_term_weight l - ord#compute_term_weight r) in
       if weightdiff > !max_diff (* this literal is better *)
         then (max_diff := weightdiff; idx := i)
     | _ -> ())
    lits;
  if !idx > -1  (* found such a negative literal *)
    then lits.(!idx).lit_selected <- true

let select_complex ~ord lits =
  try
    (* select a x!=t if there is one *)
    Array.iter (fun lit ->
      match lit.lit_eqn with
      | Equation (l, r, false) when T.is_var l || T.is_var r ->
        lit.lit_selected <- true; raise Exit  (* select x!=t *)
      | _ -> ())
      lits;
    (* otherwise, select a ground l!=r if there is one *)
    Array.iter (fun lit -> 
      match lit.lit_eqn with
      | Equation (l, r, false) when T.is_ground_term l && T.is_ground_term r ->
        lit.lit_selected <- true; raise Exit  (* select x!=t *)
      | _ -> ())
      lits;
    (* otherwise do as select_diff_neg_lit *)
    select_diff_neg_lit ~ord lits
  with Exit -> ()

let select_complex_except_RR_horn ~ord lits =
  (* find whether there is exactly one positive literal, and its index *)
  let cnt = ref 0
  and idx = ref (-1) in
  Array.iteri
    (fun i lit -> if C.pos_eqn lit.lit_eqn then (idx := i; incr cnt))
    lits;
  (* check whether c is range-restricted Horn clause, ie if all variables
     occur in the only positive literal of the clause *)
  let range_restricted lit lits =
    let vars = C.vars_of_eqn lit.lit_eqn in
    try
      Array.iter (fun lit' ->
        let vars' = C.vars_of_eqn lit'.lit_eqn in
        (* check whether all variables of lit' occur in lit *)
        if not (Vector.for_all vars' (Vector.member ~cmp:T.eq_term vars))
          then raise Exit)
        lits;
      true
    with Exit -> false
  in
  (* select nothing if it's a conditional rewrite rule *)
  if !cnt = 1 && range_restricted lits.(!idx) lits
    then ()
    else select_complex ~ord lits

let default_selection ~ord = select_complex ~ord

(** table of name -> functions *)
let functions =
  let table = Hashtbl.create 17 in
  Hashtbl.add table "NoSelection" (fun ~ord lits -> select_nothing lits);
  Hashtbl.add table "MaxGoal" (fun ~ord lits -> select_max_goal lits);
  Hashtbl.add table "SelectDiffNegLit" select_diff_neg_lit;
  Hashtbl.add table "SelectComplex" select_complex;
  Hashtbl.add table "SelectComplexExceptRRHorn" select_complex_except_RR_horn;
  table

(** selection function from string (may fail) *)
let selection_from_string ~ord s =
  try
    let select = Hashtbl.find functions s in
    select ~ord
  with Not_found ->
    failwith ("no such selection function: "^s)

(** available names for selection functions *)
let available_selections () =
  let l = ref [] in
  Hashtbl.iter (fun name select -> l := name :: !l) functions;
  !l

let check_selected c =
  if c.cselected = 0 then ()
  else
    try (* check that at least one selected lit is negative *)
      Array.iter (fun lit -> if lit.lit_selected && C.neg_eqn lit.lit_eqn then raise Exit) c.clits;
      failwith "literals are selected but none of them is negative"
    with Exit -> ()
