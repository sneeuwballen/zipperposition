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

let select_max_goal clause =
  (* find negative lits *)
  let negative_lits = List.filter
    (fun ((Equation (_,_,sign,_), _)) -> not sign)
    (C.maxlits clause) in
  match negative_lits with
  | [] -> []  (* select nothing *)
  | (_,idx)::_ -> [idx]  (* select one negative max goal *)

let select_nothing _ = []

let select_diff_neg_lit ~ord c =
  (* find a negative literal with maximal difference between
     the weights of the sides of the equation *)
  let rec find_lit best_diff best_idx idx lits = match lits with
  | [] -> best_idx
  | (Equation (l, r, false, _))::lits' ->
    let weightdiff = abs (l.tsize - r.tsize) in
    if weightdiff > best_diff
      then find_lit weightdiff idx (idx+1) lits'
      else find_lit best_diff best_idx (idx+1) lits'
  | _::lits' -> find_lit best_diff best_idx (idx+1) lits'
  in
  (* search such a lit among the clause's lits *)
  match find_lit 0 (-1) 0 c.clits with
  | -1 -> []
  | n -> [n]

let select_complex ~ord c =
  (* find x!=y in literals *)
  let rec find_noteqvars idx lits = match lits with
  | [] -> -1
  | (Equation (l, r, false,_))::lits' ->
    if T.is_var l && T.is_var r then idx else find_noteqvars (idx+1) lits'
  | _::lits' -> find_noteqvars (idx+1) lits'
  (* find the smallest ground negative literal *)
  and find_neg_ground best_weight best_idx idx lits = match lits with
  | [] -> best_idx
  | (Equation (l, r, false, _))::lits' ->
    let weight = l.tsize + r.tsize in
    if T.is_ground_term l && T.is_ground_term r && weight < best_weight
      then find_neg_ground weight idx (idx+1) lits'
      else find_neg_ground best_weight best_idx (idx+1) lits'
  | _::lits' -> find_neg_ground best_weight best_idx (idx+1) lits'
  in
  (* try x!=y, else try ground negative, else delegate *)
  let i = find_noteqvars 0 c.clits in
  if i >= 0
    then [i]
    else
      let i = find_neg_ground 0 (-1) 0 c.clits in
      if i >= 0
        then [i]
        else select_diff_neg_lit ~ord c (* delegate to select_diff_neg_lit *)

let select_complex_except_RR_horn ~ord c =
  (* find whether there is exactly one positive literal *)
  let rec find_uniq_pos lits = match lits with
  | [] -> None
  | (Equation (l,r,true,_) as lit)::lits' ->
    begin match find_uniq_pos lits' with
    | None -> Some lit  (* really unique *)
    | Some _ -> None (* there is another *)
    end
  | _::lits' -> find_uniq_pos lits'
  in
  (* check whether c is range-restricted Horn clause *)
  let is_closed_RR_horn c = 
    match find_uniq_pos c.clits with
    | None -> false
    | Some lit ->
      (* check that all variables of the clause occur in the head *)
      List.length (C.vars_of_lit lit) = List.length c.cvars
  in
  if is_closed_RR_horn c
    then []  (* do not select (conditional rewrite rule) *)
    else select_complex ~ord c  (* like select_complex *)


let default_selection ~ord = select_complex ~ord

(** table of name -> functions *)
let functions =
  let table = Hashtbl.create 17 in
  Hashtbl.add table "NoSelection" (fun ~ord c -> select_nothing c);
  Hashtbl.add table "MaxGoal" (fun ~ord c -> select_max_goal c);
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
  if C.selected c = [] then ()
  else assert
    (List.exists
      (fun idx -> C.neg_lit (C.get_lit c idx))
      (C.selected c))
