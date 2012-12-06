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

(** Select all positives literals *)
let select_positives hc =
  let rec find_pos acc i lits =
    if i = Array.length lits then acc
    else if C.pos_lit lits.(i) then find_pos (i::acc) (i+1) lits
    else find_pos acc (i+1) lits
  in find_pos [] 0 hc.hclits

let select_max_goal ?(strict=true) hc =
  (* find negative lits *)
  let rec find_maxneg lits i =
    if i = Array.length lits then [] else (* select nothing *)
    if C.neg_lit lits.(i) && C.is_maxlit hc i
      then if strict
        then [i] (* select one negative max goal *)
        else i :: select_positives hc (* negative max goal + positive lits *)
      else []
  in find_maxneg hc.hclits 0

let select_diff_neg_lit ?(strict=true) ~ord hc =
  (* find a negative literal with maximal difference between
     the weights of the sides of the equation *)
  let rec find_lit best_diff best_idx lits i =
    if i = Array.length lits then best_idx
    else match lits.(i) with
      | Equation (l, r, false, _) ->
        let weightdiff = abs (l.tsize - r.tsize) in
        if weightdiff > best_diff
          then find_lit weightdiff i lits (i+1) (* prefer this lit *)
          else find_lit best_diff best_idx lits (i+1)
      | _ -> find_lit best_diff best_idx lits (i+1)
  in
  (* search such a lit among the clause's lits *)
  match find_lit 0 (-1) hc.hclits 0 with
  | -1 -> []
  | n when strict -> [n]
  | n -> n :: select_positives hc

let select_complex ?(strict=true) ~ord hc =
  (* find x!=y in literals *)
  let rec find_noteqvars lits idx =
    if idx = Array.length lits then -1 else
    match lits.(idx) with
    | Equation (l, r, false,_) ->
      if T.is_var l && T.is_var r then idx else find_noteqvars lits (idx+1)
    | _ -> find_noteqvars lits (idx+1) 
  (* find the smallest ground negative literal *)
  and find_neg_ground best_weight best_idx lits idx =
    if idx = Array.length lits then best_idx else
    match lits.(idx) with
    | Equation (l, r, false, _) ->
      let weight = l.tsize + r.tsize in
      if T.is_ground_term l && T.is_ground_term r && weight < best_weight
        then find_neg_ground weight idx lits (idx+1)
        else find_neg_ground best_weight best_idx lits (idx+1)
    | _ -> find_neg_ground best_weight best_idx lits (idx+1)
  in
  (* try x!=y, else try ground negative, else delegate *)
  let i = find_noteqvars hc.hclits 0 in
  if i >= 0
    then if strict then [i] else i :: select_positives hc
    else
      let i = find_neg_ground 0 (-1) hc.hclits 0 in
      if i >= 0
        then if strict then [i] else i :: select_positives hc
        else select_diff_neg_lit ~strict ~ord hc (* delegate to select_diff_neg_lit *)

let select_complex_except_RR_horn ?(strict=true) ~ord hc =
  if Theories.is_RR_horn_clause hc
    then []  (* do not select (conditional rewrite rule) *)
    else select_complex ~strict ~ord hc  (* like select_complex *)

let default_selection ~ord = select_complex ~strict:true ~ord

(** table of name -> functions *)
let functions =
  let table = Hashtbl.create 17 in
  Hashtbl.add table "NoSelection" (fun ~ord c -> no_select c);
  Hashtbl.add table "MaxGoal" (fun ~ord c -> select_max_goal ~strict:true c);
  Hashtbl.add table "MaxGoalNS" (fun ~ord c -> select_max_goal ~strict:false c);
  Hashtbl.add table "SelectDiffNegLit" (select_diff_neg_lit ~strict:true);
  Hashtbl.add table "SelectDiffNegLitNS" (select_diff_neg_lit ~strict:false);
  Hashtbl.add table "SelectComplex" (select_complex ~strict:true);
  Hashtbl.add table "SelectComplexNS" (select_complex ~strict:false);
  Hashtbl.add table "SelectComplexExceptRRHorn" (select_complex_except_RR_horn ~strict:true);
  Hashtbl.add table "SelectComplexExceptRRHornNS" (select_complex_except_RR_horn ~strict:false);
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

let check_selected hc =
  if C.selected hc = [||] then ()
  else assert
    (Utils.array_foldi
      (fun acc i lit -> acc || C.neg_lit lit && C.is_selected hc i)
      false hc.hclits)
