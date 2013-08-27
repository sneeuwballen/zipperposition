(*
Zipperposition: a functional superposition prover for prototyping
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** Selection functions. Note for splitting: SelectComplex already selects
    in priority "big" negative literals, ie literals that are not split symbols. *)

open Logtk

module T = Term
module S = Substs
module BV = Bitvector
module Lit = Literal

type t = Literal.t array -> int list

let no_select _ = []

(** Select all positives literals *)
let select_positives lits =
  let rec find_pos acc i lits =
    if i = Array.length lits then acc
    else if Literal.is_pos lits.(i) then find_pos (i::acc) (i+1) lits
    else find_pos acc (i+1) lits
  in find_pos [] 0 lits

let select_max_goal ~strict ~ord lits =
  let maxlits = Literal.maxlits ~ord lits in
  (* find negative lits *)
  let rec find_maxneg lits i =
    if i = Array.length lits then [] else (* select nothing *)
    if Literal.is_neg lits.(i) && BV.get maxlits i
      then if strict
        then [i] (* select one negative max goal *)
        else i :: select_positives lits (* negative max goal + positive lits *)
      else []
  in find_maxneg lits 0

let select_diff_neg_lit ~strict ~ord lits =
  (* find a negative literal with maximal difference between
     the weights of the sides of the equation *)
  let rec find_lit best_diff best_idx lits i =
    if i = Array.length lits then best_idx
    else match lits.(i) with
      | Literal.Equation (l, r, false, _) ->
        let weightdiff = abs (T.size l - T.size r) in
        if weightdiff > best_diff
          then find_lit weightdiff i lits (i+1) (* prefer this lit *)
          else find_lit best_diff best_idx lits (i+1)
      | _ -> find_lit best_diff best_idx lits (i+1)
  in
  (* search such a lit among the clause's lits *)
  match find_lit (-1) (-1) lits 0 with
  | -1 -> []
  | n when strict -> [n]
  | n -> n :: select_positives lits

let select_complex ~strict ~ord lits =
  (* find the ground negative literal with highest diff in size *)
  let rec find_neg_ground best_diff best_i lits i =
    if i = Array.length lits then best_i else
    match lits.(i) with
    | Literal.Equation (l, r, false, _) when T.is_ground l && T.is_ground r ->
      let diff = abs (T.size l - T.size r) in
      if diff > best_diff
        then find_neg_ground diff i lits (i+1)
        else find_neg_ground best_diff best_i lits (i+1)
    | _ -> find_neg_ground best_diff best_i lits (i+1)
  in
  (* try to find ground negative lit with bigger weight difference, else delegate *)
  let i = find_neg_ground (-1) (-1) lits 0 in
  if i >= 0
    then if strict then [i] else i :: select_positives lits
    else select_diff_neg_lit ~strict ~ord lits (* delegate to select_diff_neg_lit *)

let select_complex_except_RR_horn ~strict ~ord lits =
  if Literal.is_RR_horn_clause lits
    then []  (* do not select (conditional rewrite rule) *)
    else select_complex ~strict ~ord lits  (* like select_complex *)

let default_selection ~ord = select_complex ~strict:true ~ord

(** table of name -> functions *)
let functions =
  let table = Hashtbl.create 17 in
  Hashtbl.add table "NoSelection" (fun ~ord c -> no_select c);
  Hashtbl.add table "MaxGoal" (select_max_goal ~strict:true);
  Hashtbl.add table "MaxGoalNS" (select_max_goal ~strict:false);
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
