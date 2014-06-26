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

module T = FOTerm
module S = Substs.FO
module Lit = Literal
module Lits = Literals
module BV = CCBV

type t = Literal.t array -> BV.t

let no_select _ = BV.empty ()

(* does the clause belong to pure superposition, without other theories? *)
let _pure_superposition lits =
  Util.array_forall
    (function
      | Lit.Prop _
      | Lit.Equation _
      | Lit.True
      | Lit.False -> true
      | _ -> false)
    lits

(* checks that [bv] is an acceptable selection for [lits]. In case
  some literal is selected, at least one negative literal must be selected. *)
let _validate_select lits bv =
  BV.is_empty bv
  ||
  try
    Array.iteri
      (fun i lit ->
        if Lit.is_neg lit && BV.get bv i then raise Exit
      ) lits;
    false
  with Exit -> true

(** Select all positives literals *)
let select_positives lits =
  if _pure_superposition lits
    then Lits.pos lits
    else BV.empty ()

let select_max_goal ~strict ~ord lits =
  if _pure_superposition lits
  then
    let bv = Lits.maxlits ~ord lits in
    BV.filter bv (fun i -> Lit.is_neg lits.(i));
    try
      (* keep only first satisfying lit *)
      let i = BV.first bv in
      BV.clear bv;
      BV.set bv i;
      if not strict
        then BV.union_into ~into:bv (select_positives lits);
      assert (_validate_select lits bv);
      bv
    with Not_found ->
      BV.empty ()  (* empty one *)
  else BV.empty ()

let select_diff_neg_lit ~strict ~ord lits =
  (* find a negative literal with maximal difference between
     the weights of the sides of the equation *)
  let rec find_lit best_diff best_idx lits i =
    if i = Array.length lits then best_idx
    else
      let weightdiff, ok = match lits.(i) with
        | Lit.Equation(l,r,false) ->
          abs (T.size l - T.size r), true
        | Lit.Prop (p, false) ->
          T.size p - 1, true
        | _ -> 0, false
      in
      let best_idx, best_diff =
        if ok && weightdiff > best_diff
          then i, weightdiff  (* prefer this lit *)
          else best_idx, best_diff
      in
      find_lit best_diff best_idx lits (i+1)
  in
  (* search such a lit among the clause's lits *)
  if _pure_superposition lits
  then match find_lit (-1) (-1) lits 0 with
    | -1 -> BV.empty ()
    | n when strict -> BV.of_list [n]
    | n ->
      let bv = select_positives lits in
      BV.set bv n;
      assert (_validate_select lits bv);
      bv
  else BV.empty ()

let select_complex ~strict ~ord lits =
  (* find the ground negative literal with highest diff in size *)
  let rec find_neg_ground best_diff best_i lits i =
    if i = Array.length lits then best_i else
      let weightdiff, ok = match lits.(i) with
        | Lit.Equation(l,r,false) when Lit.is_ground lits.(i) ->
          abs (T.size l - T.size r), true
        | Lit.Prop (p, false) when Lit.is_ground lits.(i) ->
          T.size p - 1, true
        | _ -> 0, false
      in
      let best_i, best_diff =
        if ok && weightdiff > best_diff
          then i, weightdiff  (* prefer this lit *)
          else best_i, best_diff
      in
      find_neg_ground best_diff best_i lits (i+1)
  in
  (* try to find ground negative lit with bigger weight difference, else delegate *)
  if _pure_superposition lits
  then
    let i = find_neg_ground (-1) (-1) lits 0 in
    if i >= 0
      then if strict
        then BV.of_list [i]
        else (
          let bv = select_positives lits in
          let _ = BV.set bv i in
          assert (_validate_select lits bv);
          bv
        )
      else
        select_diff_neg_lit ~strict ~ord lits (* delegate to select_diff_neg_lit *)
  else BV.empty ()

let select_complex_except_RR_horn ~strict ~ord lits =
  if not (_pure_superposition lits) || Lits.is_RR_horn_clause lits
    then BV.empty ()  (* do not select (conditional rewrite rule) *)
    else select_complex ~strict ~ord lits  (* like select_complex *)

(** {2 Global selection Functions} *)

let default_selection ~ord =
  select_complex ~strict:true ~ord

let __table = Hashtbl.create 17
  (** table of name -> functions *)

let () =
  Hashtbl.add __table "NoSelection" (fun ~ord c -> no_select c);
  Hashtbl.add __table "MaxGoal" (select_max_goal ~strict:true);
  Hashtbl.add __table "MaxGoalNS" (select_max_goal ~strict:false);
  Hashtbl.add __table "SelectDiffNegLit" (select_diff_neg_lit ~strict:true);
  Hashtbl.add __table "SelectDiffNegLitNS" (select_diff_neg_lit ~strict:false);
  Hashtbl.add __table "SelectComplex" (select_complex ~strict:true);
  Hashtbl.add __table "SelectComplexNS" (select_complex ~strict:false);
  Hashtbl.add __table "SelectComplexExceptRRHorn" (select_complex_except_RR_horn ~strict:true);
  Hashtbl.add __table "SelectComplexExceptRRHornNS" (select_complex_except_RR_horn ~strict:false);
  ()

(** selection function from string (may fail) *)
let selection_from_string ~ord s =
  try
    let select = Hashtbl.find __table s in
    select ~ord
  with Not_found ->
    failwith ("no such selection function: "^s)

(** available names for selection functions *)
let available_selections () =
  let l = ref [] in
  Hashtbl.iter (fun name select -> l := name :: !l) __table;
  !l

let register name f =
  (if Hashtbl.mem __table name
    then failwith ("selection function " ^ name ^ " already defined"));
  Hashtbl.add __table name f
