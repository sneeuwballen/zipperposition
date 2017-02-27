
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** Selection functions. Note for splitting: SelectComplex already selects
    in priority "big" negative literals, ie literals that are not split symbols. *)

open Logtk

module T = FOTerm
module S = Subst.FO
module Lit = Literal
module Lits = Literals
module BV = CCBV

type t = Literal.t array -> BV.t

let no_select _ = BV.empty ()

(* does the clause belong to pure superposition, without other theories? *)
let _pure_superposition lits =
  CCArray.for_all
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

let select_diff_neg_lit ~strict ~ord:_ lits =
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

let tbl_ = Hashtbl.create 17
(** table of name -> functions *)

let () =
  Hashtbl.add tbl_ "NoSelection" (fun ~ord:_ c -> no_select c);
  Hashtbl.add tbl_ "MaxGoal" (select_max_goal ~strict:true);
  Hashtbl.add tbl_ "MaxGoalNS" (select_max_goal ~strict:false);
  Hashtbl.add tbl_ "SelectDiffNegLit" (select_diff_neg_lit ~strict:true);
  Hashtbl.add tbl_ "SelectDiffNegLitNS" (select_diff_neg_lit ~strict:false);
  Hashtbl.add tbl_ "SelectComplex" (select_complex ~strict:true);
  Hashtbl.add tbl_ "SelectComplexNS" (select_complex ~strict:false);
  Hashtbl.add tbl_ "SelectComplexExceptRRHorn" (select_complex_except_RR_horn ~strict:true);
  Hashtbl.add tbl_ "SelectComplexExceptRRHornNS" (select_complex_except_RR_horn ~strict:false);
  ()

(** selection function from string (may fail) *)
let selection_from_string ~ord s =
  try
    let select = Hashtbl.find tbl_ s in
    select ~ord
  with Not_found ->
    failwith ("no such selection function: "^s)

(** available names for selection functions *)
let available_selections () = CCHashtbl.keys_list tbl_

let register name f =
  if Hashtbl.mem tbl_ name
  then failwith ("selection function " ^ name ^ " already defined");
  Hashtbl.add tbl_ name f

let () =
  let set_select s = Params.select := s in
  Params.add_opts
    [ "--select",
      Arg.Symbol (available_selections (), set_select),
      " set literal selection function"
    ]
