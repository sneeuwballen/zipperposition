(*
    ||M||  This file is part of HELM, an Hypertextual, Electronic
    ||A||  Library of Mathematics, developed at the Computer Science
    ||T||  Department, University of Bologna, Italy.
    ||I||
    ||T||  HELM is free software; you can redistribute it and/or
    ||A||  modify it under the terms of the GNU General Public License
    \   /  version 2 or (at your option) any later version.
     \ /   This software is distributed as is, NO WARRANTY.
      V_______________________________________________________________ *)

open Types
open Hashcons
open Format

module T = Terms
module C = Clauses
module I = Index
module PS = ProofState

(* Main pretty printing functions *)

(* print a list of items using the printing function *)
let rec pp_list ?(sep=", ") pp_item  formatter = function
  | x::y::xs -> fprintf formatter "%a%s@,%a"
      pp_item x sep (pp_list ~sep:sep pp_item) (y::xs)
  | x::[] -> pp_item formatter x
  | [] -> ()

(* print a term *)
let rec pp_foterm formatter t = match t.node.term with
  | Leaf x -> Signature.pp_symbol formatter x
  | Var i -> fprintf formatter "X%d" i
  | Node (head::args) -> fprintf formatter
      "@[<h>%a(%a)@]" pp_foterm head (pp_list ~sep:", " pp_foterm) args
  | Node [] -> failwith "bad term"

let string_of_direction = function
    | Left2Right -> "Left to right"
    | Right2Left -> "Right to left"
    | Nodir -> "No direction"

let string_of_pos s = match s with
  | _ when s == C.left_pos -> "left"
  | _ when s == C.right_pos -> "right"
  | _ -> assert false

(* print substitution *)
let pp_substitution formatter subst =
  fprintf formatter "@[<h>";
  List.iter
    (fun (v, t) ->
       fprintf formatter "?%a ->@, %a@;" pp_foterm v pp_foterm t)
    subst;
  fprintf formatter "@]"

(* print proof
let pp_proof bag ~formatter:f p =
  let rec aux eq = function
    | Terms.Exact t ->
        fprintf f "%d: Exact (" eq;
        pp_foterm f t;
        fprintf f ")@;";
    | Terms.Step (rule,eq1,eq2,dir,pos,subst) ->
        fprintf f "%d: %s("
          eq (string_of_rule rule);
      fprintf f "|%d with %d dir %s))" eq1 eq2
        (string_of_direction dir);
      let (_, _, _, proof1),_,_ = Terms.get_from_bag eq1 bag in
      let (_, _, _, proof2),_,_ = Terms.get_from_bag eq2 bag in
        fprintf f "@[<v 2>";
          aux eq1 proof1;
          aux eq2 proof2;
        fprintf f "@]";
  in
    fprintf f "@[<v>";
    aux 0 p;
    fprintf f "@]"
;;
*)

let string_of_comparison = function
  | Lt -> "=<="
  | Gt -> "=>="
  | Eq -> "==="
  | Incomparable -> "=?="
  | Invertible -> "=<->="

let pp_literal formatter = function
  | Equation (left, right, false, _) when right = T.true_symbol ->
    fprintf formatter "~%a" pp_foterm left
  | Equation (left, right, true, _) when right = T.true_symbol ->
    pp_foterm formatter left
  | Equation (left, right, true, _) when left = T.true_symbol ->
    pp_foterm formatter right
  | Equation (left, right, false, _) when left = T.true_symbol ->
    fprintf formatter "~%a" pp_foterm right
  | Equation (left, right, sign, ord) ->
    if sign
    then fprintf formatter "@[%a@ %a@ %a@]"
        pp_foterm left pp_foterm T.eq_symbol pp_foterm right
    else fprintf formatter "@[<hv 2>%a !%a@ %a@]"
        pp_foterm left pp_foterm T.eq_symbol pp_foterm right

let pp_clause formatter {clits=lits} =
  fprintf formatter "@[<hv 2>%a@]" (pp_list ~sep:" | " pp_literal) lits

let pp_clause_pos formatter (c, pos) =
  fprintf formatter "[%a at @[<h>%a@]]@;"
  pp_clause c (pp_list ~sep:"." pp_print_int) pos

let pp_hclause_pos formatter (c, pos) =
  fprintf formatter "[%a at @[<h>%a@]]@;"
  pp_clause c.node (pp_list ~sep:"." pp_print_int) pos

let pp_bag formatter bag =
  fprintf formatter "@[<hov>";
  C.M.iter
    (fun _ hc -> fprintf formatter "%a@;" pp_clause hc.node)
    bag.C.bag_clauses;
  fprintf formatter "@]"

let pp_index formatter idx = 
  let print_dt_path path set =
    let l = I.ClauseSet.elements set in
    fprintf formatter "%s : @[<hov>%a@]@;"
      (I.FotermIndexable.string_of_path path)
      (pp_list ~sep:", " pp_hclause_pos) l
  in
  fprintf formatter "index:@.root_index=@[<v 2>";
  I.DT.iter idx.I.root_index print_dt_path;
  fprintf formatter "@]@;subterm_index=@[<v 2>";
  I.DT.iter idx.I.subterm_index print_dt_path;
  fprintf formatter "@]@;"

let pp_state formatter state =
  Format.fprintf formatter "state {%d active clauses; %d passive_clauses}"
    (C.size_bag state.PS.active_set.PS.active_clauses)
    (C.size_bag state.PS.passive_set.PS.passive_clauses)

let debug_state formatter state =
  Format.fprintf formatter
    "@[<v 2>state {%d active clauses; %d passive_clauses;@;active:%a@;passive:%a@]@;"
    (C.size_bag state.PS.active_set.PS.active_clauses)
    (C.size_bag state.PS.passive_set.PS.passive_clauses)
    pp_bag state.PS.active_set.PS.active_clauses
    pp_bag state.PS.passive_set.PS.passive_clauses
