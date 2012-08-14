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

(* print a list of items using the printing function *)
let rec pp_list ?(sep=", ") pp_item  formatter = function
  | x::y::xs -> fprintf formatter "%a%s@,%a"
      pp_item x sep (pp_list ~sep:sep pp_item) (y::xs)
  | x::[] -> pp_item formatter x
  | [] -> ()

let rec pp_foterm formatter t = match t.node.term with
  | Leaf x -> Signature.pp_symbol formatter x
  | Var i -> fprintf formatter "X%d" i
  | Node (head::args) -> fprintf formatter
      "@[<h>%a(%a)@]" pp_foterm head (pp_list ~sep:", " pp_foterm) args
  | Node [] -> failwith "bad term"

let pp_signature formatter symbols =
  Format.fprintf formatter "@[<h>sig %a@]" (pp_list ~sep:" > " T.pp_symbol) symbols

let string_of_direction = function
    | Left2Right -> "Left to right"
    | Right2Left -> "Right to left"
    | Nodir -> "No direction"

let string_of_pos s = match s with
  | _ when s == C.left_pos -> "left"
  | _ when s == C.right_pos -> "right"
  | _ -> assert false

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
  | Equation (left, right, false, _) when right = T.true_term ->
    fprintf formatter "~%a" pp_foterm left
  | Equation (left, right, true, _) when right = T.true_term ->
    pp_foterm formatter left
  | Equation (left, right, true, _) when left = T.true_term ->
    pp_foterm formatter right
  | Equation (left, right, false, _) when left = T.true_term ->
    fprintf formatter "~%a" pp_foterm right
  | Equation (left, right, sign, ord) ->
    if sign
    then fprintf formatter "@[%a@ %a@ %a@]"
        pp_foterm left pp_foterm T.eq_term pp_foterm right
    else fprintf formatter "@[<hv 2>%a !%a@ %a@]"
        pp_foterm left pp_foterm T.eq_term pp_foterm right

let pp_clause formatter {clits=lits} =
  fprintf formatter "@[<h>%a@]" (pp_list ~sep:" | " pp_literal) lits

let pp_clause_pos formatter (c, pos) =
  fprintf formatter "@[<h>[%a at @[<h>%a@]]@]"
  pp_clause c (pp_list ~sep:"." pp_print_int) pos

let pp_hclause formatter c =
  fprintf formatter "@[<h>hclause(%d) %a@]" c.tag pp_clause c.node

let pp_hclause_pos formatter (c, pos, _) =
  fprintf formatter "@[<h>[%a at @[<h>%a@]]@]"
  pp_hclause c (pp_list ~sep:"." pp_print_int) pos

let pp_bag formatter bag =
  fprintf formatter "@[<v>";
  C.M.iter
    (fun _ hc -> fprintf formatter "%a@;" pp_clause hc.node)
    bag.C.bag_clauses;
  fprintf formatter "@]"

let pp_index ?(all_clauses=false) formatter idx = 
  let print_dt_path path set =
  if all_clauses
    then let l = I.ClauseSet.elements set in
    fprintf formatter "%s : @[<hov>%a@]@;"
      (I.FotermIndexable.string_of_path path)
      (pp_list ~sep:", " pp_hclause_pos) l
    else fprintf formatter "@[<h>%s : %d clauses/pos@]@;"
      (I.FotermIndexable.string_of_path path)
      (I.ClauseSet.cardinal set)
  in
  fprintf formatter "index:@.root_index=  @[<v>";
  I.DT.iter idx.I.root_index print_dt_path;
  fprintf formatter "@]@.unit_root_index=  @[<v>";
  I.DT.iter idx.I.unit_root_index print_dt_path;
  fprintf formatter "@]@.subterm_index=  @[<v>";
  I.DT.iter idx.I.subterm_index print_dt_path;
  fprintf formatter "@]@."

let pp_queue formatter q =
  Format.fprintf formatter "@[<h>queue %s@]" q#name

let pp_queue_weight formatter (q, w) =
  Format.fprintf formatter "@[<h>queue %s, %d@]" q#name w

let pp_queues formatter qs =
  Format.fprintf formatter "@[<hov>%a@]" (pp_list ~sep:"; " pp_queue_weight) qs

