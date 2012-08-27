(*
zipperposition: a functional superposition prover for prototyping
copyright (c) 2012 simon cruanes

this is free software; you can redistribute it and/or
modify it under the terms of the gnu general public license
as published by the free software foundation; either version 2
of the license, or (at your option) any later version.

this is distributed in the hope that it will be useful,
but without any warranty; without even the implied warranty of
merchantability or fitness for a particular purpose.  see the
gnu general public license for more details.

you should have received a copy of the gnu general public license
along with this program; if not, write to the free software
foundation, inc., 51 franklin street, fifth floor, boston, ma
02110-1301 usa.
*)

(** module for superposition with equivalence reasoning and delayed clausal form *)

open Types
open Hashcons
module T = Terms
module O = Orderings
module S = FoSubst
module Utils = FoUtils
module Sup = Superposition

let false_symbol = T.false_symbol
let true_symbol = T.true_symbol
let eq_symbol = "="
let exists_symbol = "$$exists"
let forall_symbol = "$$forall"
let lambda_symbol = "$$lambda"
let not_symbol = "$$not"
let imply_symbol = "$$imply"
let and_symbol = "$$and"
let or_symbol = "$$or"

let db_symbol = "$$db"
let succ_db_symbol = "$$s"
let subst_symbol = "$$subst"

let is_symmetric_symbol s =
  s = eq_symbol || s = or_symbol || s = and_symbol

let is_infix_symbol s =
  s = eq_symbol || s = or_symbol || s = and_symbol || s = imply_symbol

(* check whether s is a binding symbol *)
let is_binder_symbol s = s = lambda_symbol

(** check whether the term is (Leaf s) *)
let check_sym t s = match t.node.term with
  | Var _ -> false
  | Node _ -> false
  | Leaf s' -> s = s'

(* TODO special case for special symbols *)
let rec pp_foterm formatter t = match t.node.term with
  | Node (({node={term=Leaf s}} as head)::args) ->
    (* general case for nodes *)
    if is_infix_symbol s
      then begin
        match args with
        | [l;r] -> Format.fprintf formatter "@[<h>%a %a %a@]" pp_foterm l
            pp_foterm head pp_foterm r
        | _ -> assert false (* infix and not binary? *)
      end else Format.fprintf formatter "@[<h>%a(%a)@]" pp_foterm head
        (Utils.pp_list ~sep:", " pp_foterm) args
  | Leaf s when s = lambda_symbol -> Format.pp_print_string formatter "λ"
  | Leaf s when s = exists_symbol -> Format.pp_print_string formatter "∃"
  | Leaf s when s = forall_symbol -> Format.pp_print_string formatter "∀"
  | Leaf s when s = and_symbol -> Format.pp_print_string formatter "&"
  | Leaf s when s = or_symbol -> Format.pp_print_string formatter "|"
  | Leaf s when s = imply_symbol -> Format.pp_print_string formatter "→"
  | Leaf s -> Format.pp_print_string formatter s
  | Var i -> Format.fprintf formatter "X%d" i
  | Node _ -> failwith "bad term"

let pp_clause formatter clause =
  let pp_lit formatter = function 
  | Equation (l,r,true,_) when T.eq_foterm r T.true_term ->
    pp_foterm formatter l
  | Equation (l,r,true,_) when T.eq_foterm l T.true_term ->
    pp_foterm formatter r
  | Equation (l,r,true,_)  ->
    Format.fprintf formatter "%a = %a" pp_foterm l pp_foterm r
  | Equation (l,r,false,_) when T.eq_foterm r T.true_term ->
    Format.fprintf formatter "~%a" pp_foterm l
  | Equation (l,r,false,_) when T.eq_foterm l T.true_term ->
    Format.fprintf formatter "~%a" pp_foterm r
  | Equation (l,r,false,_)  ->
    Format.fprintf formatter "%a != %a" pp_foterm l pp_foterm r
  in Utils.pp_list ~sep:" | " pp_lit formatter clause.clits

(* replace 0 by s in t *)
let db_replace t s =
  (* lift the De Bruijn symbol *)
  let mk_succ db = T.mk_node [T.mk_leaf succ_db_symbol T.univ_sort; db] in
  (* replace db by s in t *)
  let rec replace db s t = match t.node.term with
  | _ when T.eq_foterm t db -> s
  | Leaf _ | Var _ -> t
  | Node (({node={term=Leaf symb}} as hd)::tl) when is_binder_symbol symb ->
    (* lift the De Bruijn to replace *)
    T.mk_node (hd :: (List.map (replace (mk_succ db) s) tl))
  | Node ({node={term=Leaf s}}::_) when s = succ_db_symbol || s = db_symbol ->
    t (* no the good De Bruijn symbol *)
  | Node l -> T.mk_node (List.map (replace db s) l)
  (* replace the 0 De Bruijn index by s in t *)
  in
  replace (T.mk_leaf db_symbol T.univ_sort) s t

(* constraint on the ordering *)
let symbol_constraint =
  O.compose_constraints
    (O.max_constraint [succ_db_symbol; db_symbol])
    (O.min_constraint [eq_symbol; imply_symbol; forall_symbol; exists_symbol;
                       or_symbol; and_symbol; false_symbol; true_symbol])

(* creation of a new skolem symbol *)
let skolem =
  let count = ref 0 in  (* current symbol counter *)
  fun ord args ->
    let new_symbol = "$$sk_" ^ (string_of_int !count) in
    incr count;
    (* build the new term first *)
    let new_term =
      if args = [] then T.mk_leaf new_symbol T.univ_sort
      else T.mk_node ((T.mk_leaf new_symbol T.univ_sort) :: args)
    in
    (* update the ordering *)
    new_term, ord#refresh ()

(* list of inference rules TODO *)
let inference_rules = []

(* axioms TODO *)
let axioms = []
