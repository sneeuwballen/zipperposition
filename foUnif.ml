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

(* $Id: nCic.ml 9058 2008-10-13 17:42:30Z tassi $ *)

exception UnificationFailure of string Lazy.t;;

(* returns (a in l, b in l) *)
let mem2 a b l =
  let rec aux found_a found_b = function
    | x :: tl ->
       let found_a = found_a || x = a in
       let found_b = found_b || x = b in
       if found_a && found_b then true, true
       else aux found_a found_b tl
    | [] -> found_a, found_b
  in
   aux false false l

module Subst = FoSubst
module Utils = FoUtils

open Hashcons
open Terms

(* do both unification and match *)
let unification_match locked_vars t1 t2 =
  let rec occurs_check subst what where =
    match where.node.term with
    | Terms.Var _ when Utils.eq_foterm where what -> true
    | Terms.Var _ ->
        let t = Subst.lookup where subst in
        if not (Utils.eq_foterm t where)
          then occurs_check subst what t else false
    | Terms.Node l -> List.exists (occurs_check subst what) l
    | _ -> false
  and unif subst s t =
    let s = match s.node.term with Terms.Var _ -> Subst.lookup s subst | _ -> s
    and t = match t.node.term with Terms.Var _ -> Subst.lookup t subst | _ -> t in
    match s.node.term, t.node.term with
    | _, _ when Utils.eq_foterm s t -> subst
    | Terms.Var _, Terms.Var _ -> Subst.build_subst s t subst
    | Terms.Var _, _ when occurs_check subst s t ->
        raise (UnificationFailure (lazy "Inference.unification.unif"))
    | Terms.Var _, _ when (List.mem s locked_vars) ->
        raise (UnificationFailure (lazy "Inference.unification.unif"))
    | Terms.Var _, _ -> Subst.build_subst s t subst
    | _, Terms.Var _ when occurs_check subst t s ->
        raise (UnificationFailure (lazy "Inference.unification.unif"))
    | _, Terms.Var _ when (List.mem t locked_vars) ->
        raise (UnificationFailure (lazy "Inference.unification.unif"))
    | _, Terms.Var _ -> Subst.build_subst t s subst
    | Terms.Node l1, Terms.Node l2 -> (
        try
          List.fold_left2
            (fun subst' s t -> unif subst' s t)
            subst l1 l2
        with Invalid_argument _ ->
          raise (UnificationFailure (lazy "Inference.unification.unif"))
      )
    | _, _ ->
        raise (UnificationFailure (lazy "Inference.unification.unif")) in
  let subst = unif Subst.id_subst t1 t2 in
  subst

let unification a b = unification_match [] a b

let match_term a b = unification_match (Terms.vars_of_term b) a b

(* Sets of variables in s and t are assumed to be disjoint  *)
let alpha_eq s t =
  let rec equiv subst s t =
    let s = match s.node.term with Terms.Var _ -> Subst.lookup s subst | _ -> s
    and t = match t.node.term with Terms.Var _ -> Subst.lookup t subst | _ -> t

    in
    match s.node.term, t.node.term with
      | _, _ when Utils.eq_foterm s t -> subst
      | Terms.Var _, Terms.Var _
          when (not (List.exists (fun (_,k) -> k=t) subst)) ->
          let subst = Subst.build_subst s t subst in
            subst
      | Terms.Node l1, Terms.Node l2 -> (
          try
            List.fold_left2
              (fun subst' s t -> equiv subst' s t)
              subst l1 l2
          with Invalid_argument _ ->
            raise (UnificationFailure (lazy "Inference.unification.unif"))
        )
      | _, _ ->
          raise (UnificationFailure (lazy "Inference.unification.unif"))
  in
    equiv Subst.id_subst s t

