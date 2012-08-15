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

module T = Terms
module S = FoSubst

(* returns (a in l, b in l) *)
let mem2 a b l =
  let rec aux found_a found_b = function
    | x :: tl ->
      let found_a = found_a || x = a in
      let found_b = found_b || x = b in
      if found_a && found_b
        then true, true
        else aux found_a found_b tl
    | [] -> found_a, found_b
  in
   aux false false l

(* do both unification and match *)
let rec unif locked_vars subst s t =
  if s.node.sort <> t.node.sort then raise (UnificationFailure (lazy "different sorts"));
  let s = match s.node.term with Var _ -> S.apply_subst subst s | _ -> s
  and t = match t.node.term with Var _ -> S.apply_subst subst t | _ -> t in
  match s.node.term, t.node.term with
  | _, _ when T.eq_foterm s t -> subst
  | Var _, Var _ ->
      let s_locked, t_locked = mem2 s t locked_vars in
      if s_locked then
        if t_locked then
          raise (UnificationFailure (lazy "Inference.unification.unif"))
        else
          S.build_subst t s subst
      else
        S.build_subst s t subst
  | Var _, _ when occurs_check subst s t || List.mem s locked_vars ->
      raise (UnificationFailure (lazy "Inference.unification.unif"))
  | Var _, _ -> S.build_subst s t subst
  | _, Var _ when occurs_check subst t s || List.mem t locked_vars ->
      raise (UnificationFailure (lazy "Inference.unification.unif"))
  | _, Var _ -> S.build_subst t s subst
  | Node l1, Node l2 -> (
      try
        List.fold_left2
          (fun subst' s t -> unif locked_vars subst' s t)
          subst l1 l2
      with Invalid_argument _ ->
        raise (UnificationFailure (lazy "Inference.unification.unif"))
    )
  | _, _ ->
      raise (UnificationFailure (lazy "Inference.unification.unif"))
and occurs_check subst what where =
  match where.node.term with
  | Var _ when T.eq_foterm where what -> true
  | Var _ ->
      let t = S.lookup where subst in
      if not (T.eq_foterm t where)
        then occurs_check subst what t
        else false
  | Node l -> List.exists (occurs_check subst what) l
  | _ -> false

(* full unification, no locked variables *)
let unification a b = unif [] S.id_subst a b

(* matching, lock variables of b (b must generalize a) *)
let matching a b = unif (T.vars_of_term b) S.id_subst a b

(* Sets of variables in s and t are assumed to be disjoint  *)
let alpha_eq s t =
  let rec equiv subst s t =
    let s = match s.node.term with Var _ -> S.lookup s subst | _ -> s
    and t = match t.node.term with Var _ -> S.lookup t subst | _ -> t

    in
    match s.node.term, t.node.term with
      | _, _ when T.eq_foterm s t -> subst
      | Var _, Var _
          when (not (List.exists (fun (_,k) -> k=t) subst)) ->
          let subst = S.build_subst s t subst in
            subst
      | Node l1, Node l2 -> (
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
    equiv S.id_subst s t

