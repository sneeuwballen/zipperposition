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


open Types
open Hashcons

module T = Terms
module S = FoSubst

let enable = true

let prof_unification = HExtlib.profile ~enable "unification"
let prof_matching = HExtlib.profile ~enable "matching"

(** does var appear in t (even when expanding bindings)? *)
let rec occurs_check var t =
  match t.term with
  | _ when T.is_ground_term t -> false
  | Var _ when T.eq_term var t -> true
  | Var _ when not (T.eq_term t (T.get_binding t)) ->
    occurs_check var (T.get_binding t)  (* see in binding *)
  | Var _ -> false
  | Leaf _ -> assert false
  | Node _ -> List.exists (occurs_check var) t.vars

(** if t is a variable, return its current binding *)
let rec get_var_binding t =
  if T.is_var t && not (T.eq_term (T.get_binding t) t)
    then get_var_binding (T.get_binding t)
    else t

let unification subst a b =
  (* recursive unification *)
  let rec unif subst s t =
    (if s.sort <> t.sort then raise UnificationFailure);
    let s = get_var_binding s
    and t = get_var_binding t in
    match s.term, t.term with
    | _, _ when T.eq_term s t -> subst
    | _, _ when T.is_ground_term s && T.is_ground_term t -> raise UnificationFailure
        (* distinct ground terms cannot be unified *)
    | Var _, _ when occurs_check s t -> raise UnificationFailure
    | Var _, _ ->
      T.set_binding s (T.expand_bindings t);
      S.update_binding subst s
    | _, Var _ when occurs_check t s -> raise UnificationFailure
    | _, Var _ ->
      T.set_binding t (T.expand_bindings s);
      S.update_binding subst t
    | Node l1, Node l2 when List.length l1 = List.length l2 ->
      let subst = unify_composites subst l1 l2 in (* unify non-vars *)
      unify_vars subst l1 l2  (* unify vars *)
    | _, _ -> raise UnificationFailure
  (* unify pairwise when pairs contain no variable at root *)
  and unify_composites subst l1 l2 =
    match l1, l2 with
    | [], [] -> subst
    | x1::l1', x2::l2' ->
      let subst = if T.is_var x1 || T.is_var x2 then subst else unif subst x1 x2 in
      unify_composites subst l1' l2'
    | _ -> assert false
  (* unify pairwise when pairs contain at least one variable at root *)
  and unify_vars subst l1 l2 =
    match l1, l2 with
    | [], [] -> subst
    | x1::l1', x2::l2' ->
      let subst = if T.is_var x1 || T.is_var x2 then unif subst x1 x2 else subst in
      unify_vars subst l1' l2'
    | _ -> assert false
  (* setup and cleanup *)
  and root_unify () =
    T.reset_vars a;
    T.reset_vars b;
    S.apply_subst_bind subst;
    let subst = unif subst a b in
    subst
  in
  prof_unification.HExtlib.profile root_unify ()

let matching_locked ~locked subst a b =
  let locked = T.THashSet.from_list locked in
  (* recursive matching *)
  let rec unif subst s t =
    (if s.sort <> t.sort then raise UnificationFailure);
    let s = get_var_binding s
    and t = get_var_binding t in
    match s.term, t.term with
    | _, _ when T.eq_term s t -> subst
    | _, _ when T.is_ground_term s && T.is_ground_term t ->
        (* distinct ground terms cannot be matched *) 
        raise UnificationFailure
    | Var _, _ when T.THashSet.member locked s || occurs_check s t ->
      raise UnificationFailure
    | Var _, _ ->
      T.set_binding s (T.expand_bindings t);
      S.update_binding subst s
    | Node l1, Node l2 when List.length l1 = List.length l2 ->
      List.fold_left2 unif subst l1 l2
    | _, _ -> raise UnificationFailure
  (* main matching procedure, with setup *)
  and root_match () =
    T.reset_vars a;
    T.reset_vars b;
    S.apply_subst_bind subst;
    unif subst a b
  in
  prof_matching.HExtlib.profile root_match ()

let matching subst a b = matching_locked ~locked:b.vars subst a b

(** Sets of variables in s and t are assumed to be disjoint  *)
let alpha_eq s t =
  let rec equiv subst s t =
    let s = match s.term with Var _ -> S.lookup s subst | _ -> s
    and t = match t.term with Var _ -> S.lookup t subst | _ -> t

    in
    match s.term, t.term with
      | _, _ when T.eq_term s t -> subst
      | Var _, Var _
          when (not (List.exists (fun (_,k) -> k=t) subst)) ->
          let subst = S.build_subst s t subst in
            subst
      | Node l1, Node l2 -> (
          try
            List.fold_left2
              (fun subst' s t -> equiv subst' s t)
              subst l1 l2
          with Invalid_argument _ -> raise UnificationFailure
        )
      | _, _ -> raise UnificationFailure
  in
    equiv S.id_subst s t

