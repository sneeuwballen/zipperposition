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

(* returns (a in l, b in l) *)
let mem2 a b l =
  let rec aux found_a found_b = function
    | x :: tl ->
      let found_a = found_a || T.eq_term x a in
      let found_b = found_b || T.eq_term x b in
      if found_a && found_b
        then true, true
        else aux found_a found_b tl
    | [] -> found_a, found_b
  in
   aux false false l

(** occur check of what in where *)
let rec occurs_check subst what where =
  match where.term with
  | Var _ when T.eq_term where what -> true
  | Var _ ->
      let t = S.lookup where subst in
      if not (T.eq_term t where)
        then occurs_check subst what t
        else false
  | Node l -> List.exists (occurs_check subst what) l
  | _ -> false

let unification a b =
  (* recursive unification *)
  let rec unif subst s t =
    if s.sort <> t.sort then raise (UnificationFailure (lazy "different sorts"));
    let s = S.apply_subst subst s
    and t = S.apply_subst subst t in
    match s.term, t.term with
    | _, _ when T.eq_term s t -> subst
    | _, _ when T.is_ground_term s && T.is_ground_term t ->
        (* distinct ground terms cannot be unified *)
        raise (UnificationFailure (lazy "distinct ground terms"))
    | Var _, Var _ -> S.build_subst s t subst
    | Var _, _ when occurs_check subst s t -> raise (UnificationFailure (lazy "occur check"))
    | Var _, _ -> S.build_subst s t subst
    | _, Var _ when occurs_check subst t s -> raise (UnificationFailure (lazy "occur check"))
    | _, Var _ -> S.build_subst t s subst
    | Node l1, Node l2 -> (
        try
          List.fold_left2 unif subst l1 l2  (* recursive pairwise unification *)
        with Invalid_argument _ ->
          raise (UnificationFailure (lazy "arglists of distinct lengths"))
      )
    | _, _ -> raise (UnificationFailure (lazy "incompatible terms"))
  in
  prof_unification.HExtlib.profile (unif S.id_subst a) b

let matching_locked ~locked subst a b =
  (* recursive matching *)
  let rec unif subst s t =
    if s.sort <> t.sort then raise (UnificationFailure (lazy "different sorts"));
    let s = S.apply_subst subst s
    and t = S.apply_subst subst t in
    match s.term, t.term with
    | _, _ when T.eq_term s t -> subst
    | _, _ when T.is_ground_term s && T.is_ground_term t ->
        (* distinct ground terms cannot be matched *) 
        raise (UnificationFailure (lazy "distinct ground terms"))
    | Var _, _ when occurs_check subst s t || List.mem s locked ->
      raise (UnificationFailure (lazy "occur check"))
    | Var _, _ -> S.build_subst s t subst
    | Node l1, Node l2 -> (
        try
          List.fold_left2 unif subst l1 l2  (* recursive pairwise unification *)
        with Invalid_argument _ ->
          raise (UnificationFailure (lazy "arglists of distinct lengths"))
      )
    | _, _ -> raise (UnificationFailure (lazy "incompatible terms"))
  in
  prof_matching.HExtlib.profile (unif subst a) b

let matching a b = matching_locked ~locked:(T.vars_of_term b) S.id_subst a b

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
          with Invalid_argument _ ->
            raise (UnificationFailure (lazy "Inference.unification.unif"))
        )
      | _, _ ->
          raise (UnificationFailure (lazy "Inference.unification.unif"))
  in
    equiv S.id_subst s t

