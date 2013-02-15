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
module Utils = FoUtils

let enable = true

let prof_unification = Utils.mk_profiler "unification"
let prof_matching = Utils.mk_profiler "matching"

(** Does [v] appear in [t] if we apply the substitution? *)
let occurs_check subst v o_v t o_t =
  let rec check v o_v t o_t =
    if T.is_ground_term t then false
      else match t.term with
      | Var _ when v == t && o_v = o_t -> true
      | Var _ ->  (* if [t] is a var bound by [subst], check in its image *) 
        (try let (t', o_t') = S.lookup subst (t, o_t) in
              check v o_v t' o_t'
        with Not_found -> false)
      | BoundVar _ -> false
      | Bind (_, t') -> check v o_v t' o_t
      | Node (_, l) -> check_list v o_v l o_t
  and check_list v o_v l o_l = match l with
  | [] -> false
  | t::l' -> check v o_v t o_l || check_list v o_v l' o_l
  in
  check v o_v t o_t

(** Unify terms, returns a substitution or raises UnificationFailure *)
let unification subst (a, o_a) (b, o_b) =
  Utils.enter_prof prof_unification;
  (* recursive unification *)
  let rec unif subst s o_s t o_t =
    (if s.sort != t.sort then raise UnificationFailure);
    let s, o_s = S.get_var subst (s, o_s)
    and t, o_t = S.get_var subst (t, o_t) in
    match s.term, t.term with
    | _ when s == t && (T.is_ground_term s || o_s = o_t) ->
      subst (* the terms are equal under any substitution *)
    | _ when T.is_ground_term s && T.is_ground_term t ->
      raise UnificationFailure (* terms are not equal, and ground. failure. *)
    | Var _, _ ->
      if occurs_check subst s o_s t o_t
        then raise UnificationFailure (* occur check *)
        else S.bind subst (s, o_s) (t, o_t) (* bind s *)
    | _, Var _ ->
      if occurs_check subst t o_t s o_s
        then raise UnificationFailure (* occur check *)
        else S.bind subst (t, o_t) (s, o_s) (* bind s *)
    | Bind (f, t1'), Bind (g, t2') when f == g -> unif subst t1' o_s t2' o_t
    | BoundVar i, BoundVar j -> if i = j then subst else raise UnificationFailure
    | Node (f, l1), Node (g, l2) when f == g && List.length l1 = List.length l2 ->
      unif_list subst l1 o_s l2 o_t
    | _, _ -> raise UnificationFailure
  (* unify pair of lists of terms *)
  and unif_list subst l1 o_1 l2 o_2 = match l1, l2 with
  | [], [] -> subst
  | [], _ | _, [] -> raise UnificationFailure
  | t1::l1', t2::l2' ->
    let subst = unif subst t1 o_1 t2 o_2 in
    unif_list subst l1' o_1 l2' o_2
  in
  (* try unification, and return solution/exception (with profiler handling) *)
  try
    let subst = unif subst a o_a b o_b in
    Utils.debug 4 (lazy (Utils.sprintf "unify @[<h>%a[%d] and %a[%d]@] yields %a"
                  !T.pp_term#pp a o_a !T.pp_term#pp b o_b S.pp_substitution subst));
    Utils.exit_prof prof_unification;
    subst
  with UnificationFailure as e ->
    Utils.exit_prof prof_unification;
    raise e

(** [matching a b] returns sigma such that sigma(a) = b, or raises
    UnificationFailure. Only variables from the context of [a] can
    be bound in the substitution. *)
let matching subst (a, o_a) (b, o_b) =
  Utils.enter_prof prof_matching;
  (* recursive matching *)
  let rec unif subst s o_s t o_t =
    (if s.sort != t.sort then raise UnificationFailure);
    let s, o_s = S.get_var subst (s, o_s) in
    match s.term, t.term with
    | _ when s == t && (T.is_ground_term s || o_s = o_t) ->
      subst (* the terms are equal under any substitution *)
    | _ when T.is_ground_term s && T.is_ground_term t ->
      raise UnificationFailure (* terms are not equal, and ground. failure. *)
    | Var _, _ ->
      if occurs_check subst s o_s t o_t || o_s <> o_a
        then raise UnificationFailure
          (* occur check, or [s] is not in the initial
             context [o_a] in which variables can be bound. *)
        else S.bind subst (s, o_s) (t, o_t) (* bind s *)
    | Bind (f, t1'), Bind (g, t2') when f == g -> unif subst t1' o_s t2' o_t
    | BoundVar i, BoundVar j -> if i = j then subst else raise UnificationFailure
    | Node (f, l1), Node (g, l2) when f == g && List.length l1 = List.length l2 ->
      unif_list subst l1 o_s l2 o_t
    | _, _ -> raise UnificationFailure
  (* unify pair of lists of terms *)
  and unif_list subst l1 o_1 l2 o_2 = match l1, l2 with
  | [], [] -> subst
  | [], _ | _, [] -> raise UnificationFailure
  | t1::l1', t2::l2' ->
    let subst = unif subst t1 o_1 t2 o_2 in
    unif_list subst l1' o_1 l2' o_2
  in
  (* try matching, and return solution/exception (with profiler handling) *)
  try
    let subst = unif subst a o_a b o_b in
    Utils.debug 4 (lazy (Utils.sprintf "match @[<h>%a[%d] and %a[%d]@] yields %a"
                  !T.pp_term#pp a o_a !T.pp_term#pp b o_b S.pp_substitution subst));
    Utils.exit_prof prof_matching;
    subst
  with UnificationFailure as e ->
    Utils.exit_prof prof_matching;
    raise e
