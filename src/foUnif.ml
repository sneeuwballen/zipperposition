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


open Basic
open Symbols

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
      | Bind (_, _, t') -> check v o_v t' o_t
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
    | Bind (f, _, t1'), Bind (g, _, t2') when f == g -> unif subst t1' o_s t2' o_t
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
    Utils.debug 4 "unify @[<h>%a[%d] and %a[%d]@] yields %a"
                  !T.pp_term#pp a o_a !T.pp_term#pp b o_b S.pp_substitution subst;
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
    | Bind (f, _, t1'), Bind (g, _, t2') when f == g -> unif subst t1' o_s t2' o_t
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
    Utils.debug 4 "match @[<h>%a[%d] and %a[%d]@] yields %a" !T.pp_term#pp a o_a
                !T.pp_term#pp b o_b S.pp_substitution subst;
    Utils.exit_prof prof_matching;
    subst
  with UnificationFailure as e ->
    Utils.exit_prof prof_matching;
    raise e

(** [matching_ac a b] returns substitutions such that [subst(a) =_AC b]. It
    is much more costly than [matching]. By default [is_ac] returns true only
    for symbols that have [attr_ac], and [is_com] only for [attr_commut].
    [offset] is used to create new variables. *)
let matching_ac ?(is_ac=fun s -> has_attr attr_ac s)
                ?(is_com=fun s -> has_attr attr_commut s)
                ?offset
                subst (a, o_a) (b, o_b) =
  (* function to get fresh variables *)
  let offset = match offset with
    | Some o -> o
    | None -> ref (max (T.max_var (T.vars a) + o_a + 1)
                       (T.max_var (T.vars b) + o_b + 1)) in
  (* avoid index collisions *)
  let fresh_var sort =
    let v = T.mk_var !offset sort in
    incr offset;
    v
  in
  (* recursive matching. [k] is called with solutions *)
  let rec unif subst s o_s t o_t k =
    Utils.debug 4 "try unif @[<h>%a[%d] and %a[%d]@]" !T.pp_term#pp s o_s !T.pp_term#pp t o_t;
    if s.sort != t.sort then ()
    else let s, o_s = S.get_var subst (s, o_s) in
      match s.term, t.term with
      | Var _, Var _ when s == t && o_s = o_t -> k subst (* trivial success *)
      | Var _, _ ->
        if occurs_check subst s o_s t o_t || o_s <> o_a
          then ()
            (* occur check, or [s] is not in the initial
               context [o_a] in which variables can be bound. *)
          else k (S.bind subst (s, o_s) (t, o_t)) (* bind s and continue *)
      | Bind (f, _, t1'), Bind (g, _, t2') when f == g ->
        unif subst t1' o_s t2' o_t k
      | BoundVar i, BoundVar j -> if i = j then k subst
      | Node (f, l1), Node (g, l2) when f == g && is_ac f ->
        (* flatten into a list of terms that do not have [f] as head symbol *)
        let l1 = T.flatten_ac f l1
        and l2 = T.flatten_ac f l2 in 
        (* eliminate terms that are common to l1 and l2 *)
        let l1, l2 = eliminate_common l1 l2 in
        (* permutative matching *)
        unif_ac subst f l1 o_s [] l2 o_t k
      | Node (f, [x1;y1]), Node (g, [x2;y2]) when f == g && is_com f ->
        unif_com subst x1 y1 o_s x2 y2 o_t k
      | Node (f, l1), Node (g, l2) when f == g && List.length l1 = List.length l2 ->
        unif_list subst l1 o_s l2 o_t k  (* regular decomposition *)
      | _, _ -> ()  (* failure, close branch *)
  (* unify pair of lists of terms, with given continuation. *)
  and unif_list subst l1 o_1 l2 o_2 k =
    match l1, l2 with
    | [], [] -> k subst  (* success *)
    | t1::l1', t2::l2' ->
      unif subst t1 o_1 t2 o_2
        (fun subst -> unif_list subst l1' o_1 l2' o_2 k)
    | [], _ | _, [] -> assert false
  (* unify terms under a commutative symbol (try both sides) *)
  and unif_com subst x1 y1 o_1 x2 y2 o_2 k =
    unif subst x1 o_1 x2 o_2 (fun subst -> unif subst y1 o_1 y2 o_2 k);
    unif subst x1 o_1 y2 o_2 (fun subst -> unif subst y1 o_1 x2 o_2 k);
    ()
  (* try all permutations of [left@right] against [l1]. [left,right] is a
     zipper over terms to be matched against [l1]. *)
  and unif_ac subst f l1 o_1 left right o_2 k =
    Utils.debug 4 "@[<h>unif_ac (%a) [%a] [%a]@@[%a]@]" S.pp_substitution subst
      (Utils.pp_list !T.pp_term#pp) l1
      (Utils.pp_list !T.pp_term#pp) left
      (Utils.pp_list !T.pp_term#pp) right;
    match l1, left, right with
    | [], [], [] -> k subst  (* success *)
    | _ when List.length l1 > List.length left + List.length right ->
      ()  (* failure, too many patterns *)
    | x1::l1', left, x2::right' ->
      assert (x1.sort == x2.sort);
      (* try one-to-one of x1 against x2 *)
      unif subst x1 o_1 x2 o_2
        (fun subst ->
          (* continue without x1 and x2 *)
          unif_ac subst f l1' o_1 [] (left @ right') o_2 k);
      (* try x1 against right', keeping x2 on the side *)
      unif_ac subst f l1 o_1 (x2::left) right' o_2 k;
      (* try to bind x1 to [x2+z] where [z] is fresh,
         if len(l1) < len(left+right) *)
      if T.is_var x1 && List.length l1 < List.length left + List.length right then
        let z = fresh_var x2.sort in
        (* offset trick: we need [z] in both contexts o_1 and o_2, so we
           bind it so that (z,o_2) -> (z,o_1), and use (z,o_1) to continue
           the matching *)
        let subst' = S.bind subst (z,o_2) (z,o_1) in
        let x2' = T.mk_node f x2.sort [x2; z] in
        let subst' = S.bind subst' (x1,o_1) (x2',o_2) in
        unif_ac subst' f (z::l1') o_1 left right' o_2 k
    | x1::l1', left, [] -> ()
    | [], _, _ -> ()  (* failure, some terms are not matched *)
  (* eliminate common occurrences of terms in [l1] and [l2] *)
  and eliminate_common l1 l2 = l1, l2 (* TODO *)
  in
  (* sequence of solutions. Substitutions are restricted to the variables
     of [a]. *)
  let seq k = unif subst a o_a b o_b k in
  Sequence.from_iter seq

