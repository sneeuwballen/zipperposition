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
let prof_fast_matching = HExtlib.profile ~enable "fast_matching"

(* add pairs of terms from l1, l2 to the deque. l1 and l2 must have same length *)
let rec add_pairs q l1 l2 =
  match l1, l2 with
  | [], [] -> ()
  | x1::l1', x2::l2' when T.is_var x1 || T.is_var x2 ->
    Deque.push_back q (x1, x2)  (* check variables as late as possible *)
  | x1::l1', x2::l2' ->
    Deque.push_front q (x1, x2)
  | _ -> assert false

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
    (if s.sort <> t.sort then raise UnificationFailure);
    let s = S.apply_subst subst s
    and t = S.apply_subst subst t in
    match s.term, t.term with
    | _, _ when T.eq_term s t -> subst
    | _, _ when T.is_ground_term s && T.is_ground_term t -> raise UnificationFailure
        (* distinct ground terms cannot be unified *)
    | Var _, Var _ -> S.build_subst s t subst
    | Var _, _ when occurs_check subst s t -> raise UnificationFailure
    | Var _, _ -> S.build_subst s t subst
    | _, Var _ when occurs_check subst t s -> raise UnificationFailure
    | _, Var _ -> S.build_subst t s subst
    | Node l1, Node l2 -> (
        try
          List.fold_left2 unif subst l1 l2  (* recursive pairwise unification *)
        with Invalid_argument _ -> raise UnificationFailure
      )
    | _, _ -> raise UnificationFailure
  in
  prof_unification.HExtlib.profile (unif S.id_subst a) b

let matching_locked ~locked subst a b =
  (* recursive matching *)
  let rec unif subst s t =
    (if s.sort <> t.sort then raise UnificationFailure);
    let s = S.apply_subst subst s
    and t = S.apply_subst subst t in
    match s.term, t.term with
    | _, _ when T.eq_term s t -> subst
    | _, _ when T.is_ground_term s && T.is_ground_term t ->
        (* distinct ground terms cannot be matched *) 
        raise UnificationFailure
    | Var _, _ when occurs_check subst s t || List.mem s locked ->
      raise UnificationFailure
    | Var _, _ -> S.build_subst s t subst
    | Node l1, Node l2 -> (
        try
          List.fold_left2 unif subst l1 l2  (* recursive pairwise unification *)
        with Invalid_argument _ -> raise UnificationFailure
      )
    | _, _ -> raise UnificationFailure
  in
  prof_matching.HExtlib.profile (unif subst a) b

let matching a b = matching_locked ~locked:(T.vars_of_term b) S.id_subst a b

let _fast_matching t1 t2 =
  let locked_vars = T.THashSet.from_list t2.vars in
  let stack = T.TBindingStack.create () in
  let q = Deque.create () in
  Deque.push_front q (t1, t2);
  try
    while not (Deque.is_empty q) do
      (* take next pair to match, after dereferencing *)
      let (t1, t2) = Deque.take_front q in
      let t1 = T.get_binding t1
      and t2 = T.get_binding t2 in
      (* match t1 with t2 now *)
      match t1.term, t2.term with
      | _, _ when T.eq_term t1 t2 -> ()  (* trivial success *)
      | Var _, _ when T.THashSet.member locked_vars t1 -> raise Exit
      | Var _, _ -> (* bind t1 to t2 *)
        T.TBindingStack.push stack t1;
        T.set_binding t1 t2
      | _, Var _ -> assert (T.THashSet.member locked_vars t2); raise Exit
      | Node l1, Node l2 when List.length l1 = List.length l2 ->
        add_pairs q l1 l2
      | _ -> raise Exit (* failure *)
    done;
    (* build subst from bindings of variables in t1 *)
    let subst = List.fold_left S.update_binding S.id_subst t1.vars in
    T.TBindingStack.restore_state stack T.TBindingStack.root_state;  (* restore old bindings *)
    subst
  with Exit ->
    T.TBindingStack.restore_state stack T.TBindingStack.root_state;  (* restore old bindings *)
    raise UnificationFailure

let fast_matching t1 t2 =
  prof_fast_matching.HExtlib.profile (_fast_matching t1) t2

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

