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

(** Equational literals *)

open Basic
open Symbols

module T = Terms
module S = FoSubst
module Utils = FoUtils

let eq l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1,ord1), Equation (l2,r2,sign2,ord2) ->
      sign1 = sign2 && l1 == l2 && r1 == r2 && ord1 = ord2

let eq_com l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1,o1), Equation (l2,r2,sign2,o2) ->
      sign1 = sign2 &&
      ((T.eq_term l1 l2 && T.eq_term r1 r2 && o1 = o2) ||
       (T.eq_term l1 r2 && T.eq_term r1 l2 && o1 = (Utils.not_partial o2)))

let compare l1 l2 =
  match l1, l2 with
  | Equation (l1,r1,sign1,o1), Equation (l2,r2,sign2,o2) ->
      let c = Pervasives.compare o1 o2 in
      if c <> 0 then c else
        let c = T.compare_term l1 l2 in
        if c <> 0 then c else
          let c = T.compare_term r1 r2 in
          if c <> 0 then c else
            Pervasives.compare sign1 sign2

let to_multiset lit = match lit with
  | Equation (l, r, true, _) -> [l; r]
  | Equation (l, r, false, _) -> [l; l; r; r]

let compare_partial ~ord l1 l2 =
  (* Utils.multiset_partial ord#compare (lit_to_multiset l1) (lit_to_multiset l2) *)
  match l1, l2 with
  | Equation (s, t, sign_st, _), Equation (u, v, sign_uv, _) ->
    let s_u = ord#compare s u
    and s_v = ord#compare s v
    and t_u = ord#compare t u
    and t_v = ord#compare t v in
    match s_u, s_v, t_u, t_v, sign_st, sign_uv with
    | Eq, _, _, Eq, _, _
    | _, Eq, Eq, _, _, _ ->
      if sign_st = sign_uv then Eq
      else if sign_st then Lt
      else (assert sign_uv; Gt)
    | Gt, Gt, _, _, _, _        (* s dominates *)
    | _, _, Gt, Gt, _, _ -> Gt  (* t dominates *)
    | Gt, Eq, _, _, false, true (* s = v & s > u *)
    | Eq, Gt, _, _, false, true (* s = u & s > v *)
    | _, _, Gt, Eq, false, true (* t = v & t > u *)
    | _, _, Eq, Gt, false, true -> Gt (* t = u & t > v *)
    | Lt, _, Lt, _, _, _        (* u dominates *)
    | _, Lt, _, Lt, _, _ -> Lt  (* v dominates *)
    | Eq, _, Lt, _, true, false (* s = u, t < u *)
    | Lt, _, Eq, _, true, false (* t = u, s < u *)
    | _, Eq, _, Lt, true, false (* s = v, t < v *)
    | _, Lt, _, Eq, true, false -> Lt (* t = v, s < v *)
    | Eq, _, _, Gt, _, _        (* s = u, t > v *)
    | Gt, _, _, Eq, _, _        (* s > u, t = v *)
    | _, Eq, Gt, _, _, _        (* s = v, t > u *)
    | _, Gt, Eq, _, _, _        (* s > v, t = u *)
      when sign_uv = sign_st -> Gt
    | Eq, _, _, Lt, _, _        (* s = u, t < v *)
    | Lt, _, _, Eq, _, _        (* s < u, t = v *)
    | _, Eq, Lt, _, _, _        (* s = v, t < u *)
    | _, Lt, Eq, _, _, _        (* s < v, t = u *)
      when sign_uv = sign_st -> Lt
    | Eq, Eq, _, _, false, true (* s = u, s = v *)
    | _, _, Eq, Eq, false, true -> Gt (* t = u, t = v *)
    | _, Eq, _, Eq, true, false (* s = v, t = v *)
    | Eq, _, Eq, _, true, false -> Lt (* s = u, t = u *)
    | _ -> Incomparable

let hash lit = match lit with
  | Equation (l, r, sign, o) ->
    if sign
      then Hash.hash_int3 (Hash.hash_string o) l.tag r.tag
      else Hash.hash_int3 (Hash.hash_string o) r.tag l.tag

let weight = function
  | Equation (l, r, _ ,_) -> l.tsize + r.tsize

let depth = function
  | Equation (l, r, _ ,_) -> max (T.depth l) (T.depth r)

let is_pos lit = match lit with
  | Equation (_,_,sign,_) -> sign

let is_neg lit = match lit with
  | Equation (_,_,sign,_) -> not sign

let equational = function
  | Equation (l, r, _,_) -> l != T.true_term && r != T.true_term

let orientation_of = function
  | Equation (_, _, _, ord) -> ord

let check_type a b = if a.sort <> b.sort
  then raise (SortError "sides of equations of different sorts") else ()

let mk_eq ~ord a b =
  check_type a b;
  Equation (a, b, true, ord#compare a b)

let mk_neq ~ord a b = 
  check_type a b;
  Equation (a, b, false, ord#compare a b)

let mk_lit ~ord a b sign =
  check_type a b;
  Equation (a, b, sign, ord#compare a b)

let apply_subst ?(recursive=true) ~ord subst (lit,offset) =
  if offset = 0 && S.is_empty subst then lit  (* no shifting *)
  else match lit with
  | Equation (l,r,sign,_) ->
    let new_l = S.apply_subst ~recursive subst (l,offset)
    and new_r = S.apply_subst ~recursive subst (r,offset) in
    mk_lit ~ord new_l new_r sign

let reord ~ord (Equation (l,r,sign,_)) = mk_lit ~ord l r sign

let rec lit_of_fof ~ord ((Equation (l,r,sign,_)) as lit) =
  match l.term, r.term with
  (* deal with trivial literals *)
  | _ when T.eq_term l T.true_term && T.eq_term r T.false_term ->
    mk_lit ~ord T.true_term T.true_term (not sign)
  | _ when T.eq_term r T.true_term && T.eq_term l T.false_term ->
    mk_lit ~ord T.true_term T.true_term (not sign)
  | _ when T.eq_term l r ->
    mk_lit ~ord T.true_term T.true_term sign
  (* deal with false/true *)
  | _ when T.eq_term l T.false_term ->
    assert (r.sort = bool_);
    lit_of_fof ~ord (mk_lit ~ord r T.true_term (not sign))
  | _ when T.eq_term r T.false_term ->
    assert (l.sort = bool_);
    lit_of_fof ~ord (mk_lit ~ord l T.true_term (not sign))
  (* deal with negation *)
  | Node (s, [t]), _ when s = not_symbol && T.eq_term r T.true_term ->
    lit_of_fof ~ord (mk_lit ~ord t T.true_term (not sign))
  | _, Node (s, [t]) when s = not_symbol && T.eq_term l T.true_term ->
    lit_of_fof ~ord (mk_lit ~ord t T.true_term (not sign))
  (* deal with equality symbol *)
  | Node (s, [a; b]), _ when s = eq_symbol && T.eq_term r T.true_term ->
    lit_of_fof ~ord (mk_lit ~ord a b sign)
  | _, Node (s, [a; b]) when s = eq_symbol && T.eq_term l T.true_term ->
    lit_of_fof ~ord (mk_lit ~ord a b sign)
  (* default is just reordering *)
  | _ -> reord ~ord lit

let term_of_lit lit =
  match lit with
  | Equation (left, right, false, _) when T.eq_term right T.true_term ->
    T.mk_not left
  | Equation (left, right, true, _) when T.eq_term right T.true_term ->
    left
  | Equation (left, right, true, _) when T.eq_term left T.true_term ->
    right
  | Equation (left, right, false, _) when T.eq_term left T.true_term ->
    T.mk_not right
  | Equation (left, right, sign, ord) ->
    if sign then T.mk_eq left right else T.mk_not (T.mk_eq left right)

let negate (Equation (l,r,sign,ord)) = Equation (l,r,not sign,ord)

let fmap ~ord f = function
  | Equation (left, right, sign, _) ->
    let new_left = f left
    and new_right = f right in
    Equation (new_left, new_right, sign, ord#compare new_left new_right)

let add_vars set = function
  | Equation (l, r, _, _) ->
    T.add_vars set l;
    T.add_vars set r

let vars lit =
  let set = T.THashSet.create () in
  add_vars set lit;
  T.THashSet.to_list set

let eq_lits lits1 lits2 =
  let rec check i =
    if i = Array.length lits1 then true else
    eq_com lits1.(i) lits2.(i) && check (i+1)
  in
  if Array.length lits1 <> Array.length lits2
    then false
    else check 0

let compare_lits lits1 lits2 = 
  let rec check i =
    if i = Array.length lits1 then 0 else
      let cmp = compare lits1.(i) lits2.(i) in
      if cmp = 0 then check (i+1) else cmp
  in
  if Array.length lits1 <> Array.length lits2
    then Array.length lits1 - Array.length lits2
    else check 0

let hash_lits lits =
  let h = ref 0 in
  Array.iter
    (fun (Equation (l, r, sign, _)) ->
      h := Hash.combine (Hash.combine !h l.tag) r.tag)
    lits;
  !h

let weight_lits lits =
  Array.fold_left (fun w lit -> w + weight lit) 0 lits

let depth_lits lits =
  Array.fold_left (fun d lit -> max d (depth lit)) 0 lits

let vars_lits lits =
  let set = T.THashSet.create () in
  for i = 0 to Array.length lits - 1 do
    add_vars set lits.(i);
  done;
  T.THashSet.to_list set

let ground_lits lits =
  let rec check i = if i = Array.length lits then true
    else match lits.(i) with
    | Equation (l, r, _, _) ->
      T.is_ground_term l && T.is_ground_term r && check (i+1)
  in check 0

let term_of_lits lits =
  match lits with
  | [||] -> T.false_term
  | _ -> Array.fold_left
    (fun t lit -> T.mk_or t (term_of_lit lit))
    (term_of_lit lits.(0)) (Array.sub lits 1 (Array.length lits - 1))

(** Apply the substitution to the array of literals, with offset *)
let apply_subst_lits ?(recursive=true) ~ord subst (lits,offset) =
  Array.map
    (fun lit -> apply_subst ~recursive ~ord subst (lit, offset))
    lits

let apply_subst_list ?(recursive=true) ~ord subst (lits, offset) =
  List.map
    (fun lit -> apply_subst ~recursive ~ord subst (lit, offset))
    lits

(** Convert the lits into a sequence of equations *)
let lits_to_seq lits =
  Sequence.from_iter
    (fun k ->
      for i = 0 to Array.length lits - 1 do
        match lits.(i) with | Equation (l,r,sign,_) -> k (l,r,sign)
      done)

let pp_literal formatter lit =
  match lit with
  | Equation (l, r, sign, _) when T.eq_term r T.true_term ->
    if sign
      then !T.pp_term#pp formatter l
      else Format.fprintf formatter "¬%a" !T.pp_term#pp l
  | Equation (l, r, sign, _) when T.eq_term l T.true_term ->
    if sign
      then !T.pp_term#pp formatter r
      else Format.fprintf formatter "¬%a" !T.pp_term#pp r
  | Equation (l, r, sign, _) when l.sort == bool_ ->
    if sign
      then Format.fprintf formatter "%a <=> %a" !T.pp_term#pp l !T.pp_term#pp r
      else Format.fprintf formatter "%a <~> %a" !T.pp_term#pp l !T.pp_term#pp r
  | Equation (l, r, sign, _) ->
    if sign
      then Format.fprintf formatter "%a = %a" !T.pp_term#pp l !T.pp_term#pp r
      else Format.fprintf formatter "%a != %a" !T.pp_term#pp l !T.pp_term#pp r

let pp_lits formatter lits = 
  Utils.pp_arrayi ~sep:" | "
    (fun formatter i lit -> Format.fprintf formatter "%a" pp_literal lit)
    formatter lits

let bij ~ord =
  let open Bij in
  map
    ~inject:(fun (Equation (l,r,sign,_)) -> l,r,sign)
    ~extract:(fun (l,r,sign) -> mk_lit ~ord l r sign)
    (triple Terms.bij Terms.bij bool_)

let bij_lits ~ord =
  let open Bij in
  map
    ~inject:(fun a -> Array.to_list a)
    ~extract:(fun l -> Array.of_list l)
    (list_ (bij ~ord))
