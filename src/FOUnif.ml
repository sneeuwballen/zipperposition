(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Unification and matching algorithms} *)

module T = FOTerm
module S = Substs.FO
module F = FOFormula

open T

type term = FOTerm.t
type subst = Substs.FO.t
type scope = Substs.scope

let prof_unification = Util.mk_profiler "unification"
let prof_matching = Util.mk_profiler "matching"
let prof_variant = Util.mk_profiler "alpha-equiv"
let prof_ac_matching = Util.mk_profiler "ac-matching"

exception Fail

(** Does [v] appear in [t] if we apply the substitution?
  TODO: maybe cause problem with polymorphism, e.g. X:'a = f(X:'int)? *)
let occurs_check subst v sc_v t sc_t =
  let rec check v sc_v t sc_t =
    if T.is_ground t then false
      else match t.term with
      | Var _ when v == t && sc_v = sc_t -> true
      | Var _ ->  (* if [t] is a var bound by [subst], check in its image *) 
        (try let (t', sc_t') = S.lookup subst t sc_t in
              check v sc_v t' sc_t'
        with Not_found -> false)
      | BoundVar _ -> false
      | Node (_, l) -> check_list v sc_v l sc_t
  and check_list v sc_v l sc_l = match l with
  | [] -> false
  | t::l' -> check v sc_v t sc_l || check_list v sc_v l' sc_l
  in
  check v sc_v t sc_t

(** Unify terms, returns a substitution or raises Fail *)
let unification ?(subst=S.empty) a sc_a b sc_b =
  Util.enter_prof prof_unification;
  (* recursive unification *)
  let rec unif subst s sc_s t sc_t =
    let s, sc_s = S.get_var subst s sc_s
    and t, sc_t = S.get_var subst t sc_t in
    (* unify types first *)
    let subst = TypeUnif.unify_fo ~subst s.T.ty sc_s t.T.ty sc_t in
    match s.term, t.term with
    | _ when s == t && (T.is_ground s || sc_s = sc_t) ->
      subst (* the terms are equal under any substitution *)
    | _ when T.is_ground s && T.is_ground t ->
      raise Fail (* terms are not equal, and ground. failure. *)
    | Var _, Var _ -> S.bind subst s sc_s t sc_t
    | Var _, _ ->
      if occurs_check subst s sc_s t sc_t
        then raise Fail (* occur check *)
        else S.bind subst s sc_s t sc_t (* bind s *)
    | _, Var _ ->
      if occurs_check subst t sc_t s sc_s
        then raise Fail (* occur check *)
        else S.bind subst t sc_t s sc_s (* bind s *)
    | BoundVar i, BoundVar j -> if i = j then subst else raise Fail
    | Node (f, l1), Node (g, l2) when f == g && List.length l1 = List.length l2 ->
      unif_list subst l1 sc_s l2 sc_t
    | _, _ -> raise Fail
  (* unify pair of lists of terms *)
  and unif_list subst l1 sc_1 l2 sc_2 = match l1, l2 with
  | [], [] -> subst
  | [], _ | _, [] -> raise Fail
  | t1::l1', t2::l2' ->
    let subst = unif subst t1 sc_1 t2 sc_2 in
    unif_list subst l1' sc_1 l2' sc_2
  in
  (* try unification, and return solution/exception (with profiler handling) *)
  try
    let subst = unif subst a sc_a b sc_b in
    Util.exit_prof prof_unification;
    subst
  with Fail as e ->
    Util.exit_prof prof_unification;
    raise e

(** [matching a b] returns sigma such that sigma(a) = b, or raises
    Fail. Only variables from the context of [a] can
    be bound in the substitution. *)
let matching ?(subst=S.empty) a sc_a b sc_b =
  Util.enter_prof prof_matching;
  (* recursive matching *)
  let rec unif subst s sc_s t sc_t =
    let s, sc_s = S.get_var subst s sc_s in
    (* match types first *)
    let subst = TypeUnif.match_fo ~subst s.T.ty sc_s t.T.ty sc_t in
    match s.term, t.term with
    | _ when s == t && (T.is_ground s || sc_s = sc_t) ->
      subst (* the terms are equal under any substitution *)
    | _ when T.is_ground s && T.is_ground t ->
      raise Fail (* terms are not equal, and ground. failure. *)
    | Var _, Var _ when sc_s <> sc_t ->
      let ty_s = T.get_type s in
      let ty_t = T.get_type t in
      let subst = TypeUnif.unify_fo ~subst ty_s sc_s ty_t sc_t in
      S.bind subst s sc_s t sc_t
    | Var _, _ ->
      if occurs_check subst s sc_s t sc_t || sc_s <> sc_a
        then raise Fail
          (* occur check, or [s] is not in the initial
             context [sc_a] in which variables can be bound. *)
        else S.bind subst s sc_s t sc_t (* bind s *)
    | BoundVar i, BoundVar j -> if i = j then subst else raise Fail
    | Node (f, l1), Node (g, l2) when f == g && List.length l1 = List.length l2 ->
      unif_list subst l1 sc_s l2 sc_t
    | _, _ -> raise Fail
  (* unify pair of lists of terms *)
  and unif_list subst l1 sc_1 l2 sc_2 = match l1, l2 with
  | [], [] -> subst
  | [], _ | _, [] -> raise Fail
  | t1::l1', t2::l2' ->
    let subst = unif subst t1 sc_1 t2 sc_2 in
    unif_list subst l1' sc_1 l2' sc_2
  in
  (* try matching, and return solution/exception (with profiler handling) *)
  try
    let subst = unif subst a sc_a b sc_b in
    Util.exit_prof prof_matching;
    subst
  with Fail as e ->
    Util.exit_prof prof_matching;
    raise e

let variant ?(subst=S.empty) a sc_a b sc_b =
  Util.enter_prof prof_variant;
  (* recursive variant checking *)
  let rec unif subst s sc_s t sc_t =
    let s, sc_s = S.get_var subst s sc_s in
    let t, sc_t = S.get_var subst t sc_t in
    let subst = TypeUnif.variant_fo ~subst s.T.ty sc_s t.T.ty sc_t in
    match s.term, t.term with
    | _ when s == t && (T.is_ground s || sc_s = sc_t) ->
      subst (* the terms are equal under any substitution *)
    | _ when T.is_ground s && T.is_ground t ->
      raise Fail (* terms are not equal, and ground. failure. *)
    | Var i, Var j when i <> j && sc_s = sc_t -> raise Fail
    | Var _, Var _ -> S.bind subst s sc_s t sc_t (* bind s *)
    | BoundVar i, BoundVar j -> if i = j then subst else raise Fail
    | Node (f, l1), Node (g, l2) when f == g && List.length l1 = List.length l2 ->
      unif_list subst l1 sc_s l2 sc_t
    | _, _ -> raise Fail
  (* unify pair of lists of terms *)
  and unif_list subst l1 sc_1 l2 sc_2 = match l1, l2 with
  | [], [] -> subst
  | [], _ | _, [] -> raise Fail
  | t1::l1', t2::l2' ->
    let subst = unif subst t1 sc_1 t2 sc_2 in
    unif_list subst l1' sc_1 l2' sc_2
  in
  try
    let subst = unif subst a sc_a b sc_b in
    Util.exit_prof prof_variant;
    subst
  with Fail as e ->
    Util.exit_prof prof_variant;
    raise e

let are_variant t1 t2 =
  try
    let _ = variant t1 0 t2 1 in
    true
  with Fail ->
    false

(** [matching_ac a b] returns substitutions such that [subst(a) =_AC b]. It
    is much more costly than [matching]. By default [is_ac] returns true only
    for symbols that have [attr_ac], and [is_com] only for [attr_commut].
    [offset] is used to create new variables. *)
let matching_ac ?(is_ac=fun s -> Symbol.has_attr Symbol.attr_ac s)
                ?(is_com=fun s -> Symbol.has_attr Symbol.attr_commut s)
                ?offset ?(subst=S.empty) a sc_a b sc_b =
  (* function to get fresh variables *)
  let offset = match offset with
    | Some o -> o
    | None -> ref (max (T.max_var (T.vars a) + sc_a + 1)
                       (T.max_var (T.vars b) + sc_b + 1)) in
  (* avoid index collisions *)
  let fresh_var ~ty =
    let v = T.mk_var ~ty !offset in
    incr offset;
    v
  in
  (* recursive matching. [k] is called with solutions *)
  let rec unif subst s sc_s t sc_t k =
    try
      let s, sc_s = S.get_var subst s sc_s in
      (* first match types *)
      let subst = TypeUnif.match_fo ~subst s.T.ty sc_s t.T.ty sc_t in
      match s.term, t.term with
      | Var _, Var _ when s == t && sc_s = sc_t -> k subst (* trivial success *)
      | Var _, _ ->
        if occurs_check subst s sc_s t sc_t || sc_s <> sc_a
          then ()
            (* occur check, or [s] is not in the initial
               context [sc_a] in which variables can be bound. *)
          else k (S.bind subst s sc_s t sc_t) (* bind s and continue *)
      | BoundVar i, BoundVar j -> if i = j then k subst
      | Node (f, l1), Node (g, l2) when f == g && is_ac f ->
        (* flatten into a list of terms that do not have [f] as head symbol *)
        let l1 = T.flatten_ac f l1
        and l2 = T.flatten_ac f l2 in 
        (* eliminate terms that are common to l1 and l2 *)
        let l1, l2 = eliminate_common l1 l2 in
        (* permutative matching *)
        unif_ac ~ty:s.T.ty subst f l1 sc_s [] l2 sc_t k
      | Node (f, [x1;y1]), Node (g, [x2;y2]) when f == g && is_com f ->
        unif_com subst x1 y1 sc_s x2 y2 sc_t k
      | Node (f, l1), Node (g, l2) when f == g && List.length l1 = List.length l2 ->
        unif_list subst l1 sc_s l2 sc_t k  (* regular decomposition *)
      | _, _ -> ()  (* failure, close branch *)
    with TypeUnif.Error _ -> ()
  (* unify pair of lists of terms, with given continuation. *)
  and unif_list subst l1 sc_1 l2 sc_2 k =
    match l1, l2 with
    | [], [] -> k subst  (* success *)
    | t1::l1', t2::l2' ->
      unif subst t1 sc_1 t2 sc_2
        (fun subst -> unif_list subst l1' sc_1 l2' sc_2 k)
    | [], _ | _, [] -> assert false
  (* unify terms under a commutative symbol (try both sides) *)
  and unif_com subst x1 y1 sc_1 x2 y2 sc_2 k =
    unif subst x1 sc_1 x2 sc_2 (fun subst -> unif subst y1 sc_1 y2 sc_2 k);
    unif subst x1 sc_1 y2 sc_2 (fun subst -> unif subst y1 sc_1 x2 sc_2 k);
    ()
  (* try all permutations of [left@right] against [l1]. [left,right] is a
     zipper over terms to be matched against [l1]. *)
  and unif_ac ~ty subst f l1 sc_1 left right sc_2 k =
    match l1, left, right with
    | [], [], [] -> k subst  (* success *)
    | _ when List.length l1 > List.length left + List.length right ->
      ()  (* failure, too many patterns *)
    | x1::l1', left, x2::right' ->
      (* try one-to-one of x1 against x2 *)
      unif subst x1 sc_1 x2 sc_2
        (fun subst ->
          (* continue without x1 and x2 *)
          unif_ac ~ty subst f l1' sc_1 [] (left @ right') sc_2 k);
      (* try x1 against right', keeping x2 on the side *)
      unif_ac ~ty subst f l1 sc_1 (x2::left) right' sc_2 k;
      (* try to bind x1 to [x2+z] where [z] is fresh,
         if len(l1) < len(left+right) *)
      if T.is_var x1 && List.length l1 < List.length left + List.length right then
        let z = fresh_var ~ty:(T.get_type x1) in
        (* offset trick: we need [z] in both contexts sc_1 and sc_2, so we
           bind it so that (z,sc_2) -> (z,sc_1), and use (z,sc_1) to continue
           the matching *)
        let subst' = S.bind subst z sc_2 z sc_1 in
        let x2' = T.mk_node ~ty f [x2; z] in
        let subst' = S.bind subst' x1 sc_1 x2' sc_2 in
        unif_ac ~ty subst' f (z::l1') sc_1 left right' sc_2 k
    | x1::l1', left, [] -> ()
    | [], _, _ -> ()  (* failure, some terms are not matched *)
  (* eliminate common occurrences of terms in [l1] and [l2] *)
  and eliminate_common l1 l2 = l1, l2 (* TODO *)
  in
  (* sequence of solutions. Substsitutions are restricted to the variables
     of [a]. *)
  let seq k =
    Util.enter_prof prof_ac_matching;
    unif subst a sc_a b sc_b k;
    Util.exit_prof prof_ac_matching
  in
  Sequence.from_iter seq

(** {2 Unification on formulas} *)

let form_unify ?(subst=S.empty) f1 sc_1 f2 sc_2 =
  assert false

let form_variant ?(subst=S.empty) f1 sc_1 f2 sc_2 =
  (* CPS, with [k] the continuation that is given the answer
    substitutions *)
  let rec unif subst f1 f2 k = match f1.F.form, f2.F.form with
    | _ when F.eq f1 f2 -> k subst
    | F.Atom p1, F.Atom p2 ->
      begin try
        let subst = variant ~subst p1 sc_1 p2 sc_2 in
        k subst
      with Fail -> ()
      end
    | F.Equal (t11, t12), F.Equal (t21, t22) ->
      begin try
        let subst = variant ~subst t11 sc_1 t21 sc_2 in
        let subst = variant ~subst t12 sc_1 t22 sc_2 in
        k subst
      with Fail -> ()
      end;
      begin try
        let subst = variant ~subst t11 sc_1 t22 sc_2 in
        let subst = variant ~subst t12 sc_1 t21 sc_2 in
        k subst
      with Fail -> ()
      end;
    | F.Not f1', F.Not f2' -> unif subst f1' f2' k
    | F.Imply (f11, f12), F.Imply (f21, f22)
    | F.Equiv(f11, f12), F.Imply (f21, f22) ->
      unif subst f11 f21 (fun subst -> unif subst f21 f22 k)
    | F.And l1, F.And l2
    | F.Or l1, F.Or l2 ->
      if List.length l1 = List.length l2
        then unif_ac subst l1 [] l2 k
        else ()  (* not. *)
    | F.Exists (ty1,f1'), F.Exists (ty2,f2')
    | F.Forall (ty1,f1'), F.Forall (ty2,f2') ->
      let subst = TypeUnif.unify_fo ~subst ty1 sc_1 ty2 sc_2 in  (* FIXME: in other functions *)
      unif subst f1' f2' k
    | F.True, F.True
    | F.False, F.False -> k subst  (* yep :) *)
    | _ -> ()  (* failure :( *)
  (* invariant: [l1] and [left @ right] always have the same length *)
  and unif_ac subst l1 left right k = match l1, left, right with
    | [], [], [] -> k subst  (* success! *)
    | f1::l1', left, f2::right' ->
      (* f1 = f2 ? *)
      unif subst f1 f2
        (fun subst -> unif_ac subst l1' [] (left @ right') k);
      (* f1 against right', keep f2 for later *)
      unif_ac subst l1 (f2::left) right' k;
      ()
    | _::_, left, [] -> ()
    | _ -> assert false
  in
  (* flattening (for and/or) *)
  let f1 = F.flatten f1 in
  let f2 = F.flatten f2 in
  (* bottom continuation *)
  let seq k = unif subst f1 f2 k in
  Sequence.from_iter seq

let form_are_variant f1 f2 =
  try
    let _ = form_variant f1 0 f2 1 in
    true
  with Fail ->
    false
