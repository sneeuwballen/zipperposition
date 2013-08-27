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

module T = Term
module S = Substs

open Term

let prof_unification = Util.mk_profiler "unification"
let prof_matching = Util.mk_profiler "matching"

exception Fail

let types signature t1 t2 =
  if T.is_var t1 || T.is_var t2
    then
      let ctx = TypeInference.Ctx.of_signature signature in
      TypeInference.check_term_term ctx t1 t2
    else true

(** Does [v] appear in [t] if we apply the substitution? *)
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
      | Bind (_, t') -> check v sc_v t' sc_t
      | Node (_, l) -> check_list v sc_v l sc_t
      | At (t1, t2) -> check v sc_v t1 sc_t || check v sc_v t2 sc_t
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
    match s.term, t.term with
    | _ when s == t && (T.is_ground s || sc_s = sc_t) ->
      subst (* the terms are equal under any substitution *)
    | _ when T.is_ground s && T.is_ground t ->
      raise Fail (* terms are not equal, and ground. failure. *)
    | Var _, _ ->
      if occurs_check subst s sc_s t sc_t
        then raise Fail (* occur check *)
        else S.bind subst s sc_s t sc_t (* bind s *)
    | _, Var _ ->
      if occurs_check subst t sc_t s sc_s
        then raise Fail (* occur check *)
        else S.bind subst t sc_t s sc_s (* bind s *)
    | Bind (f, t1'), Bind (g, t2') when f == g -> unif subst t1' sc_s t2' sc_t
    | BoundVar i, BoundVar j -> if i = j then subst else raise Fail
    | Node (f, l1), Node (g, l2) when f == g && List.length l1 = List.length l2 ->
      unif_list subst l1 sc_s l2 sc_t
    | At (s1, s2), At (t1, t2) ->
      let subst = unif subst s1 sc_s t1 sc_t in
      unif subst s2 sc_s t2 sc_t
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
    match s.term, t.term with
    | _ when s == t && (T.is_ground s || sc_s = sc_t) ->
      subst (* the terms are equal under any substitution *)
    | _ when T.is_ground s && T.is_ground t ->
      raise Fail (* terms are not equal, and ground. failure. *)
    | Var _, _ ->
      if occurs_check subst s sc_s t sc_t || sc_s <> sc_a
        then raise Fail
          (* occur check, or [s] is not in the initial
             context [sc_a] in which variables can be bound. *)
        else S.bind subst s sc_s t sc_t (* bind s *)
    | Bind (f, t1'), Bind (g, t2') when f == g -> unif subst t1' sc_s t2' sc_t
    | BoundVar i, BoundVar j -> if i = j then subst else raise Fail
    | Node (f, l1), Node (g, l2) when f == g && List.length l1 = List.length l2 ->
      unif_list subst l1 sc_s l2 sc_t
    | At (s1, s2), At (t1, t2) ->
      let subst = unif subst s1 sc_s t1 sc_t in
      unif subst s2 sc_s t2 sc_t
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
  (* recursive variant checking *)
  let rec unif subst s sc_s t sc_t =
    let s, sc_s = S.get_var subst s sc_s in
    let t, sc_t = S.get_var subst t sc_t in
    match s.term, t.term with
    | _ when s == t && (T.is_ground s || sc_s = sc_t) ->
      subst (* the terms are equal under any substitution *)
    | _ when T.is_ground s && T.is_ground t ->
      raise Fail (* terms are not equal, and ground. failure. *)
    | Var _, Var _ -> S.bind subst s sc_s t sc_t (* bind s *)
    | Bind (f, t1'), Bind (g, t2') when f == g -> unif subst t1' sc_s t2' sc_t
    | BoundVar i, BoundVar j -> if i = j then subst else raise Fail
    | Node (f, l1), Node (g, l2) when f == g && List.length l1 = List.length l2 ->
      unif_list subst l1 sc_s l2 sc_t
    | At (s1, s2), At (t1, t2) ->
      let subst = unif subst s1 sc_s t1 sc_t in
      unif subst s2 sc_s t2 sc_t
    | _, _ -> raise Fail
  (* unify pair of lists of terms *)
  and unif_list subst l1 sc_1 l2 sc_2 = match l1, l2 with
  | [], [] -> subst
  | [], _ | _, [] -> raise Fail
  | t1::l1', t2::l2' ->
    let subst = unif subst t1 sc_1 t2 sc_2 in
    unif_list subst l1' sc_1 l2' sc_2
  in
  unif subst a sc_a b sc_b

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
  let fresh_var () =
    let v = T.mk_var !offset in
    incr offset;
    v
  in
  (* recursive matching. [k] is called with solutions *)
  let rec unif subst s sc_s t sc_t k =
    let s, sc_s = S.get_var subst s sc_s in
      match s.term, t.term with
      | Var _, Var _ when s == t && sc_s = sc_t -> k subst (* trivial success *)
      | Var _, _ ->
        if occurs_check subst s sc_s t sc_t || sc_s <> sc_a
          then ()
            (* occur check, or [s] is not in the initial
               context [sc_a] in which variables can be bound. *)
          else k (S.bind subst s sc_s t sc_t) (* bind s and continue *)
      | Bind (f, t1'), Bind (g, t2') when f == g ->
        unif subst t1' sc_s t2' sc_t k
      | BoundVar i, BoundVar j -> if i = j then k subst
      | Node (f, l1), Node (g, l2) when f == g && is_ac f ->
        (* flatten into a list of terms that do not have [f] as head symbol *)
        let l1 = T.flatten_ac f l1
        and l2 = T.flatten_ac f l2 in 
        (* eliminate terms that are common to l1 and l2 *)
        let l1, l2 = eliminate_common l1 l2 in
        (* permutative matching *)
        unif_ac subst f l1 sc_s [] l2 sc_t k
      | Node (f, [x1;y1]), Node (g, [x2;y2]) when f == g && is_com f ->
        unif_com subst x1 y1 sc_s x2 y2 sc_t k
      | Node (f, l1), Node (g, l2) when f == g && List.length l1 = List.length l2 ->
        unif_list subst l1 sc_s l2 sc_t k  (* regular decomposition *)
      | At (s1, s2), At (t1, t2) ->
        unif subst s1 sc_s t1 sc_t
          (fun subst' -> unif subst' s2 sc_s t2 sc_t k)
      | _, _ -> ()  (* failure, close branch *)
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
  and unif_ac subst f l1 sc_1 left right sc_2 k =
    match l1, left, right with
    | [], [], [] -> k subst  (* success *)
    | _ when List.length l1 > List.length left + List.length right ->
      ()  (* failure, too many patterns *)
    | x1::l1', left, x2::right' ->
      (* try one-to-one of x1 against x2 *)
      unif subst x1 sc_1 x2 sc_2
        (fun subst ->
          (* continue without x1 and x2 *)
          unif_ac subst f l1' sc_1 [] (left @ right') sc_2 k);
      (* try x1 against right', keeping x2 on the side *)
      unif_ac subst f l1 sc_1 (x2::left) right' sc_2 k;
      (* try to bind x1 to [x2+z] where [z] is fresh,
         if len(l1) < len(left+right) *)
      if T.is_var x1 && List.length l1 < List.length left + List.length right then
        let z = fresh_var () in
        (* offset trick: we need [z] in both contexts sc_1 and sc_2, so we
           bind it so that (z,sc_2) -> (z,sc_1), and use (z,sc_1) to continue
           the matching *)
        let subst' = S.bind subst z sc_2 z sc_1 in
        let x2' = T.mk_node f [x2; z] in
        let subst' = S.bind subst' x1 sc_1 x2' sc_2 in
        unif_ac subst' f (z::l1') sc_1 left right' sc_2 k
    | x1::l1', left, [] -> ()
    | [], _, _ -> ()  (* failure, some terms are not matched *)
  (* eliminate common occurrences of terms in [l1] and [l2] *)
  and eliminate_common l1 l2 = l1, l2 (* TODO *)
  in
  (* sequence of solutions. Substsitutions are restricted to the variables
     of [a]. *)
  let seq k = unif subst a sc_a b sc_b k in
  Sequence.from_iter seq

