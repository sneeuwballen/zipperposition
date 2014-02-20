
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

(** {1 Unification and Matching} *)

module T = ScopedTerm

exception Fail
  (** Raised when a unification/matching attempt fails *)

type scope = Substs.scope
type subst = Substs.t

let prof_unification = Util.mk_profiler "unification"
let prof_matching = Util.mk_profiler "matching"
let prof_variant = Util.mk_profiler "alpha-equiv"
let prof_ac_matching = Util.mk_profiler "ac-matching"

(** {2 Signature} *)

module type S = sig
  type term

  val unification : ?subst:subst -> term -> scope -> term -> scope -> subst
    (** Unify terms, returns a subst or
        @raise Fail if the terms are not unifiable *)

  val matching : ?subst:subst -> pattern:term -> scope -> term -> scope -> subst
    (** [matching ~pattern scope_p b scope_b] returns
        [sigma] such that [sigma pattern = b], or
        @raise Fail if the terms do not match.
        Only variables from the scope of [pattern] can  be bound in the subst. *)

  val variant : ?subst:subst -> term -> scope -> term -> scope -> subst
    (** Succeeds iff the first term is a variant of the second, ie
        if they are alpha-equivalent *)

  val are_unifiable : term -> term -> bool

  val matches : pattern:term -> term -> bool

  val are_variant : term -> term -> bool
end

(** {2 Base (scoped terms)} *)

type term = T.t

(* Does [v] appear in [t] if we apply the substitution? *)
let occurs_check subst v sc_v t sc_t =
  let rec check v sc_v t sc_t =
    match T.ty t with
    | _ when T.ground t -> false
    | T.NoType -> false
    | T.HasType ty ->
      (* check type and subterms *)
      check v sc_t ty sc_t ||
      match T.view t with
      | T.Var _
      | T.RigidVar _->
          if T.eq v t && sc_v = sc_t then true
          else
            (* if [t] is a var bound by [subst], check in its image *)
            begin try
              let t', sc_t' = Substs.lookup subst t sc_t in
              check v sc_v t' sc_t'
            with Not_found -> false
            end
      | T.Const _ | T.BVar _ -> false
      | T.Bind (_, varty, t') -> check v sc_v varty sc_t || check v sc_v t' sc_t
      | T.Record (l, rest) ->
          begin match rest with
            | None -> false
            | Some r -> check v sc_v r sc_t
          end ||
          List.exists (fun (_,t') -> check v sc_v t' sc_t) l
      | T.SimpleApp (_, l)
      | T.Multiset l ->
        List.exists (fun t' -> check v sc_v t' sc_t) l
      | T.At (l, r) -> check v sc_v l sc_t || check v sc_v r sc_t
      | T.App (hd, l) ->
        check v sc_v hd sc_t ||
        List.exists (fun t' -> check v sc_v t' sc_t) l  (* TODO: unroll *)
  in
  check v sc_v t sc_t

(* unify lists using the given "unificator" *)
let rec unif_list ~unif subst l1 sc1 l2 sc2 = match l1, l2 with
  | [], [] -> subst
  | _, []
  | [], _ -> raise Fail
  | t1::l1', t2::l2' ->
      let subst = unif subst t1 sc1 t2 sc2 in
      unif_list ~unif subst l1' sc1 l2' sc2

module RecordUnif = struct
  type t = {
    kind : T.Kind.t;
    ty : T.t;
    fields : (string * T.t) list;
    discarded : (string * T.t) list;  (* discarded fields *)
    rest : T.t option
  }

  (* given a record term [t] with fields [l] and rest [rest],
     build a RecordUnif.t structure
     The term must have a type. *)
  let of_record t l rest =
    let r = {
      kind=T.kind t;
      ty= (match T.ty t with T.NoType -> assert false | T.HasType ty -> ty);
      fields = l;
      discarded = [];
      rest;
    } in
    r

  (* convert back to a record *)
  let to_record r =
    T.record ~kind:r.kind ~ty:r.ty (r.fields @ r.discarded) ~rest:r.rest

  (* discard first field *)
  let discard r = match r.fields with
    | [] -> assert false
    | (n,t)::l ->
      { r with fields=l; discarded=(n,t)::r.discarded; }

  (* discard all fields *)
  let discard_all r =
    {r with fields=[]; discarded=(r.fields @ r.discarded); }

  let set_rest r ~rest = { r with rest; }

  (* remove next field *)
  let pop_field r = match r.fields with
    | [] -> assert false
    | (n,t)::l -> { r with fields=l;  }

  let fields r = r.fields
end

module RU = RecordUnif

(* unify/match records together.
   l1, l2: to unify; l1', l2': to unify with rest1, rest2.
   we use the fact that l1 and l2 are sorted w.r.t keys. *)
let rec unif_records ~unif subst r1 sc1 r2 sc2 =
  (* Util.debug 5 "unif_records %a %a" T.pp (RU.to_record r1) T.pp (RU.to_record * r2); *)
  match RU.fields r1, RU.fields r2 with
  | [], _
  | _, [] ->
      let r1 = RU.discard_all r1 in
      let r2 = RU.discard_all r2 in
      __unif_rest ~unif subst r1 sc1 r2 sc2
  | (n1,t1)::_, (n2,t2)::_ ->
      begin match String.compare n1 n2 with
      | 0 ->
          let subst = unif subst t1 sc1 t2 sc2 in
          let r1 = RU.pop_field r1 in
          let r2 = RU.pop_field r2 in
          unif_records ~unif subst r1 sc1 r2 sc2
      | n when n < 0 ->
          (* n1 too small, ditch it into l1' *)
          let r1 = RU.discard r1 in
          unif_records ~unif subst r1 sc1 r2 sc2
      | _ ->
          (* n2 too small, ditch it into l2' *)
          let r2 = RU.discard r2 in
          unif_records ~unif subst r1 sc1 r2 sc2
      end
(* unify the row variables, if any, with the unmatched columns of each term.
    we first unify the record composed of discard fields of r1, with
    the row of r2, and then conversely. *)
and __unif_rest ~unif subst r1 sc1 r2 sc2 =
  assert (r1.RU.fields = []);
  assert (r2.RU.fields = []);
  (* Util.debug 5 "unif_rest %a %a" T.pp (RU.to_record r1) T.pp (RU.to_record r2); *)
  match r1.RU.rest, r1.RU.discarded, r2.RU.rest, r2.RU.discarded with
  | None, [], None, [] -> subst  (* no row, no remaining fields *)
  | None, _, _, _::_
  | _, _::_, None, _ ->
      (* must unify an empty rest against a non-empty set of discarded fields,
       * that is impossible *)
      raise Fail
  | Some rest1, [], _, _ ->
      (* no discarded fields in r1, so we only need to
       * unify rest1 with { l2 | rest2 } *)
      let t2 = RU.to_record r2 in
      unif subst rest1 sc1 t2 sc2
  | _, _, Some rest2, [] ->
      (* symmetric case of the previous one *)
      let t1 = RU.to_record r1 in
      unif subst t1 sc1 rest2 sc2
  | Some rest1, l1, Some rest2, l2 ->
      (* create fresh var R and unify rest1 with {l2 | R}
       * and rest2 with { l1 | R } *)
      let r = T.const ~kind:r1.RU.kind ~ty:r1.RU.ty (Symbol.Base.fresh_var ()) in
      let t1 = RU.to_record (RU.set_rest r1 ~rest:(Some r)) in
      let t2 = RU.to_record (RU.set_rest r1 ~rest:(Some r)) in
      let subst = unif subst rest1 sc1 t2 sc2 in
      let subst = unif subst t1 sc1 rest2 sc2 in
      subst

(* FIXME: how to deal with multiple unifiers? Make substitutions hold
 * several possibilities? Or use CPS, and wrap it into multiple-answer
 * and single-answer functions
 *
 * CPS looks nice. It should return, at top-level, a
 *      (subst, continuation) option
 * so that dropping the continuation if needed is easy and efficient *)

(* unify multisets pairwise. Precondition: l1 and l2 have the same length *)
let rec __unif_multiset ~unif subst l1 r1 sc1 l2 sc2 = assert false

let unif_multiset ~unif subst l1 sc1 l2 sc2 = assert false (* TODO *)

let unification ?(subst=Substs.empty) a sc_a b sc_b =
  Util.enter_prof prof_unification;
  (* recursive unification *)
  let rec unif subst s sc_s t sc_t =
    let s, sc_s = Substs.get_var subst s sc_s
    and t, sc_t = Substs.get_var subst t sc_t in
    (* first, unify types *)
    let subst = match T.ty s, T.ty t with
      | T.NoType, T.NoType -> subst
      | T.NoType, _
      | _, T.NoType -> raise Fail
      | T.HasType ty1, T.HasType ty2 ->
        unif subst ty1 sc_s ty2 sc_t
    in
    match T.view s, T.view t with
    | _ when T.eq s t && (T.ground s || sc_s = sc_t) ->
      subst (* the terms are equal under any substitution *)
    | _ when T.ground s && T.ground t ->
      raise Fail (* terms are not equal, and ground. failure. *)
    | T.RigidVar i, T.RigidVar j when i <> j && sc_s = sc_t -> raise Fail
    | T.RigidVar _, T.RigidVar _ ->
      Substs.bind subst s sc_s t sc_t (* bind s *)
    | T.RigidVar _, _
    | _, T.RigidVar _ -> raise Fail
    | T.Var _, _ ->
      if occurs_check subst s sc_s t sc_t
        then raise Fail (* occur check *)
        else Substs.bind subst s sc_s t sc_t (* bind s *)
    | _, T.Var _ ->
      if occurs_check subst t sc_t s sc_s
        then raise Fail (* occur check *)
        else Substs.bind subst t sc_t s sc_s (* bind s *)
    | T.Bind (s1, varty1, t1'), T.Bind (s2, varty2, t2') when Symbol.eq s1 s2 ->
      let subst = unif subst varty1 sc_s varty2 sc_t in
      unif subst t1' sc_s t2' sc_t
    | T.BVar i, T.BVar j -> if i = j then subst else raise Fail
    | T.Const f, T.Const g when Symbol.eq f g -> subst
    | T.App (hd1, l1), T.App (hd2, l2) when List.length l1 = List.length l2 ->
      let subst = unif subst hd1 sc_s hd2 sc_t in
      unif_list ~unif subst l1 sc_s l2 sc_t
    | T.Record (l1, rest1), T.Record (l2, rest2) ->
        let r1 = RU.of_record s l1 rest1 in
        let r2 = RU.of_record t l2 rest2 in
        unif_records ~unif subst r1 sc_s r2 sc_t
    | T.SimpleApp (s1,l1), T.SimpleApp (s2, l2) when Symbol.eq s1 s2 ->
      unif_list ~unif subst l1 sc_s l2 sc_t
    | T.Multiset l1, T.Multiset l2 when List.length l1 = List.length l2 ->
      unif_multiset ~unif subst l1 sc_s l2 sc_t
    | T.At (l1, r1), T.At (l2, r2) ->
      let subst = unif subst l1 sc_s l2 sc_t in
      unif subst r1 sc_s r2 sc_t
    | _, _ -> raise Fail
  in
  (* try unification, and return solution/exception (with profiler handling) *)
  try
    let subst = unif subst a sc_a b sc_b in
    Util.exit_prof prof_unification;
    subst
  with Fail as e ->
    Util.exit_prof prof_unification;
    raise e

let matching ?(subst=Substs.empty) ~pattern sc_a b sc_b =
  Util.enter_prof prof_matching;
  (* recursive matching *)
  let rec unif subst s sc_s t sc_t =
    let s, sc_s = Substs.get_var subst s sc_s in
    let subst = match T.ty s, T.ty t with
      | T.NoType, T.NoType -> subst
      | T.NoType, _
      | _, T.NoType -> raise Fail
      | T.HasType ty1, T.HasType ty2 ->
        unif subst ty1 sc_s ty2 sc_t
    in
    match T.view s, T.view t with
    | _ when T.eq s t && (T.ground s || sc_s = sc_t) ->
      subst (* the terms are equal under any substitution *)
    | _ when T.ground s && T.ground t ->
      raise Fail (* terms are not equal, and ground. failure. *)
    | T.RigidVar i, T.RigidVar j when i <> j && sc_s = sc_t -> raise Fail
    | T.RigidVar _, T.RigidVar _ ->
      Substs.bind subst s sc_s t sc_t (* bind s *)
    | T.RigidVar _, _
    | _, T.RigidVar _ -> raise Fail
    | T.Var _, _ ->
      if occurs_check subst s sc_s t sc_t || sc_s = sc_t
        then raise Fail
          (* occur check, or [s] is in the same scope
             as [t] and belongs to the variables that need to be preserved *)
        else Substs.bind subst s sc_s t sc_t (* bind s *)
    | T.Bind (s1, varty1, t1'), T.Bind (s2, varty2, t2') when Symbol.eq s1 s2 ->
      let subst = unif subst varty1 sc_s varty2 sc_t in
      unif subst t1' sc_s t2' sc_t
    | T.BVar i, T.BVar j when i = j -> subst
    | T.Const f, T.Const g when Symbol.eq f g -> subst
    | T.App (f1, l1), T.App (f2, l2) when List.length l1 = List.length l2 ->
      let subst = unif subst f1 sc_s f2 sc_t in
      unif_list ~unif subst l1 sc_s l2 sc_t
    | T.Record (l1, rest1), T.Record (l2, rest2) ->
        let r1 = RU.of_record s l1 rest1 in
        let r2 = RU.of_record t l2 rest2 in
        unif_records ~unif subst r1 sc_s r2 sc_t
    | T.SimpleApp (s1,l1), T.SimpleApp (s2, l2) when Symbol.eq s1 s2 ->
      unif_list ~unif subst l1 sc_s l2 sc_t
    | T.Multiset l1, T.Multiset l2 when List.length l1 = List.length l2 ->
      unif_multiset ~unif subst l1 sc_s l2 sc_t
    | T.At (l1, r1), T.At (l2, r2) ->
      let subst = unif subst l1 sc_s l2 sc_t in
      unif subst r1 sc_s r2 sc_t
    | _, _ -> raise Fail
  in
  (* try matching, and return solution/exception (with profiler handling) *)
  try
    let subst = unif subst pattern sc_a b sc_b in
    Util.exit_prof prof_matching;
    subst
  with Fail as e ->
    Util.exit_prof prof_matching;
    raise e

let variant ?(subst=Substs.empty) a sc_a b sc_b =
  Util.enter_prof prof_variant;
  (* recursive variant checking *)
  let rec unif subst s sc_s t sc_t =
    let s, sc_s = Substs.get_var subst s sc_s in
    let t, sc_t = Substs.get_var subst t sc_t in
    let subst = match T.ty s, T.ty t with
      | T.NoType, T.NoType -> subst
      | T.NoType, _
      | _, T.NoType -> raise Fail
      | T.HasType ty1, T.HasType ty2 ->
        unif subst ty1 sc_s ty2 sc_t
    in
    match T.view s, T.view t with
    | _ when s == t && (T.ground s || sc_s = sc_t) ->
      subst (* the terms are equal under any substitution *)
    | _ when T.ground s && T.ground t ->
      raise Fail (* terms are not equal, and ground. failure. *)
    | T.RigidVar i, T.RigidVar j when i <> j && sc_s = sc_t -> raise Fail
    | T.RigidVar _, T.RigidVar _ ->
      Substs.bind subst s sc_s t sc_t (* bind s *)
    | T.RigidVar _, _
    | _, T.RigidVar _ -> raise Fail
    | T.Var i, T.Var j when i <> j && sc_s = sc_t -> raise Fail
    | T.Var _, T.Var _ -> Substs.bind subst s sc_s t sc_t (* bind s *)
    | T.Bind (s1, varty1, t1'), T.Bind (s2, varty2, t2') when Symbol.eq s1 s2 ->
      let subst = unif subst varty1 sc_s varty2 sc_t in
      unif subst t1' sc_s t2' sc_t
    | T.BVar i, T.BVar j when i = j -> subst
    | T.Const f, T.Const g when Symbol.eq f g -> subst
    | T.App (t1, l1), T.App (t2, l2) when List.length l1 = List.length l2 ->
      let subst = unif subst t1 sc_s t2 sc_t in
      unif_list ~unif subst l1 sc_s l2 sc_t
    | T.Record (l1, rest1), T.Record (l2, rest2) ->
        let r1 = RU.of_record s l1 rest1 in
        let r2 = RU.of_record t l2 rest2 in
        unif_records ~unif subst r1 sc_s r2 sc_t
    | T.SimpleApp (s1,l1), T.SimpleApp (s2, l2) when Symbol.eq s1 s2 ->
      unif_list ~unif subst l1 sc_s l2 sc_t
    | T.Multiset l1, T.Multiset l2 when List.length l1 = List.length l2 ->
      unif_multiset ~unif subst l1 sc_s l2 sc_t
    | T.At (l1, r1), T.At (l2, r2) ->
      let subst = unif subst l1 sc_s l2 sc_t in
      unif subst r1 sc_s r2 sc_t
    | _, _ -> raise Fail
  in
  try
    let subst =
      if sc_a = sc_b
      then
        if T.eq a b then subst else raise Fail
      else unif subst a sc_a b sc_b
    in
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

let matches ~pattern t =
  try
    let _ = matching ~pattern 0 t 1 in
    true
  with Fail ->
    false

let are_unifiable t1 t2 =
  try
    let _ = unification t1 0 t2 1 in
    true
  with Fail ->
    false

(** {2 Specializations} *)

module Ty = struct
  type term = Type.t

  let unification =
    (unification :> ?subst:subst -> term -> scope -> term -> scope -> subst)

  let matching =
    (matching :> ?subst:subst -> pattern:term -> scope -> term -> scope -> subst)

  let variant =
    (variant :> ?subst:subst -> term -> scope -> term -> scope -> subst)

  let are_unifiable =
    (are_unifiable :> term -> term -> bool)

  let matches =
    (matches :> pattern:term -> term -> bool)

  let are_variant =
    (are_variant :> term -> term -> bool)
end

module FO = struct
  type term = FOTerm.t

  let unification =
    (unification :> ?subst:subst -> term -> scope -> term -> scope -> subst)

  let matching =
    (matching :> ?subst:subst -> pattern:term -> scope -> term -> scope -> subst)

  let variant =
    (variant :> ?subst:subst -> term -> scope -> term -> scope -> subst)

  let are_unifiable =
    (are_unifiable :> term -> term -> bool)

  let matches =
    (matches :> pattern:term -> term -> bool)

  let are_variant =
    (are_variant :> term -> term -> bool)
end

module HO = struct
  type term = HOTerm.t

  let unification =
    (unification :> ?subst:subst -> term -> scope -> term -> scope -> subst)

  let matching =
    (matching :> ?subst:subst -> pattern:term -> scope -> term -> scope -> subst)

  let variant =
    (variant :> ?subst:subst -> term -> scope -> term -> scope -> subst)

  let are_unifiable =
    (are_unifiable :> term -> term -> bool)

  let matches =
    (matches :> pattern:term -> term -> bool)

  let are_variant =
    (are_variant :> term -> term -> bool)
end

module Form = struct
  module F = Formula.FO

  let variant ?(subst=Substs.empty) f1 sc_1 f2 sc_2 =
    (* CPS, with [k] the continuation that is given the answer
      substitutions *)
    let rec unif subst f1 f2 k = match F.view f1, F.view f2 with
      | _ when F.eq f1 f2 -> k subst
      | F.Atom p1, F.Atom p2 ->
        begin try
          let subst = FO.variant ~subst p1 sc_1 p2 sc_2 in
          k subst
        with Fail -> ()
        end
      | F.Eq (t11, t12), F.Eq (t21, t22)
      | F.Neq (t11, t12), F.Neq (t21, t22) ->
        begin try
          let subst = FO.variant ~subst t11 sc_1 t21 sc_2 in
          let subst = FO.variant ~subst t12 sc_1 t22 sc_2 in
          k subst
        with Fail -> ()
        end;
        begin try
          let subst = FO.variant ~subst t11 sc_1 t22 sc_2 in
          let subst = FO.variant ~subst t12 sc_1 t21 sc_2 in
          k subst
        with Fail -> ()
        end;
      | F.Not f1', F.Not f2' -> unif subst f1' f2' k
      | F.Imply (f11, f12), F.Imply (f21, f22)
      | F.Xor (f11, f12), F.Xor (f21, f22)
      | F.Equiv(f11, f12), F.Imply (f21, f22) ->
        unif subst f11 f21 (fun subst -> unif subst f21 f22 k)
      | F.And l1, F.And l2
      | F.Or l1, F.Or l2 ->
        if List.length l1 = List.length l2
          then unif_ac subst l1 [] l2 k
          else ()  (* not. *)
      | F.Exists (ty1,f1'), F.Exists (ty2,f2')
      | F.Forall (ty1,f1'), F.Forall (ty2,f2') ->
        begin try
          let subst = Ty.variant ~subst ty1 sc_1 ty2 sc_2 in
          unif subst f1' f2' k
        with Fail -> ()
        end
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

  let are_variant f1 f2 =
    let seq = variant f1 0 f2 1 in
    not (Sequence.is_empty seq)
end

