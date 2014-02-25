
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

(** {2 Result of multiple Unification} *)
module Res = struct
  type t =
    | End
    | Ok of subst * (unit -> t)
    (** Result of unification provides a continuation to get other
     * substitutions, in case the unification is n-ary. *)

  let rec to_list = function
    | End -> []
    | Ok (subst, f) -> subst :: to_list (f ())

  let rec to_seq res k = match res with
    | End -> ()
    | Ok (s, f) -> k s; to_seq (f ()) k

  let rec fold f acc res = match res with
    | End -> acc
    | Ok (s, cont) -> fold f (f acc s) (cont ())

  (* takes a function that requires a success cont. and a failure cont.
   * and returns the lazy result *)
  let of_fun f =
    f ~k:(fun subst fk -> Ok (subst,fk)) ~fk:(fun () -> End)
end

(** {2 Signatures} *)

module type UNARY = sig
  type term

  val unification : ?subst:subst -> term -> scope -> term -> scope -> subst
    (** Unify terms, returns a subst or
        @raise Fail if the terms are not unifiable *)

  val matching : ?subst:subst -> pattern:term -> scope -> term -> scope -> subst
    (** [matching ~pattern scope_p b scope_b] returns
        [sigma] such that [sigma pattern = b], or fails.
        Only variables from the scope of [pattern] can  be bound in the subst.
        @raise Fail if the terms do not match.
        @raise Invalid_argument if the two scopes are equal *)

  val matching_same_scope : ?subst:subst -> scope:scope -> pattern:term -> term -> subst
    (** matches [pattern] (more general) with the other term.
     * The two terms live in the same scope, which is passed as the
     * [scope] argument. *)

  val variant : ?subst:subst -> term -> scope -> term -> scope -> subst
    (** Succeeds iff the first term is a variant of the second, ie
        if they are alpha-equivalent *)

  val are_unifiable : term -> term -> bool

  val matches : pattern:term -> term -> bool

  val are_variant : term -> term -> bool
end

module type NARY = sig
  type term
  type result = Res.t

  val unification : ?subst:subst -> term -> scope -> term -> scope -> result
    (** unification of two terms *)

  val matching : ?subst:subst -> pattern:term -> scope -> term -> scope -> result
    (** matching of two terms.
     * @raise Invalid_argument if the two scopes are equal. *)

  val variant : ?subst:subst -> term -> scope -> term -> scope -> result
    (** alpha-equivalence checking of two terms *)

  val are_unifiable : term -> term -> bool

  val matches : pattern:term -> term -> bool

  val are_variant : term -> term -> bool
end

(** {2 Base (scoped terms)} *)

type term = T.t

(* Does [v] appear in [t] if we apply the substitution? *)
let occurs_check subst v sc_v t sc_t =
  let rec check t sc_t =
    if T.ground t then false
    else match T.ty t with
    | T.NoType -> false
    | T.HasType ty ->
      (* check type and subterms *)
      check ty sc_t ||
      match T.view t with
      | T.Var _
      | T.RigidVar _->
          if T.eq v t && sc_v = sc_t then true
          else
            (* if [t] is a var bound by [subst], check in its image *)
            begin try
              let t', sc_t' = Substs.lookup subst t sc_t in
              check t' sc_t'
            with Not_found -> false
            end
      | T.Const _ | T.BVar _ -> false
      | T.Bind (_, varty, t') -> check varty sc_t || check t' sc_t
      | T.Record (l, rest) ->
          begin match rest with
            | None -> false
            | Some r -> check r sc_t
          end ||
          List.exists (fun (_,t') -> check t' sc_t) l
      | T.SimpleApp (_, l)
      | T.Multiset l ->
        List.exists (fun t' -> check t' sc_t) l
      | T.At (l, r) -> check l sc_t || check r sc_t
      | T.App (hd, l) ->
        check hd sc_t ||
        List.exists (fun t' -> check t' sc_t) l  (* TODO: unroll *)
  in
  check t sc_t

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

(** {2 Nary-unification} *)

module Nary = struct
  type term = T.t
  type result = Res.t
  type fcont = unit -> result
  type cont = Substs.t -> fcont -> result
  type unif = Substs.t -> T.t -> scope -> T.t -> scope -> k:cont -> fk:fcont -> result

  (* unify lists pairwise *)
  let rec __unify_list ~(unif:unif) subst l1 sc1 l2 sc2 ~(k:cont) ~(fk:fcont) : result =
    match l1, l2 with
    | [], [] -> k subst fk
    | [], _
    | _, [] -> fk ()
    | t1::l1', t2::l2' ->
        unif subst t1 sc1 t2 sc2
          ~k:(fun subst fk -> __unify_list ~unif subst l1' sc1 l2' sc2 ~k ~fk)
          ~fk

  (* unify the row variables, if any, with the unmatched columns of each term.
      we first unify the record composed of discard fields of r1, with
      the row of r2, and then conversely.*)
  let __unify_record_rest ~unif subst r1 sc1 r2 sc2 ~k ~fk =
    assert (r1.RU.fields = []);
    assert (r2.RU.fields = []);
    (* Util.debug 5 "unif_rest %a %a" T.pp (RU.to_record r1) T.pp (RU.to_record r2); *)
    match r1.RU.rest, r1.RU.discarded, r2.RU.rest, r2.RU.discarded with
    | None, [], None, [] ->
        k subst fk  (* no row, no remaining fields *)
    | None, _, _, _::_
    | _, _::_, None, _ ->
        (* must unify an empty rest against a non-empty set of discarded
         * fields, that is impossible *)
        fk ()
    | Some rest1, [], _, _ ->
        (* no discarded fields in r1, so we only need to
         * unify rest1 with { l2 | rest2 } *)
        let t2 = RU.to_record r2 in
        unif subst rest1 sc1 t2 sc2 ~k ~fk
    | _, _, Some rest2, [] ->
        (* symmetric case of the previous one *)
        let t1 = RU.to_record r1 in
        unif subst t1 sc1 rest2 sc2 ~k ~fk
    | Some rest1, l1, Some rest2, l2 ->
        (* create fresh var R and unify rest1 with {l2 | R} (t2)
         * and rest2 with { l1 | R } (t1) *)
        let r = T.const ~kind:r1.RU.kind ~ty:r1.RU.ty (Symbol.Base.fresh_var ()) in
        let t1 = RU.to_record (RU.set_rest r1 ~rest:(Some r)) in
        let t2 = RU.to_record (RU.set_rest r1 ~rest:(Some r)) in
        unif subst rest1 sc1 t2 sc2
          ~k:(fun subst fk -> unif subst t1 sc1 rest2 sc2 ~k ~fk)
          ~fk

  (* unify the two records *)
  let rec __unify_records ~unif subst r1 sc1 r2 sc2 ~k ~fk =
    match RU.fields r1, RU.fields r2 with
    | [], _
    | _, [] ->
        let r1 = RU.discard_all r1 in
        let r2 = RU.discard_all r2 in
        __unify_record_rest ~unif subst r1 sc1 r2 sc2 ~k ~fk
    | (n1,t1)::_, (n2,t2)::_ ->
        begin match String.compare n1 n2 with
        | 0 ->
          unif subst t1 sc1 t2 sc2
            ~k:(fun subst fk ->
              let r1' = RU.pop_field r1 in
              let r2' = RU.pop_field r2 in
              __unify_records ~unif subst r1' sc1 r2' sc2 ~k ~fk)
            ~fk
        | n when n < 0 ->
            (* n1 too small, ditch it into l1' *)
            let r1' = RU.discard r1 in
            __unify_records ~unif subst r1' sc1 r2 sc2 ~k ~fk
        | _ ->
            (* n2 too small, ditch it into l2' *)
            let r2' = RU.discard r2 in
            __unify_records ~unif subst r1 sc1 r2' sc2 ~k ~fk
        end

  (* unify multisets pairwise. Precondition: l1 and l2 have the same length. *)
  let rec __unify_multiset ~unif subst l1 sc1 l2 sc2 ~k ~fk =
    match l2 with
    | [] -> k subst fk  (* success! *)
    | t2::l2' -> 
        (* need to unify some element of [l1] with [t2] first *)
        __choose_pair ~unif subst [] l1 sc1 t2 l2' sc2 ~k ~fk
  (* choose an element of [l1] to unify with the head of [l2] *)
  and __choose_pair ~unif subst left right sc1 t2 l2' sc2 ~k ~fk =
    match right with
    | [] -> fk ()   (* exhausted all possibilities *)
    | t1::right' ->
        unif subst t1 sc1 t2 sc2
          ~k:(fun subst fk ->
            (* upon success, unify remaining pairs *)
            __unify_multiset ~unif subst (left@right') sc1 l2' sc2 ~k ~fk)
          ~fk:(fun () ->
            (* upon failure, try to unify [t2] with another term of [right] *)
            __choose_pair ~unif subst (t1::left) right' sc1 t2 l2' sc2 ~k ~fk)

  let unification ?(subst=Substs.empty) a sc_a b sc_b =
    let rec unif subst s sc_s t sc_t ~k ~fk =
      let s, sc_s = Substs.get_var subst s sc_s
      and t, sc_t = Substs.get_var subst t sc_t in
      (* unify types. On success, unify terms. *)
      match T.ty s, T.ty t with
        | T.NoType, T.NoType ->
          unif_terms subst s sc_s t sc_t ~k ~fk
        | T.NoType, _
        | _, T.NoType -> fk ()
        | T.HasType ty1, T.HasType ty2 ->
          unif subst ty1 sc_s ty2 sc_t
            ~k:(fun subst fk -> unif_terms subst s sc_s t sc_t ~k ~fk)
            ~fk
    and unif_terms subst s sc_s t sc_t ~(k:cont) ~(fk:fcont) : result =
      match T.view s, T.view t with
      | _ when T.eq s t && (T.ground s || sc_s = sc_t) ->
        k subst fk (* the terms are equal under any substitution *)
      | _ when T.ground s && T.ground t ->
        fk () (* terms are not equal, and ground. failure. *)
      | T.RigidVar i, T.RigidVar j when sc_s = sc_t && i = j -> k subst fk
      | T.RigidVar i, T.RigidVar j when sc_s <> sc_t ->
          k (Substs.bind subst s sc_s t sc_t) fk
      | T.RigidVar _, _
      | _, T.RigidVar _ -> fk ()  (* fail *)
      | T.Var _, _ ->
        if occurs_check subst s sc_s t sc_t || not (T.DB.closed t)
          then fk () (* occur check *)
          else k (Substs.bind subst s sc_s t sc_t) fk (* bind s *)
      | _, T.Var _ ->
        if occurs_check subst t sc_t s sc_s || not (T.DB.closed s)
          then fk () (* occur check *)
          else k (Substs.bind subst t sc_t s sc_s) fk (* bind s *)
      | T.Bind (s1, varty1, t1'), T.Bind (s2, varty2, t2') when Symbol.eq s1 s2 ->
        unif subst varty1 sc_s varty2 sc_t
          ~k:(fun subst fk -> unif subst t1' sc_s t2' sc_t ~k ~fk)
          ~fk
      | T.BVar i, T.BVar j when i = j -> k subst fk
      | T.Const f, T.Const g when Symbol.eq f g -> k subst fk
      | T.App (hd1, l1), T.App (hd2, l2) when List.length l1 = List.length l2 ->
        unif subst hd1 sc_s hd2 sc_t
          ~k:(fun subst fk -> __unify_list ~unif subst l1 sc_s l2 sc_t ~k ~fk)
          ~fk
      | T.Record (l1, rest1), T.Record (l2, rest2) ->
        let r1 = RU.of_record s l1 rest1 in
        let r2 = RU.of_record t l2 rest2 in
        __unify_records ~unif subst r1 sc_s r2 sc_t ~k ~fk
      | T.SimpleApp (s1,l1), T.SimpleApp (s2, l2) when Symbol.eq s1 s2 ->
        __unify_list ~unif subst l1 sc_s l2 sc_t ~k ~fk
      | T.Multiset l1, T.Multiset l2 when List.length l1 = List.length l2 ->
        __unify_multiset ~unif subst l1 sc_s l2 sc_t ~k ~fk
      | T.At (l1, r1), T.At (l2, r2) ->
        unif subst l1 sc_s l2 sc_t
          ~k:(fun subst fk -> unif subst r1 sc_s r2 sc_t ~k ~fk)
          ~fk
      | _, _ -> fk ()  (* fail *)
    in
    Res.of_fun (unif subst a sc_a b sc_b)

  let matching ?(subst=Substs.empty) ~pattern sc_pattern b sc_b =
    let rec unif subst s sc_s t sc_t ~k ~fk =
      let s, sc_s = Substs.get_var subst s sc_s
      and t, sc_t = Substs.get_var subst t sc_t in
      (* unify types. On success, unify terms. *)
      match T.ty s, T.ty t with
        | T.NoType, T.NoType ->
          unif_terms subst s sc_s t sc_t ~k ~fk
        | T.NoType, _
        | _, T.NoType -> fk ()
        | T.HasType ty1, T.HasType ty2 ->
          unif subst ty1 sc_s ty2 sc_t
            ~k:(fun subst fk -> unif_terms subst s sc_s t sc_t ~k ~fk)
            ~fk
    and unif_terms subst s sc_s t sc_t ~(k:cont) ~(fk:fcont) : Res.t =
      match T.view s, T.view t with
      | _ when T.eq s t && (T.ground s || sc_s = sc_t) ->
        k subst fk (* the terms are equal under any substitution *)
      | _ when T.ground s && T.ground t ->
        fk () (* terms are not equal, and ground. failure. *)
      | T.RigidVar i, T.RigidVar j when sc_s = sc_t && i = j -> k subst fk
      | T.RigidVar i, T.RigidVar j when sc_s <> sc_t ->
          k (Substs.bind subst s sc_s t sc_t) fk
      | T.RigidVar _, _
      | _, T.RigidVar _ -> fk ()  (* fail *)
      | T.Var _, _ ->
        if occurs_check subst s sc_s t sc_t || sc_s = sc_t
          then fk ()
          else k (Substs.bind subst s sc_s t sc_t) fk (* bind s *)
      | T.Bind (s1, varty1, t1'), T.Bind (s2, varty2, t2') when Symbol.eq s1 s2 ->
        unif subst varty1 sc_s varty2 sc_t
          ~k:(fun subst fk -> unif subst t1' sc_s t2' sc_t ~k ~fk)
          ~fk
      | T.BVar i, T.BVar j when i = j -> k subst fk
      | T.Const f, T.Const g when Symbol.eq f g -> k subst fk
      | T.App (hd1, l1), T.App (hd2, l2) when List.length l1 = List.length l2 ->
        unif subst hd1 sc_s hd2 sc_t
          ~k:(fun subst fk -> __unify_list ~unif subst l1 sc_s l2 sc_t ~k ~fk)
          ~fk
      | T.Record (l1, rest1), T.Record (l2, rest2) ->
        let r1 = RU.of_record s l1 rest1 in
        let r2 = RU.of_record t l2 rest2 in
        __unify_records ~unif subst r1 sc_s r2 sc_t ~k ~fk
      | T.SimpleApp (s1,l1), T.SimpleApp (s2, l2) when Symbol.eq s1 s2 ->
        __unify_list ~unif subst l1 sc_s l2 sc_t ~k ~fk
      | T.Multiset l1, T.Multiset l2 when List.length l1 = List.length l2 ->
        __unify_multiset ~unif subst l1 sc_s l2 sc_t ~k ~fk
      | T.At (l1, r1), T.At (l2, r2) ->
        unif subst l1 sc_s l2 sc_t
          ~k:(fun subst fk -> unif subst r1 sc_s r2 sc_t ~k ~fk)
          ~fk
      | _, _ -> fk ()  (* fail *)
    in
    Res.of_fun (unif subst pattern sc_pattern b sc_b)

  let variant ?(subst=Substs.empty) a sc_a b sc_b =
    let rec unif subst s sc_s t sc_t ~k ~fk =
      let s, sc_s = Substs.get_var subst s sc_s
      and t, sc_t = Substs.get_var subst t sc_t in
      (* unify types. On success, unify terms. *)
      match T.ty s, T.ty t with
        | T.NoType, T.NoType ->
          unif_terms subst s sc_s t sc_t ~k ~fk
        | T.NoType, _
        | _, T.NoType -> fk ()
        | T.HasType ty1, T.HasType ty2 ->
          unif subst ty1 sc_s ty2 sc_t
            ~k:(fun subst fk -> unif_terms subst s sc_s t sc_t ~k ~fk)
            ~fk
    and unif_terms subst s sc_s t sc_t ~(k:cont) ~(fk:fcont) : Res.t =
      match T.view s, T.view t with
      | _ when T.eq s t && (T.ground s || sc_s = sc_t) ->
        k subst fk (* the terms are equal under any substitution *)
      | _ when T.ground s && T.ground t ->
        fk () (* terms are not equal, and ground. failure. *)
      | T.RigidVar i, T.RigidVar j when sc_s = sc_t && i = j -> k subst fk
      | T.RigidVar i, T.RigidVar j when sc_s <> sc_t ->
          k (Substs.bind subst s sc_s t sc_t) fk
      | T.RigidVar _, _
      | _, T.RigidVar _ -> fk ()  (* fail *)
      | T.Var i, T.Var j when i <> j && sc_s = sc_t -> fk ()
      | T.Var _, T.Var _ when sc_s <> sc_t ->
          k (Substs.bind subst s sc_s t sc_t) fk (* bind s *)
      | T.Bind (s1, varty1, t1'), T.Bind (s2, varty2, t2') when Symbol.eq s1 s2 ->
        unif subst varty1 sc_s varty2 sc_t
          ~k:(fun subst fk -> unif subst t1' sc_s t2' sc_t ~k ~fk)
          ~fk
      | T.BVar i, T.BVar j when i = j -> k subst fk
      | T.Const f, T.Const g when Symbol.eq f g -> k subst fk
      | T.App (hd1, l1), T.App (hd2, l2) when List.length l1 = List.length l2 ->
        unif subst hd1 sc_s hd2 sc_t
          ~k:(fun subst fk -> __unify_list ~unif subst l1 sc_s l2 sc_t ~k ~fk)
          ~fk
      | T.Record (l1, rest1), T.Record (l2, rest2) ->
        let r1 = RU.of_record s l1 rest1 in
        let r2 = RU.of_record t l2 rest2 in
        __unify_records ~unif subst r1 sc_s r2 sc_t ~k ~fk
      | T.SimpleApp (s1,l1), T.SimpleApp (s2, l2) when Symbol.eq s1 s2 ->
        __unify_list ~unif subst l1 sc_s l2 sc_t ~k ~fk
      | T.Multiset l1, T.Multiset l2 when List.length l1 = List.length l2 ->
        __unify_multiset ~unif subst l1 sc_s l2 sc_t ~k ~fk
      | T.At (l1, r1), T.At (l2, r2) ->
        unif subst l1 sc_s l2 sc_t
          ~k:(fun subst fk -> unif subst r1 sc_s r2 sc_t ~k ~fk)
          ~fk
      | _, _ -> fk ()  (* fail *)
    in
    Res.of_fun (unif subst a sc_a b sc_b)

  let are_variant t1 t2 =
    match variant t1 0 t2 1 with
    | Res.End -> false
    | Res.Ok _ -> true

  let matches ~pattern t =
    match matching ~pattern 0 t 1 with
    | Res.End -> false
    | Res.Ok _ -> true

  let are_unifiable t1 t2 =
    match unification t1 0 t2 1 with
    | Res.End -> false
    | Res.Ok _ -> true
end

(** {2 Unary Unification} *)

module Unary = struct
  type term = T.t

  (* unify lists using the given "unificator" and continuation [k] *)
  let rec unif_list ~unif subst l1 sc1 l2 sc2 = match l1, l2 with
    | [], [] -> subst
    | _, []
    | [], _ -> raise Fail
    | t1::l1', t2::l2' ->
        let subst = unif subst t1 sc1 t2 sc2 in
        unif_list ~unif subst l1' sc1 l2' sc2

  (* unify/match records together.
     l1, l2: to unify; l1', l2': to unify with rest1, rest2.
     we use the fact that l1 and l2 are sorted w.r.t keys. *)
  let rec unif_records ~unif subst r1 sc1 r2 sc2 =
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
            (* n2 too small, ditch it into l1' *)
            let r2 = RU.discard r2 in
            unif_records ~unif subst r1 sc1 r2 sc2
        end
  (* unify the row variables, if any, with the unmatched columns of each term.
      we first unify the record composed of discard fields of r1, with
      the row of r2, and then conversely. *)
  and __unif_rest ~unif subst r1 sc1 r2 sc2 =
    assert (r1.RU.fields = []);
    assert (r2.RU.fields = []);
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

  let unification ?(subst=Substs.empty) a sc_a b sc_b =
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
      | T.At (l1, r1), T.At (l2, r2) ->
        let subst = unif subst l1 sc_s l2 sc_t in
        unif subst r1 sc_s r2 sc_t
      | _, _ -> raise Fail
    in
    (* try unification, and return solution/exception (with profiler handling) *)
    let subst = unif subst a sc_a b sc_b in
    subst

  let matching ?(subst=Substs.empty) ~pattern sc_a b sc_b =
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
      | T.Var _, _ ->
        if occurs_check subst s sc_s t sc_t || sc_s = sc_t
          then raise Fail
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
      | T.At (l1, r1), T.At (l2, r2) ->
        let subst = unif subst l1 sc_s l2 sc_t in
        unif subst r1 sc_s r2 sc_t
      | _, _ -> raise Fail
    in
    let subst = unif subst pattern sc_a b sc_b in
    subst

  let matching_same_scope ?(subst=Substs.empty) ~scope ~pattern b =
    (* recursive matching. Blocked vars are [blocked] *)
    let rec unif ~blocked subst s scope t scope  =
      let s, sc_s = Substs.get_var subst s scope in
      let subst = match T.ty s, T.ty t with
        | T.NoType, T.NoType -> subst
        | T.NoType, _
        | _, T.NoType -> raise Fail
        | T.HasType ty1, T.HasType ty2 ->
          unif ~blocked subst ty1 scope ty2 scope
      in
      match T.view s, T.view t with
      | _ when T.eq s t ->
        subst (* the terms are equal under any substitution *)
      | _ when T.ground s && T.ground t ->
        raise Fail (* terms are not equal, and ground. failure. *)
      | T.Var _, _ ->
        if occurs_check subst s scope t scope || T.Set.mem s blocked
          then raise Fail
          else Substs.bind subst s scope t scope (* bind s *)
      | T.Bind (s1, varty1, t1'), T.Bind (s2, varty2, t2') when Symbol.eq s1 s2 ->
        let subst = unif ~blocked subst varty1 scope varty2 scope in
        unif ~blocked subst t1' scope t2' scope
      | T.BVar i, T.BVar j when i = j -> subst
      | T.Const f, T.Const g when Symbol.eq f g -> subst
      | T.App (f1, l1), T.App (f2, l2) when List.length l1 = List.length l2 ->
        let subst = unif ~blocked subst f1 scope f2 scope in
        unif_list ~unif:(unif ~blocked) subst l1 scope l2 scope
      | T.Record (l1, rest1), T.Record (l2, rest2) ->
          let r1 = RU.of_record s l1 rest1 in
          let r2 = RU.of_record t l2 rest2 in
          unif_records ~unif:(unif ~blocked) subst r1 scope r2 scope
      | T.SimpleApp (s1,l1), T.SimpleApp (s2, l2) when Symbol.eq s1 s2 ->
        unif_list ~unif:(unif ~blocked) subst l1 scope l2 scope
      | T.At (l1, r1), T.At (l2, r2) ->
        let subst = unif ~blocked subst l1 scope l2 scope in
        unif ~blocked subst r1 scope r2 scope
      | _, _ -> raise Fail
    in
    (* compute set of free vars of [b], that cannot be bound *)
    let blocked = T.Seq.add_set T.Set.empty (T.Seq.vars b) in
    let subst = unif ~blocked subst pattern scope b scope in
    subst

  let variant ?(subst=Substs.empty) a sc_a b sc_b =
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
      | T.At (l1, r1), T.At (l2, r2) ->
        let subst = unif subst l1 sc_s l2 sc_t in
        unif subst r1 sc_s r2 sc_t
      | _, _ -> raise Fail
    in
    if sc_a = sc_b
      then
        if T.eq a b then subst else raise Fail
      else unif subst a sc_a b sc_b

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
end

(** {2 Specializations} *)

module Ty = struct
  open Unary
  type term = Type.t

  let unification =
    (unification :> ?subst:subst -> term -> scope -> term -> scope -> subst)

  let matching =
    (matching :> ?subst:subst -> pattern:term -> scope -> term -> scope -> subst)

  let matching_same_scope =
    (matching_same_scope :> ?subst:subst -> scope:scope -> pattern:term -> term -> subst)

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
  open Unary
  type term = FOTerm.t

  let unification =
    (unification :> ?subst:subst -> term -> scope -> term -> scope -> subst)

  let matching =
    (matching :> ?subst:subst -> pattern:term -> scope -> term -> scope -> subst)

  let matching_same_scope =
    (matching_same_scope :> ?subst:subst -> scope:scope -> pattern:term -> term -> subst)

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
  open Nary
  type term = HOTerm.t
  type result = Res.t

  let unification =
    (unification :> ?subst:subst -> term -> scope -> term -> scope -> result)

  let matching =
    (matching :> ?subst:subst -> pattern:term -> scope -> term -> scope -> result)

  let variant =
    (variant :> ?subst:subst -> term -> scope -> term -> scope -> result)

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
    let rec unif subst f1 f2 ~k ~fk = match F.view f1, F.view f2 with
      | _ when F.eq f1 f2 -> k subst fk
      | F.Atom p1, F.Atom p2 ->
        begin try
          let subst = FO.variant ~subst p1 sc_1 p2 sc_2 in
          k subst fk
        with Fail -> fk ()
        end
      | F.Eq (t11, t12), F.Eq (t21, t22)
      | F.Neq (t11, t12), F.Neq (t21, t22) ->
        begin try
          let subst = FO.variant ~subst t11 sc_1 t21 sc_2 in
          let subst = FO.variant ~subst t12 sc_1 t22 sc_2 in
          k subst fk
        with Fail -> fk ()
        end;
        begin try
          let subst = FO.variant ~subst t11 sc_1 t22 sc_2 in
          let subst = FO.variant ~subst t12 sc_1 t21 sc_2 in
          k subst fk
        with Fail -> fk ()
        end;
      | F.Not f1', F.Not f2' -> unif subst f1' f2' ~k ~fk
      | F.Imply (f11, f12), F.Imply (f21, f22)
      | F.Xor (f11, f12), F.Xor (f21, f22)
      | F.Equiv(f11, f12), F.Imply (f21, f22) ->
        unif subst f11 f21
          ~k:(fun subst fk -> unif subst f21 f22 ~k ~fk)
          ~fk
      | F.And l1, F.And l2
      | F.Or l1, F.Or l2 ->
        if List.length l1 = List.length l2
          then unif_ac subst l1 [] l2 ~k ~fk
          else fk ()  (* not. *)
      | F.Exists (ty1,f1'), F.Exists (ty2,f2')
      | F.Forall (ty1,f1'), F.Forall (ty2,f2') ->
        begin try
          let subst = Ty.variant ~subst ty1 sc_1 ty2 sc_2 in
          unif subst f1' f2' ~k ~fk
        with Fail -> fk ()
        end
      | F.True, F.True
      | F.False, F.False -> k subst fk (* yep :) *)
      | _ -> fk ()  (* failure *)
    (* invariant: [l1] and [left @ right] always have the same length *)
    and unif_ac subst l1 left right ~k ~fk = match l1, left, right with
      | [], [], [] -> k subst fk  (* success! *)
      | f1::l1', left, f2::right' ->
        (* f1 = f2 ? *)
        unif subst f1 f2
          ~k:(fun subst fk -> unif_ac subst l1' [] (left @ right') ~k ~fk)
          ~fk:(fun () ->
            (* f1 against right', keep f2 for later *)
            unif_ac subst l1 (f2::left) right' ~k ~fk)
      | _::_, left, [] -> fk ()
      | _ -> assert false
    in
    (* flattening (for and/or) *)
    let f1 = F.flatten f1 in
    let f2 = F.flatten f2 in
    Res.of_fun (unif subst f1 f2)

  let are_variant f1 f2 =
    match variant f1 0 f2 1 with
    | Res.End -> false
    | Res.Ok _ -> true
end

