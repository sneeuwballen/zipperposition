
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Unification and Matching} *)

module T = InnerTerm
module S = Subst

exception Fail

type subst = S.t
type 'a sequence = ('a -> unit) -> unit

let prof_unify = Util.mk_profiler "unify"
let prof_matching = Util.mk_profiler "matching"

let fail () = raise Fail

(** {2 Signatures} *)

module type S = Unif_intf.S

(** {2 Base (scoped terms)} *)

(* Does [v] appear in [t] if we apply the substitution,
   or is [t] open? *)
let occurs_check ~depth subst (v,sc_v) t =
  let rec check ~depth (t,sc_t) = match T.ty t with
    | T.NoType -> false
    | T.HasType ty ->
      (* check type and subterms *)
      check ~depth (ty, sc_t) ||
      match T.view t with
        | T.Var v' ->
          (HVar.equal T.equal v v' && sc_v = sc_t)
          ||
          begin match Subst.find subst (v',sc_t) with
            | None -> false
            | Some t' -> check ~depth t'
          end
        | T.DB i -> i>=depth (* not closed! *)
        | T.Const _ -> false
        | T.Bind (_, varty, t') ->
          check ~depth (varty,sc_t) ||
          check ~depth:(depth+1) (t',sc_t)
        | T.AppBuiltin (_, l) -> check_l ~depth l sc_t
        | T.App (hd, l) ->
          check ~depth (hd,sc_t) ||
          check_l ~depth l sc_t
  and check_l ~depth l sc = match l with
    | [] -> false
    | [t] -> check ~depth (t,sc)
    | t :: tail -> check ~depth (t,sc) || check_l ~depth tail sc
  in
  check ~depth t

(* in HO, we have [f1 l1] and [f2 l2], where application is left-associative.
   we need to unify from the right (the outermost application is on
   the right) so this returns pairs to unify. *)
let pair_lists_ f1 l1 f2 l2 =
  let len1 = List.length l1 and len2 = List.length l2 in
  if len1 = len2 then f1::l1, f2::l2
  else if len1 < len2
  then
    let l2_1, l2_2 = CCList.take_drop (len2 - len1) l2 in
    (* NOTE: this should work, because there is only one way
       to type application, therefore it should be well-typed *)
    let f2' = T.app ~ty:(T.ty_exn f1) f2 l2_1 in
    f1 :: l1, f2' :: l2_2
  else
    let l1_1, l1_2 = CCList.take_drop (len1 - len2) l1 in
    (T.app ~ty:(T.ty_exn f2) f1 l1_1) :: l1_2, f2 :: l2

type protected =
  | P_vars of T.VarSet.t (* blocked variables *)
  | P_scope of int (* blocked scope *)

type op =
  | O_unify
  | O_match_protect of protected
  | O_variant
  | O_equal

(** {2 Unary Unification} *)

module Inner = struct
  type ty = T.t
  type term = T.t

  (* public "bind" function that performs occur check *)
  let bind subst v t =
    if occurs_check ~depth:0 subst v t
    then fail()
    else if S.mem subst v then fail()
    else S.bind subst v t

  let rec unif_rec ~op subst t1s t2s =
    let t1,sc1 = S.deref subst t1s
    and t2,sc2 = S.deref subst t2s in
    (* first, unify types *)
    let subst = match T.ty t1, T.ty t2 with
      | T.NoType, T.NoType -> subst
      | T.NoType, _
      | _, T.NoType -> fail()
      | T.HasType ty1, T.HasType ty2 ->
        unif_rec ~op subst (ty1,sc1) (ty2,sc2)
    in
    unif_term ~op subst t1 sc1 t2 sc2
  and unif_term ~op subst t1 sc1 t2 sc2 =
    let view1 = T.view t1 and view2 = T.view t2 in
    match view1, view2 with
      | _ when sc1=sc2 && T.equal t1 t2 ->
        subst (* the terms are equal under any substitution *)
      | T.Var _, _
      | _, T.Var _ ->
        begin match view1, view2, op with
          | T.Var v1, T.Var v2, O_equal ->
            if HVar.equal T.equal v1 v2 && sc1=sc2
            then subst else fail()
          | T.Var v1, T.Var v2, (O_unify | O_variant | O_match_protect _)
            when HVar.equal T.equal v1 v2 && sc1=sc2 -> subst
          | T.Var v1, _, O_match_protect (P_vars s) when T.VarSet.mem v1 s ->
            assert (sc1=sc2);
            fail() (* blocked variable *)
          | T.Var _, _, O_match_protect (P_scope sc) when sc1 = sc ->
            fail() (* variable belongs to the protected scope *)
          | T.Var v1, _, (O_unify | O_match_protect _) ->
            if occurs_check ~depth:0 subst (v1,sc1) (t2,sc2)
            then fail () (* occur check or t2 is open *)
            else S.bind subst (v1,sc1) (t2,sc2)
          | T.Var v1, T.Var _, O_variant ->
            S.bind subst (v1,sc1) (t2,sc2)
          | _, T.Var v2, O_unify ->
            if occurs_check ~depth:0 subst (v2,sc2) (t1,sc1)
            then fail() (* occur check *)
            else S.bind subst (v2,sc2) (t1,sc1)
          | _ -> fail ()  (* fail *)
        end
      | T.Bind (s1, varty1, t1'), T.Bind (s2, varty2, t2') when Binder.equal s1 s2 ->
        (* FIXME: should carry a "depth" parameter for closedness checks? *)
        let subst = unif_rec ~op subst (varty1,sc1) (varty2,sc2) in
        unif_rec ~op subst (t1',sc1) (t2',sc2)
      | T.DB i, T.DB j -> if i = j then subst else raise Fail
      | T.Const f, T.Const g when ID.equal f g -> subst
      | T.App (f1, l1), T.App (f2, l2) ->
        begin match T.view f1, T.view f2 with
          | T.Const id1, T.Const id2 ->
            if ID.equal id1 id2 && List.length l1 = List.length l2
            then unif_list ~op subst l1 sc1 l2 sc2
            else fail()
          | _ ->
            (* currying: unify "from the right" *)
            let l1, l2 = pair_lists_ f1 l1 f2 l2 in
            unif_list ~op subst l1 sc1 l2 sc2
        end
      | T.AppBuiltin (s1,l1), T.AppBuiltin (s2, l2) when Builtin.equal s1 s2 ->
        unif_list ~op subst l1 sc1 l2 sc2
      | _, _ -> raise Fail

  (* unify lists using the given "unificator" and continuation [k] *)
  and unif_list ~op subst l1 sc1 l2 sc2 = match l1, l2 with
    | [], [] -> subst
    | _, []
    | [], _ -> fail ()
    | t1::l1', t2::l2' ->
      let subst = unif_rec ~op subst (t1,sc1) (t2,sc2) in
      unif_list ~op subst l1' sc1 l2' sc2

  let unification ?(subst=Subst.empty) a b =
    Util.with_prof prof_unify
      (fun () -> unif_rec ~op:O_unify subst a b) ()

  let matching ?(subst=Subst.empty) ~pattern b =
    if Scoped.same_scope pattern b then invalid_arg "Unif.matching: same scopes";
    let scope = Scoped.scope b in
    Util.with_prof prof_matching
      (fun () -> unif_rec ~op:(O_match_protect (P_scope scope)) subst pattern b)
      ()

  let matching_same_scope
      ?(protect=Sequence.empty) ?(subst=S.empty) ~scope ~pattern b =
    (* set of variables that should not be bound, including the
       free variables of [b] *)
    let protect = Sequence.append protect (T.Seq.vars b) in
    let blocked = T.VarSet.of_seq protect in
    Util.with_prof prof_matching
      (fun () ->
         unif_rec ~op:(O_match_protect (P_vars blocked)) subst
           (Scoped.make pattern scope) (Scoped.make b scope))
      ()

  let matching_adapt_scope ?protect ?subst ~pattern t =
    if Scoped.same_scope pattern t
    then matching_same_scope ?protect ?subst
        ~scope:(Scoped.scope t) ~pattern:(Scoped.get pattern) (Scoped.get t)
    else matching ?subst ~pattern t

  let variant ?(subst=Subst.empty) a b =
    unif_rec ~op:O_variant subst a b

  let equal ~subst a b =
    try
      ignore (unif_rec ~op:O_equal subst a b);
      true
    with Fail -> false

  let are_variant t1 t2 =
    try
      let _ = variant (Scoped.make t1 0) (Scoped.make t2 1) in
      true
    with Fail ->
      false

  let matches ~pattern t =
    try
      let _ = matching ~pattern:(Scoped.make pattern 0) (Scoped.make t 1) in
      true
    with Fail ->
      false

  let are_unifiable t1 t2 =
    try
      let _ = unification (Scoped.make t1 0) (Scoped.make t2 1) in
      true
    with Fail ->
      false
end

(** {2 Specializations} *)

module Ty = struct
  open Inner
  type ty = Type.t
  type term = Type.t

  let bind =
    (bind :> subst -> ty HVar.t Scoped.t -> term Scoped.t -> subst)

  let unification =
    (unification :> ?subst:subst -> term Scoped.t -> term Scoped.t -> subst)

  let matching =
    (matching :> ?subst:subst ->
     pattern:term Scoped.t -> term Scoped.t -> subst)

  let matching_same_scope =
    (matching_same_scope :> ?protect:(Type.t HVar.t Sequence.t) -> ?subst:subst ->
     scope:int -> pattern:term -> term -> subst)

  let matching_adapt_scope =
    (matching_adapt_scope :>
       ?protect:(Type.t HVar.t Sequence.t) -> ?subst:subst ->
     pattern:term Scoped.t -> term Scoped.t -> subst)

  let variant =
    (variant :> ?subst:subst -> term Scoped.t -> term Scoped.t -> subst)

  let equal =
    (equal :> subst:subst -> term Scoped.t -> term Scoped.t -> bool)

  let are_unifiable =
    (are_unifiable :> term -> term -> bool)

  let matches =
    (matches :> pattern:term -> term -> bool)

  let are_variant =
    (are_variant :> term -> term -> bool)
end

module FO = struct
  open Inner
  type ty = Type.t
  type term = FOTerm.t

  let bind =
    (bind :> subst -> ty HVar.t Scoped.t -> term Scoped.t -> subst)

  let unification =
    (unification :> ?subst:subst -> term Scoped.t -> term Scoped.t -> subst)

  let matching =
    (matching :> ?subst:subst ->
     pattern:term Scoped.t -> term Scoped.t -> subst)

  let matching_same_scope =
    (matching_same_scope :> ?protect:(Type.t HVar.t Sequence.t) -> ?subst:subst ->
     scope:int -> pattern:term -> term -> subst)

  let matching_adapt_scope =
    (matching_adapt_scope :> ?protect:(Type.t HVar.t Sequence.t) -> ?subst:subst ->
     pattern:term Scoped.t -> term Scoped.t -> subst)

  let variant =
    (variant :> ?subst:subst -> term Scoped.t -> term Scoped.t -> subst)

  let equal =
    (equal :> subst:subst -> term Scoped.t -> term Scoped.t -> bool)

  let are_unifiable =
    (are_unifiable :> term -> term -> bool)

  let matches =
    (matches :> pattern:term -> term -> bool)

  let are_variant =
    (are_variant :> term -> term -> bool)
end
