
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Unification and Matching} *)

module T = InnerTerm
module S = Subst
module US = Unif_subst

exception Fail

type unif_subst = Unif_subst.t
type subst = Subst.t
type term = InnerTerm.t
type ty = InnerTerm.t
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

let unif_array_com ?(size=`Same) subst ~op (a1,sc1) (a2,sc2) k =
  let module BV = CCBV in
  (* match a1.(i...) with a2\bv *)
  let rec iter2 subst bv i =
    if i = Array.length a1
    then k subst (* success *)
    else iter3 subst bv i 0
  (* find a matching literal for a1.(i), within a2.(j...)\bv *)
  and iter3 subst bv i j =
    if j = Array.length a2
    then () (* fail *)
    else (
      if not (BV.get bv j) then (
        (* try to match i-th literal of a1 with j-th literal of a2 *)
        BV.set bv j;
        op subst (a1.(i),sc1) (a2.(j),sc2)
          (fun subst -> iter2 subst bv (i+1));
        BV.reset bv j
      );
      iter3 subst bv i (j+1)
    )
  in
  let size_ok = match size with
    | `Same -> Array.length a1 = Array.length a2
    | `Smaller -> Array.length a1 <= Array.length a2
  in
  if size_ok then (
    let bv = BV.create ~size:(Array.length a1) false in
    iter2 subst bv 0
  )

let unif_list_com ?size subst ~op (l1,sc1) (l2,sc2) =
  unif_array_com ?size subst ~op (Array.of_list l1,sc1) (Array.of_list l2,sc2)

let rec unif_list subst ~op (l1,sc1) (l2,sc2) k = match l1, l2 with
  | [], [] -> k subst
  | [], _ | _, [] -> ()
  | x1 :: tail1, x2 :: tail2 ->
    op subst (x1,sc1) (x2,sc2)
      (fun subst -> unif_list subst ~op (tail1,sc1)(tail2,sc2) k)

let pair_lists_right f1 l1 f2 l2 : _ list * _ list =
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

let pair_lists_left l1 ret1 l2 ret2 : _ list * _ list =
  let len1 = List.length l1 and len2 = List.length l2 in
  if len1 = len2 then ret1::l1, ret2::l2
  else if len1 < len2
  then (
    let l2_1, l2_2 = CCList.take_drop len1 l2 in
    let ret2' = T.arrow l2_2 ret2 in
    ret1 :: l1, ret2' :: l2_1
  ) else (
    let l1_1, l1_2 = CCList.take_drop len2 l1 in
    let ret1' = T.arrow l1_2 ret1 in
    ret1' :: l1_1, ret2 :: l2
  )

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

  let has_unifiable_ty (t:T.t): bool = match T.ty t with
    | T.NoType -> false
    | T.HasType ty -> T.type_is_unifiable ty

  let has_non_unifiable_ty (t:T.t): bool = match T.ty t with
    | T.NoType -> false
    | T.HasType ty -> not (T.type_is_unifiable ty)

  (* @param op which operation to perform (unification,matching,alpha-eq)
     @param root if we are at the root of the original problem. This is
     useful for constraints (only allowed in subterms, where [root=false])
  *)
  let rec unif_rec ~op ~root subst t1s t2s : unif_subst =
    let t1,sc1 = US.deref subst t1s
    and t2,sc2 = US.deref subst t2s in
    (* first, unify types *)
    let subst = match T.ty t1, T.ty t2 with
      | T.NoType, T.NoType -> subst
      | T.NoType, _
      | _, T.NoType -> fail()
      | T.HasType ty1, T.HasType ty2 ->
        unif_rec ~op ~root:true subst (ty1,sc1) (ty2,sc2)
    in
    unif_term ~op ~root subst t1 sc1 t2 sc2

  and unif_term ~op ~root subst t1 sc1 t2 sc2 : unif_subst =
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
            if occurs_check ~depth:0 (US.subst subst) (v1,sc1) (t2,sc2)
            then fail () (* occur check or t2 is open *)
            else US.bind subst (v1,sc1) (t2,sc2)
          | T.Var v1, T.Var _, O_variant when sc1<>sc2 ->
            US.bind subst (v1,sc1) (t2,sc2)
          | _, T.Var v2, O_unify ->
            if occurs_check ~depth:0 (US.subst subst) (v2,sc2) (t1,sc1)
            then fail() (* occur check *)
            else US.bind subst (v2,sc2) (t1,sc1)
          | _ -> fail ()  (* fail *)
        end
      | T.Bind (s1, varty1, t1'), T.Bind (s2, varty2, t2') when Binder.equal s1 s2 ->
        (* FIXME: should carry a "depth" parameter for closedness checks? *)
        let subst = unif_rec ~op ~root:true subst (varty1,sc1) (varty2,sc2) in
        unif_rec ~op ~root:false subst (t1',sc1) (t2',sc2)
      | T.DB i, T.DB j -> if i = j then subst else raise Fail
      | T.Const f, T.Const g ->
        if ID.equal f g
        then subst
        else if op=O_unify && not root && has_non_unifiable_ty t1
        then US.add_constr (Unif_constr.make (t1,sc1)(t2,sc2)) subst
        else raise Fail
      | T.App (f1, l1), T.App (f2, l2) ->
        begin match T.view f1, T.view f2 with
          | T.Const id1, T.Const id2 ->
            if ID.equal id1 id2 &&
               List.length l1 = List.length l2
            then (
              (* just unify subterms pairwise *)
              unif_list ~op subst l1 sc1 l2 sc2
            ) else if op=O_unify && not root && has_non_unifiable_ty t1
            (* TODO: notion of value, here, to fail fast in some cases *)
            then US.add_constr (Unif_constr.make (t1,sc1) (t2,sc2)) subst
            else fail()
          | T.Var _, _
          | _, T.Var _ ->
            (* currying: unify "from the right" *)
            let l1, l2 = pair_lists_right f1 l1 f2 l2 in
            unif_list ~op subst l1 sc1 l2 sc2
          | _ -> fail()
        end
      | T.AppBuiltin (Builtin.Arrow, ret1::args1),
        T.AppBuiltin (Builtin.Arrow, ret2::args2) ->
        (* unify [a -> b] and [a' -> b'], virtually *)
        let l1, l2 = pair_lists_left args1 ret1 args2 ret2 in
        unif_list ~op subst l1 sc1 l2 sc2
      | _ when op=O_unify && not root && has_non_unifiable_ty t1 ->
        (* push pair as a constraint, because of typing. *)
        US.add_constr (Unif_constr.make (t1,sc1) (t2,sc2)) subst
      | T.AppBuiltin (s1,l1), T.AppBuiltin (s2, l2) when Builtin.equal s1 s2 ->
        unif_list ~op subst l1 sc1 l2 sc2
      | _, _ -> raise Fail

  (* unify lists using the given "unificator" and continuation [k] *)
  and unif_list ~op subst l1 sc1 l2 sc2 : unif_subst = match l1, l2 with
    | [], [] -> subst
    | _, []
    | [], _ -> fail ()
    | t1::l1', t2::l2' ->
      let subst = unif_rec ~op ~root:false subst (t1,sc1) (t2,sc2) in
      unif_list ~op subst l1' sc1 l2' sc2

  let unify_full ?(subst=US.empty) a b =
    Util.with_prof prof_unify
      (fun () -> unif_rec ~root:true ~op:O_unify subst a b) ()

  let unify_syn ?(subst=Subst.empty) a b =
    let subst = US.of_subst subst in
    let subst = unify_full ~subst a b in
    if US.has_constr subst
    then raise Fail
    else US.subst subst

  let matching ?(subst=Subst.empty) ~pattern b =
    if Scoped.same_scope pattern b then invalid_arg "Unif.matching: same scopes";
    let scope = Scoped.scope b in
    Util.with_prof prof_matching
      (fun () ->
         let subst = US.of_subst subst in
         let subst =
           unif_rec ~root:true ~op:(O_match_protect (P_scope scope)) subst pattern b
         in
         assert (not @@ US.has_constr subst);
         US.subst subst)
      ()

  let matching_same_scope
      ?(protect=Sequence.empty) ?(subst=S.empty) ~scope ~pattern b =
    (* set of variables that should not be bound, including the
       free variables of [b] *)
    let protect = Sequence.append protect (T.Seq.vars b) in
    let blocked = T.VarSet.of_seq protect in
    Util.with_prof prof_matching
      (fun () ->
         let subst = US.of_subst subst in
         let subst =
           unif_rec ~op:(O_match_protect (P_vars blocked)) ~root:true subst
             (Scoped.make pattern scope) (Scoped.make b scope)
         in
         assert (not @@ US.has_constr subst);
         US.subst subst)
      ()

  let matching_adapt_scope ?protect ?subst ~pattern t =
    if Scoped.same_scope pattern t
    then matching_same_scope ?protect ?subst
        ~scope:(Scoped.scope t) ~pattern:(Scoped.get pattern) (Scoped.get t)
    else matching ?subst ~pattern t

  let variant ?(subst=Subst.empty) a b =
    let subst = US.of_subst subst in
    let subst = unif_rec ~op:O_variant ~root:true subst a b in
    assert (not @@ US.has_constr subst);
    let subst = US.subst subst in
    if Subst.is_renaming subst then subst else raise Fail

  let equal ~subst a b =
    let subst = US.of_subst subst in
    try
      let subst = unif_rec ~op:O_equal ~root:true subst a b in
      assert (not @@ US.has_constr subst);
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

  let are_unifiable_full t1 t2 =
    try
      let _ = unify_full (Scoped.make t1 0) (Scoped.make t2 1) in
      true
    with Fail ->
      false

  let are_unifiable_syn t1 t2 =
    try
      let _ = unify_syn (Scoped.make t1 0) (Scoped.make t2 1) in
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

  let unify_full =
    (unify_full :> ?subst:unif_subst -> term Scoped.t -> term Scoped.t -> unif_subst)

  let unify_syn =
    (unify_syn :> ?subst:subst -> term Scoped.t -> term Scoped.t -> subst)

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

  let are_unifiable_full =
    (are_unifiable_full :> term -> term -> bool)

  let are_unifiable_syn =
    (are_unifiable_syn :> term -> term -> bool)

  let matches =
    (matches :> pattern:term -> term -> bool)

  let are_variant =
    (are_variant :> term -> term -> bool)

  let type_is_unifiable = (T.type_is_unifiable :> term -> bool)
end

module FO = struct
  open Inner
  type ty = Type.t
  type term = Term.t

  let bind =
    (bind :> subst -> ty HVar.t Scoped.t -> term Scoped.t -> subst)

  let unify_full =
    (unify_full :> ?subst:unif_subst -> term Scoped.t -> term Scoped.t -> unif_subst)

  let unify_syn =
    (unify_syn :> ?subst:subst -> term Scoped.t -> term Scoped.t -> subst)

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

  let are_unifiable_full =
    (are_unifiable_full :> term -> term -> bool)

  let are_unifiable_syn =
    (are_unifiable_syn :> term -> term -> bool)

  let matches =
    (matches :> pattern:term -> term -> bool)

  let are_variant =
    (are_variant :> term -> term -> bool)

  (* anti-unification of the two terms with at most one disagreement point *)
  let anti_unify ?(cut=max_int) (t:term)(u:term): (term * term) list option =
    let module T = Term in
    let pairs = ref [] in
    let len = ref 0 in
    let rec aux t u = match T.view t, T.view u with
      | _ when T.equal t u -> () (* trivial *)
      | _ when not (Type.equal (T.ty t) (T.ty u)) ->
        raise Exit (* irreconciliable *)
      | _ when Type.equal (Type.returns (T.ty t)) Type.tType ->
        raise Exit (* distinct types *)
      | T.App (f, ts), T.App (g, us) when T.equal f g &&
                                          List.length ts = List.length us ->
        List.iter2 aux ts us
      | _ ->
        incr len;
        if !len <= cut then (
          pairs := (t, u) :: !pairs;
        ) else raise Exit (* went above cut *)
    in
    assert (not (T.equal t u));
    try
      aux t u;
      Some !pairs
    with Exit -> None

  let pair_lists_ =
    (pair_lists_right :> term -> term list -> term -> term list -> InnerTerm.t list * InnerTerm.t list)

  let pair_lists f1 l1 f2 l2 =
    let l1, l2 = pair_lists_ f1 l1 f2 l2 in
    Term.of_term_unsafe_l l1, Term.of_term_unsafe_l l2
end
