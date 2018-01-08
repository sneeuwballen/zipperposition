
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

let section = Util.Section.make "unif"

let prof_unify = Util.mk_profiler "unify"
let prof_matching = Util.mk_profiler "matching"

let fail () = raise Fail

let _allow_partial_skolem_application = ref false
let _allow_pattern_unif = ref true

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

(** During matching or variant checking, we need to {b protect} some variables
    from being bound.
    Typically, when matching [u] against pattern [t], we can bind variables
    from [t] but not from [u] (since [u] must be preserved).

    Two styles of protection exist:

    - by explicit set of variables (when both [t] and [u] live in the
      same scope)
    - by scope: all variables in this scope are protected, EXCEPT
        fresh variables. Fresh variables are not protected because
        pattern unification/matching will move both terms into
        the same (protected) scope, but we still need to bind
        (freshly renamed) variables from the pattern.

*)
type protected =
  | P_vars of T.VarSet.t (* blocked variables *)
  | P_scope of int (* blocked scope *)

let pp_protected out = function
  | P_scope c ->
    Format.fprintf out "protect[%d]" c
  | P_vars s ->
    Format.fprintf out "protect{@[%a@]}@])" (T.VarSet.pp HVar.pp) s

(** The various operations to perform.

    - unification
    - matching (with some variables protected, see {!protected})
    - variant checking (equality modulo renaming)
    - equality (modulo some substitution)
*)
type op =
  | O_unify
  | O_match_protect of protected
  | O_variant of protected
  | O_equal

let pp_op out = function
  | O_unify -> CCFormat.string out "unify"
  | O_equal -> CCFormat.string out "equal"
  | O_variant p -> Format.fprintf out "(@[variant %a@])" pp_protected p
  | O_match_protect p -> Format.fprintf out "(@[match %a@])" pp_protected p

(** {2 Unary Unification} *)

module Inner = struct
  type ty = T.t
  type term = T.t

  (* public "bind" function that performs occur check *)
  let bind ?(check=true) subst v t =
    if check && occurs_check ~depth:0 subst v t
    then fail()
    else if S.mem subst v then fail()
    else S.bind subst v t

  (* public "update" function to replace a binding (with occur check) *)
  let update ?(check=true) subst v t =
    if check && occurs_check ~depth:0 subst v t
    then fail()
    else if not (S.mem subst v) then fail()
    else S.update subst v t

  (* is the type of [t] prop, or some other non-syntactically unifiable type? *)
  let has_non_unifiable_type_or_is_prop (t:T.t): bool = match T.ty t with
    | T.NoType -> false
    | T.HasType ty -> T.type_is_prop ty || not (T.type_is_unifiable ty)

  (* change the scope of variables in this term so they live in [scope]
      NOTE: terrible hack starts here:
       rename variables of [t'] to fresh variables that will
       live in [scope] *)
  let restrict_to_scope subst (t,sc_t) ~into:scope : Unif_subst.t * term =
    let rec aux sc_t subst t : US.t * term = match T.ty t with
      | T.NoType -> subst, t
      | T.HasType ty ->
        let subst, ty = aux sc_t subst ty in
        begin match T.view t with
          | T.Var v ->
            begin match Subst.find (US.subst subst) (v,sc_t) with
              | Some (u,sc_u) ->
                if sc_t = scope
                then
                  (* Variable is already in [scope] *)
                  let subst, u' = aux sc_u subst u in
                  let subst = US.update subst (v,scope) (u', scope) in
                  subst, T.var v
                else if T.is_var u && sc_u = scope
                then
                  (* We already have a corresponging variable in [scope]. Use that one.*)
                  subst, u
                else (
                  (* Create a corresponding variable v' in [scope]. *)
                  let v' = HVar.fresh ~ty () in
                  (* Recursive call on u, giving u' *)
                  let subst, u' = aux sc_u subst u in
                  (* Modify the substitution from v -> u into v -> v', v' -> u' *)
                  let subst = US.update subst (v,sc_t) (T.var v', scope) in
                  let subst = US.bind subst (v',scope) (u', scope) in
                  subst, T.var v'
                )
              | None ->
                if sc_t = scope
                then subst, T.var (HVar.cast ~ty v)
                else (
                  (* make a copy into [scope] *)
                  let v' = HVar.fresh ~ty () in
                  US.bind subst (v,sc_t) (T.var v', scope), T.var v'
                )
            end
          | T.App (f, l) ->
            let subst, f = aux sc_t subst f in
            let subst, l = CCList.fold_map (aux sc_t) subst l in
            subst, T.app ~ty f l
          | T.AppBuiltin (b,l) ->
            let subst, l = CCList.fold_map (aux sc_t) subst l in
            subst, T.app_builtin ~ty b l
          | T.DB i -> subst, T.bvar ~ty i
          | T.Const id -> subst, T.const ~ty id
          | T.Bind (b, tyvar, body) ->
            let subst, varty = aux sc_t subst tyvar in
            let subst, body = aux sc_t subst body in
            subst, T.bind ~ty ~varty b body
        end
    in
    aux sc_t subst t

  (* dereference head of term, and reduce to WHNF.
     NOTE: assumes the term will stay in the same scope, fails otherwise *)
  let rec whnf_deref_rec (subst:US.t) (t,sc_t) : US.t * T.t =
    begin match T.view t with
      | T.Var _ ->
        let u, sc_u = US.deref subst (t,sc_t) in
        assert (sc_t=sc_u);
        if T.equal t u then subst, u
        else whnf_deref_rec subst (u,sc_t) (* fixpoint, maybe [u] is reducible *)
      | T.App (f0, l) ->
        let subst, f = whnf_deref_rec subst (f0,sc_t) in
        let t =
          if T.equal f0 f then t else T.app ~ty:(T.ty_exn t) f l
        in
        (* now reduce to WHNF *)
        let u = Lambda.Inner.whnf t in
        if T.equal t u
        then subst, t
        else whnf_deref_rec subst (u,sc_t) (* reduce further? *)
      | _ -> subst, t
    end

  let whnf_deref subst t =
    let subst, u = whnf_deref_rec subst t in
    (*Format.printf "(@[whnf_deref@ :subst %a@ `%a`@ :yields `%a`@])@."
      US.pp subst (Scoped.pp T.pp) t T.pp u;*)
    subst, u

  module B_vars : sig
    type t = private {
      left: T.t DBEnv.t;
      right: T.t DBEnv.t;
    }

    val empty : t
    val make : T.t DBEnv.t -> T.t DBEnv.t -> t
    val pp : t CCFormat.printer
  end = struct
    type t = {
      left: T.t DBEnv.t;
      right: T.t DBEnv.t;
    }

    let make left right =
      assert (DBEnv.size left = DBEnv.size right);
      {left; right}

    let empty : t = make DBEnv.empty DBEnv.empty

    let pp out (b:t) =
      Format.fprintf out "{@[@[%a@] |@ @[%a@]@]}"
        (DBEnv.pp T.pp) b.left
        (DBEnv.pp T.pp) b.right
  end

  (* list of distinct terms? *)
  let distinct_term_l l : bool =
    List.length l = (T.Set.of_list l |> T.Set.cardinal)

  (* distinct set of variables? *)
  let distinct_bvar_l ~bvars l : bool =
    let n = DBEnv.size bvars in
    List.for_all
      (fun t -> match T.view t with
         | T.DB i -> i < n
         | _ -> false)
      l &&
    distinct_term_l l

  (* distinct ground terms *)
  let distinct_ground_l l : bool =
    List.for_all T.is_ground l && distinct_term_l l

  (* given [l], a list of distinct (ground) terms, and [rhs],
     replace [l] by distinct fresh variables indices in [rhs],
     and return [λvars. rhs] *)
  let lift_terms (l:T.t list) (rhs:T.t) : T.t =
    assert (List.for_all T.is_ground l);
    let vars =
      List.map (fun t -> HVar.fresh ~ty:(T.ty_exn t) ()) l
    in
    (* now replace [l] by [vars] *)
    let body =
      let m =
        List.map2 (fun t v -> t, T.var v) l vars
        |> T.Map.of_list
      in
      T.replace_m rhs m
    in
    T.fun_of_fvars vars body

  (* assuming all elements are [Some x], get the list of [x] *)
  let env_l_dense (e:'a DBEnv.t) : 'a list =
    DBEnv.to_list e
    |> List.map
      (function | Some x -> x | None -> assert false)

  (* Abstract on given bound variables *)
  let fun_of_bvars ~bvars (l:T.t list) (t:T.t) : T.t =
    assert (List.for_all T.is_bvar l);
    let n = List.length l in
    let env =
      DBEnv.to_list_i bvars
      |> CCList.filter_map
        (function
          | None -> None
          | Some (i, _) -> match CCList.find_idx (T.is_bvar_i i) l with
            | None -> None
            | Some (j, t_bvar) ->
              let ty = T.ty_exn t_bvar in
              (* map DB i into db (n-j) *)
              Some (i, T.bvar ~ty (n-j-1)))
      |> DBEnv.of_list
    in
    T.DB.eval env t
    |> T.fun_l (List.map T.ty_exn l)

  let restrict_fun1
    : unif_subst -> ty:T.t -> to_:T.t DBEnv.t -> scope:Scoped.scope ->
    (_ HVar.t * T.t list) -> unif_subst
    = fun subst ~ty ~to_:subset ~scope (v,args) ->
      assert (not (US.mem subst (v,scope)));
      (* only keep bound args *)
      let args =
        List.filter
          (fun t -> match T.view t with
             | T.DB i -> DBEnv.mem subset i
             | _ -> assert false)
          args
      in
      let n = List.length args in
      (* fresh variable *)
      let ty_fun = T.arrow (List.map T.ty_exn args) ty in
      let f = HVar.fresh ~ty:ty_fun () in
      (* new function *)
      let rhs =
        T.app ~ty
          (T.var f)
          (List.mapi (fun i a -> T.bvar ~ty:(T.ty_exn a) (n-i-1)) args)
        |> T.fun_l (List.map T.ty_exn args)
      in
      US.bind subst (v,scope) (rhs,scope)

  let is_match_op = function O_match_protect _ -> true | _ -> false

  (* restrict functions to their common set of arguments that are
     variables present in [bvars],
     by creating a new function [H] and binding both to
     [λall_vars. H (l1 ∩ l2)] *)
  let restrict_fun2
    : unif_subst -> ty_ret:T.t -> bvars:B_vars.t -> scope:Scoped.scope ->
    _ -> _ -> unif_subst
    = fun subst ~ty_ret ~bvars ~scope (v1,l1) (v2,l2) ->
      assert (not (HVar.equal T.equal v1 v2)); (* non-trivial *)
      assert (not (US.mem subst (v1,scope)));
      assert (not (US.mem subst (v2,scope)));
      (* compute intersection *)
      let inter =
        List.filter
          (fun t -> match T.view t with
             | T.DB i ->
               DBEnv.mem bvars.B_vars.left i &&
               List.exists (T.is_bvar_i i) l2
             | _ -> assert false)
          l1
      in
      (* type of new function *)
      let ty_fun = T.arrow (List.map T.ty_exn inter) ty_ret in
      (* fresh variable *)
      let f = HVar.fresh ~ty:ty_fun () in
      (* build terms to replace [v1] and [v2] *)
      let mk_rhs l =
        let n = List.length l in
        let args =
          List.map
            (fun a ->
               let i = CCList.find_idx (T.equal a) l |> CCOpt.get_exn |> fst in
               T.bvar ~ty:(T.ty_exn a) (n-i-1))
            inter
        in
        let body = T.app ~ty:ty_ret (T.var f) args in
        T.fun_l (List.map T.ty_exn l) body
      in
      let rhs1 = mk_rhs l1 in
      let rhs2 = mk_rhs l2 in
      let subst = US.bind subst (v1,scope) (rhs1,scope) in
      let subst = US.bind subst (v2,scope) (rhs2,scope) in
      subst

  (* delay pair, closing it if necessary *)
  let delay ~bvars ~tags subst t1 sc1 t2 sc2 =
    if T.equal t1 t2 && sc1=sc2 then subst (* trivial *)
    else (
      let u1 = T.fun_l (env_l_dense bvars.B_vars.left |> List.rev) t1 in
      let u2 = T.fun_l (env_l_dense bvars.B_vars.right |> List.rev) t2 in
      if T.DB.closed u1 && T.DB.closed u2 then (
        US.add_constr (Unif_constr.make ~tags (u1,sc1)(u2,sc2)) subst
      ) else (
        fail()
      )
    )

  let partial_skolem_fail f l1 l2 =
    not !_allow_partial_skolem_application &&
    List.length l1 - List.length l2 < ID.num_mandatory_args f

  (* @param op which operation to perform (unification,matching,alpha-eq)
     @param root if we are at the root of the original problem. This is
     @param env typing environment for binders
     useful for constraints (only allowed in subterms, where [root=false])
  *)
  let rec unif_rec ~op ~root ~bvars subst t1s t2s : unif_subst =
    let t1,sc1 = US.deref subst t1s
    and t2,sc2 = US.deref subst t2s in
    begin match T.ty t1, T.ty t2 with
      | T.NoType, T.NoType ->
        assert (t1 == t2 && t1 == T.tType);
        subst
      | T.NoType, _
      | _, T.NoType -> fail()
      | T.HasType ty1, T.HasType ty2 ->
        (* unify types, then terms *)
        let subst = unif_rec ~op ~root:true ~bvars subst (ty1,sc1) (ty2,sc2) in
        unif_term ~op ~root ~bvars subst t1 sc1 t2 sc2
    end

  and unif_term ~op ~root ~bvars subst t1 sc1 t2 sc2 : unif_subst =
    let view1 = T.view t1 and view2 = T.view t2 in
    let delay() = delay ~bvars subst t1 sc1 t2 sc2 in
    (* fast check for terms in WHNF *)
    let is_whnf t sc_t = match T.view t with
      | T.App (f, _) ->
        not (T.is_lambda f) &&
        not (T.is_var f && US.mem subst (T.as_var_exn f,sc_t))
      | _ -> true
    in
    (*Format.printf "(@[unif_rec@ :t1 `%a`@ :t2 `%a`@ :op %a@ :subst @[%a@]@ :bvars %a@])@."
      (Scoped.pp T.pp) (t1,sc1) (Scoped.pp T.pp) (t2,sc2)
      pp_op op US.pp subst B_vars.pp bvars;*)
    assert (not (T.is_a_type t1 && Type.is_forall (Type.of_term_unsafe t1)));
    begin match view1, view2 with
      | _ when sc1=sc2 && T.equal t1 t2 ->
        subst (* the terms are equal under any substitution *)
      | T.Var _, _ when is_whnf t2 sc2 ->
        unif_vars ~op subst t1 sc1 t2 sc2
      | _, T.Var _ when is_whnf t1 sc1 ->
        unif_vars ~op subst t1 sc1 t2 sc2
      | T.DB i, T.DB j -> if i = j then subst else raise Fail
      | T.Const f, T.Const g ->
        if ID.equal f g
        then subst
        else if op=O_unify && not root && has_non_unifiable_type_or_is_prop t1
        then (
          let tags = T.type_non_unifiable_tags (T.ty_exn t1) in
          US.add_constr (Unif_constr.make ~tags (t1,sc1)(t2,sc2)) subst
        )
        else raise Fail
      | T.App ({T.term=T.Const id1; _}, l1),
        T.App ({T.term=T.Const id2; _}, l2) ->
        (* first-order applications *)
        if ID.equal id1 id2 &&
           List.length l1 = List.length l2
        then (
          (* just unify subterms pairwise *)
          unif_list ~op ~bvars subst l1 sc1 l2 sc2
        ) else if op=O_unify && not root && has_non_unifiable_type_or_is_prop t1 then (
          (* TODO: notion of value, here, to fail fast in some cases.
             e.g.  [a + 1 = a] should fail immediately *)
          let tags = T.type_non_unifiable_tags (T.ty_exn t1) in
          delay ~tags ()
        ) else fail()
      | T.App ({T.term=(T.Var _ | T.DB _ | T.Bind (Binder.Lambda, _, _)); _}, _), _
      | _, T.App ({T.term=(T.Var _ | T.DB _ | T.Bind (Binder.Lambda, _, _)); _}, _)
      | T.Bind (Binder.Lambda, _, _), _
      | _, T.Bind (Binder.Lambda, _, _) ->
        (* perform HO unification after moving both terms into same
           scope [sc2] *)
        begin match op with
          | O_match_protect (P_scope sc2') | O_variant (P_scope sc2') ->
            assert (sc2=sc2');
            if sc1=sc2' then (
              (* no renaming at all, same scope already *)
              unif_ho ~op ~root ~bvars subst t1 t2 ~scope:sc2
            ) else (
              let subst, t1 = restrict_to_scope subst (t1,sc1) ~into:sc2 in
              (* NOTE: we collapse both scopes together. The idea now
                 is that by renaming variables of [t1] we allow
                 the (fresh) variables of [t1] to bind, but not the
                 variables of [t2] *)
              unif_ho ~op ~root ~bvars subst t1 t2 ~scope:sc2
            )
          | O_match_protect (P_vars _) | O_variant (P_vars _) | O_equal ->
            (* rename in [t1] but not [t2] *)
            let subst, t1 = restrict_to_scope subst (t1,sc1) ~into:sc2 in
            unif_ho ~op ~root ~bvars subst t1 t2 ~scope:sc2
          | O_unify ->
            let subst, t1 = restrict_to_scope subst (t1,sc1) ~into:sc2 in
            let subst, t2 = restrict_to_scope subst (t2,sc2) ~into:sc2 in
            unif_ho ~op ~root ~bvars subst t1 t2 ~scope:sc2
        end
      | T.AppBuiltin (Builtin.Arrow, ret1::args1),
        T.AppBuiltin (Builtin.Arrow, ret2::args2) ->
        (* unify [a -> b] and [a' -> b'], virtually *)
        let l1, l2 = pair_lists_left args1 ret1 args2 ret2 in
        unif_list ~op ~bvars subst l1 sc1 l2 sc2
      | T.Bind ((Binder.Forall | Binder.Exists | Binder.ForallTy) as b1, varty1, t1'),
        T.Bind (b2, varty2, t2') when b1=b2 ->
        (* unify types, then enter bodies *)
        let subst =
          unif_rec ~op ~root:true ~bvars subst (varty1,sc1) (varty2,sc2)
        in
        unif_rec ~op ~root:false ~bvars
          subst (t1',sc1) (t2',sc2)
      | T.Bind ((Binder.Forall | Binder.Exists), _, _), _
      | _, T.Bind ((Binder.Forall | Binder.Exists), _, _) ->
        delay ~tags:[] () (* cannot unify non-atomic propositions, so delay *)
      | T.AppBuiltin (Builtin.Int n1,[]),
        T.AppBuiltin (Builtin.Int n2,[]) ->
        if Z.equal n1 n2 then subst else raise Fail (* int equality *)
      | T.AppBuiltin (Builtin.Rat n1,[]),
        T.AppBuiltin (Builtin.Rat n2,[]) ->
        if Q.equal n1 n2 then subst else raise Fail (* rational equality *)
      | T.AppBuiltin (Builtin.True, _), _
      | T.AppBuiltin (Builtin.False, _), _ ->
        if T.equal t1 t2 then subst else raise Fail (* boolean equality *)
      | _ when op=O_unify && not root && has_non_unifiable_type_or_is_prop t1 ->
        let tags = T.type_non_unifiable_tags (T.ty_exn t1) in
        delay ~tags () (* push pair as a constraint, because of typing. *)
      | T.AppBuiltin (s1,l1), T.AppBuiltin (s2, l2) when Builtin.equal s1 s2 ->
        (* try to unify/match builtins pairwise *)
        unif_list ~op ~bvars subst l1 sc1 l2 sc2
      | _, _ -> raise Fail
    end

  and unif_vars ~op subst t1 sc1 t2 sc2 : unif_subst =
    begin match T.view t1, T.view t2, op with
      | T.Var v1, T.Var v2, O_equal ->
        if HVar.equal T.equal v1 v2 && sc1=sc2
        then subst else fail()
      | T.Var v1, T.Var v2, _
        when HVar.equal T.equal v1 v2 && sc1=sc2 -> subst
      | T.Var v1, _, O_match_protect (P_vars s) when T.VarSet.mem v1 s ->
        assert (sc1=sc2);
        fail() (* blocked variable *)
      | T.Var v1, _, O_match_protect (P_scope sc)
        when sc1 = sc && not (HVar.is_fresh v1) ->
        fail() (* variable belongs to the protected scope and is not fresh *)
      | T.Var v1, _, O_match_protect (P_scope _) ->
        (* no need for occur check when matching from distinct scopes *)
        US.bind subst (v1,sc1) (t2,sc2)
      | T.Var v1, _, (O_unify | O_match_protect (P_vars _)) ->
        if occurs_check ~depth:0 (US.subst subst) (v1,sc1) (t2,sc2)
        then fail () (* occur check or t2 is open *)
        else US.bind subst (v1,sc1) (t2,sc2)
      | T.Var v1, T.Var _, O_variant (P_vars s) when not (T.VarSet.mem v1 s) ->
        US.bind subst (v1,sc1) (t2,sc2)
      | T.Var v1, T.Var _, O_variant (P_scope sc')
        when sc1<>sc' || HVar.is_fresh v1 ->
        US.bind subst (v1,sc1) (t2,sc2)
      | _, T.Var v2, O_unify ->
        if occurs_check ~depth:0 (US.subst subst) (v2,sc2) (t1,sc1)
        then fail() (* occur check *)
        else US.bind subst (v2,sc2) (t1,sc1)
      | _ -> fail ()  (* fail *)
    end

  (* unify lists pairwise *)
  and unif_list ~op ~bvars subst l1 sc1 l2 sc2 : unif_subst = match l1, l2 with
    | [], [] -> subst
    | _, []
    | [], _ -> fail ()
    | t1::l1', t2::l2' ->
      let subst = unif_rec ~op ~root:false ~bvars subst (t1,sc1) (t2,sc2) in
      unif_list ~op ~bvars subst l1' sc1 l2' sc2

  (* non-trivial cases of HO unification *)
  and unif_ho ~op ~root ~bvars subst t1_0 t2_0 ~scope : unif_subst =
    (* first, normalize and un-app both terms *)
    let subst, t1 = whnf_deref subst (t1_0,scope) in
    let subst, t2 = whnf_deref subst (t2_0,scope) in
    (*Format.printf
      "(@[unif_ho@ :t1 `%a`@ :t1_nf `%a`@ :t2 `%a`@ :t2_nf `%a`@ \
       :sc %d :subst %a@ :op %a@ :bvars %a@])@."
      T.pp t1_0 T.pp t1 T.pp t2_0 T.pp t2 scope US.pp subst pp_op op B_vars.pp bvars;*)
    let f1, l1 = T.as_app t1 in
    let f2, l2 = T.as_app t2 in
    let delay() = delay ~bvars subst t1 scope t2 scope in
    (* case where heads are the same *)
    let same_rigid_head() =
      if List.length l1 = List.length l2
      then (
        (* just unify subterms pairwise *)
        unif_list ~op ~bvars subst l1 scope l2 scope
      ) else if op=O_unify && not root && has_non_unifiable_type_or_is_prop t1 then (
        let tags = T.type_non_unifiable_tags (T.ty_exn t1) in
        delay ~tags ()
      ) else fail()
    in
    begin match T.view f1, T.view f2 with
      | _ when T.equal f1 f2 -> same_rigid_head()
      | T.Bind (Binder.Lambda, _, _),
        T.Bind (Binder.Lambda, _, _) ->
        assert (l1=[] && l2=[]);
        let new_vars1, f1, new_vars2, f2 =
          T.open_bind2 Binder.Lambda f1 f2
        in
        unif_rec ~op ~root:false
          ~bvars:(B_vars.make
              (DBEnv.push_l_rev bvars.B_vars.left new_vars1)
              (DBEnv.push_l_rev bvars.B_vars.right new_vars2))
          subst (f1,scope) (f2,scope)
      | T.Bind (Binder.Lambda, _, _), _ ->
        (* [λx. t = u] becomes [t = u x] *)
        assert (l1=[]);
        let new_vars, f1 = T.open_bind Binder.Lambda f1 in
        let n = List.length new_vars in
        unif_rec ~op ~root
          ~bvars:(B_vars.make
              (DBEnv.push_l_rev bvars.B_vars.left new_vars)
              (DBEnv.push_l_rev bvars.B_vars.right new_vars))
          subst
          (f1,scope)
          (T.app ~ty:(T.ty_exn f1)
             (T.DB.shift n t2)
             (List.mapi (fun i ty->T.bvar ~ty (n-i-1)) new_vars), scope)
      | _, T.Bind (Binder.Lambda, _, _) ->
        (* same as above *)
        assert (l2=[]);
        let new_vars, f2 = T.open_bind Binder.Lambda f2 in
        let n = List.length new_vars in
        unif_rec ~op ~root
          ~bvars:(B_vars.make
              (DBEnv.push_l_rev bvars.B_vars.left new_vars)
              (DBEnv.push_l_rev bvars.B_vars.right new_vars))
          subst
          (T.app ~ty:(T.ty_exn f2)
             (T.DB.shift n t1)
             (List.mapi (fun i ty -> T.bvar ~ty (n-i-1)) new_vars), scope)
          (f2,scope)
      | T.Const id1, T.Const id2 ->
        (* first-order applications *)
        if ID.equal id1 id2 then same_rigid_head()
        else if op=O_unify && not root && has_non_unifiable_type_or_is_prop t1
        then (
          let tags = T.type_non_unifiable_tags (T.ty_exn t1) in
          delay ~tags () (* push pair as a constraint, because of typing. *)
        )
        else fail()
      | T.DB i1, T.DB i2 ->
        if i1=i2 then same_rigid_head() else fail()
      | T.Var _, _ when l1=[] ->
        unif_rec ~op ~bvars ~root subst (t1,scope) (t2, scope) (* to bind *)
      | _, T.Var _ when l2=[] ->
        unif_rec ~op ~bvars ~root subst (t1,scope) (t2, scope) (* to bind *)
      | T.Const f, T.Var _  when partial_skolem_fail f l1 l2 ->
        fail()
      | T.Var _, T.Const g when partial_skolem_fail g l2 l1 ->
        fail()
      | T.Var v1, T.Const _ ->
        begin match op with
          | O_match_protect (P_scope sc2')
            when sc2' = scope && not (HVar.is_fresh v1) -> fail()
          | O_match_protect (P_vars s) when T.VarSet.mem v1 s -> fail()
          | O_unify | O_match_protect _ -> ()
          | O_variant _ | O_equal -> fail()
        end;
        (*Format.printf
          "(@[unif_ho.flex_rigid@ `@[:f1 %a :l1 %a@]`@ :t2 `%a`@ :subst %a@ :bvars %a@])@."
          (Scoped.pp T.pp) (f1,scope) (CCFormat.Dump.list T.pp) l1
          (Scoped.pp T.pp) (t2,scope) US.pp subst B_vars.pp bvars;*)
        if !_allow_pattern_unif && distinct_bvar_l ~bvars:bvars.B_vars.left l1 then (
          (* flex/rigid pattern unif *)
          flex_rigid ~bvars:bvars.B_vars.left subst f1 l1 t2 ~scope
        ) else if !_allow_pattern_unif && distinct_ground_l l1 then (
          (* [v t = t2] becomes [v = λx. t2[x/t]] *)
          let t2 = lift_terms l1 t2 in
          unif_rec ~op ~root ~bvars subst (f1,scope) (t2,scope)
        ) else if l2<>[] then (
          (* λfree-HO: unify with currying, "from the right" *)
          let l1, l2 = pair_lists_right f1 l1 f2 l2 in
          (* Variables do not take type arguments. So we can fail early if `hd l2`
             does take type arguments. This avoids errors with the debug output. *)
          assert (T.expected_ty_vars (HVar.ty v1) = 0);
          if T.expected_ty_vars (T.ty_exn (List.hd l2)) != 0 then fail();
          unif_list ~op ~bvars subst l1 scope l2 scope
        ) else fail()
      | T.Const _, T.Var v2 ->
        (*Format.printf
          "(@[unif_ho.flex_rigid@ `@[:f2 %a :l2 %a@]`@ :t1 `%a`@ :subst %a@ :bvars %a@])@."
          (Scoped.pp T.pp) (f2,scope) (CCFormat.Dump.list T.pp) l2
          (Scoped.pp T.pp) (t1,scope) US.pp subst B_vars.pp bvars;*)
        if !_allow_pattern_unif && distinct_bvar_l ~bvars:bvars.B_vars.right l2 && op=O_unify then (
          (* flex/rigid pattern unif *)
          flex_rigid ~bvars:bvars.B_vars.right subst f2 l2 t1 ~scope
        ) else if !_allow_pattern_unif && distinct_ground_l l2 && op=O_unify then (
          (* [t1 = v t] becomes [v = λx. t1[x/t]] *)
          let t1 = lift_terms l2 t1 in
          unif_rec ~op ~root ~bvars subst (t1,scope) (f2,scope)
        ) else if l1<>[] then (
          (* λfree-HO: unify with currying, "from the right" *)
          let l1, l2 = pair_lists_right f1 l1 f2 l2 in
          (* Variables do not take type arguments. So we can fail early if `hd l1`
             does take type arguments. This avoids errors with the debug output. *)
          assert (T.expected_ty_vars (HVar.ty v2) = 0);
          if T.expected_ty_vars (T.ty_exn (List.hd l1)) != 0 then fail();
          unif_list ~op ~bvars subst l1 scope l2 scope
        ) else fail()
      | T.Var v1, T.Var v2
        when op=O_unify &&
             !_allow_pattern_unif &&
             distinct_bvar_l ~bvars:bvars.B_vars.left l1 &&
             distinct_bvar_l ~bvars:bvars.B_vars.right l2 ->
        (* flex/flex equation for pattern unif *)
        flex_flex_unif subst ~bvars ~ty_ret:(T.ty_exn t1) ~scope
          v1 l1 v2 l2
      | T.Var v1, T.Var v2
        when is_match_op op &&
             !_allow_pattern_unif &&
             distinct_bvar_l ~bvars:bvars.B_vars.left l1 &&
             distinct_bvar_l ~bvars:bvars.B_vars.right l2 &&
             CCList.subset ~eq:T.equal l2 l1 ->
        (* TODO: use equality mod subst for [subset] check *)
        flex_flex_matching ~op subst ~bvars ~ty_ret:(T.ty_exn t2) ~scope v1 l1 v2 l2
      | T.Var _, T.Var _ ->
        (* λfree-HO: unify with currying, "from the right" *)
        let l1, l2 = pair_lists_right f1 l1 f2 l2 in
        unif_list ~op ~bvars subst l1 scope l2 scope
      | _ -> fail()
    end

  (* flex/rigid pair *)
  and flex_rigid ~bvars subst f1 l1 t2 ~scope : unif_subst =
    Util.debugf ~section 5
      "(@[flex_rigid@ :subst %a@ `@[%a %a@]`@ :rhs `%a`@ :bvars %a@])"
      (fun k->k US.pp subst T.pp f1 (Util.pp_list T.pp) l1 T.pp t2
          (DBEnv.pp T.pp) bvars);
    assert (l1<>[]);
    assert (List.for_all T.is_bvar l1);
    assert (T.is_var f1);
    (* bind [v1 := λl1. t2], then traverse [t2] *)
    let rhs = fun_of_bvars ~bvars l1 t2 in
    let subst =
      unif_rec ~op:O_unify ~root:true ~bvars:B_vars.empty
        subst (f1,scope) (rhs,scope)
    in
    Util.debugf ~section 5 "(@[flex_rigid_bind@ :subst %a@])" (fun k->k US.pp subst);
    (* now ensure that RHS is consistent with assignment *)
    Util.debugf ~section 5 "(@[proj@ :bvars %a@ :in `%a`@])"
      (fun k->k (DBEnv.pp T.pp) bvars T.pp t2);
    proj_fun ~bvars subst (t2,scope)

  (* project on a set of DB indices in [vars] *)
  and proj_fun ~bvars subst (t,sc_t) : unif_subst =
    let subst, t = whnf_deref subst (t,sc_t) in
    let f, l = T.as_app t in
    begin match T.view f with
      | T.Const _ -> proj_fun_l ~bvars subst (l,sc_t)
      | T.Bind (b, _, _) ->
        assert (l=[]);
        let new_vars, body = T.open_bind b f in
        proj_fun ~bvars:(DBEnv.push_l_rev bvars new_vars) subst (body,sc_t)
      | T.App _ -> assert false
      | T.AppBuiltin (_, l2) ->
        assert (l=[]);
        proj_fun_l ~bvars subst (l2,sc_t)
      | T.DB i ->
        if DBEnv.mem bvars i
        then proj_fun_l ~bvars subst (l,sc_t)
        else fail() (* this variable is not in scope anymore *)
      | T.Var v ->
        if l=[] then subst
        else if List.for_all T.is_bvar l then (
          (* retrict [v] on [bvars], as a pattern. *)
          restrict_fun1 subst ~ty:(T.ty_exn t) ~to_:bvars ~scope:sc_t (v,l)
        ) else fail()
    end

  and proj_fun_l ~bvars subst (l,sc) : unif_subst =
    List.fold_left
      (fun subst t -> proj_fun ~bvars subst (t,sc))
      subst l

  (* flex/flex unif pair: find common subset of arguments, introduce
     function variable parametrized by this subset
     @param ty_ret the type of the terms to unify
  *)
  and flex_flex_unif ~bvars subst ~ty_ret v1 l1 v2 l2 ~scope : unif_subst =
    Util.debugf ~section 5
      "(@[flex_flex@ `@[%a %a@]`[%d]@ `@[%a %a@]`[%d]@ :subst %a@ :bvars %a@])"
      (fun k->k
          HVar.pp v1 (Util.pp_list T.pp) l1 scope
          HVar.pp v2 (Util.pp_list T.pp) l2 scope
          US.pp subst B_vars.pp bvars);
    let subst =
      restrict_fun2 subst ~ty_ret ~bvars ~scope (v1,l1) (v2,l2)
    in
    (*Format.printf "(@[flex_flex_yield@ :subst %a@])@." US.pp subst;*)
    subst

  (* flex/flex matching pair:
     similar to flex/flex above, but we project [v1] over [l2] using [v2]
     @param ty_ret the type of the terms to match
  *)
  and flex_flex_matching ~op ~bvars subst ~ty_ret v1 l1 v2 l2 ~scope : unif_subst =
    Util.debugf ~section 5
      "(@[flex_flex_matching@ `@[%a %a@]`[%d]@ `@[%a %a@]`[%d]@ :subst %a@ :bvars %a@])"
      (fun k->k HVar.pp v1 (Util.pp_list T.pp) l1 scope
          HVar.pp v2 (Util.pp_list T.pp) l2 scope
          US.pp subst B_vars.pp bvars);
    (* check matching conditions *)
    begin match op with
      | O_match_protect (P_scope sc')
        when sc'=scope && not (HVar.is_fresh v1) -> fail()
      | O_match_protect (P_vars s) when T.VarSet.mem v1 s -> fail()
      | _ -> ()
    end;
    let n = List.length l1 in
    (* imitate [t2] *)
    let rhs =
      T.app ~ty:ty_ret
        (T.var v2)
        (List.map
           (fun a ->
              let i = CCList.find_idx (T.equal a) l1 |> CCOpt.get_exn|>fst in
              T.bvar ~ty:(T.ty_exn a) (n-i-1))
           l2)
      |> T.fun_l (List.map T.ty_exn l1)
    in
    let subst = US.bind subst (v1,scope) (rhs,scope) in
    (*Format.printf "(@[flex_flex_match_yield@ :subst %a@])@." US.pp subst;*)
    subst

  (* equality modulo subst *)
  and equal_ ~subst a b : bool =
    try
      let _ = unif_rec ~op:O_equal ~root:true ~bvars:B_vars.empty  subst a b in
      true
    with Fail -> false

  let unify_full ?(subst=US.empty) a b : unif_subst =
    Util.with_prof prof_unify
      (fun () -> unif_rec ~root:true ~op:O_unify ~bvars:B_vars.empty subst a b) ()

  let unify_syn ?(subst=Subst.empty) a b : Subst.t =
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
           unif_rec subst pattern b
             ~root:true ~op:(O_match_protect (P_scope scope)) ~bvars:B_vars.empty
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
           unif_rec
             subst (Scoped.make pattern scope) (Scoped.make b scope)
             ~op:(O_match_protect (P_vars blocked)) ~root:true ~bvars:B_vars.empty
         in
         assert (not @@ US.has_constr subst);
         US.subst subst)
      ()

  let matching_adapt_scope ?protect ?subst ~pattern t =
    if Scoped.same_scope pattern t
    then matching_same_scope ?protect ?subst
        ~scope:(Scoped.scope t) ~pattern:(Scoped.get pattern) (Scoped.get t)
    else matching ?subst ~pattern t

  let variant ?(subst=Subst.empty) ((_,sc1)as a) ((t2,sc2) as b) =
    let subst = US.of_subst subst in
    let op =
      let protect =
        if sc1=sc2
        then P_vars (T.Seq.vars t2 |> T.VarSet.of_seq)
        else P_scope sc2
      in
      O_variant protect
    in
    let subst = unif_rec ~op ~root:true ~bvars:B_vars.empty subst a b in
    assert (not @@ US.has_constr subst);
    let subst = US.subst subst in
    if Subst.is_renaming subst then subst else raise Fail

  (* equality modulo subst *)
  let equal ~subst a b =
    let subst = US.of_subst subst in
    try
      let subst = unif_rec ~op:O_equal ~root:true ~bvars:B_vars.empty  subst a b in
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
    (bind :> ?check:bool -> subst -> ty HVar.t Scoped.t -> term Scoped.t -> subst)

  let update =
    (update :> ?check:bool -> subst -> ty HVar.t Scoped.t -> term Scoped.t -> subst)

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
    (bind :> ?check:bool -> subst -> ty HVar.t Scoped.t -> term Scoped.t -> subst)

  let update =
    (update :> ?check:bool -> subst -> ty HVar.t Scoped.t -> term Scoped.t -> subst)

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


let () =
  Options.add_opts
    [  "--partial-skolem",
       Arg.Set _allow_partial_skolem_application,
       " allow partial application of skolem constants (sound only assuming the axiom of choice)";
       "--no-unif-pattern", Arg.Clear _allow_pattern_unif, " disable pattern unification";
       "--unif-pattern", Arg.Set _allow_pattern_unif, " enable pattern unification";
    ]
