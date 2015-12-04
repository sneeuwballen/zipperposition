
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Unification and Matching} *)

module T = ScopedTerm
module DBE = DBEnv
module S = Substs

(* TODO:
    - reduce [{foo=t, .... | ...}.foo] into t
    - when unifying [X.foo] with [t], unify X with [{foo=t | \rho}]
*)

exception Fail

type scope = S.scope
type subst = S.t
type env = ScopedTerm.t DBE.t
type 'a sequence = ('a -> unit) -> unit

let prof_unify = Util.mk_profiler "unify"
let prof_matching = Util.mk_profiler "matching"

let fail () = raise Fail

(** {2 Signatures} *)

module type UNARY = sig
  type term

  val unification : ?env1:env -> ?env2:env -> ?subst:subst ->
    term Scoped.t -> term Scoped.t -> subst
  (** Unify terms, returns a subst or
      @raise Fail if the terms are not unifiable
      @param env1 environment for the first term
      @param env2 environment for the second term *)

  val matching : ?allow_open:bool -> ?env1:env -> ?env2:env -> ?subst:subst ->
    pattern:term Scoped.t -> term Scoped.t -> subst
  (** [matching ~pattern scope_p b scope_b] returns
      [sigma] such that [sigma pattern = b], or fails.
      Only variables from the scope of [pattern] can  be bound in the subst.
      @param subst initial substitution (default empty)
      @param allow_open if true, variables can bind to non-closed DB terms (default [false])
      @raise Fail if the terms do not match.
      @raise Invalid_argument if the two scopes are equal *)

  val matching_same_scope : ?env1:env -> ?env2:env -> ?protect:(term Sequence.t) -> ?subst:subst ->
    scope:scope -> pattern:term -> term -> subst
  (** matches [pattern] (more general) with the other term.
      The two terms live in the same scope, which is passed as the
      [scope] argument. It needs to gather the variables of the
      other term to make sure they are not bound.
      @param scope the common scope of both terms
      @param protect a sequence of variables to protect (they cannot
        be bound during matching!). Variables of the second term
        are automatically protected. *)

  val matching_adapt_scope : ?env1:env -> ?env2:env -> ?protect:(term Sequence.t) -> ?subst:subst ->
    pattern:term Scoped.t -> term Scoped.t -> subst
  (** Call either {!matching} or {!matching_same_scope} depending on
      whether the given scopes are the same or not.
      @param protect used if scopes are the same, see {!matching_same_scope} *)

  val variant : ?env1:env -> ?env2:env -> ?subst:subst ->
    term Scoped.t -> term Scoped.t -> subst
  (** Succeeds iff the first term is a variant of the second, ie
      if they are alpha-equivalent *)

  val equal : ?env1:env -> ?env2:env -> subst:subst -> term Scoped.t -> term Scoped.t -> bool
  (** [equal subst t1 s1 t2 s2] returns [true] iff the two terms
      are equal under the given substitution, i.e. if applying the
      substitution will return the same term. *)

  val are_unifiable : term -> term -> bool

  val matches : pattern:term -> term -> bool

  val are_variant : term -> term -> bool
end

module type NARY = sig
  type term

  val unification : ?env1:env -> ?env2:env -> ?subst:subst ->
    term Scoped.t -> term Scoped.t -> subst Sequence.t
  (** unification of two terms *)

  val matching : ?allow_open:bool -> ?env1:env -> ?env2:env -> ?subst:subst ->
    pattern:term Scoped.t -> term Scoped.t -> subst Sequence.t
  (** matching of two terms.
      @param allow_open if true, variables can bind to non-closed DB terms (default [false])
      @raise Invalid_argument if the two scopes are equal. *)

  val variant : ?env1:env -> ?env2:env -> ?subst:subst ->
    term Scoped.t -> term Scoped.t -> subst Sequence.t
  (** alpha-equivalence checking of two terms *)

  val are_unifiable : term -> term -> bool

  val matches : pattern:term -> term -> bool

  val are_variant : term -> term -> bool
end

(** {2 Base (scoped terms)} *)

(* Does [v] appear in [t] if we apply the substitution,
   or is [t] open? *)
let occurs_check ~env subst v t =
  let rec check ~env t = match T.ty t.Scoped.value with
    | T.NoType -> false
    | T.HasType ty ->
        (* check type and subterms *)
        check ~env (Scoped.set t ty) ||
        match T.view t.Scoped.value with
        | T.Var v' ->
            (HVar.equal (Scoped.get v) v' && Scoped.same_scope v t)
            ||
            begin match Substs.find subst (Scoped.set t v') with
              | None -> false
              | Some t' -> check ~env t'
            end
        | T.DB i ->
            begin match DBE.find env i with
              | None -> true  (* not closed! *)
              | Some t' ->
                  assert (T.DB.closed t');
                  check ~env (Scoped.set t t')
            end
        | T.Const _ -> false
        | T.Bind (_, varty, t') ->
            check ~env varty sc_t ||
            check ~env:(DBE.push_none env) t' sc_t
        | T.Record (l, rest) ->
            CCOpt.maybe (HVar.equal v) false rest
            ||
            List.exists (fun (_,t') -> check ~env t' sc_t) l
        | T.SimpleApp (_, l)
        | T.AppBuiltin (_, l)
        | T.Multiset l ->
            List.exists (fun t' -> check ~env t' sc_t) l
        | T.App (hd, l) ->
            check ~env hd sc_t ||
            List.exists (fun t' -> check ~env t' sc_t) l  (* TODO: unroll *)
  in
  check ~env t sc_t

module RecordUnif = struct
  type t = {
    ty : T.t;
    fields : (string * T.t) list;
    discarded : (string * T.t) list;  (* discarded fields *)
    rest : T.t HVar.t option
  }

  (* given a record term [t] with fields [l] and rest [rest],
     build a RecordUnif.t structure
     The term must have a type. *)
  let of_record t l rest =
    let r = {
      ty=T.ty_exn t;
      fields = l;
      discarded = [];
      rest;
    } in
    r

  (* convert back to a record *)
  let to_record r =
    let ty_fields = List.map (fun (name,t) -> name, T.ty_exn t) in
    (* compute remaining type *)
    let ty = if T.equal r.ty T.tType
      then T.tType
      else (
        assert (T.is_record r.ty || T.is_var r.ty);
        (* type of fields that remain *)
        T.record_flatten ~ty:T.tType
          (ty_fields r.fields @ ty_fields r.discarded)
          ~rest:(CCOpt.map HVar.ty r.rest)
      )
    in
    T.record ~ty (r.fields @ r.discarded) ~rest:r.rest

  (* discard first field *)
  let discard r = match r.fields with
    | [] -> assert false
    | (n,t)::l ->
        { r with fields=l; discarded=(n,t)::r.discarded; }

  (* discard all fields *)
  let discard_all r =
    {r with fields=[]; discarded=(r.fields @ r.discarded); }

  let set_rest r ~rest = { r with rest; }

  let occurs_check ~env subst v sc_v r sc_r =
    let check_l l =
      List.exists (fun (_,t) -> occurs_check ~env subst v sc_v t sc_r) l
    in
    begin match r.rest with
      | Some v' -> HVar.equal v v' && sc_v = sc_r
      | None -> false
    end
    || check_l r.fields || check_l r.discarded

  (* remove next field *)
  let pop_field r = match r.fields with
    | [] -> assert false
    | _::l -> { r with fields=l;  }

  let fields r = r.fields

  let _pp out r =
    let pp_list = CCFormat.list (CCFormat.pair CCFormat.string T.pp) in
    let pp_rest out x = match x with
      | None -> ()
      | Some r -> Format.fprintf out "| %a" HVar.pp r
    in
    Format.fprintf out
      "@[<hov2>record_unif {@,fields=@[%a@],@ discarded=@[%a@]@ %a@,}@]"
      pp_list r.fields pp_list r.discarded pp_rest r.rest
end

module RU = RecordUnif

(** {2 Nary-unification} *)

module Nary = struct
  type term = T.t

  (** {6 Deal with commutating binders} *)

  module Permutation = struct
    (* Set of permutations of [0... size-1] *)
    type t = {
      size : int;
      num_set : int; (* already set variables, [= size] or [< size-1] *)
      fn : (int * int) list;  (* partial mapping [length = num_set] *)
    }

    (* permutation of size 1 *)
    let singleton = {size=1; num_set=1; fn=[0,0]}

    (* new permutation of given size *)
    let make size =
      assert (size >= 1);
      if size=1 then singleton else {size; num_set=0; fn=[]}

    let rec remove_ x l = match l with
      | [] -> []
      | y :: tl when x=y -> tl
      | y :: tl -> y :: remove_ x tl

    (* if all numbers but one are set, set the last one *)
    let complete_ p =
      assert (p.num_set < p.size);
      if p.num_set + 1 = p.size
      then
        let l = CCList.(0 -- (p.size-1)) in
        let missing_x =
          l |> List.filter (fun x -> not (List.mem_assoc x p.fn))
          |> List.hd
        and missing_y =
          List.fold_left (fun l (_,y) -> remove_ y l) l p.fn |> List.hd
        in
        let fn = (missing_x, missing_y) :: p.fn in
        {p with num_set=p.size; fn}
      else p

    (* add [x->y] to the permutation *)
    let bind p x y =
      assert (0 <= x && x < p.size);
      assert (0 <= y && y < p.size);
      try
        let y' = List.assoc x p.fn in
        if y=y' then Some p else None (* not applicative *)
      with Not_found ->
        if List.exists (fun (_,y') -> y=y') p.fn
        then None (* not injective *)
        else
          let fn = (x,y) :: p.fn in
          let num_set = p.num_set + 1 in
          Some (complete_ {p with fn; num_set; })

    type db_perm =
      | Deref of int  (* permutation for db [n] is stored at db [n - deref] *)
      | Perm of t     (* partial permutation*)

    (* maintains a set of permutations of De Bruijn indices occurring under
       consecutive commutating binders *)
    type db_perms = db_perm DBE.t

    (* push one De Bruijn, easy *)
    let push_one env = DBE.push env (Perm singleton)

    (* push n consecutive quantifiers ([n >= 1]) *)
    let push_many env n =
      assert (n >= 1);
      let perm = make n in
      let rec f env n = match n with
        | 1 -> DBE.push env (Perm perm)
        | _ ->
            let env' = DBE.push env (Deref (n-1)) in
            f env' (n-1)
      in
      let env' = f env n in
      assert (DBE.size env' = n + DBE.size env);
      assert (n = 1 || DBE.find_exn env' 1 = Deref 1);
      env'

    let pop_many = DBE.pop_many

    (* permutation [n] belongs to in [env], and the offset of the permutation *)
    let rec find_perm env n = match DBE.find_exn env n with
      | Deref offset -> find_perm env (n-offset)
      | Perm set -> n, set

    (* [n1] in left term, and [n2] in right term, must correspond.
       this filters out permutations that don't fit.
       Returns either [Some env'] where [env'] satisfies [n1 = n2], or [None] *)
    let must_match env n1 n2 =
      let offset, perm = find_perm env n1 in
      CCOpt.(
        bind perm (n1-offset) (n2-offset)
        >|= fun perm' ->
        DBE.set env offset (Perm perm')
      )

    (*
    let print out env =
      let pp_perm out p =
        Format.fprintf out "@[<hv2>{perm%d:@,%a}@]" p.size
          (CCList.print ~start:"" ~stop:""
             (fun out (x,y) -> Format.fprintf out "%d→%d" x y))
          p.fn
      in
      let pp_each out = function
        | Deref i -> Format.fprintf out "@-%d" i
        | Perm p -> pp_perm out p
      in
      DBE.print pp_each out env
    *)
  end

  (* do consecutive occurrences of the binder commute with one another?
     e.g. [![X]: ![Y]: A]   is the same as  [![Y]: ![X]: A] *)
  let binder_commutes = function
    | Binder.Forall
    | Binder.Exists -> true
    | _ -> false

  type combined_env = {
    env1 : env;
    env2 : env;
    permutation : Permutation.db_perms;
  }

  let push_none env = {
    env1 = DBE.push_none env.env1;
    env2 = DBE.push_none env.env2;
    permutation = Permutation.push_one env.permutation;
  }

  let push_many env size = {
    env1 = DBE.push_none_multiple env.env1 size;
    env2 = DBE.push_none_multiple env.env2 size;
    permutation = Permutation.push_many env.permutation size;
  }

  let pop_many env n = {
    env1 = DBE.pop_many env.env1 n;
    env2 = DBE.pop_many env.env2 n;
    permutation = Permutation.pop_many env.permutation n;
  }

  let make_db_equal env n1 n2 =
    match Permutation.must_match env.permutation n1 n2 with
    | None -> None
    | Some permutation' -> Some { env with permutation=permutation' }

  (*
  let print_env out e = Permutation.print out e.permutation
  *)

  (** {6 Multisets, Lists, Records} *)

  type unif =
    env:combined_env -> S.t -> T.t Scoped.t -> T.t Scoped.t ->
    (env:combined_env -> subst -> unit) ->
    unit

  (* unify lists pairwise *)
  let rec unify_list_ ~env ~(unif:unif) subst l1 sc1 l2 sc2 k =
    match l1, l2 with
    | [], [] -> k ~env subst
    | [], _
    | _, [] -> ()
    | t1::l1', t2::l2' ->
        unif ~env subst t1 sc1 t2 sc2
          (fun ~env subst -> unify_list_ ~env ~unif subst l1' sc1 l2' sc2 k)

  (* unify the row variables, if any, with the unmatched columns of each term.
      we first unify the record composed of discard fields of r1, with
      the row of r2, and then conversely.*)
  let unify_record_rest_ ~env subst r1 sc1 r2 sc2 k =
    assert (r1.RU.fields = []);
    assert (r2.RU.fields = []);
    (* Util.debugff 5 "@[<hv2>unify_rec_rest@ %a and@ %a with@ %a@]"
       RU._pp r1 RU._pp r2 S.fmt subst; *)
    (* Util.debugf 5 "unif_rest %a %a" T.pp (RU.to_record r1) T.pp (RU.to_record r2); *)
    match r1.RU.rest, r1.RU.discarded, r2.RU.rest, r2.RU.discarded with
    | None, [], None, [] ->
        k ~env subst (* no row, no remaining fields *)
    | None, _, _, _::_
    | _, _::_, None, _ ->
        (* must unify an empty rest against a non-empty set of discarded
           fields, that is impossible *)
        ()
    | Some rest1, [], _, _ ->
        if RU.occurs_check ~env subst rest1 sc1 r2 sc2 then fail();
        (* no discarded fields in r1, so we only need to
           unify rest1 with { l2 | rest2 } *)
        k ~env (S.bind subst (rest1, sc1) (RU.to_record r2, sc2))
    | _, _, Some rest2, [] ->
        (* symmetric case of the previous one *)
        if RU.occurs_check ~env subst rest2 sc2 r1 sc1 then fail ();
        k ~env (S.bind subst (rest2, sc2) (RU.to_record r1, sc1))
    | Some rest1, d1, Some rest2, d2 ->
        if HVar.equal rest1 rest2 && sc1 = sc2
        then (* can only unify if common set of fields --> no discarded *)
          if d1=[] && d2=[] then k ~env subst else ()
        else
          (* create fresh var R and unify rest1 with {l2 | R} (t2)
             and rest2 with { l1 | R } (t1) *)
          let r = HVar.fresh ~ty:r1.RU.ty () in
          let t1 = RU.set_rest r1 ~rest:(Some r) in
          let t2 = RU.set_rest r2 ~rest:(Some r) in
          if RU.occurs_check ~env subst rest1 sc1 t2 sc2
          || RU.occurs_check ~env subst rest2 sc2 t1 sc1 then ()
          else (
            let subst = S.bind subst (rest1,sc1) (RU.to_record t2,sc2) in
            let subst = S.bind subst (rest2,sc2) (RU.to_record t1,sc1) in
            k ~env subst
          )

  (* unify the two records *)
  let rec unify_records_ ~env ~unif subst r1 sc1 r2 sc2 k =
    (* Util.debugff 5 "@[<hv2>unify_rec@ %a and@ %a with@ %a@]"
        RU._pp r1 RU._pp r2 S.fmt subst; *)
    match RU.fields r1, RU.fields r2 with
    | [], _
    | _, [] ->
        let r1 = RU.discard_all r1 in
        let r2 = RU.discard_all r2 in
        unify_record_rest_ ~env subst r1 sc1 r2 sc2 k
    | (n1,t1)::_, (n2,t2)::_ ->
        begin match String.compare n1 n2 with
          | 0 ->
              unif ~env subst t1 sc1 t2 sc2
                (fun ~env subst ->
                   let r1' = RU.pop_field r1 in
                   let r2' = RU.pop_field r2 in
                   unify_records_ ~env ~unif subst r1' sc1 r2' sc2 k)
          | n when n < 0 ->
              (* n1 too small, ditch it into l1' *)
              let r1' = RU.discard r1 in
              unify_records_ ~env ~unif subst r1' sc1 r2 sc2 k
          | _ ->
              (* n2 too small, ditch it into l2' *)
              let r2' = RU.discard r2 in
              unify_records_ ~env ~unif subst r1 sc1 r2' sc2 k
        end

  (* unify multisets pairwise. Precondition: l1 and l2 have the same length. *)
  let rec unify_multiset_ ~env ~unif subst l1 sc1 l2 sc2 k =
    match l2 with
    | [] -> k ~env subst (* success! *)
    | t2::l2' ->
        (* need to unify some element of [l1] with [t2] first *)
        __choose_pair ~env ~unif subst [] l1 sc1 t2 l2' sc2 k
  (* choose an element of [l1] to unify with the head of [l2] *)
  and __choose_pair ~env ~unif subst left right sc1 t2 l2' sc2 k =
    match right with
    | [] -> ()   (* exhausted all possibilities *)
    | t1::right' ->
        unif ~env subst t1 sc1 t2 sc2
          (fun ~env subst ->
             (* upon success, unify remaining pairs *)
             unify_multiset_ ~env ~unif subst (left@right') sc1 l2' sc2 k);
        (* also try to unify [t2] with another term of [right] *)
        __choose_pair ~env ~unif subst (t1::left) right' sc1 t2 l2' sc2 k

  let rec unif_commutating_binders_ ~env ~unif ~sym ~size subst t1 sc_s t2 sc_t k =
    match T.view t1, T.view t2 with
    | T.Bind (s1, _, t1'), T.Bind (s2, _, t2')
      when Binder.equal s1 sym && Binder.equal s2 sym ->
        (* recurse *)
        unif_commutating_binders_ ~env ~unif ~sym
          ~size:(size+1) subst t1' sc_s t2' sc_t k
    | _ ->
        assert (size > 0);
        (* push permutation *)
        let env = push_many env size in
        unif ~env subst t1 sc_s t2 sc_t
          (fun ~env subst -> k ~env:(pop_many env size) subst)

  (** {6 Unification, Matching, Comparison} *)

  type op =
    | Unify
    | Match
    | AlphaEquiv

  let rec unif_rec_ ~env ~op subst s t k : unit =
    Util.debugf 5 "@[<hv2>unif:@ @[%a@]@ =?=@ @[%a@]@ with @[%a@]@]"
      (fun k->k (S.pp_scoped T.pp) s (S.pp_scoped T.pp) t S.pp subst);
    let s = S.deref subst s
    and t = S.deref subst t in
    (* unify types. On success, unify terms. *)
    match T.ty (fst s), T.ty (fst t) with
    | T.NoType, T.NoType -> unif_terms_rec_ ~env ~op subst s t k
    | T.NoType, _
    | _, T.NoType -> ()
    | T.HasType ty1, T.HasType ty2 ->
        unif_rec_ ~env ~op subst (ty1, snd s) (ty2, snd t)
          (fun ~env subst -> unif_terms_rec_ ~env subst s t k)
  and unif_terms_rec_ ~env ~op subst s t k =
    match T.view (fst s), T.view (fst t) with
    | _ when T.equal (fst s) (fst t) && (snd s = snd t || T.is_ground s) ->
        k ~env subst (* the terms are equal under any substitution *)
    | T.Var _, _ ->
        if occurs_check ~env:env.env2 subst s sc_s t sc_t || not (T.DB.closed t)
        then () (* occur check *)
        else k ~env (S.bind subst s sc_s t sc_t)
    | _, T.Var _ ->
        if occurs_check ~env:env.env1 subst t sc_t s sc_s || not (T.DB.closed s)
        then () (* occur check *)
        else k ~env (S.bind subst t sc_t s sc_s)
    | T.Bind (s1, _, _), T.Bind (s2, _, _)
      when Binder.equal s1 s2 && binder_commutes s1 ->
        (* forall or exists: they might swap *)
        unif_commutating_binders_ ~env ~unif_rec_ ~sym:s1 ~size:0 subst s sc_s t sc_t k
    | T.Bind (s1, varty1, t1'), T.Bind (s2, varty2, t2') when Binder.equal s1 s2 ->
        unif_rec_ ~env subst varty1 sc_s varty2 sc_t
          (fun ~env subst -> unif_rec_ ~env:(push_none env) subst t1' sc_s t2' sc_t
              (fun ~env subst -> k ~env:(pop_many env 1) subst))
    | T.BVar i, _ when DBE.mem env.env1 i ->
        unif_rec_ ~env subst (DBE.find_exn env.env1 i) sc_s t sc_t k
    | _, T.BVar j when DBE.mem env.env2 j ->
        unif_rec_ ~env subst s sc_s (DBE.find_exn env.env2 j) sc_t k
    | T.BVar i, T.BVar j ->
        begin match make_db_equal env i j with
          | None -> ()
          | Some env' -> k ~env:env' subst
        end
    | T.Const f, T.Const g when Symbol.equal f g -> k ~env subst
    | T.App (hd1, l1), T.App (hd2, l2) when List.length l1 = List.length l2 ->
        unif_rec_ ~env subst hd1 sc_s hd2 sc_t
          (fun ~env subst -> unify_list_ ~env ~unif_rec_ subst l1 sc_s l2 sc_t k)
    | T.Record (l1, rest1), T.Record (l2, rest2) ->
        let r1 = RU.of_record s l1 rest1 in
        let r2 = RU.of_record t l2 rest2 in
        unify_records_ ~env ~unif_rec_ subst r1 sc_s r2 sc_t k
    | T.RecordGet (r1, name1), T.RecordGet (r2, name2) when name1=name2 ->
        unif_rec_ ~env subst r1 sc_s r2 sc_t k
    | T.RecordSet (r1,name1,sub1), T.RecordSet(r2,name2,sub2) when name1=name2 ->
        unif_rec_ ~env subst r1 sc_s r2 sc_t
          (fun ~env subst -> unif_rec_ ~env subst sub1 sc_s sub2 sc_t k)
    | T.SimpleApp(s1,l1), T.SimpleApp (s2, l2) when Symbol.equal s1 s2 ->
        unify_list_ ~env ~unif_rec_ subst l1 sc_s l2 sc_t k
    | T.AppBuiltin (s1,l1), T.AppBuiltin (s2, l2) when Builtin.equal s1 s2 ->
        unify_list_ ~env ~unif_rec_ subst l1 sc_s l2 sc_t k
    | T.Multiset l1, T.Multiset l2 when List.length l1 = List.length l2 ->
        unify_multiset_ ~env ~unif_rec_ subst l1 sc_s l2 sc_t k
    | T.At (l1, r1), T.At (l2, r2) ->
        unif_rec_ ~env subst l1 sc_s l2 sc_t
          (fun ~env subst -> unif_rec_ ~env subst r1 sc_s r2 sc_t k)
    | _, _ -> ()  (* fail *)

  let unification ?(env1=DBE.empty) ?(env2=DBE.empty) ?(subst=S.empty) a sc_a b sc_b =
    let env = {env1; env2; permutation=DBE.empty; } in
    fun k -> unif_rec_ ~env ~op:Unify subst a sc_a b sc_b (fun ~env:_ subst -> k subst)

  let matching ?(allow_open=false)?(env1=DBE.empty) ?(env2=DBE.empty)
      ?(subst=S.empty) ~pattern sc_pattern b sc_b =
    let rec unif ~env subst s sc_s t sc_t k =
      let s, sc_s = S.get_var subst s sc_s
      and t, sc_t = S.get_var subst t sc_t in
      (* unify types. On success, unify terms. *)
      match T.ty s, T.ty t with
      | T.NoType, T.NoType ->
          unif_terms ~env subst s sc_s t sc_t k
      | T.NoType, _
      | _, T.NoType -> ()
      | T.HasType ty1, T.HasType ty2 ->
          unif ~env subst ty1 sc_s ty2 sc_t
            (fun ~env subst -> unif_terms ~env subst s sc_s t sc_t k)
    and unif_terms ~env subst s sc_s t sc_t k =
      match T.view s, T.view t with
      | _ when T.equal s t && (T.ground s || sc_s = sc_t) ->
          k ~env subst  (* the terms are equal under any substitution *)
      | _ when T.ground s && T.ground t && T.DB.closed t ->
          () (* terms are not equal, and ground. failure. *)
      | T.Var _, _ ->
          if occurs_check ~env:env.env2 subst s sc_s t sc_t || sc_s = sc_t
             || (not allow_open && not (T.DB.closed t))
          then ()
          else k ~env (S.bind subst s sc_s t sc_t)  (* bind s *)
      | T.Bind (s1, _, _), T.Bind (s2, _, _)
        when Binder.equal s1 s2 && binder_commutes s1 ->
          (* forall or exists: they might swap *)
          unif_commutating_binders_ ~env ~unif ~sym:s1 ~size:0 subst s sc_s t sc_t k
      | T.Bind (s1, varty1, t1'), T.Bind (s2, varty2, t2') when Binder.equal s1 s2 ->
          unif ~env subst varty1 sc_s varty2 sc_t
            (fun ~env subst -> unif ~env:(push_none env) subst t1' sc_s t2' sc_t
                (fun ~env subst -> k ~env:(pop_many env 1) subst))
      | T.BVar i, _ when DBE.mem env.env1 i ->
          unif ~env subst (DBE.find_exn env.env1 i) sc_s t sc_t k
      | _, T.BVar j when DBE.mem env.env2 j ->
          unif ~env subst s sc_s (DBE.find_exn env.env2 j) sc_t k
      | T.BVar i, T.BVar j when i = j -> k ~env subst
      | T.Const f, T.Const g when Symbol.equal f g -> k ~env subst
      | T.App (hd1, l1), T.App (hd2, l2) when List.length l1 = List.length l2 ->
          unif ~env subst hd1 sc_s hd2 sc_t
            (fun ~env subst -> unify_list_ ~env ~unif subst l1 sc_s l2 sc_t k)
      | T.Record (l1, rest1), T.Record (l2, rest2) ->
          let r1 = RU.of_record s l1 rest1 in
          let r2 = RU.of_record t l2 rest2 in
          unify_records_ ~env ~unif subst r1 sc_s r2 sc_t k
      | T.RecordGet (r1, name1), T.RecordGet (r2, name2) when name1=name2 ->
          unif ~env subst r1 sc_s r2 sc_t k
      | T.RecordSet (r1,name1,sub1), T.RecordSet(r2,name2,sub2) when name1=name2 ->
          unif ~env subst r1 sc_s r2 sc_t
            (fun ~env subst -> unif ~env subst sub1 sc_s sub2 sc_t k)
      | T.SimpleApp (s1,l1), T.SimpleApp (s2, l2) when Symbol.equal s1 s2 ->
          unify_list_ ~env ~unif subst l1 sc_s l2 sc_t k
      | T.AppBuiltin (s1,l1), T.AppBuiltin (s2, l2) when Builtin.equal s1 s2 ->
          unify_list_ ~env ~unif subst l1 sc_s l2 sc_t k
      | T.Multiset l1, T.Multiset l2 when List.length l1 = List.length l2 ->
          unify_multiset_ ~env ~unif subst l1 sc_s l2 sc_t k
      | T.At (l1, r1), T.At (l2, r2) ->
          unif ~env subst l1 sc_s l2 sc_t
            (fun ~env subst -> unif ~env subst r1 sc_s r2 sc_t k)
      | _, _ -> ()  (* fail *)
    in
    let env = {env1; env2; permutation=DBE.empty;} in
    fun k -> unif ~env subst pattern sc_pattern b sc_b (fun ~env:_ s -> k s)

  let variant ?(env1=DBE.empty) ?(env2=DBE.empty) ?(subst=S.empty) a sc_a b sc_b =
    let rec unif ~env subst s sc_s t sc_t k =
      let s, sc_s = S.get_var subst s sc_s
      and t, sc_t = S.get_var subst t sc_t in
      (* unify types. On success, unify terms. *)
      match T.ty s, T.ty t with
      | T.NoType, T.NoType ->
          unif_terms ~env subst s sc_s t sc_t k
      | T.NoType, _
      | _, T.NoType -> ()
      | T.HasType ty1, T.HasType ty2 ->
          unif ~env subst ty1 sc_s ty2 sc_t
            (fun ~env subst  -> unif_terms ~env subst s sc_s t sc_t k)
    and unif_terms ~env subst s sc_s t sc_t k =
      match T.view s, T.view t with
      | _ when T.equal s t && (T.ground s || sc_s = sc_t) ->
          k ~env subst  (* the terms are equal under any substitution *)
      | _ when T.ground s && T.ground t && T.DB.closed s ->
          () (* terms are not equal, and ground. failure. *)
      | T.Var i, T.Var j when i <> j && sc_s = sc_t -> ()
      | T.Var _, T.Var _ when sc_s <> sc_t ->
          k ~env (S.bind subst s sc_s t sc_t)  (* bind s *)
      | T.Bind (s1, _, _), T.Bind (s2, _, _)
        when Binder.equal s1 s2 && binder_commutes s1 ->
          (* forall or exists: they might swap *)
          unif_commutating_binders_ ~env ~unif ~sym:s1 ~size:0 subst s sc_s t sc_t k
      | T.Bind (s1, varty1, t1'), T.Bind (s2, varty2, t2') when Binder.equal s1 s2 ->
          unif ~env subst varty1 sc_s varty2 sc_t
            (fun ~env subst -> unif ~env:(push_none env) subst t1' sc_s t2' sc_t
                (fun ~env subst -> k ~env:(pop_many env 1) subst))
      | T.BVar i, _ when DBE.mem env.env1 i ->
          unif ~env subst (DBE.find_exn env.env1 i) sc_s t sc_t k
      | _, T.BVar j when DBE.mem env.env2 j ->
          unif ~env subst s sc_s (DBE.find_exn env.env2 j) sc_t k
      | T.BVar i, T.BVar j when i = j -> k ~env subst
      | T.Const f, T.Const g when Symbol.equal f g -> k ~env subst
      | T.App (hd1, l1), T.App (hd2, l2) when List.length l1 = List.length l2 ->
          unif ~env subst hd1 sc_s hd2 sc_t
            (fun ~env subst  -> unify_list_ ~env ~unif subst l1 sc_s l2 sc_t k)
      | T.Record (l1, rest1), T.Record (l2, rest2) ->
          let r1 = RU.of_record s l1 rest1 in
          let r2 = RU.of_record t l2 rest2 in
          unify_records_ ~env ~unif subst r1 sc_s r2 sc_t k
      | T.RecordGet (r1, name1), T.RecordGet (r2, name2) when name1=name2 ->
          unif ~env subst r1 sc_s r2 sc_t k
      | T.RecordSet (r1,name1,sub1), T.RecordSet(r2,name2,sub2) when name1=name2 ->
          unif ~env subst r1 sc_s r2 sc_t
            (fun ~env subst  -> unif ~env subst sub1 sc_s sub2 sc_t k)
      | T.SimpleApp (s1,l1), T.SimpleApp (s2, l2) when Symbol.equal s1 s2 ->
          unify_list_ ~env ~unif subst l1 sc_s l2 sc_t k
      | T.AppBuiltin (s1,l1), T.AppBuiltin (s2, l2) when Builtin.equal s1 s2 ->
          unify_list_ ~env ~unif subst l1 sc_s l2 sc_t k
      | T.Multiset l1, T.Multiset l2 when List.length l1 = List.length l2 ->
          unify_multiset_ ~env ~unif subst l1 sc_s l2 sc_t k
      | T.At (l1, r1), T.At (l2, r2) ->
          unif ~env subst l1 sc_s l2 sc_t
            (fun ~env subst  -> unif ~env subst r1 sc_s r2 sc_t k)
      | _, _ -> ()  (* fail *)
    in
    let env = {env1; env2; permutation=DBE.empty} in
    fun k -> unif ~env subst a sc_a b sc_b (fun ~env:_ s -> k s)

  let are_variant t1 t2 =
    not (Sequence.is_empty (variant t1 0 t2 1))

  let matches ~pattern t =
    not (Sequence.is_empty (matching ~pattern 0 t 1))

  let are_unifiable t1 t2 =
    not (Sequence.is_empty (unification t1 0 t2 1))
end

(** {2 Unary Unification} *)

module Unary = struct
  type term = T.t

  (* unify lists using the given "unificator" and continuation [k] *)
  let rec unif_list ~env1 ~env2 ~unif subst l1 sc1 l2 sc2 = match l1, l2 with
    | [], [] -> subst
    | _, []
    | [], _ -> fail ()
    | t1::l1', t2::l2' ->
        let subst = unif ~env1 ~env2 subst t1 sc1 t2 sc2 in
        unif_list ~env1 ~env2 ~unif subst l1' sc1 l2' sc2

  (* unify/match records together.
     l1, l2: to unify; l1', l2': to unify with rest1, rest2.
     we use the fact that l1 and l2 are sorted w.r.t keys. *)
  let rec unif_records ~env1 ~env2 ~unif subst r1 sc1 r2 sc2 =
    match RU.fields r1, RU.fields r2 with
    | [], _
    | _, [] ->
        let r1 = RU.discard_all r1 in
        let r2 = RU.discard_all r2 in
        unif_rest_ ~env1 ~env2 ~unif subst r1 sc1 r2 sc2
    | (n1,t1)::_, (n2,t2)::_ ->
        begin match String.compare n1 n2 with
          | 0 ->
              let subst = unif ~env1 ~env2 subst t1 sc1 t2 sc2 in
              let r1 = RU.pop_field r1 in
              let r2 = RU.pop_field r2 in
              unif_records ~env1 ~env2 ~unif subst r1 sc1 r2 sc2
          | n when n < 0 ->
              (* n1 too small, ditch it into l1' *)
              let r1 = RU.discard r1 in
              unif_records ~env1 ~env2 ~unif subst r1 sc1 r2 sc2
          | _ ->
              (* n2 too small, ditch it into l1' *)
              let r2 = RU.discard r2 in
              unif_records ~env1 ~env2 ~unif subst r1 sc1 r2 sc2
        end
  (* unify the row variables, if any, with the unmatched columns of each term.
      we first unify the record composed of discard fields of r1, with
      the row of r2, and then conversely. *)
  and unif_rest_ ~env1 ~env2 ~unif subst r1 sc1 r2 sc2 =
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
        unif ~env1 ~env2 subst rest1 sc1 t2 sc2
    | _, _, Some rest2, [] ->
        (* symmetric case of the previous one *)
        let t1 = RU.to_record r1 in
        unif ~env1 ~env2 subst t1 sc1 rest2 sc2
    | Some rest1, d1, Some rest2, d2 ->
        let rest1, sc1 = S.get_var subst rest1 sc1 in
        let rest2, sc2 = S.get_var subst rest2 sc2 in
        if T.is_var rest1 && T.equal rest1 rest2 && sc1=sc2
        then if d1=[] && d2=[]
          then subst
          else raise Fail (* impossible to unify discarded fields *)
        else
          (* create fresh var R and unify rest1 with {l2 | R}
           * and rest2 with { l1 | R } *)
          let r = T.fresh_var ~ty:r1.RU.ty () in
          let t1 = RU.to_record (RU.set_rest r1 ~rest:(Some r)) in
          let t2 = RU.to_record (RU.set_rest r1 ~rest:(Some r)) in
          let subst = unif ~env1 ~env2 subst rest1 sc1 t2 sc2 in
          let subst = unif ~env1 ~env2 subst t1 sc1 rest2 sc2 in
          subst

  let unification ?(env1=DBE.empty) ?(env2=DBE.empty) ?(subst=S.empty) a sc_a b sc_b =
    Util.enter_prof prof_unify;
    (* recursive unification *)
    let rec unif ~env1 ~env2 subst s sc_s t sc_t =
      let s, sc_s = S.get_var subst s sc_s
      and t, sc_t = S.get_var subst t sc_t in
      (* first, unify types *)
      let subst = match T.ty s, T.ty t with
        | T.NoType, T.NoType -> subst
        | T.NoType, _
        | _, T.NoType -> raise Fail
        | T.HasType ty1, T.HasType ty2 ->
            unif ~env1 ~env2 subst ty1 sc_s ty2 sc_t
      in
      match T.view s, T.view t with
      | _ when T.equal s t && (T.ground s || sc_s = sc_t) ->
          subst (* the terms are equal under any substitution *)
      | _ when T.ground s && T.ground t && T.DB.closed s ->
          raise Fail (* terms are not equal, and ground. failure. *)
      | T.Var _, _ ->
          if occurs_check ~env:env2 subst s sc_s t sc_t || not (T.DB.closed t)
          then raise Fail (* occur check *)
          else S.bind subst s sc_s t sc_t (* bind s *)
      | _, T.Var _ ->
          if occurs_check ~env:env1 subst t sc_t s sc_s || not (T.DB.closed s)
          then raise Fail (* occur check *)
          else S.bind subst t sc_t s sc_s (* bind s *)
      | T.Bind (s1, varty1, t1'), T.Bind (s2, varty2, t2') when Binder.equal s1 s2 ->
          let subst = unif ~env1 ~env2 subst varty1 sc_s varty2 sc_t in
          unif ~env1:(DBE.push_none env1) ~env2:(DBE.push_none env2)
            subst t1' sc_s t2' sc_t
      | T.BVar i, _ when DBE.mem env1 i ->
          unif ~env1:DBE.empty ~env2 subst (DBE.find_exn env1 i) sc_s t sc_t
      | _, T.BVar j when DBE.mem env2 j ->
          unif ~env1 ~env2:DBE.empty subst s sc_s (DBE.find_exn env2 j) sc_t
      | T.BVar i, T.BVar j -> if i = j then subst else raise Fail
      | T.Const f, T.Const g when Symbol.equal f g -> subst
      | T.App (hd1, l1), T.App (hd2, l2) when List.length l1 = List.length l2 ->
          let subst = unif ~env1 ~env2 subst hd1 sc_s hd2 sc_t in
          unif_list ~unif ~env1 ~env2 subst l1 sc_s l2 sc_t
      | T.Record (l1, rest1), T.Record (l2, rest2) ->
          let r1 = RU.of_record s l1 rest1 in
          let r2 = RU.of_record t l2 rest2 in
          unif_records ~unif ~env1 ~env2 subst r1 sc_s r2 sc_t
      | T.RecordGet (r1, name1), T.RecordGet (r2, name2) when name1=name2 ->
          unif ~env1 ~env2 subst r1 sc_s r2 sc_t
      | T.RecordSet (r1,name1,sub1), T.RecordSet(r2,name2,sub2) when name1=name2 ->
          let subst = unif ~env1 ~env2 subst r1 sc_s r2 sc_t in
          unif ~env1 ~env2 subst sub1 sc_s sub2 sc_t
      | T.SimpleApp (s1,l1), T.SimpleApp (s2, l2) when Symbol.equal s1 s2 ->
          unif_list ~unif ~env1 ~env2 subst l1 sc_s l2 sc_t
      | T.AppBuiltin (s1,l1), T.AppBuiltin (s2, l2) when Builtin.equal s1 s2 ->
          unif_list ~unif ~env1 ~env2 subst l1 sc_s l2 sc_t
      | T.At (l1, r1), T.At (l2, r2) ->
          let subst = unif ~env1 ~env2 subst l1 sc_s l2 sc_t in
          unif ~env1 ~env2 subst r1 sc_s r2 sc_t
      | _, _ -> raise Fail
    in
    (* try unification, and return solution/exception (with profiler handling) *)
    try
      let subst = unif ~env1 ~env2 subst a sc_a b sc_b in
      Util.exit_prof prof_unify;
      subst
    with e ->
      Util.exit_prof prof_unify;
      raise e

  let matching ?(allow_open=false) ?(env1=DBE.empty) ?(env2=DBE.empty)
      ?(subst=S.empty) ~pattern sc_a b sc_b =
    Util.enter_prof prof_matching;
    (* recursive matching *)
    let rec unif ~env1 ~env2 subst s sc_s t sc_t =
      let s, sc_s = S.get_var subst s sc_s in
      let t, sc_t = S.get_var subst t sc_t in
      let subst = match T.ty s, T.ty t with
        | T.NoType, T.NoType -> subst
        | T.NoType, _
        | _, T.NoType -> raise Fail
        | T.HasType ty1, T.HasType ty2 ->
            unif ~env1 ~env2 subst ty1 sc_s ty2 sc_t
      in
      match T.view s, T.view t with
      | _ when T.equal s t && (T.ground s || sc_s = sc_t) ->
          subst (* the terms are equal under any substitution *)
      | _ when T.ground s && T.ground t && T.DB.closed s ->
          raise Fail (* terms are not equal, and ground. failure. *)
      | T.Var _, _ ->
          if occurs_check ~env:env2 subst s sc_s t sc_t || sc_s = sc_t
             || (not allow_open && not (T.DB.closed t))
          then raise Fail
          else S.bind subst s sc_s t sc_t (* bind s *)
      | T.Bind (s1, varty1, t1'), T.Bind (s2, varty2, t2') when Binder.equal s1 s2 ->
          let subst = unif ~env1 ~env2 subst varty1 sc_s varty2 sc_t in
          unif ~env1:(DBE.push_none env1) ~env2:(DBE.push_none env2)
            subst t1' sc_s t2' sc_t
      | T.BVar i, _ when DBE.mem env1 i ->
          unif ~env1:DBE.empty ~env2 subst (DBE.find_exn env1 i) sc_s t sc_t
      | _, T.BVar j when DBE.mem env2 j ->
          unif ~env1 ~env2:DBE.empty subst s sc_s (DBE.find_exn env2 j) sc_t
      | T.BVar i, T.BVar j -> if i = j then subst else raise Fail
      | T.Const f, T.Const g when Symbol.equal f g -> subst
      | T.App (f1, l1), T.App (f2, l2) when List.length l1 = List.length l2 ->
          let subst = unif ~env1 ~env2 subst f1 sc_s f2 sc_t in
          unif_list ~unif ~env1 ~env2 subst l1 sc_s l2 sc_t
      | T.Record (l1, rest1), T.Record (l2, rest2) ->
          let r1 = RU.of_record s l1 rest1 in
          let r2 = RU.of_record t l2 rest2 in
          unif_records ~unif ~env1 ~env2 subst r1 sc_s r2 sc_t
      | T.RecordGet (r1, name1), T.RecordGet (r2, name2) when name1=name2 ->
          unif ~env1 ~env2 subst r1 sc_s r2 sc_t
      | T.RecordSet (r1,name1,sub1), T.RecordSet(r2,name2,sub2) when name1=name2 ->
          let subst = unif ~env1 ~env2 subst r1 sc_s r2 sc_t in
          unif ~env1 ~env2 subst sub1 sc_s sub2 sc_t
      | T.SimpleApp(s1,l1), T.SimpleApp (s2, l2) when Symbol.equal s1 s2 ->
          unif_list ~unif ~env1 ~env2 subst l1 sc_s l2 sc_t
      | T.AppBuiltin (s1,l1), T.AppBuiltin (s2, l2) when Builtin.equal s1 s2 ->
          unif_list ~unif ~env1 ~env2 subst l1 sc_s l2 sc_t
      | T.At (l1, r1), T.At (l2, r2) ->
          let subst = unif ~env1 ~env2 subst l1 sc_s l2 sc_t in
          unif ~env1 ~env2 subst r1 sc_s r2 sc_t
      | _, _ -> raise Fail
    in
    try
      let subst = unif ~env1 ~env2 subst pattern sc_a b sc_b in
      Util.exit_prof prof_matching;
      subst
    with e ->
      Util.exit_prof prof_matching;
      raise e

  let matching_same_scope ?(env1=DBE.empty) ?(env2=DBE.empty) ?(protect=Sequence.empty) ?(subst=S.empty)
      ~scope ~pattern b =
    (* recursive matching. Blocked vars are [blocked] *)
    let rec unif ~env1 ~env2 ~blocked subst s _scope t scope  =
      let s, sc_s = S.get_var subst s scope in
      let t, sc_t = S.get_var subst t scope in
      let subst = match T.ty s, T.ty t with
        | T.NoType, T.NoType -> subst
        | T.NoType, _
        | _, T.NoType -> raise Fail
        | T.HasType ty1, T.HasType ty2 ->
            unif ~env1 ~env2 ~blocked subst ty1 scope ty2 scope
      in
      match T.view s, T.view t with
      | _ when T.equal s t ->
          subst (* the terms are equal under any substitution *)
      | _ when T.ground s && T.ground t  && T.DB.closed s->
          raise Fail (* terms are not equal, and ground. failure. *)
      | T.Var _, _ ->
          if occurs_check ~env:env2 subst s scope t scope || T.Set.mem s blocked
          then raise Fail
          else S.bind subst s scope t scope (* bind s *)
      | T.Bind (s1, varty1, t1'), T.Bind (s2, varty2, t2') when Binder.equal s1 s2 ->
          let subst = unif ~env1 ~env2 ~blocked subst varty1 sc_s varty2 sc_t in
          unif
            ~env1:(DBE.push_none env1) ~env2:(DBE.push_none env2) ~blocked
            subst t1' scope t2' scope
      | T.BVar i, _ when DBE.mem env1 i ->
          unif ~env1:DBE.empty ~env2 ~blocked
            subst (DBE.find_exn env1 i) scope t scope
      | _, T.BVar j when DBE.mem env2 j ->
          unif ~env1 ~env2:DBE.empty ~blocked
            subst s scope (DBE.find_exn env2 j) scope
      | T.BVar i, T.BVar j -> if i = j then subst else raise Fail
      | T.Const f, T.Const g when Symbol.equal f g -> subst
      | T.App (f1, l1), T.App (f2, l2) when List.length l1 = List.length l2 ->
          let subst = unif ~env1 ~env2 ~blocked subst f1 scope f2 scope in
          unif_list ~env1 ~env2 ~unif:(unif ~blocked) subst l1 scope l2 scope
      | T.Record (l1, rest1), T.Record (l2, rest2) ->
          let r1 = RU.of_record s l1 rest1 in
          let r2 = RU.of_record t l2 rest2 in
          unif_records ~env1 ~env2 ~unif:(unif ~blocked) subst r1 scope r2 scope
      | T.RecordGet (r1, name1), T.RecordGet (r2, name2) when name1=name2 ->
          unif ~env1 ~env2 ~blocked subst r1 sc_s r2 sc_t
      | T.RecordSet (r1,name1,sub1), T.RecordSet(r2,name2,sub2) when name1=name2 ->
          let subst = unif ~env1 ~env2 ~blocked subst r1 sc_s r2 sc_t in
          unif ~env1 ~env2 ~blocked subst sub1 sc_s sub2 sc_t
      | T.SimpleApp (s1,l1), T.SimpleApp (s2, l2) when Symbol.equal s1 s2 ->
          unif_list ~env1 ~env2 ~unif:(unif ~blocked) subst l1 scope l2 scope
      | T.AppBuiltin (s1,l1), T.AppBuiltin (s2, l2) when Builtin.equal s1 s2 ->
          unif_list ~env1 ~env2 ~unif:(unif ~blocked) subst l1 scope l2 scope
      | T.At (l1, r1), T.At (l2, r2) ->
          let subst = unif ~env1 ~env2 ~blocked subst l1 scope l2 scope in
          unif ~env1 ~env2 ~blocked subst r1 scope r2 scope
      | _, _ -> raise Fail
    in
    (* compute set of free vars of [b], that cannot be bound *)
    let protect = Sequence.append protect (T.Seq.vars b) in
    let blocked = T.Seq.add_set T.Set.empty protect in
    let subst = unif ~env1 ~env2 ~blocked subst pattern scope b scope in
    subst

  let matching_adapt_scope ?(env1=DBE.empty) ?(env2=DBE.empty) ?protect
      ?(subst=S.empty) ~pattern s_p t s_t =
    if s_p = s_t
    then matching_same_scope ~env1 ~env2 ?protect ~subst ~scope:s_p ~pattern t
    else matching ~env1 ~env2 ~subst ~pattern s_p t s_t

  let variant ?(env1=DBE.empty) ?(env2=DBE.empty) ?(subst=S.empty) a sc_a b sc_b =
    (* recursive variant checking *)
    let rec unif ~env1 ~env2 subst s sc_s t sc_t =
      let s, sc_s = S.get_var subst s sc_s in
      let t, sc_t = S.get_var subst t sc_t in
      let subst = match T.ty s, T.ty t with
        | T.NoType, T.NoType -> subst
        | T.NoType, _
        | _, T.NoType -> raise Fail
        | T.HasType ty1, T.HasType ty2 ->
            unif ~env1 ~env2 subst ty1 sc_s ty2 sc_t
      in
      match T.view s, T.view t with
      | _ when s == t && (T.ground s || sc_s = sc_t) ->
          subst (* the terms are equal under any substitution *)
      | _ when T.ground s && T.ground t && T.DB.closed s ->
          raise Fail (* terms are not equal, and ground. failure. *)
      | T.Var i, T.Var j when i <> j && sc_s = sc_t -> raise Fail
      | T.Var _, T.Var _ -> S.bind subst s sc_s t sc_t (* bind s *)
      | T.Bind (s1, varty1, t1'), T.Bind (s2, varty2, t2') when Binder.equal s1 s2 ->
          let subst = unif ~env1 ~env2 subst varty1 sc_s varty2 sc_t in
          unif ~env1:(DBE.push_none env1) ~env2:(DBE.push_none env2)
            subst t1' sc_s t2' sc_t
      | T.BVar i, _ when DBE.mem env1 i ->
          unif ~env1:DBE.empty ~env2 subst (DBE.find_exn env1 i) sc_s t sc_t
      | _, T.BVar j when DBE.mem env2 j ->
          unif ~env1 ~env2:DBE.empty subst s sc_s (DBE.find_exn env2 j) sc_t
      | T.BVar i, T.BVar j -> if i = j then subst else raise Fail
      | T.Const f, T.Const g when Symbol.equal f g -> subst
      | T.App (t1, l1), T.App (t2, l2) when List.length l1 = List.length l2 ->
          let subst = unif ~env1 ~env2 subst t1 sc_s t2 sc_t in
          unif_list ~unif ~env1 ~env2 subst l1 sc_s l2 sc_t
      | T.Record (l1, rest1), T.Record (l2, rest2) ->
          let r1 = RU.of_record s l1 rest1 in
          let r2 = RU.of_record t l2 rest2 in
          unif_records ~unif ~env1 ~env2 subst r1 sc_s r2 sc_t
      | T.RecordGet (r1, name1), T.RecordGet (r2, name2) when name1=name2 ->
          unif ~env1 ~env2 subst r1 sc_s r2 sc_t
      | T.RecordSet (r1,name1,sub1), T.RecordSet(r2,name2,sub2) when name1=name2 ->
          let subst = unif ~env1 ~env2 subst r1 sc_s r2 sc_t in
          unif ~env1 ~env2 subst sub1 sc_s sub2 sc_t
      | T.AppBuiltin (s1,l1), T.AppBuiltin (s2, l2) when Builtin.equal s1 s2 ->
          unif_list ~unif ~env1 ~env2 subst l1 sc_s l2 sc_t
      | T.SimpleApp(s1,l1), T.SimpleApp (s2, l2) when Symbol.equal s1 s2 ->
          unif_list ~unif ~env1 ~env2 subst l1 sc_s l2 sc_t
      | T.At (l1, r1), T.At (l2, r2) ->
          let subst = unif ~env1 ~env2 subst l1 sc_s l2 sc_t in
          unif ~env1 ~env2 subst r1 sc_s r2 sc_t
      | _, _ -> raise Fail
    in
    if sc_a = sc_b
    then
      if T.equal a b then subst else raise Fail
    else unif ~env1 ~env2 subst a sc_a b sc_b

  let rec _eq ~env1 ~env2 ~subst t1 s1 t2 s2 =
    let t1, s1 = S.get_var subst t1 s1 in
    let t2, s2 = S.get_var subst t2 s2 in
    begin match T.ty t1, T.ty t2 with
      | T.NoType, T.NoType -> true
      | T.NoType, T.HasType _
      | T.HasType _, T.NoType -> false
      | T.HasType ty1, T.HasType ty2 -> _eq ~env1 ~env2 ~subst ty1 s1 ty2 s2
    end &&
    match T.view t1, T.view t2 with
    | T.Var i, T.Var j -> i=j && s1 = s2
    | T.Const f, T.Const g -> Symbol.equal f g
    | T.BVar i, _ when DBE.mem env1 i ->
        _eq ~env1:DBE.empty ~env2 ~subst (DBE.find_exn env1 i) s1 t2 s2
    | _, T.BVar j when DBE.mem env2 j ->
        _eq ~env1 ~env2:DBE.empty ~subst t1 s1 (DBE.find_exn env2 j) s2
    | T.BVar i, T.BVar j -> i=j
    | T.Bind (f1, varty1, t1'), T.Bind (f2, varty2, t2') when Binder.equal f1 f2 ->
        _eq ~env1 ~env2 ~subst varty1 s1 varty2 s2
        &&
        _eq
          ~env1:(DBE.push_none env1) ~env2:(DBE.push_none env2)
          ~subst t1' s1 t2' s2
    | T.App (t1, l1), T.App (t2, l2) when List.length l1 = List.length l2 ->
        _eq ~env1 ~env2 ~subst t1 s1 t2 s2
        &&
        List.for_all2 (fun t1 t2 -> _eq ~env1 ~env2 ~subst t1 s1 t2 s2) l1 l2
    | T.SimpleApp (f1,l1), T.SimpleApp (f2, l2) when Symbol.equal f1 f2 ->
        begin try
            List.for_all2 (fun t1 t2 -> _eq ~env1 ~env2 ~subst t1 s1 t2 s2) l1 l2
          with Invalid_argument _ -> false
        end
    | T.AppBuiltin (f1,l1), T.AppBuiltin (f2, l2) when Builtin.equal f1 f2 ->
        begin try
            List.for_all2 (fun t1 t2 -> _eq ~env1 ~env2 ~subst t1 s1 t2 s2) l1 l2
          with Invalid_argument _ -> false
        end
    | T.At (l1, r1), T.At (l2, r2) ->
        _eq ~env1 ~env2 ~subst l1 s1 l2 s2
        &&
        _eq ~env1 ~env2 ~subst r1 s1 r2 s2
    | T.Record (l1, rest1), T.Record (l2, rest2) ->
        begin try
            List.for_all2
              (fun (n1,t1) (n2,t2) -> n1 = n2 && _eq ~env1 ~env2 ~subst t1 s1 t2 s2)
              l1 l2
          with Invalid_argument _ -> false
        end
        &&
        begin match rest1, rest2 with
          | Some r1, Some r2 -> _eq ~env1 ~env2 ~subst r1 s1 r2 s2
          | None, None -> true
          | Some _, None
          | None, Some _ -> false
        end
    | T.Multiset _, T.Multiset _   (* not unary *)
    | _, _ -> false

  let equal ?(env1=DBE.empty) ?(env2=DBE.empty) ~subst t1 s1 t2 s2 =
    _eq ~env1 ~env2 ~subst t1 s1 t2 s2

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
    (unification :> ?env1:env -> ?env2:env -> ?subst:subst -> term Scoped.t -> term Scoped.t -> subst)

  let matching =
    (matching :> ?allow_open:bool -> ?env1:env -> ?env2:env -> ?subst:subst ->
     pattern:term Scoped.t -> term Scoped.t -> subst)

  let matching_same_scope =
    (matching_same_scope :>  ?env1:env -> ?env2:env -> ?protect:(term Sequence.t) -> ?subst:subst ->
     scope:scope -> pattern:term -> term -> subst)

  let matching_adapt_scope =
    (matching_adapt_scope :> ?env1:env -> ?env2:env ->
     ?protect:(term Sequence.t) -> ?subst:subst ->
     pattern:term Scoped.t -> term Scoped.t -> subst)

  let variant =
    (variant :> ?env1:env -> ?env2:env -> ?subst:subst -> term Scoped.t -> term Scoped.t -> subst)

  let equal =
    (equal :> ?env1:env -> ?env2:env -> subst:subst -> term Scoped.t -> term Scoped.t -> bool)

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
    (unification :> ?env1:env -> ?env2:env -> ?subst:subst -> term Scoped.t -> term Scoped.t -> subst)

  let matching =
    (matching :> ?allow_open:bool -> ?env1:env -> ?env2:env -> ?subst:subst ->
     pattern:term Scoped.t -> term Scoped.t -> subst)

  let matching_same_scope =
    (matching_same_scope :>  ?env1:env -> ?env2:env -> ?protect:(term Sequence.t) -> ?subst:subst ->
     scope:scope -> pattern:term -> term -> subst)

  let matching_adapt_scope =
    (matching_adapt_scope :> ?env1:env -> ?env2:env ->  ?protect:(term Sequence.t) -> ?subst:subst ->
     pattern:term Scoped.t -> term Scoped.t -> subst)

  let variant =
    (variant :> ?env1:env -> ?env2:env -> ?subst:subst -> term Scoped.t -> term Scoped.t -> subst)

  let equal =
    (equal :> ?env1:env -> ?env2:env -> subst:subst -> term Scoped.t -> term Scoped.t -> bool)

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
  type result = res

  let unification =
    (unification :> ?env1:env -> ?env2:env -> ?subst:subst ->
     term Scoped.t -> term Scoped.t -> result)

  let matching =
    (matching :> ?allow_open:bool -> ?env1:env -> ?env2:env ->
     ?subst:subst -> pattern:term Scoped.t -> term Scoped.t -> result)

  let variant =
    (variant :> ?env1:env -> ?env2:env -> ?subst:subst ->
     term Scoped.t -> term Scoped.t -> result)

  let are_unifiable =
    (are_unifiable :> term -> term -> bool)

  let matches =
    (matches :> pattern:term -> term -> bool)

  let are_variant =
    (are_variant :> term -> term -> bool)
end
