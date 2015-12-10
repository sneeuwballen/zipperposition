
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Unification and Matching} *)

module T = InnerTerm
module S = Substs

exception Fail

type subst = S.t
type 'a sequence = ('a -> unit) -> unit

let prof_unify = Util.mk_profiler "unify"
let prof_matching = Util.mk_profiler "matching"

let fail () = raise Fail

(** {2 Signatures} *)

module type UNARY = Unif_intf.UNARY
module type NARY = Unif_intf.NARY

(** {2 Base (scoped terms)} *)

(* Does [v] appear in [t] if we apply the substitution,
   or is [t] open? *)
let occurs_check ~depth subst v t =
  let rec check ~depth t = match T.ty t.Scoped.value with
    | T.NoType -> false
    | T.HasType ty ->
        (* check type and subterms *)
        check ~depth (Scoped.set t ty) ||
        match T.view t.Scoped.value with
        | T.Var v' ->
            (HVar.equal (Scoped.get v) v' && Scoped.same_scope v t)
            ||
            begin match Substs.find subst (Scoped.set t v') with
              | None -> false
              | Some t' -> check ~depth t'
            end
        | T.DB i -> i>=depth (* not closed! *)
        | T.Const _ -> false
        | T.Bind (_, varty, t') ->
            check ~depth (Scoped.set t varty) ||
            check ~depth:(depth+1) (Scoped.set t t')
        | T.Record (l, rest) ->
            CCOpt.maybe (HVar.equal v.Scoped.value) false rest
            ||
            List.exists (fun (_,t') -> check ~depth (Scoped.set t t')) l
        | T.SimpleApp (_, l)
        | T.AppBuiltin (_, l)
        | T.Multiset l ->
            List.exists (fun t' -> check ~depth (Scoped.set t t')) l
        | T.App (hd, l) ->
            check ~depth (Scoped.set t hd) ||
            List.exists (fun t' -> check ~depth (Scoped.set t t')) l  (* TODO: unroll *)
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

  let occurs_check ~depth subst v r =
    let check_l l =
      List.exists (fun (_,t) -> occurs_check ~depth subst v (Scoped.set r t)) l
    in
    begin match r.Scoped.value.rest with
      | Some v' -> HVar.equal v.Scoped.value v' && Scoped.same_scope v r
      | None -> false
    end
    || check_l r.Scoped.value.fields || check_l r.Scoped.value.discarded

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
    type db_perms = db_perm DBEnv.t

    (* push one De Bruijn, easy *)
    let push_one env = DBEnv.push env (Perm singleton)

    (* push n consecutive quantifiers ([n >= 1]) *)
    let push_many env n =
      assert (n >= 1);
      let perm = make n in
      let rec f env n = match n with
        | 1 -> DBEnv.push env (Perm perm)
        | _ ->
            let env' = DBEnv.push env (Deref (n-1)) in
            f env' (n-1)
      in
      let env' = f env n in
      assert (DBEnv.size env' = n + DBEnv.size env);
      assert (n = 1 || DBEnv.find_exn env' 1 = Deref 1);
      env'

    let pop_many = DBEnv.pop_many

    (* permutation [n] belongs to in [env], and the offset of the permutation *)
    let rec find_perm env n = match DBEnv.find_exn env n with
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
        DBEnv.set env offset (Perm perm')
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
      DBEnv.print pp_each out env
    *)
  end

  type env = {
    depth: int;  (* numbre of binders *)
    permutation : Permutation.db_perms;
  }

  (* do consecutive occurrences of the binder commute with one another?
     e.g. [![X]: ![Y]: A]   is the same as  [![Y]: ![X]: A] *)
  let binder_commutes = function
    | Binder.Forall
    | Binder.Exists -> true
    | _ -> false

  let push_none env = {
    depth = env.depth + 1;
    permutation = Permutation.push_one env.permutation;
  }

  let push_many env size = {
    depth = env.depth + size;
    permutation = Permutation.push_many env.permutation size;
  }

  let pop_many env n = {
    depth = env.depth - n;
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
    env:env -> S.t -> T.t Scoped.t -> T.t Scoped.t ->
    (env:env -> subst -> unit) ->
    unit

  (* unify lists pairwise *)
  let rec unify_list_ ~env ~unif subst l1 l2 k =
    match l1.Scoped.value, l2.Scoped.value with
    | [], [] -> k ~env subst
    | [], _
    | _, [] -> ()
    | t1::l1', t2::l2' ->
        unif ~env subst (Scoped.set l1 t1) (Scoped.set l1 t2)
          (fun ~env subst ->
            unify_list_ ~env ~unif subst (Scoped.set l1 l1') (Scoped.set l2 l2') k)

  (* unify the row variables, if any, with the unmatched columns of each term.
      we first unify the record composed of discard fields of r1, with
      the row of r2, and then conversely.*)
  let unify_record_rest_ ~env subst r1 r2 k =
    assert (r1.Scoped.value.RU.fields = []);
    assert (r2.Scoped.value.RU.fields = []);
    (* Util.debugff 5 "@[<hv2>unify_rec_rest@ %a and@ %a with@ %a@]"
       RU._pp r1 RU._pp r2 S.fmt subst; *)
    (* Util.debugf 5 "unif_rest %a %a" T.pp (RU.to_record r1) T.pp (RU.to_record r2); *)
    let rv1 = r1.Scoped.value and rv2 = r2.Scoped.value in
    match rv1.RU.rest, rv1.RU.discarded, rv2.RU.rest, rv2.RU.discarded with
    | None, [], None, [] ->
        k ~env subst (* no row, no remaining fields *)
    | None, _, _, _::_
    | _, _::_, None, _ ->
        (* must unify an empty rest against a non-empty set of discarded
           fields, that is impossible *)
        ()
    | Some rest1, [], _, _ ->
        (* no discarded fields in r1, so we only need to
           unify rest1 with { l2 | rest2 } *)
        if not (RU.occurs_check ~depth:env.depth subst (Scoped.set r1 rest1) r2)
        then
          let subst = S.bind subst (Scoped.set r1 rest1) (Scoped.map RU.to_record r2) in
          k ~env subst
    | _, _, Some rest2, [] ->
        (* symmetric case of the previous one *)
        if not (RU.occurs_check ~depth:env.depth subst (Scoped.set r2 rest2) r1)
        then
          let subst = S.bind subst (Scoped.set r2 rest2) (Scoped.map RU.to_record r1) in
          k ~env subst
    | Some rest1, d1, Some rest2, d2 ->
        if HVar.equal rest1 rest2 && Scoped.same_scope r1 r2
        then (* can only unify if common set of fields --> no discarded *)
          if d1=[] && d2=[] then k ~env subst else ()
        else
          (* create fresh var R and unify rest1 with {l2 | R} (t2)
             and rest2 with { l1 | R } (t1) *)
          let r = HVar.fresh ~ty:rv1.RU.ty () in
          let t1 = Scoped.set r1 (RU.set_rest rv1 ~rest:(Some r)) in
          let t2 = Scoped.set r2 (RU.set_rest rv2 ~rest:(Some r)) in
          if RU.occurs_check ~depth:env.depth subst (Scoped.set r1 rest1) t2
          || RU.occurs_check ~depth:env.depth subst (Scoped.set r2 rest2) t1 then ()
          else (
            let subst = S.bind subst (Scoped.set r1 rest1) (Scoped.map RU.to_record t2) in
            let subst = S.bind subst (Scoped.set r2 rest2) (Scoped.map RU.to_record t1) in
            k ~env subst
          )

  (* unify the two records *)
  let rec unify_records_ ~env ~unif subst r1 r2 k =
    (* Util.debugff 5 "@[<hv2>unify_rec@ %a and@ %a with@ %a@]"
        RU._pp r1 RU._pp r2 S.fmt subst; *)
    match RU.fields r1.Scoped.value, RU.fields r2.Scoped.value with
    | [], _
    | _, [] ->
        let r1 = Scoped.map RU.discard_all r1 in
        let r2 = Scoped.map RU.discard_all r2 in
        unify_record_rest_ ~env subst r1 r2 k
    | (n1,t1)::_, (n2,t2)::_ ->
        begin match String.compare n1 n2 with
          | 0 ->
              unif ~env subst (Scoped.set r1 t1) (Scoped.set r2 t2)
                (fun ~env subst ->
                   let r1' = Scoped.map RU.pop_field r1 in
                   let r2' = Scoped.map RU.pop_field r2 in
                   unify_records_ ~env ~unif subst r1' r2' k)
          | n when n < 0 ->
              (* n1 too small, ditch it into l1' *)
              let r1' = Scoped.map RU.discard r1 in
              unify_records_ ~env ~unif subst r1' r2 k
          | _ ->
              (* n2 too small, ditch it into l2' *)
              let r2' = Scoped.map RU.discard r2 in
              unify_records_ ~env ~unif subst r1 r2' k
        end

  (* unify multisets pairwise. Precondition: l1 and l2 have the same length. *)
  let rec unify_multiset_ ~env ~unif subst l1 l2 k =
    match l2.Scoped.value with
    | [] -> k ~env subst (* success! *)
    | t2::l2' ->
        (* need to unify some element of [l1] with [t2] first *)
        cnoose_pair_ ~env ~unif subst [] l1 (Scoped.set l2 t2) (Scoped.set l2 l2') k
  (* choose an element of [l1] to unify with the head of [l2] *)
  and cnoose_pair_ ~env ~unif subst left right t2 l2' k =
    match right.Scoped.value with
    | [] -> ()   (* exhausted all possibilities *)
    | t1::right' ->
        unif ~env subst (Scoped.set right t1) t2
          (fun ~env subst ->
             (* upon success, unify remaining pairs *)
             unify_multiset_ ~env ~unif subst
              (Scoped.set right (left@right')) l2' k);
        (* also try to unify [t2] with another term of [right] *)
        cnoose_pair_ ~env ~unif
          subst (t1::left) (Scoped.set right right') t2 l2' k

  let rec unif_commutating_binders_ ~env ~unif ~sym ~size subst t1 t2 k =
    match T.view t1.Scoped.value, T.view t2.Scoped.value with
    | T.Bind (s1, _, t1'), T.Bind (s2, _, t2')
      when Binder.equal s1 sym && Binder.equal s2 sym ->
        (* recurse *)
        unif_commutating_binders_ ~env ~unif ~sym
          ~size:(size+1) subst (Scoped.set t1 t1') (Scoped.set t2 t2') k
    | _ ->
        assert (size > 0);
        (* push permutation *)
        let env = push_many env size in
        unif ~env subst t1 t2
          (fun ~env subst -> k ~env:(pop_many env size) subst)

  (** {6 Unification, Matching, Comparison} *)

  type op =
    | O_unify
    | O_match
    | O_variant
    | O_equal

  let rec unif_rec_ ~env ~op subst t1 t2 k : unit =
    Util.debugf 5 "@[<hv2>unif:@ @[%a@]@ =?=@ @[%a@]@ with @[%a@]@]"
      (fun k->k (Scoped.pp T.pp) t1 (Scoped.pp T.pp) t2 S.pp subst);
    let t1 = S.deref subst t1
    and t2 = S.deref subst t2 in
    (* unify types. On success, unify terms. *)
    match T.ty t1.Scoped.value, T.ty t2.Scoped.value with
    | T.NoType, T.NoType -> unif_terms_rec_ ~env ~op subst t1 t2 k
    | T.NoType, _
    | _, T.NoType -> ()
    | T.HasType ty1, T.HasType ty2 ->
        unif_rec_ ~env ~op subst (Scoped.set t1 ty1) (Scoped.set t2 ty2)
          (fun ~env subst -> unif_terms_rec_ ~env ~op subst t1 t2 k)
  and unif_terms_rec_ ~env ~op subst t1 t2 k =
    let view1 = T.view t1.Scoped.value and view2 = T.view t2.Scoped.value in
    match view1, view2 with
    | _ when T.equal t1.Scoped.value t2.Scoped.value
     && (Scoped.same_scope t1 t2 || T.is_ground t1.Scoped.value) ->
        k ~env subst (* the terms are equal under any substitution *)
    | T.Var _, _
    | _, T.Var _ ->
        (* the interesting case: depending on what operation we perform,
           some combinations are not allowed *)
        begin match view1, view2, op with
        | T.Var v1, T.Var v2, O_equal ->
            if HVar.equal v1 v2 && Scoped.same_scope t1 t2
            then k ~env subst
        | T.Var v1, T.Var v2, (O_unify | O_match | O_variant) ->
            if HVar.equal v1 v2 && Scoped.same_scope t1 t2
            then k ~env subst
            else k ~env (S.bind subst (Scoped.set t1 v1) t2)
        | T.Var v1, _, (O_unify | O_match) ->
            if occurs_check ~depth:env.depth subst (Scoped.set t1 v1) t2
            then () (* occur check or t2 is open *)
            else k ~env (S.bind subst (Scoped.set t1 v1) t2)
        | _, T.Var v2, O_unify ->
            if occurs_check ~depth:env.depth subst (Scoped.set t2 v2) t1
            then () (* occur check *)
            else k ~env (S.bind subst (Scoped.set t2 v2) t1)
        | _ -> ()  (* fail *)
        end
    | T.Bind (s1, _, _), T.Bind (s2, _, _)
      when Binder.equal s1 s2 && binder_commutes s1 ->
        (* forall or exists: they might swap *)
        unif_commutating_binders_ ~env ~unif:(unif_rec_ ~op) ~sym:s1 ~size:0 subst t1 t2 k
    | T.Bind (s1, varty1, t1'), T.Bind (s2, varty2, t2') when Binder.equal s1 s2 ->
        unif_rec_ ~env ~op subst (Scoped.set t1 varty1) (Scoped.set t2 varty2)
          (fun ~env subst ->
            unif_rec_ ~op ~env:(push_none env) subst (Scoped.set t1 t1') (Scoped.set t2 t2')
              (fun ~env subst -> k ~env:(pop_many env 1) subst))
    | T.DB i, T.DB j ->
        begin match make_db_equal env i j with
          | None -> ()
          | Some env' -> k ~env:env' subst
        end
    | T.Const f, T.Const g when ID.equal f g -> k ~env subst
    | T.App (f1, l1), T.App (f2, l2) ->
        begin match T.view f1, T.view f2 with
        | T.Const id1, T.Const id2 ->
            if ID.equal id1 id2 && List.length l1 = List.length l2
            then unify_list_ ~env ~unif:(unif_rec_ ~op) subst
              (Scoped.set t1 l1) (Scoped.set t2 l2) k
        | _ ->
            let l1, l2 = pair_lists_ f1 l1 f2 l2 in
            unify_list_ ~env ~unif:(unif_rec_ ~op) subst
              (Scoped.set t1 l1) (Scoped.set t2 l2) k
        end
    | T.Record (l1, rest1), T.Record (l2, rest2) ->
        let r1 = RU.of_record t1.Scoped.value l1 rest1 in
        let r2 = RU.of_record t2.Scoped.value l2 rest2 in
        unify_records_ ~env ~unif:(unif_rec_ ~op)
          subst (Scoped.set t1 r1) (Scoped.set t2 r2) k
    | T.SimpleApp(s1,l1), T.SimpleApp (s2, l2) when ID.equal s1 s2 ->
        unify_list_ ~env ~unif:(unif_rec_ ~op) subst
          (Scoped.set t1 l1) (Scoped.set t2 l2) k
    | T.AppBuiltin (s1,l1), T.AppBuiltin (s2, l2) when Builtin.equal s1 s2 ->
        unify_list_ ~env ~unif:(unif_rec_ ~op) subst
          (Scoped.set t1 l1) (Scoped.set t2 l2) k
    | T.Multiset l1, T.Multiset l2 when List.length l1 = List.length l2 ->
        unify_multiset_ ~env ~unif:(unif_rec_ ~op) subst
          (Scoped.set t1 l1) (Scoped.set t2 l2) k
    | _, _ -> ()  (* fail *)

  let unification ?(subst=S.empty) a b =
    let env = {depth=0; permutation=DBEnv.empty; } in
    fun k -> unif_rec_ ~env ~op:O_unify subst a b (fun ~env:_ subst -> k subst)

  let matching ?(subst=S.empty) ~pattern t =
    if Scoped.same_scope pattern t then invalid_arg "Unif.matching: same scopes";
    let env = {depth=0; permutation=DBEnv.empty; } in
    fun k -> unif_rec_ ~env ~op:O_match subst pattern t (fun ~env:_ subst -> k subst)

  let variant ?(subst=S.empty) a b =
    let env = {depth=0; permutation=DBEnv.empty; } in
    fun k -> unif_rec_ ~env ~op:O_variant subst a b (fun ~env:_ subst -> k subst)

  let are_variant t1 t2 =
    not (Sequence.is_empty (variant (Scoped.make t1 0) (Scoped.make t2 1)))

  let matches ~pattern t =
    not (Sequence.is_empty (matching ~pattern:(Scoped.make pattern 0) (Scoped.make t 1)))

  let are_unifiable t1 t2 =
    not (Sequence.is_empty (unification (Scoped.make t1 0) (Scoped.make t2 1)))
end

(** {2 Unary Unification} *)

module Unary = struct
  type ty = T.t
  type term = T.t

  (* unify lists using the given "unificator" and continuation [k] *)
  let rec unif_list ~unif subst l1 l2 = match l1.Scoped.value, l2.Scoped.value with
    | [], [] -> subst
    | _, []
    | [], _ -> fail ()
    | t1::l1', t2::l2' ->
        let subst = unif subst (Scoped.set l1 t1) (Scoped.set l2 t2) in
        unif_list ~unif subst (Scoped.set l1 l1') (Scoped.set l2 l2')

  (* unify/match records together.
     l1, l2: to unify; l1', l2': to unify with rest1, rest2.
     we use the fact that l1 and l2 are sorted w.r.t keys. *)
  let rec unif_records ~unif subst r1 r2 =
    match RU.fields r1.Scoped.value, RU.fields r2.Scoped.value with
    | [], _
    | _, [] ->
        let r1 = Scoped.map RU.discard_all r1 in
        let r2 = Scoped.map RU.discard_all r2 in
        unif_rest_ subst r1 r2
    | (n1,t1)::_, (n2,t2)::_ ->
        begin match String.compare n1 n2 with
          | 0 ->
              let subst = unif subst (Scoped.set r1 t1) (Scoped.set r2 t2) in
              let r1 = Scoped.map RU.pop_field r1 in
              let r2 = Scoped.map RU.pop_field r2 in
              unif_records ~unif subst r1 r2
          | n when n < 0 ->
              (* n1 too small, ditch it into l1' *)
              let r1 = Scoped.map RU.discard r1 in
              unif_records ~unif subst r1 r2
          | _ ->
              (* n2 too small, ditch it into l1' *)
              let r2 = Scoped.map RU.discard r2 in
              unif_records ~unif subst r1 r2
        end

  (* unify the row variables, if any, with the unmatched columns of each term.
      we first unify the record composed of discard fields of r1, with
      the row of r2, and then conversely. *)
  and unif_rest_ subst r1 r2 =
    let rv1 = r1.Scoped.value and rv2 = r2.Scoped.value in
    assert (rv1.RU.fields = []);
    assert (rv2.RU.fields = []);
    match rv1.RU.rest, rv1.RU.discarded, rv2.RU.rest, rv2.RU.discarded with
    | None, [], None, [] -> subst  (* no row, no remaining fields *)
    | None, _, _, _::_
    | _, _::_, None, _ ->
        (* must unify an empty rest against a non-empty set of discarded fields,
           that is impossible *)
        fail()
    | Some rest1, [], _, _ ->
        (* no discarded fields in r1, so we only need to
           unify rest1 with { l2 | rest2 } *)
        if RU.occurs_check ~depth:0 subst (Scoped.set r1 rest1) r2 then fail();
        S.bind subst (Scoped.set r1 rest1) (Scoped.map RU.to_record r2)
    | _, _, Some rest2, [] ->
        (* symmetric case of the previous one *)
        if RU.occurs_check ~depth:0 subst (Scoped.set r2 rest2) r1 then fail();
        S.bind subst (Scoped.set r2 rest2) (Scoped.map RU.to_record r1)
    | Some rest1, d1, Some rest2, d2 ->
        if HVar.equal rest1 rest2 && Scoped.same_scope r1 r2
        then
          if d1=[] && d2=[] then subst
          else raise Fail (* impossible to unify discarded fields *)
        else (
          (* create fresh var R and unify rest1 with {l2 | R}
             and rest2 with { l1 | R } *)
          let r = HVar.fresh ~ty:rv1.RU.ty () in
          let t1 = Scoped.set r1 (RU.set_rest rv1 ~rest:(Some r)) in
          let t2 = Scoped.set r2 (RU.set_rest rv2 ~rest:(Some r)) in
          if RU.occurs_check ~depth:0 subst (Scoped.set r1 rest1) t2
          || RU.occurs_check ~depth:0 subst (Scoped.set r2 rest2) t1 then fail()
          else (
            let subst = S.bind subst (Scoped.set r1 rest1) (Scoped.map RU.to_record t2) in
            let subst = S.bind subst (Scoped.set r2 rest2) (Scoped.map RU.to_record t1) in
            subst
          )
        )

  type op =
    | O_unify
    | O_match of T.VarSet.t option (* blocked variables *)
    | O_variant
    | O_equal

  let bind subst v t =
    if occurs_check ~depth:0 subst v t then fail()
    else S.bind subst v t

  let rec unif_rec ~op subst t1 t2 =
    let t1 = S.deref subst t1
    and t2 = S.deref subst t2 in
    (* first, unify types *)
    let subst = match T.ty t1.Scoped.value, T.ty t2.Scoped.value with
      | T.NoType, T.NoType -> subst
      | T.NoType, _
      | _, T.NoType -> raise Fail
      | T.HasType ty1, T.HasType ty2 ->
          unif_rec ~op subst (Scoped.set t1 ty1) (Scoped.set t2 ty2)
    in
    unif_term ~op subst t1 t2
  and unif_term ~op subst t1 t2 =
    let view1 = T.view t1.Scoped.value and view2 = T.view t2.Scoped.value in
    match view1, view2 with
    | _ when T.equal t1.Scoped.value t2.Scoped.value
     && (Scoped.same_scope t1 t2 || T.is_ground t1.Scoped.value) ->
        subst (* the terms are equal under any substitution *)
    | T.Var _, _
    | _, T.Var _ ->
        begin match view1, view2, op with
        | T.Var v1, T.Var v2, O_equal ->
            if HVar.equal v1 v2 && Scoped.same_scope t1 t2
            then subst else fail()
        | T.Var v1, T.Var v2, (O_unify | O_variant | O_match _)
          when HVar.equal v1 v2 && Scoped.same_scope t1 t2 -> subst
        | T.Var v1, _, O_match (Some s) when T.VarSet.mem v1 s ->
            fail() (* blocked variable *)
        | T.Var v1, _, (O_unify | O_match _) ->
            if occurs_check ~depth:0 subst (Scoped.set t1 v1) t2
            then fail () (* occur check or t2 is open *)
            else S.bind subst (Scoped.set t1 v1) t2
        | _, T.Var v2, O_unify ->
            if occurs_check ~depth:0 subst (Scoped.set t2 v2) t1
            then fail() (* occur check *)
            else S.bind subst (Scoped.set t2 v2) t1
        | _ -> fail ()  (* fail *)
        end
    | T.Bind (s1, varty1, t1'), T.Bind (s2, varty2, t2') when Binder.equal s1 s2 ->
        let subst = unif_rec ~op subst (Scoped.set t1 varty1) (Scoped.set t2 varty2) in
        unif_rec ~op subst (Scoped.set t1 t1') (Scoped.set t2 t2')
    | T.DB i, T.DB j -> if i = j then subst else raise Fail
    | T.Const f, T.Const g when ID.equal f g -> subst
    | T.App (f1, l1), T.App (f2, l2) ->
        begin match T.view f1, T.view f2 with
        | T.Const id1, T.Const id2 ->
            if ID.equal id1 id2 && List.length l1 = List.length l2
            then unif_list ~unif:(unif_rec ~op) subst
              (Scoped.set t1 l1) (Scoped.set t2 l2)
            else fail()
        | _ ->
            let l1, l2 = pair_lists_ f1 l1 f2 l2 in
            unif_list ~unif:(unif_rec ~op) subst
              (Scoped.set t1 l1) (Scoped.set t2 l2)
        end
    | T.Record (l1, rest1), T.Record (l2, rest2) ->
        let r1 = RU.of_record t1.Scoped.value l1 rest1 in
        let r2 = RU.of_record t2.Scoped.value l2 rest2 in
        unif_records ~unif:(unif_rec ~op) subst
          (Scoped.set t1 r1)(Scoped.set t2 r2)
    | T.SimpleApp (s1,l1), T.SimpleApp (s2, l2) when ID.equal s1 s2 ->
        unif_list ~unif:(unif_rec ~op) subst (Scoped.set t1 l1)(Scoped.set t2 l2)
    | T.AppBuiltin (s1,l1), T.AppBuiltin (s2, l2) when Builtin.equal s1 s2 ->
        unif_list ~unif:(unif_rec ~op) subst (Scoped.set t1 l1)(Scoped.set t2 l2)
    | _, _ -> raise Fail

  let unification ?(subst=Substs.empty) a b =
    Util.with_prof prof_unify
      (fun () -> unif_rec ~op:O_unify subst a b)

  let matching ?(subst=Substs.empty) ~pattern b =
    if Scoped.same_scope pattern b then invalid_arg "Unif.matching: same scopes";
    Util.with_prof prof_matching
      (fun () -> unif_rec ~op:(O_match None) subst pattern b)

  let matching_same_scope
  ?(protect=Sequence.empty) ?(subst=S.empty) ~scope ~pattern b =
    (* set of variables that should not be bound, including the
       free variables of [b] *)
    let protect = Sequence.append protect (T.Seq.vars b) in
    let blocked = T.VarSet.of_seq protect in
    Util.with_prof prof_matching
      (fun () -> unif_rec ~op:(O_match (Some blocked)) subst
        (Scoped.make pattern scope) (Scoped.make b scope))

  let matching_adapt_scope ?protect ?subst ~pattern t =
    if Scoped.same_scope pattern t
    then matching_same_scope ?protect ?subst
      ~scope:t.Scoped.scope ~pattern:pattern.Scoped.value t.Scoped.value
    else matching ?subst ~pattern t

  let variant ?(subst=Substs.empty) a b =
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
  open Unary
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
    (matching_same_scope :> ?protect:(term HVar.t Sequence.t) -> ?subst:subst ->
     scope:int -> pattern:term -> term -> subst)

  let matching_adapt_scope =
    (matching_adapt_scope :>
     ?protect:(term HVar.t Sequence.t) -> ?subst:subst ->
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
  open Unary
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
    (matching_same_scope :> ?protect:(term HVar.t Sequence.t) -> ?subst:subst ->
     scope:int -> pattern:term -> term -> subst)

  let matching_adapt_scope =
    (matching_adapt_scope :> ?protect:(term HVar.t Sequence.t) -> ?subst:subst ->
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

module HO = struct
  open Nary
  type term = HOTerm.t
  type result = S.t Sequence.t

  let unification =
    (unification :> ?subst:subst ->
     term Scoped.t -> term Scoped.t -> result)

  let matching =
    (matching :> ?subst:subst -> pattern:term Scoped.t -> term Scoped.t -> result)

  let variant =
    (variant :> ?subst:subst ->
     term Scoped.t -> term Scoped.t -> result)

  let are_unifiable =
    (are_unifiable :> term -> term -> bool)

  let matches =
    (matches :> pattern:term -> term -> bool)

  let are_variant =
    (are_variant :> term -> term -> bool)
end
