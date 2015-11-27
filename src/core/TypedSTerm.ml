
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Simple Typed Terms}.

These terms are scoped, and possibly typed. Type inference should be
performed on them. *)

module Hash = CCHash

type location = ParseLocation.t

type t = {
  term : view;
  ty : t option;
  loc : location option;
}
and view =
  | Var of ID.t               (** variable *)
  | BVar of ID.t              (** bound variable *)
  | Const of Symbol.t         (** constant *)
  | App of t * t list         (** apply term *)
  | Bind of Binder.t * t * t  (** bind variable in term *)
  | AppBuiltin of Builtin.t * t list
  | Multiset of t list
  | Record of (string * t) list * t option  (** extensible record *)
  | Meta of ID.t * t option ref (** Unification variable *)

type term = t

let rec view t = match t.term with
  | Meta (_, {contents=Some t'}) -> view t'
  | _ -> t.term

let loc t = t.loc
let ty t = t.ty

let ty_exn t = match t.ty with
  | None -> assert false
  | Some x -> x

let __to_int = function
  | Var _ -> 0
  | BVar _ -> 1
  | Const _ -> 3
  | App _ -> 4
  | Bind _ -> 5
  | Multiset _ -> 6
  | Record _ -> 7
  | AppBuiltin _ -> 8
  | Meta _ -> 9

let rec compare t1 t2 = match view t1, view t2 with
  | Var s1, Var s2
  | BVar s1, BVar s2 -> ID.compare s1 s2
  | Const s1, Const s2 -> Symbol.compare s1 s2
  | App (s1,l1), App (s2, l2) ->
      CCOrd.(
        compare s1 s2
        <?> (CCOrd.list_ compare, l1, l2)
      )
  | Bind (s1, v1, t1), Bind (s2, v2, t2) ->
      CCOrd.(
        Binder.compare s1 s2
        <?> (compare, v1, v2)
        <?> (compare, t1, t2)
      )
  | AppBuiltin (b1,l1), AppBuiltin (b2,l2) ->
      CCOrd.(
        Builtin.compare b1 b2
        <?> (CCOrd.list_ compare, l1, l2)
      )
  | Multiset l1, Multiset l2 ->
      let l1 = List.sort compare l1 and l2 = List.sort compare l2 in
      CCOrd.list_ compare l1 l2
  | Record (l1, rest1), Record (l2, rest2) ->
      CCOrd.(
        CCOpt.compare compare rest1 rest2
        <?> (cmp_fields, l1, l2)
      )
  | Meta (id1,_), Meta (id2,_) -> ID.compare id1 id2
  | Var _, _
  | BVar _, _
  | Const _, _
  | App _, _
  | Bind _, _
  | Multiset _, _
  | AppBuiltin _, _
  | Meta _, _
  | Record _, _ -> __to_int t1.term - __to_int t2.term
and cmp_field x y = CCOrd.pair String.compare compare x y
and cmp_fields x y = CCOrd.list_ cmp_field x y

let equal t1 t2 = compare t1 t2 = 0

let rec hash_fun t h = match t.term with
  | Var s -> h |> Hash.string_ "var" |> ID.hash_fun s
  | BVar s -> h |> Hash.string_ "bvar" |> ID.hash_fun s
  | Const s -> h |> Hash.string_ "const" |> Symbol.hash_fun s
  | App (s, l) -> h |> Hash.string_ "app" |> hash_fun s |> Hash.list_ hash_fun l
  | Multiset l ->
    h |> Hash.string_ "multiset" |> Hash.list_ hash_fun l
  | AppBuiltin (b,l) ->
    h |> Builtin.hash_fun b |> Hash.list_ hash_fun l
  | Bind (s,v,t') ->
    h |> Binder.hash_fun s |> hash_fun t' |> hash_fun v
  | Record (l, rest) ->
    h |> Hash.opt hash_fun rest
      |> Hash.list_ (fun (n,t) h -> Hash.string_ n (hash_fun t h)) l
  | Meta (id,_) -> ID.hash_fun id h

let hash x = Hash.apply hash_fun x

let _pp_list p buf l = CCFormat.list ~start:"" ~stop:"" ~sep:", " p buf l

let rec pp out t = match view t with
  | Var s
  | BVar s ->
      ID.pp out s;
      begin match ty t with
      | None -> ()
      | Some ty -> Format.fprintf out ":%a" _pp_inner ty
      end
  | Const s -> Symbol.pp out s
  | App (_, []) -> assert false
  | App (f, l) ->
      Format.fprintf out "@[<2>%a@ %a@]" _pp_inner f (Util.pp_list ~sep:" " pp) l
  | Bind (s, v, t) ->
      Format.fprintf out "@[<2>%a %a.@ %a@]" Binder.pp s _pp_inner v _pp_inner t
  | Record (l, None) ->
      Format.fprintf out "{%a}" (_pp_list _pp_field) l
  | Record (l, Some r) ->
      Format.fprintf out "{%a | %a}" (_pp_list _pp_field) l pp r
  | AppBuiltin (b, [a]) when Builtin.is_prefix b ->
    Format.fprintf out "@[%a %a@]" Builtin.pp b pp a
  | AppBuiltin (b, [t1;t2]) when Builtin.is_infix b ->
    Format.fprintf out "@[<2>%a@ %a@ %a@]" pp t1 Builtin.pp b pp t2
  | AppBuiltin (b, l) ->
    Format.fprintf out "@[%a(%a)@]" Builtin.pp b (Util.pp_list pp) l
  | Multiset l ->
      Format.fprintf out "[%a]" (_pp_list _pp_inner) l
  | Meta (id, r) ->
      assert (!r = None);
      Format.fprintf out "?%a" ID.pp id
and _pp_inner buf t = match view t with
  | AppBuiltin (_, _::_)
  | App _
  | Bind _ -> Format.fprintf buf "(%a)" pp t  (* avoid ambiguities *)
  | _ -> pp buf t
and _pp_field buf (name,t) =
  Format.fprintf buf "%s=%a" name _pp_inner t

exception IllFormedTerm of string

let _make ?loc ?ty view = {term=view; loc; ty; }

let var ?loc ?ty s = _make ?loc ?ty (Var s)
let bvar ?loc ?ty s = _make ?loc ?ty (BVar s)
let const ?loc ?ty s = _make ?loc ?ty (Const s)
let app_builtin ?loc ?ty b l = _make ?loc ?ty (AppBuiltin (b,l))
let builtin ?loc ?ty b = _make ?loc ?ty (AppBuiltin (b,[]))
let meta ?loc ?ty id = _make ?loc ?ty (Meta (id, ref None))
let meta_of_string ?loc ?ty name = _make ?loc ?ty (Meta (ID.make name, ref None))

let meta_full ?loc ?ty id r =
  _make ?loc ?ty (Meta (id,r))

let app ?loc ?ty s l = match l with
  | [] -> s
  | _::_ -> _make ?loc ?ty (App(s,l))

let bind ?loc ?ty s v l =
  match v.term with
  | BVar _ -> _make ?loc ?ty (Bind(s,v,l))
  | _ ->
      let msg = CCFormat.sprintf "in binder, expected variable, got %a" pp v in
      raise (IllFormedTerm msg)

let bind_list ?loc ?ty s vars t =
  List.fold_right (fun v t -> bind ?loc ?ty s v t) vars t

let multiset ?loc ?ty l = _make ?loc ?ty (Multiset l)

let record ?loc ?ty l ~rest =
  match rest with
  | None
  | Some {term=Var _; _} ->
      let l = List.sort cmp_field l in
      _make ?loc ?ty (Record (l, rest))
  | Some {term=Record (l', rest'); _} ->
      let l = List.sort cmp_field (l@l') in
      _make ?loc ?ty (Record(l, rest'))
  | Some t' ->
      let msg = CCFormat.sprintf "ill-formed record row: %a" pp t' in
      raise (IllFormedTerm msg)

let at_loc ?loc t = {t with loc; }
let with_ty ?ty t = {t with ty; }

let of_int ?ty i = builtin ?ty (Builtin.of_int i)
let of_string ?loc ?ty s = const ?loc ?ty (Symbol.of_string s)

let tType = builtin Builtin.tType
let wildcard = builtin Builtin.wildcard

let fresh_var ?loc ?ty () = var ?loc ?ty (ID.gensym ())
let fresh_bvar ?loc ?ty () = bvar ?loc ?ty (ID.gensym ())

(** {2 Utils} *)

let is_var = function | {term=Var _; _} -> true | _ -> false
let is_bvar = function | {term=BVar _; _} -> true | _ -> false
let is_meta t = match view t with Meta _ -> true | _ -> false

let rec ground t =
  CCOpt.maybe ground true t.ty
  &&
  match t.term with
  | Var _ -> false
  | BVar _
  | Const _ -> true
  | App (f, l) -> ground f && List.for_all ground l
  | AppBuiltin (_,l) -> List.for_all ground l
  | Bind (_, v, t') -> ground v && ground t'
  | Record (l, rest) ->
      CCOpt.maybe ground true rest
      &&
      List.for_all (fun (_,t') -> ground t') l
  | Multiset l -> List.for_all ground l
  | Meta (_,_) -> false

module Set = Sequence.Set.Make(struct type t = term let compare = compare end)
module Map = Sequence.Map.Make(struct type t = term let compare = compare end)
module Tbl = Hashtbl.Make(struct type t = term let equal = equal let hash = hash end)

module Seq = struct
  let subterms t k =
    let rec iter t =
      k t;
      CCOpt.iter iter t.ty;
      match t.term with
      | Meta _
      | Var _
      | BVar _
      | Const _ -> ()
      | App (f, l) -> iter f; List.iter iter l
      | Bind (_, v, t') -> iter v; iter t'
      | Record (l, rest) ->
          CCOpt.iter iter rest;
          List.iter (fun (_,t) -> iter t) l
      | AppBuiltin (_,l)
      | Multiset l -> List.iter iter l
    in iter t

  let vars t = subterms t |> Sequence.filter is_var

  let metas t =
    subterms t
      |> Sequence.filter_map
        (fun t -> match view t with
          | Meta (a,r) -> assert (!r=None); Some (a, r)
          | _ -> None)

  let subterms_with_bound t k =
    let rec iter set t =
      k (t,set);
      CCOpt.iter (iter set) t.ty;
      match t.term with
      | Meta _
      | Var _
      | BVar _
      | Const _ -> ()
      | App (f, l) -> iter set f; List.iter (iter set) l
      | Bind (_, v, t') ->
          let set' = Set.add v set in
          iter set' v; iter set' t'
      | Record (l, rest) ->
          CCOpt.iter (iter set) rest;
          List.iter (fun (_,t) -> iter set t) l
      | AppBuiltin (_,l)
      | Multiset l -> List.iter (iter set) l
    in iter Set.empty t
end

let vars t = Seq.vars t |> Set.of_seq |> Set.to_list

let closed t =
  Seq.subterms_with_bound t
  |> Sequence.for_all (fun (v,set) -> not (is_bvar v) || Set.mem v set)

let close_all ?ty s t = bind_list ?ty s (vars t) t

let to_string = CCFormat.to_string pp

let _pp_term = pp

(** {2 Substitutions, Unification} *)

module UStack = struct
  type t = {
    mutable size: int;
    mutable l: term option ref list;  (* list of bindings to undo *)
  }

  let create () = {
    size=0;
    l=[];
  }

  type snapshot = int

  let snapshot ~st:t = t.size

  let restore ~st:t i =
    let rec unwind l size i =
      if size>i then
        match l with
        | [] -> assert false
        | r :: l' -> r := None; unwind l' (size-1) i
      else l
    in
    if i<t.size then (
      let l = unwind t.l t.size i in
      t.l <- l;
      t.size <- i
    )

  (* bind [r] to [x] *)
  let bind ~st r x =
    assert (!r = None);
    st.l <- r :: st.l;
    st.size <- st.size + 1;
    r := Some x
end

exception UnifyFailure of term * term

let () = Printexc.register_printer
  (function
    | UnifyFailure (t1,t2) ->
      Some (
        CCFormat.sprintf "@[<2>could not unify `@[%a@]` and `@[%a@]`" pp t1 pp t2)
    | _ -> None)

let _fail_unif t1 t2 = raise (UnifyFailure (t1,t2))

let occur_check_ v t =
  assert (is_meta v);
  let rec check t =
    v == t ||
    CCOpt.maybe check false t.ty ||
    match view t with
      | Meta _
      | Var _ -> equal v t
      | BVar _
      | Const _ -> false
      | App (f, l) -> check f || List.exists check l
      | Bind (_, v, t) -> check v || check t
      | AppBuiltin (_,l)
      | Multiset l -> List.exists check l
      | Record (l, rest) ->
          CCOpt.maybe check false rest ||
          List.exists (fun (_,t) -> check t) l
  in
  check t

let unify ?(st=UStack.create()) t1 t2 =
  let rec unif_rec t1 t2 =
    if t1==t2 then ()
    else (
      unify_tys t1 t2;
      unify_terms t1 t2
    )
  and unify_tys t1 t2 = match t1.ty, t2.ty with
    | None, None -> ()
    | Some ty1, Some ty2 -> unif_rec ty1 ty2
    | Some _, None
    | None, Some _ -> _fail_unif t1 t2
  and unify_terms t1 t2 = match view t1, view t2 with
    | Meta (_, r), _ ->
        assert (!r = None);
        if occur_check_ t1 t2 || not (closed t2)
          then _fail_unif t1 t2
          else UStack.bind ~st r t2
    | _, Meta (_, r) ->
        assert (!r = None);
        if occur_check_ t2 t1 || not (closed t1)
          then _fail_unif t1 t2
          else UStack.bind ~st r t1
    | Var v1, Var v2
    | BVar v1, BVar v2 ->
        if not (ID.equal v1 v2) then _fail_unif t1 t2
    | App (f1,l1), App (f2,l2) when List.length l1=List.length l2 ->
        unif_rec f1 f2;
        unif_l l1 l2
    | AppBuiltin (b1,l1), AppBuiltin (b2,l2) when List.length l1=List.length l2 ->
        if Builtin.equal b1 b2
        then unif_l l1 l2
        else _fail_unif t1 t2
    | Multiset l1, Multiset l2 when List.length l1 = List.length l2 ->
        (* unification is n-ary, so we choose the first satisfying, if any *)
        _unify_multi l1 l2
    | Record (l1, r1), Record (l2, r2) ->
        let rest1, rest2 = _unify_record_fields l1 l2 in
        let fail() = _fail_unif t1 t2 in
        _unify_record_rest ~fail ?ty:t1.ty r2 rest1;
        _unify_record_rest ~fail ?ty:t2.ty r1 rest2
    | Var _, _
    | BVar _, _
    | Const _, _
    | App _, _
    | Bind _, _
    | Multiset _, _
    | Record _, _
    | AppBuiltin _, _ -> _fail_unif t1 t2
  and unif_l l1 l2 = List.iter2 unif_rec l1 l2
  and _unify_multi l1 l2 = match l1 with
    | [] -> assert (l2 = []); () (* success *)
    | t1::l1' ->
        _unify_multiset_with t1 l1' [] l2
  and _unify_multiset_with t1 l1 rest2 l2 = match l2 with
    | [] -> ()
    | t2::l2' ->
        (* save current state, then try to unify t1 and t2 *)
        let snapshot = UStack.snapshot ~st in
        begin try
          unif_rec t1 t2;
          _unify_multi l1 (rest2@l2')
        with UnifyFailure _ ->
          (* backtrack *)
          UStack.restore ~st snapshot;
          _unify_multiset_with t1 l1 (t2::rest2) l2'
        end;
  and _unify_record_fields l1 l2 = match l1, l2 with
    | [], _
    | _, [] -> l1, l2
    | (n1,t1)::l1', (n2,t2)::l2' ->
        if n1=n2
        then (
          unif_rec t1 t2;
          _unify_record_fields l1' l2'
        ) else if n1<n2 then (
          let rest1, rest2 = _unify_record_fields l1' l2 in
          (n1,t1)::rest1, rest2
        ) else (
          let rest1, rest2 = _unify_record_fields l1 l2' in
          rest1, (n2,t2)::rest2
        )
  and _unify_record_rest ~fail ?ty r rest = match r, rest with
    | None, [] -> ()
    | None, _::_ -> fail()
    | Some t, _ ->
        begin match view t with
        | Meta (_, v) ->
            let t' = record ?ty rest ~rest:None in
            if occur_check_ t t'
            then fail()
            else UStack.bind ~st v t'
        | _ -> fail()
        end
  in
  unif_rec t1 t2
