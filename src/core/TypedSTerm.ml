
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
  | Var of t Var.t (** variable *)
  | Const of ID.t (** constant *)
  | App of t * t list (** apply term *)
  | Bind of Binder.t * t Var.t * t (** bind variable in term *)
  | AppBuiltin of Builtin.t * t list
  | Multiset of t list
  | Record of (string * t) list * t option (** extensible record *)
  | Meta of meta_var (** Unification variable *)

and meta_var = t Var.t * t option ref

type term = t

let rec deref t = match t.term with
  | Meta (_, {contents=Some t'}) -> deref t'
  | _ -> t

let view t = match t.term with
  | Meta (_, {contents=Some t'}) -> (deref t').term
  | v -> v

let loc t = t.loc
let ty t = t.ty

let ty_exn t = match t.ty with
  | None -> assert false
  | Some x -> x

let to_int_ = function
  | Var _ -> 0
  | Const _ -> 3
  | App _ -> 4
  | Bind _ -> 5
  | Multiset _ -> 6
  | Record _ -> 7
  | AppBuiltin _ -> 8
  | Meta _ -> 9

let rec compare t1 t2 = match view t1, view t2 with
  | Var s1, Var s2 -> Var.compare s1 s2
  | Const s1, Const s2 -> ID.compare s1 s2
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
  | Meta (id1,_), Meta (id2,_) -> Var.compare id1 id2
  | Var _, _
  | Const _, _
  | App _, _
  | Bind _, _
  | Multiset _, _
  | AppBuiltin _, _
  | Meta _, _
  | Record _, _ -> to_int_ t1.term - to_int_ t2.term
and cmp_field x y = CCOrd.pair String.compare compare x y
and cmp_fields x y = CCOrd.list_ cmp_field x y

let equal t1 t2 = compare t1 t2 = 0

let rec hash_fun t h = match t.term with
  | Var s -> h |> Hash.string_ "var" |> Var.hash_fun s
  | Const s -> h |> Hash.string_ "const" |> ID.hash_fun s
  | App (s, l) -> h |> Hash.string_ "app" |> hash_fun s |> Hash.list_ hash_fun l
  | Multiset l ->
    h |> Hash.string_ "multiset" |> Hash.list_ hash_fun l
  | AppBuiltin (b,l) ->
    h |> Builtin.hash_fun b |> Hash.list_ hash_fun l
  | Bind (s,v,t') ->
    h |> Binder.hash_fun s |> hash_fun t' |> Var.hash_fun v
  | Record (l, rest) ->
    h |> Hash.opt hash_fun rest
      |> Hash.list_ (fun (n,t) h -> Hash.string_ n (hash_fun t h)) l
  | Meta (id,_) -> Var.hash_fun id h

let hash x = Hash.apply hash_fun x

let rec pp out t = match view t with
  | Var s ->
      Var.pp out s;
      Format.fprintf out ":%a" _pp_inner (ty_exn t)
  | Const s -> ID.pp out s
  | App (_, []) -> assert false
  | App (f, l) ->
      Format.fprintf out "@[<2>%a@ %a@]" _pp_inner f (Util.pp_list ~sep:" " pp) l
  | Bind (s, v, t) ->
      Format.fprintf out "@[<2>%a %a.@ %a@]" Binder.pp s Var.pp v _pp_inner t
  | Record (l, None) ->
      Format.fprintf out "{%a}" pp_fields l
  | Record (l, Some r) ->
      Format.fprintf out "{%a | %a}" pp_fields l pp r
  | AppBuiltin (b, [a]) when Builtin.is_prefix b ->
    Format.fprintf out "@[%a %a@]" Builtin.pp b pp a
  | AppBuiltin (Builtin.Arrow, ret::args) ->
      pp_infix_ Builtin.Arrow out (args @ [ret])
  | AppBuiltin (b, l) when Builtin.is_infix b && List.length l > 0 ->
      pp_infix_ b out l
  | AppBuiltin (b, l) ->
    Format.fprintf out "(@[%a %a@])" Builtin.pp b (Util.pp_list pp) l
  | Multiset l ->
      Format.fprintf out "[@[%a@]]" (Util.pp_list ~sep:", " _pp_inner) l
  | Meta (id, r) ->
      assert (!r = None); (* we used {!view} *)
      Format.fprintf out "?%a" Var.pp id
and _pp_inner buf t = match view t with
  | AppBuiltin (_, _::_)
  | App _
  | Bind _ -> Format.fprintf buf "(@[%a@])" pp t  (* avoid ambiguities *)
  | _ -> pp buf t
and pp_field out (name,t) =
  Format.fprintf out "%s=%a" name _pp_inner t
and pp_fields out f = Util.pp_list ~sep:", " pp_field out f
and pp_infix_ b out l = match l with
  | [] -> assert false
  | [t] -> pp out t
  | t :: l' ->
      Format.fprintf out "@[<hv>%a@ @[<hv%a@ %a@]@]"
        pp t Builtin.pp b (pp_infix_ b) l'

exception IllFormedTerm of string

let make_ ?loc ~ty view = {term=view; loc; ty=Some ty; }

let var ?loc v = make_ ?loc ~ty:v.Var.ty (Var v)
let const ?loc ~ty s = make_ ?loc ~ty (Const s)
let app_builtin ?loc ~ty b l = make_ ?loc ~ty (AppBuiltin (b,l))
let builtin ?loc ~ty b = make_ ?loc ~ty (AppBuiltin (b,[]))

let meta ?loc (v, r) =
  make_ ?loc ~ty:v.Var.ty (Meta (v,r))

let meta_of_string ?loc ~ty name =
  make_ ?loc ~ty (Meta (Var.of_string ~ty name, ref None))

let app ?loc ~ty s l = match l with
  | [] -> s
  | _::_ -> make_ ?loc ~ty (App(s,l))

let bind ?loc ~ty s v l = make_ ?loc ~ty (Bind(s,v,l))

let bind_list ?loc ~ty s vars t =
  List.fold_right (fun v t -> bind ?loc ~ty s v t) vars t

let multiset ?loc ~ty l = make_ ?loc ~ty (Multiset l)

let record ?loc ~ty l ~rest =
  let rest = CCOpt.map (var ?loc) rest in
  make_ ?loc ~ty (Record (l,rest))

let record_flatten ?loc ~ty l ~rest =
  match CCOpt.map deref rest with
  | None
  | Some {term=(Var _ | Meta _); _} ->
      let l = List.sort cmp_field l in
      make_ ?loc ~ty (Record (l, rest))
  | Some {term=Record (l', rest'); _} ->
      let l = List.sort cmp_field (l@l') in
      make_ ?loc ~ty (Record(l, rest'))
  | Some t' ->
      let msg = CCFormat.sprintf "ill-formed record row: @[%a@]" pp t' in
      raise (IllFormedTerm msg)

let at_loc ?loc t = {t with loc; }
let with_ty ~ty t = {t with ty=Some ty; }
let map_ty t ~f =
  {t with ty=match t.ty with
    | None -> None
    | Some x -> Some (f x)
  }

let of_string ?loc ~ty s = const ?loc ~ty (ID.make s)

let tType = {ty=None; loc=None; term=AppBuiltin (Builtin.TType,[]); }
let prop = builtin ~ty:tType Builtin.Prop

let fresh_var ?loc ~ty () = var ?loc (Var.gensym ~ty ())

(** {2 Utils} *)

let is_var = function | {term=Var _; _} -> true | _ -> false
let is_meta t = match view t with Meta _ -> true | _ -> false

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
      | Const _ -> ()
      | App (f, l) -> iter f; List.iter iter l
      | Bind (_, v, t') -> iter (Var.ty v); iter t'
      | Record (l, rest) ->
          CCOpt.iter iter rest;
          List.iter (fun (_,t) -> iter t) l
      | AppBuiltin (_,l)
      | Multiset l -> List.iter iter l
    in iter t

  let vars t =
    subterms t
    |> Sequence.filter_map
      (fun t -> match view t with
        | Var v -> Some v
        | _ -> None)

  let metas t =
    subterms t
      |> Sequence.filter_map
        (fun t -> match view t with
          | Meta (a,r) ->
              assert (!r=None);
              Some (a, r)
          | _ -> None)

  let subterms_with_bound t k =
    let rec iter set t =
      k (t,set);
      CCOpt.iter (iter set) t.ty;
      match t.term with
      | Meta _
      | Var _
      | Const _ -> ()
      | App (f, l) -> iter set f; List.iter (iter set) l
      | Bind (_, v, t') ->
          let set' = Var.Set.add set v in
          iter set' (Var.ty v); iter set' t'
      | Record (l, rest) ->
          CCOpt.iter (iter set) rest;
          List.iter (fun (_,t) -> iter set t) l
      | AppBuiltin (_,l)
      | Multiset l -> List.iter (iter set) l
    in
    iter Var.Set.empty t
end

let rec is_ground t =
  CCOpt.maybe is_ground true t.ty
  &&
  match t.term with
  | Var _ -> false
  | Const _ -> true
  | App (f, l) -> is_ground f && List.for_all is_ground l
  | AppBuiltin (_,l) -> List.for_all is_ground l
  | Bind (_, v, t') -> is_ground (Var.ty v) && is_ground t'
  | Record (l, rest) ->
      CCOpt.maybe is_ground true rest
      &&
      List.for_all (fun (_,t') -> is_ground t') l
  | Multiset l -> List.for_all is_ground l
  | Meta (_,_) -> false

let var_occurs ~var t =
  Seq.vars t
  |> Sequence.mem ~eq:Var.equal var

let vars t = Seq.vars t |> Var.Set.of_seq |> Var.Set.to_list

let closed t =
  Seq.subterms_with_bound t
  |> Sequence.for_all
    (fun (t,set) -> match view t with
      | Var v -> Var.Set.mem set v
      | _ -> true)

let close_all ~ty s t = bind_list ~ty s (vars t) t

(** {2 Specific Views} *)

module Ty = struct
  type t = term

  type builtin = Prop | TType | Term | Int | Rat

  type view =
    | Builtin of builtin
    | Var of t Var.t
    | App of ID.t * t list
    | Fun of t list * t
    | Forall of t Var.t * t
    | Multiset of t
    | Record of (string * t) list * t Var.t option
    | Meta of meta_var

  let view (t:t) : view = match view t with
    | Var v -> Var v
    | App (f, l) ->
        begin match view f with
        | Const id -> App (id,l)
        | _ -> assert false
        end
    | Const id -> App (id, [])
    | Bind (Binder.ForallTy, v, t) -> Forall (v,t)
    | Record (l,None) -> Record (l, None)
    | Record (l, Some r) ->
        begin match view r with
          | Var r -> Record (l, Some r)
          | _ -> assert false
        end
    | Meta (v,o) -> Meta (v,o)
    | AppBuiltin (Builtin.Prop, []) -> Builtin Prop
    | AppBuiltin (Builtin.TType, []) -> Builtin TType
    | AppBuiltin (Builtin.TyInt, []) -> Builtin Int
    | AppBuiltin (Builtin.TyRat, []) -> Builtin Rat
    | AppBuiltin (Builtin.Term, []) -> Builtin Term
    | AppBuiltin (Builtin.Arrow, ret::args) -> Fun (args, ret)
    | AppBuiltin (Builtin.Multiset, [t]) -> Multiset t
    | Multiset _
    | AppBuiltin _
    | Bind _ -> assert false

  let tType = tType
  let var = var
  let meta = meta

  let mk_fun_ ?loc args ret = app_builtin ?loc ~ty:tType Builtin.Arrow (ret::args)

  let fun_ ?loc args ret = match view ret with
    | Fun (args', ret') -> mk_fun_ ?loc (args @ args') ret'
    | _ -> mk_fun_ ?loc args ret

  let app ?loc id l =
    let ty_id = fun_ (List.map (fun _ -> tType) l) tType in
    app ?loc ~ty:tType (const ?loc ~ty:ty_id id) l

  let const ?loc id = const ?loc ~ty:tType id
  let forall ?loc v t = bind ~ty:tType ?loc Binder.ForallTy v t
  let forall_l ?loc = List.fold_right (forall ?loc)
  let multiset ?loc t = app_builtin ?loc ~ty:tType Builtin.Multiset [t]
  let record ?loc l ~rest = record ?loc ~ty:tType l ~rest
  let record_flatten ?loc l ~rest = record_flatten ?loc ~ty:tType l ~rest

  let prop = builtin ~ty:tType Builtin.Prop
  let int = builtin ~ty:tType Builtin.TyInt
  let rat = builtin ~ty:tType Builtin.TyRat
  let term = builtin ~ty:tType Builtin.Term

  let close_forall t = close_all ~ty:tType Binder.ForallTy t

  let is_tType t = match view t with
    | Builtin TType -> true
    | _ -> false

  let rec returns t = match view t with
    | Fun (_, ret) -> returns ret
    | Forall (_,t') -> returns t'
    | _ -> t

  let returns_tType t = is_tType (returns t)
end

module Form = struct
  type t = term
  type view =
    | True
    | False
    | Atom of t
    | Eq of t * t
    | Neq of t * t
    | Equiv of t * t
    | Xor of t * t
    | Imply of t * t
    | And of t list
    | Or of t list
    | Not of t
    | Forall of t Var.t * t
    | Exists of t Var.t * t

  let view t = match view t with
    | AppBuiltin (Builtin.True, []) -> True
    | AppBuiltin (Builtin.False, []) -> False
    | AppBuiltin (Builtin.And, l) -> And l
    | AppBuiltin (Builtin.Or, l) -> Or l
    | AppBuiltin (Builtin.Not, [f]) -> Not f
    | AppBuiltin (Builtin.Imply, [a;b]) -> Imply(a,b)
    | AppBuiltin (Builtin.Equiv, [a;b]) -> Equiv(a,b)
    | AppBuiltin (Builtin.Xor, [a;b]) -> Xor(a,b)
    | AppBuiltin (Builtin.Eq, [a;b]) -> Eq(a,b)
    | AppBuiltin (Builtin.Neq, [a;b]) -> Neq(a,b)
    | Bind(Binder.Forall, v, t) -> Forall (v,t)
    | Bind(Binder.Exists, v, t) -> Exists (v,t)
    | Bind((Binder.ForallTy | Binder.Lambda), _, _) -> assert false
    | Multiset _
    | Record _
    | Meta _
    | Var _
    | Const _
    | App _
    | AppBuiltin _ -> Atom t

  (** Smart constructors (perform simplifications) *)

  let true_ = builtin ~ty:Ty.prop Builtin.True
  let false_ = builtin ~ty:Ty.prop Builtin.False
  let atom t = t
  let eq ?loc a b = app_builtin ?loc ~ty:Ty.prop Builtin.Eq [a;b]
  let neq ?loc a b = app_builtin ?loc ~ty:Ty.prop Builtin.Neq [a;b]
  let equiv ?loc a b = app_builtin ?loc ~ty:Ty.prop Builtin.Equiv [a;b]
  let xor ?loc a b = app_builtin ?loc ~ty:Ty.prop Builtin.Xor [a;b]
  let imply ?loc a b = app_builtin ?loc ~ty:Ty.prop Builtin.Imply [a;b]

  let rec flatten_ (k:[<`And|`Or]) acc l = match l with
    | [] -> acc
    | t::l' ->
        let acc = match view t, k with
          | False, `Or
          | True, `And -> acc
          | And l, `And
          | Or l, `Or -> List.rev_append l acc
          | _ -> t :: acc
        in
        flatten_ k acc l'

  let and_ ?loc = function
    | [] -> true_
    | [t] -> t
    | l -> app_builtin ?loc ~ty:Ty.prop Builtin.And (flatten_ `And [] l)

  let or_ ?loc = function
    | [] -> true_
    | [t] -> t
    | l -> app_builtin ?loc ~ty:Ty.prop Builtin.Or (flatten_ `Or [] l)

  let not_ ?loc f = match view f with
    | Not f' -> f'
    | Eq (a,b) -> neq ?loc a b
    | Neq (a,b) -> eq ?loc a b
    | True -> false_
    | False -> true_
    | _ -> app_builtin ?loc ~ty:Ty.prop Builtin.Not [f]

  let forall ?loc v t = bind ?loc ~ty:Ty.prop Binder.Forall v t
  let exists ?loc v t = bind ?loc ~ty:Ty.prop Binder.Exists v t

  let forall_l ?loc = List.fold_right (forall ?loc)
  let exists_l ?loc = List.fold_right (exists ?loc)
end

let to_string = CCFormat.to_string pp

let _pp_term = pp

module Subst = struct
  type t = term ID.Map.t

  let empty = ID.Map.empty

  let pp out subst =
    CCFormat.seq ~start:"{" ~stop:"}" ~sep:", "
      (Util.pp_pair ~sep:" → " ID.pp _pp_term) out
      (ID.Map.to_seq subst)

  let to_string = CCFormat.to_string pp

  let add subst v t =
    if ID.Map.mem v.Var.id subst
      then invalid_arg
        (CCFormat.sprintf
          "@[<2>var `@[%a@]` already bound in `@[%a@]`@]" Var.pp v pp subst);
    ID.Map.add v.Var.id t subst

  let find_exn subst v = ID.Map.find v.Var.id subst
  let find subst v = try Some (ID.Map.find v.Var.id subst) with Not_found -> None

  let rec eval_head subst t =
    match view t with
    | Var v ->
        begin try
          let t' = ID.Map.find v.Var.id subst in
          eval_head subst t'
        with Not_found ->
          var ?loc:t.loc (Var.update_ty v ~f:(eval_head subst))
        end
    | Const _
    | App _
    | AppBuiltin _
    | Bind _
    | Record _
    | Meta _
    | Multiset _ -> t

  let rec eval subst t =
    match view t with
    | Var v ->
        begin try
          let t' = ID.Map.find v.Var.id subst in
          eval subst t'
        with Not_found ->
          var ?loc:t.loc (Var.update_ty v ~f:(eval subst))
        end
    | Const _ -> t
    | App (f, l) ->
        let ty = eval subst (ty_exn t) in
        app ?loc:t.loc ~ty (eval subst f) (eval_list subst l)
    | Bind (s, v, t) ->
        let ty = eval subst (ty_exn t) in
        (* bind [v] to a fresh name to avoid collision *)
        let v' = Var.copy v in
        let subst = add subst v (var v') in
        bind ?loc:t.loc ~ty s v' (eval subst t)
    | AppBuiltin (Builtin.TType,_) -> t
    | AppBuiltin (b,l) ->
        let ty = eval subst (ty_exn t) in
        app_builtin ?loc:t.loc ~ty b (eval_list subst l)
    | Record (l, rest) ->
        let ty = eval subst (ty_exn t) in
        record_flatten ?loc:t.loc ~ty
          (List.map (CCPair.map2 (eval subst)) l)
          ~rest:(CCOpt.map (eval subst) rest)
    | Multiset l ->
        let ty = eval subst (ty_exn t) in
        multiset ?loc:t.loc ~ty (eval_list subst l)
    | Meta (v,r) ->
        let v = Var.update_ty v ~f:(eval subst) in
        meta ?loc:t.loc (v, r)
  and eval_list subst l = List.map (eval subst) l
end

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

exception UnifyFailure of string * (term * term) list

let pp_stack out l =
  let pp_frame out (t1,t2) =
    Format.fprintf out "@[unifying `@[%a@]` and `@[%a@]`...@]" pp t1 pp t2
  in
  Format.fprintf out "@[%a@]" (Util.pp_list pp_frame) l

let () = Printexc.register_printer
  (function
    | UnifyFailure (msg, st) ->
      Some (
        CCFormat.sprintf "@[<2>unification error:@ %s@,%a@]" msg pp_stack st)
    | _ -> None)

let fail_unif_ st msg = raise (UnifyFailure (msg, st))
let fail_uniff_ st msg = CCFormat.ksprintf msg ~f:(fail_unif_ st)

(* check that:
  - v does not occur in t
  - t is closed
  *)
let occur_check_ v t =
  assert (is_meta v);
  let rec check bound t =
    v == t ||
    CCOpt.maybe (check bound) false t.ty ||
    match view t with
      | Meta _ -> equal v t
      | Var v' -> equal v t || not (Var.Set.mem bound v')
      | Const _ -> false
      | App (f, l) -> check bound f || List.exists (check bound) l
      | Bind (_, v, t) ->
          check bound v.Var.ty || check (Var.Set.add bound v) t
      | AppBuiltin (_,l)
      | Multiset l -> List.exists (check bound) l
      | Record (l, rest) ->
          CCOpt.maybe (check bound) false rest ||
          List.exists (fun (_,t) -> check bound t) l
  in
  check Var.Set.empty t

let are_same_meta_ r1 r2 = match r1, r2 with
  | Some r1, Some r2 ->
      begin match view r1, view r2 with
        | Meta (v1, _), Meta (v2, _) -> Var.equal v1 v2
        | _ -> false
      end
  | _ -> false

let unify ?(st=UStack.create()) t1 t2 =
  let stack = ref [] in
  let fail_ msg = fail_uniff_ !stack msg in
  let rec unif_rec t1 t2 =
    if t1==t2 then ()
    else (
      let old_stack = !stack in
      unify_tys t1 t2;
      stack := (t1,t2):: old_stack;
      unify_terms t1 t2;
      stack := old_stack; (* restore stack *)
    )
  and unify_tys t1 t2 = match t1.ty, t2.ty with
    | None, None -> ()
    | Some ty1, Some ty2 -> unif_rec ty1 ty2
    | Some _, None
    | None, Some _ -> fail_ "type do not match"
  and unify_terms t1 t2 = match view t1, view t2 with
    | Meta (_, r), _ ->
        assert (!r = None);
        if occur_check_ t1 t2 || not (closed t2)
          then fail_ "occur check"
          else UStack.bind ~st r t2
    | _, Meta (_, r) ->
        assert (!r = None);
        if occur_check_ t2 t1 || not (closed t1)
          then fail_ "occur check"
          else UStack.bind ~st r t1
    | Var v1, Var v2 ->
        if not (Var.equal v1 v2) then fail_ "incompatible variables"
    | App (f1,l1), App (f2,l2) when List.length l1=List.length l2 ->
        unif_rec f1 f2;
        unif_l l1 l2
    | AppBuiltin (b1,l1), AppBuiltin (b2,l2) when List.length l1=List.length l2 ->
        if Builtin.equal b1 b2
        then unif_l l1 l2
        else fail_ "%a and %a are not compatible" Builtin.pp b1 Builtin.pp b2
    | Multiset l1, Multiset l2 when List.length l1 = List.length l2 ->
        (* unification is n-ary, so we choose the first satisfying, if any *)
        unif_multi l1 l2
    | Record (l1, r1), Record (l2, r2) ->
        (* check if r1=r2=var. If true, then fields must be the same *)
        if are_same_meta_ r1 r2
        then
          let _,_ = unif_record_fields `MustMatch l1 l2 in
          ()
        else (
          let rest1, rest2 = unif_record_fields `Flexible l1 l2 in
          unif_record_rest ~ty:(ty_exn t1) r2 rest1;
          unif_record_rest ~ty:(ty_exn t2) r1 rest2
        )
    | Var _, _
    | Const _, _
    | App _, _
    | Bind _, _
    | Multiset _, _
    | Record _, _
    | AppBuiltin _, _ -> fail_ "incompatible shape of terms"
  and unif_l l1 l2 = List.iter2 unif_rec l1 l2
  and unif_multi l1 l2 = match l1 with
    | [] -> assert (l2 = []); () (* success *)
    | t1::l1' ->
        unif_multiset_with t1 l1' [] l2
  and unif_multiset_with t1 l1 rest2 l2 = match l2 with
    | [] -> ()
    | t2::l2' ->
        (* save current state, then try to unify t1 and t2 *)
        let snapshot = UStack.snapshot ~st in
        begin try
          unif_rec t1 t2;
          unif_multi l1 (rest2@l2')
        with UnifyFailure _ ->
          (* backtrack *)
          UStack.restore ~st snapshot;
          unif_multiset_with t1 l1 (t2::rest2) l2'
        end;
  and unif_record_fields kind l1 l2 = match l1, l2, kind with
    | [], [], _ -> [], []
    | [], _, `Flexible
    | _, [], `Flexible -> l1, l2
    | [], _, `MustMatch
    | _, [], `MustMatch ->
        fail_ "fields must match, but got %a and %a" pp_fields l1 pp_fields l2
    | (n1,t1)::l1', (n2,t2)::l2', `Flexible ->
        if n1=n2
        then (
          unif_rec t1 t2;
          unif_record_fields kind l1' l2'
        ) else if n1<n2 then (
          let rest1, rest2 = unif_record_fields kind l1' l2 in
          (n1,t1)::rest1, rest2
        ) else (
          let rest1, rest2 = unif_record_fields kind l1 l2' in
          rest1, (n2,t2)::rest2
        )
    | (n1,t1)::l1', (n2,t2)::l2', `MustMatch ->
        if n1=n2
        then (
          unif_rec t1 t2;
          unif_record_fields kind l1' l2'
        ) else fail_ "fields %a and %a do not match" pp_fields l1 pp_fields l2
  and unif_record_rest ~ty r rest = match r, rest with
    | None, [] -> ()
    | None, _::_ -> fail_ "row is absent, cannot match %a" pp_fields rest
    | Some t, _ ->
        begin match view t with
        | Meta (_, v) ->
            let t' = record ~ty rest ~rest:None in
            if occur_check_ t t'
            then fail_ "occur-check of the row %a in @[%a@]" pp t pp t'
            else UStack.bind ~st v t'
        | _ -> fail_ "record row @[%a@] is not a unification variable" pp t
        end
  in
  unif_rec t1 t2

let apply_unify ?st ty l =
  let rec aux subst ty l = match Ty.view ty, l with
  | _, [] -> Subst.eval subst ty
  | Ty.Forall (v,ty'), a :: l' ->
      let ty_a = ty_exn a in
      unify ?st ty_a tType;
      aux (Subst.add subst v a) ty' l'
  | Ty.Fun (exp, ret), _ ->
      aux_l subst exp ret l
  | (Ty.Meta _ | Ty.Var _ | Ty.App _ | Ty.Builtin _ | Ty.Multiset _ | Ty.Record _), _ ->
      fail_uniff_ [] "cannot apply type @[%a@]" pp ty
  and aux_l subst exp ret l = match exp, l with
    | [], [] -> Subst.eval subst ret
    | _, [] -> Subst.eval subst (Ty.fun_ exp ret)
    | [], _ -> fail_uniff_ [] "@[<2>cannot apply type@ @[%a@]@]" pp ret
    | exp_a :: exp', a :: l' ->
        unify ?st exp_a (ty_exn a);
        aux_l subst exp' ret l'
  in
  aux Subst.empty ty l
