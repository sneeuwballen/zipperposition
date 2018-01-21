
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Simple Typed Terms}.

    These terms are scoped, and possibly typed. Type inference should be
    performed on them. *)

module Loc = ParseLocation

type location = ParseLocation.t

let section = Util.Section.make "typedSTerm"

type t = {
  term : view;
  ty : t option;
  mutable hash: int; (* computed lazily *)
  loc : location option;
}
and term = t
and ty = t

and match_cstor = {
  cstor_id: ID.t;
  cstor_ty: ty;
  cstor_args: ty list;
}

and match_branch = match_cstor  * t Var.t list * t

and view =
  | Var of t Var.t (** variable *)
  | Const of ID.t (** constant *)
  | App of t * t list (** apply term *)
  | Ite of t * t * t
  | Match of t * match_branch list
  | Let of (t Var.t * t) list * t
  | Bind of Binder.t * t Var.t * t (** bind variable in term *)
  | AppBuiltin of Builtin.t * t list
  | Multiset of t list
  | Record of (string * t) list * t option (** extensible record *)
  | Meta of meta_var (** Unification variable *)

and meta_var = t Var.t * t option ref * [`Generalize | `BindDefault | `NoBind]

let rec deref t = match t.term with
  | Meta (_, {contents=Some t'}, _) -> deref t'
  | _ -> t

let[@inline] must_deref t : bool = match t.term with
  | Meta (_, {contents=Some _}, _) -> true
  | _ -> false

let view t = match t.term with
  | Meta (_, {contents=Some t'}, _) -> (deref t').term
  | v -> v

let loc t = t.loc
let ty t = t.ty

let ty_exn t = match t.ty with
  | None -> assert false
  | Some x -> x
let rec head_exn t = match view t with
  | Const id -> id
  | App (f,_) -> head_exn f
  | Var _
  | Bind (_,_,_)
  | Ite (_,_,_)
  | Let _
  | Match (_,_)
  | AppBuiltin (_,_)
  | Multiset _
  | Record (_,_)
  | Meta _ -> raise Not_found

let head t = try Some (head_exn t) with Not_found -> None

let is_tType t = match view t with AppBuiltin (Builtin.TType,_) -> true | _ -> false

let to_int_ = function
  | Var _ -> 0
  | Const _ -> 3
  | App _ -> 4
  | Bind _ -> 5
  | Multiset _ -> 6
  | Record _ -> 7
  | AppBuiltin _ -> 8
  | Meta _ -> 9
  | Ite _ -> 10
  | Match _ -> 11
  | Let _ -> 12

let rec hash t =
  if t.hash <> -1 then t.hash
  else (
    let h = hash_rec_ t in
    assert (h <> -1);
    t.hash <- h;
    h
  )
and hash_rec_ t = match t.term with
  | Var s -> Hash.combine2 1 (Var.hash s)
  | Const s -> Hash.combine2 2 (ID.hash s)
  | App (s, l) -> Hash.combine3 3 (hash s) (Hash.list hash l)
  | Multiset l -> Hash.combine2 4 (Hash.list hash l)
  | AppBuiltin (b,l) -> Hash.combine3 5 (Builtin.hash b) (Hash.list hash l)
  | Bind (s,v,t') -> Hash.combine4 6 (Binder.hash s) (Var.hash v) (hash t')
  | Record (l, rest) ->
    Hash.combine3 7
      (Hash.opt hash rest)
      (Hash.list (Hash.pair Hash.string hash) l)
  | Ite (a,b,c) -> Hash.combine4 8 (hash a) (hash b) (hash c)
  | Let (_, u) -> Hash.combine2 9 (hash u)
  | Match (u, _) -> Hash.combine2 10 (hash u)
  | Meta (id,_,_) -> Var.hash id

let rec compare t1 t2 =
  let h1 = hash t1 in
  let h2 = hash t2 in
  if h1<>h2 then CCInt.compare h1 h2 (* compare by hash, first *)
  else match view t1, view t2 with
    | Var s1, Var s2 -> Var.compare s1 s2
    | Const s1, Const s2 -> ID.compare s1 s2
    | App (s1,l1), App (s2, l2) ->
      CCOrd.(
        compare s1 s2
        <?> (CCOrd.list compare, l1, l2)
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
        <?> (CCOrd.list compare, l1, l2)
      )
    | Multiset l1, Multiset l2 ->
      let l1 = List.sort compare l1 and l2 = List.sort compare l2 in
      CCOrd.list compare l1 l2
    | Record (l1, rest1), Record (l2, rest2) ->
      CCOrd.(
        CCOpt.compare compare rest1 rest2
        <?> (cmp_fields, l1, l2)
      )
    | Meta (id1,_,_), Meta (id2,_,_) -> Var.compare id1 id2
    | Ite (a1,b1,c1), Ite (a2,b2,c2) ->
      CCList.compare compare [a1;b1;c1] [a2;b2;c2]
    | Let (l1,t1), Let (l2,t2) ->
      CCOrd.( compare t1 t2
        <?> (list (pair Var.compare compare), l1, l2))
    | Match (u1,l1), Match (u2,l2) ->
      let cmp_branch (c1,vars1,rhs1) (c2,vars2,rhs2) =
        CCOrd.(ID.compare c1.cstor_id c2.cstor_id
          <?> (list compare, c1.cstor_args, c2.cstor_args)
          <?> (list Var.compare, vars1,vars2)
          <?> (compare,rhs1,rhs2))
      in
      CCOrd.( compare u1 u2 <?> (list cmp_branch,l1,l2))
    | Var _, _
    | Const _, _
    | App _, _
    | Bind _, _
    | Ite _, _
    | Let _, _
    | Match _, _
    | Multiset _, _
    | AppBuiltin _, _
    | Meta _, _
    | Record _, _ -> to_int_ t1.term - to_int_ t2.term
and cmp_field x y = CCOrd.pair String.compare compare x y
and cmp_fields x y = CCOrd.list cmp_field x y

let equal t1 t2 = compare t1 t2 = 0


let rec unfold_binder b f = match f.term with
  | Bind (b', v, f') when b=b' ->
    let vars, bod = unfold_binder b f' in
    v :: vars, bod
  | _ -> [], f

let rec pp out t = match view t with
  | Var s -> Var.pp_fullc out s
  | Const s ->
    begin match ID.as_prefix s with
      | Some s -> CCFormat.string out s
      | None -> ID.pp out s
    end
  | App (_, []) -> assert false
  | App (f, l) ->
    let l =
      if !InnerTerm.show_type_arguments || is_tType (ty_exn t) then l
      else List.filter (fun t -> not (ty_exn t |> is_tType)) l
    in
    let as_infix = match view f with Const id -> ID.as_infix id | _ -> None in
    let as_prefix = match view f with Const id -> ID.as_prefix id | _ -> None in
    begin match as_infix, as_prefix, l with
      | _, _, [] -> pp out f
      | Some s, _, [a;b] ->
        Format.fprintf out "@[<1>%a@ %s@ %a@]" pp_inner a s pp_inner b
      | _, Some s, [a] ->
        Format.fprintf out "@[<1>%s@ %a@]" s pp_inner a
      | _ ->
        Format.fprintf out "@[<2>%a@ %a@]"
          pp_inner f (Util.pp_list ~sep:" " pp_inner) l
    end
  | Bind (s, _, _) ->
    let vars, body = unfold_binder s t in
    let pp_bound_var out v =
      Format.fprintf out "@[%a%a@]" Var.pp_fullc v pp_var_ty v
    in
    Format.fprintf out "@[<2>%a %a.@ %a@]"
      Binder.pp s (Util.pp_list ~sep:" " pp_bound_var) vars pp_inner body
  | Record (l, None) ->
    Format.fprintf out "{%a}" pp_fields l
  | Record (l, Some r) ->
    Format.fprintf out "{%a | %a}" pp_fields l pp r
  | AppBuiltin (Builtin.Box_opaque, [t]) ->
    Format.fprintf out "@<1>⟦@[%a@]@<1>⟧" pp_inner t
  | AppBuiltin (b, [a]) when Builtin.is_prefix b ->
    Format.fprintf out "@[%a %a@]" Builtin.pp b pp_inner a
  | AppBuiltin (Builtin.Arrow, ret::args) ->
    Format.fprintf out "@[<hv>%a@]" (pp_infix_ Builtin.Arrow) (args @ [ret])
  | AppBuiltin (Builtin.Not, [f]) -> Format.fprintf out "@[¬@ %a@]" pp f
  | AppBuiltin (b, ([t1;t2] | [_;t1;t2])) when Builtin.fixity b = Builtin.Infix_binary ->
    Format.fprintf out "@[%a %a@ %a@]"  pp_inner t1 Builtin.pp b pp_inner t2
  | AppBuiltin (b, l) when Builtin.is_infix b && List.length l > 0 ->
    Format.fprintf out "@[<hv>%a@]" (pp_infix_ b) l
  | AppBuiltin (b, []) -> Builtin.pp out b
  | AppBuiltin (b, l) ->
    Format.fprintf out "@[<2>%a@ %a@]" Builtin.pp b (Util.pp_list pp_inner) l
  | Ite (a,b,c) ->
    Format.fprintf out "@[<2>if %a@ then %a@ else %a@]" pp a pp b pp c
  | Let (l, u) ->
    let pp_binding out (v,t) = Format.fprintf out "@[%a := %a@]" Var.pp v pp t in
    Format.fprintf out "@[<2>let %a@ in %a@]"
      (Util.pp_list ~sep:" and " pp_binding) l pp u
  | Match (u, l) ->
    let pp_branch out (c,vars,rhs) =
      Format.fprintf out "@[<2>case@ @[%a%a%a@] ->@ %a@]"
        ID.pp c.cstor_id (Util.pp_list0 ~sep:" " pp_inner) c.cstor_args
        (Util.pp_list0 ~sep:" " Var.pp_fullc) vars pp rhs
    in
    Format.fprintf out "@[<hv>@[<hv2>match %a with@ %a@]@ end@]"
      pp u (Util.pp_list ~sep:" | " pp_branch) l
  | Multiset l ->
    Format.fprintf out "[@[%a@]]" (Util.pp_list ~sep:", " pp_inner) l
  | Meta (id, r, _) ->
    assert (!r = None); (* we used {!view} *)
    Format.fprintf out "?%a" Var.pp id
and pp_inner out t = match view t with
  | AppBuiltin (_, _::_) | Ite (_,_,_) | App _ | Let (_,_) | Bind _
    ->
    Format.fprintf out "(@[%a@])" pp t  (* avoid ambiguities *)
  | _ -> pp out t
and pp_field out (name,t) =
  Format.fprintf out "%s=%a" name pp_inner t
and pp_fields out f = Util.pp_list ~sep:", " pp_field out f
and pp_infix_ b out l = match l with
  | [] -> assert false
  | [t] when b=Builtin.Arrow -> pp out t
  | [t] -> pp_inner out t
  | t :: l' ->
    Format.fprintf out "@[%a@]@ %a %a"
      pp_inner t Builtin.pp b (pp_infix_ b) l'
and pp_var_ty out v =
  let ty = Var.ty v in
  match view ty with
    | AppBuiltin (Builtin.Term, []) -> ()
    | _ -> Format.fprintf out ":%a" pp_inner ty

let pp_with_ty out t = Format.fprintf out "(@[%a@,:%a@])" pp t pp (ty_exn t)

exception IllFormedTerm of string

let ill_formed m = raise (IllFormedTerm m)
let ill_formedf m = CCFormat.ksprintf m ~f:ill_formed

let () = Printexc.register_printer
    (function
      | IllFormedTerm msg -> Some ("ill formed term: " ^ msg)
      | _ -> None)

let make_ ?loc ~ty view = {term=view; loc; ty=Some ty; hash= -1;}

let var ?loc v = make_ ?loc ~ty:v.Var.ty (Var v)
let var_of_string ?loc ~ty n = var ?loc (Var.of_string ~ty n)
let const ?loc ~ty s = make_ ?loc ~ty (Const s)
let const_of_cstor ?loc c = const ?loc c.cstor_id ~ty:c.cstor_ty

let app_builtin ?loc ~ty b l =
  let mk_ b l = make_ ?loc ~ty (AppBuiltin(b,l)) in
  begin match b, l with
    | Builtin.Not, [f'] ->
      begin match view f' with
        | AppBuiltin (Builtin.Eq,l) -> mk_ Builtin.Neq l
        | AppBuiltin (Builtin.Neq,l) -> mk_ Builtin.Eq l
        | AppBuiltin (Builtin.Not,[t]) -> t
        | AppBuiltin (Builtin.True,[]) -> mk_ Builtin.False []
        | AppBuiltin (Builtin.False,[]) -> mk_ Builtin.True []
        | _ -> mk_ b l
      end
    | _ -> mk_ b l
  end

let ite ?loc a b c =
  let ty = match b.ty with None -> assert false | Some ty -> ty in
  make_ ?loc ~ty (Ite (a,b,c))
let let_ ?loc l u = match l with
  | [] -> u
  | _ ->
    let ty = ty_exn u in
    make_ ?loc ~ty (Let (l,u))
let match_ ?loc u l =
  let ty = match l with
    | (_, _, {ty=Some ty; _}) :: _ -> ty
    | _::_
    | [] -> assert false
  in
  make_ ?loc ~ty (Match (u,l))

let builtin ?loc ~ty b = make_ ?loc ~ty (AppBuiltin (b,[]))

let meta ?loc (v, r, k) =
  make_ ?loc ~ty:v.Var.ty (Meta (v,r,k))

let app ?loc ~ty s l = match view s, l with
  | _, [] -> s
  | App (f, l'), _ -> make_ ?loc ~ty (App (f, l' @ l))
  | _ -> make_ ?loc ~ty (App(s,l))

let bind ?loc ~ty s v l = make_ ?loc ~ty (Bind(s,v,l))

let bind_list ?loc ~ty s vars t =
  List.fold_right (fun v t -> bind ?loc ~ty s v t) vars t

let multiset ?loc ~ty l = make_ ?loc ~ty (Multiset l)

let rec check_no_dup_ seen l = match l with
  | [] -> ()
  | (n,_) :: l' ->
    if List.mem n seen then ill_formedf "name %s occurs twice" n;
    check_no_dup_ (n::seen) l'

let record ?loc ~ty l ~rest =
  let rest = CCOpt.map (var ?loc) rest in
  check_no_dup_ [] l;
  make_ ?loc ~ty (Record (l,rest))

let record_flatten ?loc ~ty l ~rest =
  match CCOpt.map deref rest with
    | None
    | Some {term=(Var _ | Meta _); _} ->
      let l = List.sort cmp_field l in
      make_ ?loc ~ty (Record (l, rest))
    | Some {term=Record (l', rest'); _} ->
      let l = List.sort cmp_field (l@l') in
      check_no_dup_ [] l;
      make_ ?loc ~ty (Record(l, rest'))
    | Some t' ->
      ill_formedf "ill-formed record row: @[%a@]" pp t'

let at_loc ?loc t = {t with loc; }
let with_ty ~ty t = {t with ty=Some ty; }
let map_ty t ~f =
  {t with
     hash= ~-1;
     ty=match t.ty with
       | None -> None
       | Some x -> Some (f x)
  }

let of_string ?loc ~ty s = const ?loc ~ty (ID.make s)

let tType = {ty=None; loc=None; term=AppBuiltin (Builtin.TType,[]); hash= -1; }
let prop = builtin ~ty:tType Builtin.Prop

let fresh_var ?loc ~ty () = var ?loc (Var.gensym ~ty ())

let box_opaque t: t =
  make_ ~ty:(ty_exn t) (AppBuiltin (Builtin.Box_opaque, [t]))

(** {2 Utils} *)

let is_var = function | {term=Var _; _} -> true | _ -> false
let is_meta t = match view t with Meta _ -> true | _ -> false
let is_const = function {term=Const _; _} -> true | _ -> false
let is_fun = function {term=Bind (Binder.Lambda, _, _); _} -> true | _ -> false

module Set = Sequence.Set.Make(struct type t = term let compare = compare end)
module Map = Sequence.Map.Make(struct type t = term let compare = compare end)
module Tbl = Hashtbl.Make(struct type t = term let equal = equal let hash = hash end)

module Seq = struct
  let subterms t k =
    let rec iter t =
      k t;
      CCOpt.iter iter t.ty;
      match (deref t).term with
        | Meta _
        | Var _
        | Const _ -> ()
        | App (f, l) -> iter f; List.iter iter l
        | Bind (_, v, t') -> iter (Var.ty v); iter t'
        | Record (l, rest) ->
          CCOpt.iter iter rest;
          List.iter (fun (_,t) -> iter t) l
        | Ite (a,b,c) -> iter a; iter b; iter c
        | Let (l,u) -> iter u; List.iter (fun (_,t) -> iter t) l
        | Match (u,l) ->
          iter u;
          List.iter (fun (_,_,t) -> iter t) l
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
         | Meta (a,r,k) ->
           assert (!r=None);
           Some (a, r, k)
         | _ -> None)

  let subterms_with_bound t k =
    let rec iter set t =
      k (t,set);
      CCOpt.iter (iter set) t.ty;
      match view t with
        | Meta _
        | Var _
        | Const _ -> ()
        | App (f, l) -> iter set f; List.iter (iter set) l
        | Ite (a,b,c) -> iter set a; iter set b; iter set c
        | Let (l,u) ->
          let set' =
            List.fold_left
              (fun bound' (v,u) -> iter set u; Var.Set.add bound' v)
              set l
          in
          iter set' u
        | Match (u,l) ->
          iter set u;
          List.iter
            (fun (_,vars,t) ->
               let set = List.fold_left Var.Set.add set vars in
               iter set t)
            l
        | Bind (_, v, t') ->
          let set' = Var.Set.add set v in
          iter set (Var.ty v); iter set' t'
        | Record (l, rest) ->
          CCOpt.iter (iter set) rest;
          List.iter (fun (_,t) -> iter set t) l
        | AppBuiltin (_,l)
        | Multiset l -> List.iter (iter set) l
    in
    iter Var.Set.empty t

  let free_vars t =
    subterms_with_bound t
    |> Sequence.filter_map
      (fun (t,set) -> match view t with
         | Var v when not (Var.Set.mem set v) -> Some v
         | _ -> None)
end

let rec is_ground t =
  CCOpt.map_or is_ground ~default:true t.ty
  &&
  match t.term with
    | Var _ -> false
    | Const _ -> true
    | App (f, l) -> is_ground f && List.for_all is_ground l
    | AppBuiltin (_,l) -> List.for_all is_ground l
    | Ite (a,b,c) -> is_ground a && is_ground b && is_ground c
    | Let (l,u) ->
      is_ground u
      &&
      List.for_all (fun (_,t) -> is_ground t) l
    | Match (u,l) ->
      is_ground u &&
      List.for_all (fun (_,_,t) -> is_ground t) l
    | Bind (_, v, t') -> is_ground (Var.ty v) && is_ground t'
    | Record (l, rest) ->
      CCOpt.map_or is_ground ~default:true rest
      &&
      List.for_all (fun (_,t') -> is_ground t') l
    | Multiset l -> List.for_all is_ground l
    | Meta _ -> false

let var_occurs ~var t =
  Seq.vars t
  |> Sequence.mem ~eq:Var.equal var

let as_id_app t = match view t with
  | Const id -> Some (id, ty_exn t, [])
  | App ({term=Const id; ty=Some ty; _}, l) -> Some (id, ty, l)
  | _ -> None

let vars t = Seq.vars t |> Var.Set.of_seq |> Var.Set.to_list

let free_vars_set t = Seq.free_vars t |> Var.Set.of_seq
let free_vars t = Seq.free_vars t |> Var.Set.of_seq |> Var.Set.to_list

let free_vars_l l =
  Sequence.of_list l
  |> Sequence.flat_map Seq.free_vars
  |> Var.Set.of_seq |> Var.Set.to_list

let closed t = Seq.free_vars t |> Sequence.is_empty

let close_all ~ty s t =
  let vars = free_vars t in
  bind_list ~ty s vars t

let unfold_fun = unfold_binder Binder.Lambda

(* non recursive map *)
let map ~f ~bind:f_bind b_acc t = match view t with
  | Var v -> var ?loc:t.loc (Var.update_ty ~f:(f b_acc) v)
  | Const id -> const ?loc:t.loc id ~ty:(f b_acc (ty_exn t))
  | App (hd, l) ->
    let hd = f b_acc hd in
    let l = List.map (f b_acc) l in
    app ?loc:t.loc ~ty:(f b_acc (ty_exn t)) hd l
  | Bind (s, v, body) ->
    let b_acc', v' = f_bind b_acc v in
    let body = f b_acc' body in
    bind ?loc:t.loc ~ty:(f b_acc (ty_exn t)) s v' body
  | AppBuiltin (Builtin.TType,_) -> t
  | AppBuiltin (b,l) ->
    let l = List.map (f b_acc) l in
    let ty = f b_acc (ty_exn t) in
    app_builtin ?loc:t.loc ~ty b l
  | Record (l, rest) ->
    let ty = f b_acc (ty_exn t) in
    record_flatten ?loc:t.loc ~ty
      (List.map (CCPair.map2 (f b_acc)) l)
      ~rest:(CCOpt.map (f b_acc) rest)
  | Ite (a,b,c) ->
    let a = f b_acc a in
    let b = f b_acc b in
    let c = f b_acc c in
    ite ?loc:t.loc a b c
  | Let (l, u) ->
    let b_acc', l =
      CCList.fold_map
        (fun b_acc' (v,t) ->
           let t = f b_acc t in
           let b_acc', v = f_bind b_acc' v in
           b_acc', (v,t))
        b_acc l
    in
    let u = f b_acc' u in
    let_ ?loc:t.loc l u
  | Match (u, l) ->
    let u = f b_acc u in
    let l =
      List.map
        (fun (c, vars, rhs) ->
           let b_acc, vars = CCList.fold_map f_bind b_acc vars in
           c, vars, f b_acc rhs)
        l
    in
    match_ ?loc:t.loc u l
  | Multiset l ->
    let ty = f b_acc (ty_exn t) in
    multiset ?loc:t.loc ~ty (List.map (f b_acc) l)
  | Meta (v,r,k) ->
    let v = Var.update_ty v ~f:(f b_acc) in
    meta ?loc:t.loc (v, r, k)

(** {2 Specific Views} *)

module Ty = struct
  type t = term

  type builtin = Prop | TType | Term | Int | Rat

  type view =
    | Ty_builtin of builtin
    | Ty_var of t Var.t
    | Ty_app of ID.t * t list
    | Ty_fun of t list * t
    | Ty_forall of t Var.t * t
    | Ty_multiset of t
    | Ty_record of (string * t) list * t Var.t option
    | Ty_meta of meta_var

  let t_view_ = view

  let rec view (t:t) : view = match t_view_ t with
    | Var v -> Ty_var v
    | App (f, l) ->
      begin match t_view_ f with
        | Const id -> Ty_app (id,l)
        | _ -> assert false
      end
    | Const id -> Ty_app (id, [])
    | Bind (Binder.ForallTy, v, t) -> Ty_forall (v,t)
    | Record (l,None) -> Ty_record (l, None)
    | Record (l, Some r) ->
      begin match t_view_ r with
        | Var r -> Ty_record (l, Some r)
        | _ -> assert false
      end
    | Meta (_,{contents=Some ty'},_) -> view ty'
    | Meta (v,o,k) -> Ty_meta (v,o,k)
    | AppBuiltin (Builtin.Prop, []) -> Ty_builtin Prop
    | AppBuiltin (Builtin.TType, []) -> Ty_builtin TType
    | AppBuiltin (Builtin.TyInt, []) -> Ty_builtin Int
    | AppBuiltin (Builtin.TyRat, []) -> Ty_builtin Rat
    | AppBuiltin (Builtin.Term, []) -> Ty_builtin Term
    | AppBuiltin (Builtin.Arrow, ret::args) -> Ty_fun (args, ret)
    | AppBuiltin (Builtin.Multiset, [t]) -> Ty_multiset t
    | AppBuiltin (Builtin.TyReal, []) ->
      failwith "cannot handle values of type `real`"
    | Let _
    | Ite _
    | Match _
    | Multiset _
    | AppBuiltin _
    | Bind _ -> assert false

  let equal = equal
  let compare = compare
  let hash = hash
  let hash_fun = hash

  let tType = tType
  let var = var
  let var_of_string ?loc v = var_of_string ?loc ~ty:tType v
  let meta = meta

  let mk_fun_ ?loc args ret = app_builtin ?loc ~ty:tType Builtin.Arrow (ret::args)

  let fun_ ?loc args ret = match args, view ret with
    | [], _ -> ret
    | _, Ty_fun (args', ret') -> mk_fun_ ?loc (args @ args') ret'
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
  let real = builtin ~ty:tType Builtin.TyReal
  let term = builtin ~ty:tType Builtin.Term

  let (==>) args ret = fun_ args ret

  let close_forall t = close_all ~ty:tType Binder.ForallTy t

  let unfold t =
    let rec u_forall t = match view t with
      | Ty_forall (v, t') ->
        let tyvars, args, ret = u_forall t' in
        v::tyvars, args, ret
      | _ ->
        let args, ret = u_args t in
        [], args, ret
    and u_args t = match view t with
      | Ty_fun (args, ret) ->
        let args', ret = u_args ret in
        args @ args', ret
      | _ -> [], t
    in
    u_forall t

  let rec arity t = match view t with
    | Ty_forall (_, t') ->
      let a,b = arity t' in
      a+1, b
    | Ty_fun (args, _) -> 0, List.length args
    | _ -> 0, 0

  let mangle ty =
    let add_id buf id =
      let s =
        ID.name id
        |> CCString.filter (function '#' -> false | _ -> true)
      in
      Buffer.add_string buf s
    in
    let rec aux buf t = match view t with
      | Ty_builtin TType -> Buffer.add_string buf "ty"
      | Ty_builtin Int -> Buffer.add_string buf "int"
      | Ty_builtin Rat -> Buffer.add_string buf "rat"
      | Ty_builtin Prop -> Buffer.add_string buf "prop"
      | Ty_builtin Term -> Buffer.add_string buf "i"
      | Ty_var v -> add_id buf (Var.id v)
      | Ty_app (f,[]) -> add_id buf f
      | Ty_app (f,l) ->
        add_id buf f;
        List.iter (fun sub -> Buffer.add_char buf '_'; aux buf sub) l
      | Ty_fun (args,ret) ->
        List.iter (fun sub -> aux buf sub; Buffer.add_string buf "_to_") args;
        aux buf ret;
      | Ty_forall (v,f) -> Printf.bprintf buf "pi_%a_%a" add_id (Var.id v) aux f
      | Ty_multiset _
      | Ty_record (_,_)
      | Ty_meta _ -> ()
    in
    let buf = Buffer.create 32 in
    aux buf ty;
    Buffer.contents buf

  let needs_args ty = arity ty <> (0,0)

  let is_tType = is_tType
  let is_prop t = match view t with Ty_builtin Prop -> true | _ -> false

  let rec returns t = match view t with
    | Ty_fun (_, ret) -> returns ret
    | Ty_forall (_,t') -> returns t'
    | _ -> t

  let returns_tType t = is_tType (returns t)
  let returns_prop t = is_prop (returns t)

  let rec is_mono t = match view t with
    | Ty_builtin _ -> true
    | Ty_app (_,l) -> List.for_all is_mono l
    | Ty_fun (l,ret) -> List.for_all is_mono l && is_mono ret
    | Ty_multiset t -> is_mono t
    | Ty_record (l,rest) ->
      List.for_all CCFun.(compose snd is_mono) l && rest = None
    | Ty_meta _
    | Ty_var _
    | Ty_forall (_,_) -> false
end

let fun_l ?loc vars body =
  let ty = Ty.fun_ ?loc (List.map Var.ty vars) (ty_exn body) in
  bind_list ?loc ~ty Binder.Lambda vars body

let sort_ty_vars_first : t Var.t list -> t Var.t list =
  List.sort
    (fun v1 v2 ->
       begin match Ty.is_tType (Var.ty v1), Ty.is_tType (Var.ty v2) with
         | true, false -> -1
         | false, true -> 1
         | _ -> 0
       end)

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

  let is_prop_ (t:term): bool = match t.ty with
    | Some ty -> Ty.is_prop ty
    | None -> false

  let view (t:term) = match view t with
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
    | Ite _
    | Match _
    | Let _
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
  let ite = ite
  let imply ?loc a b = app_builtin ?loc ~ty:Ty.prop Builtin.Imply [a;b]

  let eq_or_equiv t u =
    if Ty.is_prop (ty_exn t)
    then equiv t u
    else eq t u

  let neq_or_xor t u =
    if Ty.is_prop (ty_exn t)
    then xor t u
    else neq t u

  let box_opaque = box_opaque

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
    | [] -> false_
    | [t] -> t
    | l -> app_builtin ?loc ~ty:Ty.prop Builtin.Or (flatten_ `Or [] l)

  let not_ ?loc f = app_builtin ?loc ~ty:Ty.prop Builtin.Not [f]

  let forall ?loc v t = bind ?loc ~ty:Ty.prop Binder.Forall v t
  let exists ?loc v t = bind ?loc ~ty:Ty.prop Binder.Exists v t

  let forall_l ?loc = List.fold_right (forall ?loc)
  let exists_l ?loc = List.fold_right (exists ?loc)

  let unfold_binder = unfold_binder
  let unfold_forall = unfold_binder Binder.Forall

  let close_forall ?loc f =
    (* quantification over types: outermost *)
    let tyvars, vars =
      List.partition
        (fun v -> Ty.returns_tType (Var.ty v))
        (free_vars f)
    in
    forall_l ?loc tyvars (forall_l ?loc vars f)
end

let is_monomorphic t =
  Seq.subterms t
  |> Sequence.for_all (fun t -> Ty.is_mono (ty_exn t))

let is_subterm ~strict a ~of_:b =
  let subs = Seq.subterms b in
  (* drop the first element ([b]) if [strict] *)
  let subs = if strict then Sequence.drop 1 subs else subs in
  Sequence.exists (equal a) subs

(** {2 IO} *)

let to_string = CCFormat.to_string pp

let _pp_term = pp

(** {2 Subst} *)

module Subst = struct
  type t = (term, term) Var.Subst.t

  let empty = Var.Subst.empty
  let is_empty = Var.Subst.is_empty
  let mem = Var.Subst.mem

  let pp = Var.Subst.pp _pp_term

  let to_string = CCFormat.to_string pp

  let add subst v t =
    if mem subst v then (
      Util.invalid_argf
        "@[<2>var `@[%a@]`@ already bound in {@[%a@]}@]" Var.pp_full v pp subst
    );
    Var.Subst.add subst v t

  let add_l = List.fold_left (fun subst (v,t) -> add subst v t)

  let find_exn = Var.Subst.find_exn
  let find = Var.Subst.find

  let merge a b = Var.Subst.merge a b

  let rec eval_ ~recursive subst t = match view t with
    | Var v ->
      begin match Var.Subst.find subst v with
        | None ->
          var ?loc:t.loc (Var.update_ty v ~f:(eval_ ~recursive subst))
        | Some t' ->
          assert (t != t');
          if recursive then (
            eval_ ~recursive subst t'
          ) else (
            t'
          )
      end
    | _ ->
      map subst t
        ~bind:rename_var
        ~f:(eval_ ~recursive)

  (* rename variable and evaluate its type. *)
  and rename_var subst v =
    let v' = Var.copy v |> Var.update_ty ~f:(eval_ ~recursive:true subst) in
    (* (re-)bind [v] to [v'] *)
    let subst = Var.Subst.add subst v (var v') in
    subst, v'

  let eval subst t = if is_empty subst then t else eval_ ~recursive:true subst t

  let eval_nonrec subst t = if is_empty subst then t else eval_ ~recursive:false subst t
end

let rec rename subst t = match view t with
  | Var v ->
    begin
      try
        let v' = Var.Subst.find_exn subst v in
        var (Var.update_ty v' ~f:(rename subst))
      with Not_found ->
        var ?loc:t.loc (Var.update_ty v ~f:(rename subst))
    end
  | _ ->
    map subst t
      ~bind:bind_rename_var
      ~f:rename

(* rename variable and evaluate its type *)
and bind_rename_var subst v =
  let v' = Var.copy v |> Var.update_ty ~f:(rename subst) in
  let subst = Var.Subst.add subst v v' in
  subst, v'

let rename_all_vars t = rename Subst.empty t

(* apply and reduce *)
let app_whnf ?loc ~ty f l =
  let rec aux subst f l = match view f, l with
    | Bind (Binder.Lambda, v, body), a :: tail ->
      let subst = Subst.add subst v a in
      aux subst body tail
    | _ ->
      app ?loc ~ty (Subst.eval subst f) l
  in
  aux Subst.empty f l

(** {2 Table of Variables} *)

module Var_tbl = CCHashtbl.Make(struct
    type t_ = t
    type t = t_ Var.t
    let equal = Var.equal
    let hash = Var.hash
  end)

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

exception UnifyFailure of string * (term * term) list * location option

let pp_stack out l =
  let pp_ty out = function
    | None -> ()
    | Some ty -> Format.fprintf out ":%a" pp ty
  in
  let pp_frame out (t1,t2) =
    Format.fprintf out "@[unifying `@[%a@,%a@]` and `@[%a@,%a@]`@]"
      pp t1 pp_ty (ty t1) pp t2 pp_ty (ty t2)
  in
  Format.fprintf out "@[<v>%a@]" (Util.pp_list pp_frame) l

let () = Printexc.register_printer
    (function
      | UnifyFailure (msg, st,loc) ->
        Some (
          CCFormat.sprintf "@[<2>@{<Red>unification error@}:@ %s@ %a@,%a@]"
            msg pp_stack st Loc.pp_opt loc)
      | _ -> None)

let fail_unif_ ?loc st msg = raise (UnifyFailure (msg, st, loc))
let fail_uniff_ ?loc st msg = CCFormat.ksprintf msg ~f:(fail_unif_ ?loc st)

(* check that:
   - v does not occur in t
   - t is closed (if allow_open=false)
*)
let occur_check_ ~allow_open ~subst v t =
  assert (is_meta v);
  let rec check bound t =
    v == t ||
    CCOpt.map_or (check bound) ~default:false t.ty ||
    match view t with
      | Meta _ -> equal v t
      | Var v' ->
        begin match Subst.find subst v' with
          | None -> not allow_open && not (Var.Set.mem bound v')
          | Some t' -> check bound t'
        end
      | Ite (a,b,c) -> check bound a || check bound b || check bound c
      | Let (l,u) ->
        List.exists (fun (_,t) -> check bound t) l
        ||
        let bound = List.fold_left (fun set (v,_) -> Var.Set.add set v) bound l in
        check bound u
      | Match (u, l) ->
        check bound u
        ||
        List.exists
          (fun (_, vars, rhs) ->
             let bound =
               if allow_open then bound else List.fold_left Var.Set.add bound vars
             in
             check bound rhs)
          l
      | Const _ -> false
      | App (f, l) -> check bound f || List.exists (check bound) l
      | Bind (_, v, t) ->
        check bound v.Var.ty
        || check (if allow_open then bound else Var.Set.add bound v) t
      | AppBuiltin (_,l)
      | Multiset l -> List.exists (check bound) l
      | Record (l, rest) ->
        CCOpt.map_or (check bound) ~default:false rest ||
        List.exists (fun (_,t) -> check bound t) l
  in
  check Var.Set.empty t

let are_same_meta_ r1 r2 = match r1, r2 with
  | Some r1, Some r2 ->
    begin match view r1, view r2 with
      | Meta (v1, _, _), Meta (v2, _, _) -> Var.equal v1 v2
      | _ -> false
    end
  | _ -> false

let rename_vars subst v1 v2 =
  let c = var (Var.copy v1) in
  let subst = Subst.add subst v1 c in
  let subst = Subst.add subst v2 c in
  subst

let rec rename_vars_l subst l1 l2 = match l1, l2 with
  | [], [] -> subst
  | [], _
  | _, [] -> assert false
  | v1 :: tail1, v2 :: tail2 ->
    let subst = rename_vars subst v1 v2 in
    rename_vars_l subst tail1 tail2

(* normalize some terms; a more thorough version of {!deref}.
   flatten applications and arrow that contain bound variables *)
let[@inline][@unfold 1] rec normalize subst (t:term) : term = match view t with
  | App (f,l) when must_deref f ->
    normalize subst (app ?loc:t.loc ~ty:(ty_exn t) (deref f) l)
  | App ({term=Var v;_},l) when Subst.mem subst v ->
    let f = Subst.find_exn subst v in
    normalize subst (app ?loc:t.loc ~ty:(ty_exn t) (deref f) l)
  | AppBuiltin (Builtin.Arrow, ret :: args) when must_deref ret ->
    let vars, args', ret' = Ty.unfold @@ deref ret in
    if vars=[] then (
      Ty.fun_ ?loc:t.loc (args @ args') ret'
    ) else t
  | AppBuiltin (Builtin.Arrow, {term=Var v;_} :: args) when Subst.mem subst v ->
    let ret = Subst.find_exn subst v in
    let vars, args', ret' = Ty.unfold @@ deref ret in
    if vars=[] then (
      Ty.fun_ ?loc:t.loc (args @ args') ret' |> normalize subst
    ) else t
  | _ -> deref t

let unify ?(allow_open=false) ?loc ?(st=UStack.create()) ?(subst=Subst.empty) t1 t2 =
  let stack = ref [] in
  let fail_ msg = fail_uniff_ ?loc !stack msg in
  let rec unif_rec subst t1 t2 =
    if t1==t2 then ()
    else (
      let old_stack = !stack in
      unify_tys subst t1 t2;
      let t1 = normalize subst t1 in
      let t2 = normalize subst t2 in
      stack := (t1,t2) :: old_stack;
      unify_terms subst t1 t2;
      stack := old_stack; (* restore stack *)
    )
  and unify_tys subst t1 t2 = match t1.ty, t2.ty with
    | Some ty1, Some ty2 ->
      unif_rec subst ty1 ty2
    | _ ->
      (* none means one of them is tType; unification of terms will work
         that out anyway *)
      ()
  and unify_terms subst t1 t2 = match view t1, view t2 with
    | Var v, _ when Subst.mem subst v ->
      unif_rec subst (Subst.find_exn subst v) t2
    | _, Var v when Subst.mem subst v ->
      unif_rec subst t1 (Subst.find_exn subst v)
    | Meta (v1,r1,k1), Meta (v2,r2,_) ->
      if Var.equal v1 v2 then ()
      else (
        (* if some meta is [`BindDefault] and the other is [`Generalize],
           the former remains unbound, to avoid scope escaping *)
        let r, t = if k1=`BindDefault then r2, t1 else r1, t2 in
        UStack.bind ~st r (Subst.eval subst t)
      )
    | Meta (_, r, _), _ ->
      assert (!r = None);
      if occur_check_ ~allow_open ~subst t1 t2
      then fail_ "occur check"
      else UStack.bind ~st r (Subst.eval subst t2)
    | _, Meta (_, r, _) ->
      assert (!r = None);
      if occur_check_ ~allow_open ~subst t2 t1
      then fail_ "occur check"
      else UStack.bind ~st r (Subst.eval subst t1)
    | Var v1, Var v2 ->
      if not (Var.equal v1 v2)
      then fail_ "incompatible variables@ (subst {@[%a@]})" Subst.pp subst
    | Const id1, Const id2 when ID.equal id1 id2 -> ()
    | App (f1,l1), App (f2,l2) when List.length l1=List.length l2 ->
      unif_rec subst f1 f2;
      unif_l subst l1 l2
    | App (f1,l1), App (f2,l2) ->
      let n1 = List.length l1 in
      let n2 = List.length l2 in
      assert (n1 <> n2);
      (* if [t1 = f1 (hd1 @ tl1)] where [length tl1 = length l2], then
         unify [f1 hd1] with [f2], and [tl1] with [l2] *)
      if n1>n2
      then (
        let hd1, tl1 = CCList.take_drop (n1-n2) l1 in
        let f1' = app f1 hd1 ~ty:Ty.(fun_ (List.map ty_exn tl1) (ty_exn t1)) in
        unif_rec subst f1' f2;
        unif_l subst tl1 l2
      ) else (
        let hd2, tl2 = CCList.take_drop (n2-n1) l2 in
        let f2' = app f2 hd2 ~ty:Ty.(fun_ (List.map ty_exn tl2) (ty_exn t2)) in
        unif_rec subst f1 f2';
        unif_l subst l1 tl2
      )
    | Ite (a1,b1,c1), Ite (a2,b2,c2) ->
      unif_rec subst a1 a2;
      unif_rec subst b1 b2;
      unif_rec subst c1 c2;
    | Let (l, u), _ ->
      (* expand "let" on the fly *)
      let subst = Subst.add_l subst l in
      unif_rec subst u t2
    | _, Let (l, u) ->
      (* expand "let" on the fly *)
      let subst = Subst.add_l subst l in
      unif_rec subst t1 u
    | Match (u1,l1), Match (u2,l2) when List.length l1=List.length l2 ->
      unif_rec subst u1 u2;
      List.iter2
        (fun (c1,vars1,rhs1) (c2,vars2,rhs2) ->
           if List.length vars1 = List.length vars2
           && List.length c1.cstor_args = List.length c2.cstor_args then (
             if not (ID.equal c1.cstor_id c2.cstor_id)
             then fail_
                 "constructors %a and %a are not compatible (subst {@[%a@]})"
                 ID.pp c1.cstor_id ID.pp c2.cstor_id Subst.pp subst;
             unif_l subst c1.cstor_args c2.cstor_args;
             let subst = rename_vars_l subst vars1 vars2 in
             unif_rec subst rhs1 rhs2
           ) else
             fail_ "incompatible branches")
        l1 l2
    | AppBuiltin (b1,l1), AppBuiltin (b2,l2) when List.length l1=List.length l2 ->
      if Builtin.equal b1 b2
      then unif_l subst l1 l2
      else fail_ "%a and %a are not compatible (subst {@[%a@]})"
          Builtin.pp b1 Builtin.pp b2 Subst.pp subst
    | Multiset l1, Multiset l2 when List.length l1 = List.length l2 ->
      (* unification is n-ary, so we choose the first satisfying, if any *)
      unif_multi subst l1 l2
    | Record (l1, r1), Record (l2, r2) ->
      (* check if r1=r2=var. If true, then fields must be the same *)
      if are_same_meta_ r1 r2
      then (
        let rest1, rest2 = unif_record_fields `MustMatch subst l1 l2 in
        assert (rest1 = []);
        assert (rest2 = []);
        ()
      ) else (
        let rest1, rest2 = unif_record_fields `Flexible subst l1 l2 in
        unif_record_rest ~ty:(ty_exn t1) subst r2 rest1;
        unif_record_rest ~ty:(ty_exn t2) subst r1 rest2
      )
    | Bind (b1,v1,t1), Bind(b2,v2,t2) when Binder.equal b1 b2 ->
      (* unify [t1] and [t2], but ensure that [v1] and [v2] are the same.
         We use a fresh variable, because it is still forbidden to
         unify a meta with [v1] or [v2] (not closed) *)
      let subst = rename_vars subst v1 v2 in
      unif_rec subst t1 t2
    | Var _, _
    | Const _, _
    | App _, _
    | Ite _, _
    | Match _, _
    | Bind _, _
    | Multiset _, _
    | Record _, _
    | AppBuiltin _, _ ->
      fail_ "incompatible shape of terms (subst {@[%a@]})" Subst.pp subst
  and unif_l subst l1 l2 = List.iter2 (unif_rec subst) l1 l2
  and unif_multi subst l1 l2 = match l1 with
    | [] -> assert (l2 = []); () (* success *)
    | t1::l1' ->
      unif_multiset_with subst t1 l1' [] l2
  and unif_multiset_with subst t1 l1 rest2 l2 = match l2 with
    | [] -> ()
    | t2::l2' ->
      (* save current state, then try to unify t1 and t2 *)
      let snapshot = UStack.snapshot ~st in
      begin try
          unif_rec subst t1 t2;
          unif_multi subst l1 (rest2@l2')
        with UnifyFailure _ ->
          (* backtrack *)
          UStack.restore ~st snapshot;
          unif_multiset_with subst t1 l1 (t2::rest2) l2'
      end;
  and unif_record_fields kind subst l1 l2 = match l1, l2, kind with
    | [], [], _ -> [], []
    | [], _, `Flexible
    | _, [], `Flexible -> l1, l2
    | [], _, `MustMatch
    | _, [], `MustMatch ->
      fail_ "fields must match, but got %a and %a" pp_fields l1 pp_fields l2
    | (n1,t1)::l1', (n2,t2)::l2', `Flexible ->
      if n1=n2
      then (
        unif_rec subst t1 t2;
        unif_record_fields kind subst l1' l2'
      ) else if n1<n2 then (
        let rest1, rest2 = unif_record_fields kind subst l1' l2 in
        (n1,t1)::rest1, rest2
      ) else (
        let rest1, rest2 = unif_record_fields kind subst l1 l2' in
        rest1, (n2,t2)::rest2
      )
    | (n1,t1)::l1', (n2,t2)::l2', `MustMatch ->
      if n1=n2
      then (
        unif_rec subst t1 t2;
        unif_record_fields kind subst l1' l2'
      ) else fail_ "fields %a and %a do not match" pp_fields l1 pp_fields l2
  and unif_record_rest ~ty subst r rest = match r, rest with
    | None, [] -> ()
    | None, _::_ -> fail_ "row is absent, cannot match %a" pp_fields rest
    | Some t, _ ->
      begin match view t, rest with
        | Meta (_, v, _), _ ->
          let t' = record ~ty rest ~rest:None in
          if occur_check_ ~allow_open ~subst t t'
          then fail_ "occur-check of the row %a in @[%a@]" pp t pp t'
          else UStack.bind ~st v t'
        | Record ([], None), [] ->
          () (* if the meta was already bound, somehow *)
        | _ -> fail_ "record row @[%a@] is not a unification variable" pp t
      end
  in
  unif_rec subst t1 t2

let apply_unify ?gen_fresh_meta ?allow_open ?loc ?st ?(subst=Subst.empty) ty l =
  Util.debugf ~section 5 "@[<>apply `%a`@ to [@[%a@]]@]"
    (fun k->k pp ty (Util.pp_list pp) l);
  let rec aux subst ty l = match Ty.view ty, l with
    | _, [] -> Subst.eval subst ty
    | Ty.Ty_forall (v,ty'), a :: l' ->
      let ty_a = ty_exn a in
      unify ?allow_open ?loc ?st ~subst ty_a tType;
      Util.debugf ~section 5 "@[bind `%a` to `@[%a@]`@]" (fun k->k Var.pp_fullc v pp a);
      aux (Subst.add subst v a) ty' l'
    | Ty.Ty_fun (exp, ret), _ ->
      aux_l subst exp ret l
    | Ty.Ty_meta _, _ ->
      begin match gen_fresh_meta with
        | None ->
          fail_uniff_ ?loc [] "cannot apply type `@[%a@]`@ to `@[%a@]`"
            pp ty (Util.pp_list pp) l
        | Some g ->
          (* unify meta with [tyof(l) -> fresh_var()] *)
          let ret = g() |> Ty.meta in
          unify ?allow_open ?loc ?st ~subst ty
            (Ty.fun_ (List.map ty_exn l) ret);
          ret
      end
    | (Ty.Ty_var _ | Ty.Ty_app _ | Ty.Ty_builtin _
      | Ty.Ty_multiset _ | Ty.Ty_record _), _ ->
      fail_uniff_ ?loc [] "cannot apply type `@[%a@]`@ to `@[%a@]`"
        pp ty (Util.pp_list pp) l
  and aux_l subst exp ret l = match exp, l with
    | [], [] -> Subst.eval subst ret
    | _, [] -> Subst.eval subst (Ty.fun_ exp ret)
    | [], _ -> fail_uniff_ ?loc [] "@[<2>cannot apply type@ `@[%a@]`@]" pp ret
    | exp_a :: exp', a :: l' ->
      unify ?allow_open ?loc ?st ~subst exp_a (ty_exn a);
      aux_l subst exp' ret l'
  in
  aux subst ty l

let app_infer ?st ?subst f l =
  let ty = apply_unify ?st ?subst (ty_exn f) l in
  app ~ty f l

(** {2 Conversion} *)

let rec erase t = match view t with
  | Var v -> STerm.var (Var.to_string v)
  | Const s -> STerm.const (ID.to_string s)
  | App (f, l) -> STerm.app (erase f) (List.map erase l)
  | Bind (b,v,t) ->
    STerm.bind b
      [STerm.V (Var.to_string v), Some (erase (Var.ty v))]
      (erase t)
  | AppBuiltin (b, l) -> STerm.app_builtin b (List.map erase l)
  | Ite (a,b,c) -> STerm.ite (erase a) (erase b) (erase c)
  | Match (u, l) ->
    let u = erase u in
    let l =
      List.map
        (fun (c,vars,rhs) ->
           (* type arguments of [c] are ignored as being implicit *)
           let c = ID.to_string c.cstor_id in
           let vars = List.map (fun v -> STerm.V (Var.to_string v)) vars in
           let rhs = erase rhs in
           STerm.Match_case (c,vars,rhs))
        l
    in
    STerm.match_ u l
  | Multiset l -> STerm.list_ (List.map erase l)
  | Let (l, u) ->
    let l = List.map (fun (v,t) -> STerm.V (Var.to_string v), erase t) l in
    let u = erase u in
    STerm.let_ l u
  | Record (l, rest) ->
    let rest = CCOpt.map
        (fun t -> match view t with
           | Var v -> STerm.V (Var.to_string v)
           | _ -> failwith "cannot erase non-variable record raw")
        rest
    in
    STerm.record
      (List.map (fun (n,t) -> n, erase t) l)
      ~rest
  | Meta _ -> failwith "cannot erase meta"

module TPTP = struct
  let pp out t = STerm.TPTP.pp out (erase t)
  let to_string t = STerm.TPTP.to_string (erase t)
end

module ZF = struct
  let pp out t = STerm.ZF.pp out (erase t)
  let pp_inner out t = STerm.ZF.pp_inner out (erase t)
  let to_string t = STerm.ZF.to_string (erase t)
end

let pp_in = function
  | Output_format.O_zf -> ZF.pp
  | Output_format.O_tptp -> TPTP.pp
  | Output_format.O_normal -> pp
  | Output_format.O_none -> CCFormat.silent

