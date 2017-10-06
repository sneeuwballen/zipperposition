
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Terms For Proofs} *)

open Logtk

module Fmt = CCFormat

let errorf msg = Util.errorf ~where:"llterm" msg

type t = {
  view: view;
  ty: t option;
  mutable id: int; (** unique ID *)
}

and view =
  | Type
  | Const of ID.t
  | App of t * t (** curried application *)
  | Arrow of t * t (** functional arrow *)
  | Var of var (** bound var *)
  | Bind of {
      binder: Binder.t;
      ty_var: t;
      body: t;
    }
  | AppBuiltin of Builtin.t * t list
  | Ite of t * t * t

and var = t HVar.t
type term = t
type ty = t

let[@inline] view t = t.view
let[@inline] ty t = t.ty
let[@inline] ty_exn t = match t.ty with Some ty -> ty | None -> assert false
let[@inline] equal (a:t) (b:t) : bool = a == b
let[@inline] hash (a:t) : int = CCHash.int a.id
let[@inline] compare (a:t) (b:t) : int = CCInt.compare a.id b.id

module H_cons = Hashcons.Make(struct
    type t = term
    let equal a b =
      begin match a.ty, b.ty with
        | Some ty1, Some ty2 -> equal ty1 ty2
        | None, None -> true
        | Some _, None
        | None, Some _ -> false
      end &&
      begin match a.view, b.view with
        | Type, Type -> true
        | Const id1, Const id2 -> ID.equal id1 id2
        | App (f1,x1), App (f2,x2) -> equal f1 f2 && equal x1 x2
        | Arrow (a1,b1), Arrow (a2,b2) -> equal a1 a2 && equal b1 b2
        | Var v1, Var v2 -> HVar.equal equal v1 v2
        | Bind b1, Bind b2 ->
          Binder.equal b1.binder b2.binder &&
          equal b1.ty_var b2.ty_var &&
          equal b1.body b2.body
        | AppBuiltin (b1,l1), AppBuiltin (b2,l2) ->
          Builtin.equal b1 b2 &&
          CCList.equal equal l1 l2
        | Ite (a1,b1,c1), Ite (a2,b2,c2) ->
          equal a1 a2 && equal b1 b2 && equal c1 c2
        | Type, _
        | Const _, _
        | App _, _
        | Arrow _, _
        | Var _, _
        | Bind _, _
        | AppBuiltin _, _
        | Ite _, _
          -> false
      end

    let hash (a:t) : int = match a.view with
      | Type -> 1
      | Const id -> CCHash.combine2 10 (ID.hash id)
      | Var v -> CCHash.combine2 20 (HVar.hash v)
      | App (f,x) -> CCHash.combine3 30 (hash f) (hash x)
      | Arrow (a,b) -> CCHash.combine3 35 (hash a) (hash b)
      | Bind b ->
        CCHash.combine4 40 (Binder.hash b.binder) (hash b.ty_var) (hash b.body)
      | AppBuiltin (b,l) ->
        CCHash.combine3 50 (Builtin.hash b) (CCHash.list hash l)
      | Ite (a,b,c) ->
        CCHash.combine4 60 (hash a)(hash b)(hash c)

    let tag (i:int) (t:t) : unit =
      assert (t.id = -1);
      t.id <- i
  end)

let rec pp_rec depth out (t:t) = match view t with
  | Type -> Fmt.string out "type"
  | Const id -> ID.pp_fullc out id
  | App (f,a) -> Fmt.fprintf out "@[%a@ %a@]" (pp_rec depth) f (pp_inner depth) a
  | Arrow (a,b) ->
    Fmt.fprintf out "@[%a@ @<1>→ %a@]" (pp_inner depth) a (pp_rec depth) b
  | Var v -> Format.fprintf out "Y%d" (depth-HVar.id v-1)
  | AppBuiltin (Builtin.Box_opaque, [t]) ->
    Format.fprintf out "@<1>⟦@[%a@]@<1>⟧" (pp_rec depth) t
  | AppBuiltin (b, [a]) when Builtin.is_prefix b ->
    Format.fprintf out "@[%a %a@]" Builtin.pp b (pp_inner depth) a
  | AppBuiltin (b, ([t1;t2] | [_;t1;t2])) when Builtin.fixity b = Builtin.Infix_binary ->
    Format.fprintf out "@[%a %a@ %a@]"
      (pp_inner depth) t1 Builtin.pp b (pp_inner depth) t2
  | AppBuiltin (b, l) when Builtin.is_infix b && List.length l > 0 ->
    Format.fprintf out "@[<hv>%a@]" (pp_infix_ depth b) l
  | AppBuiltin (b, []) -> Builtin.pp out b
  | AppBuiltin (b, l) ->
    Format.fprintf out "(@[<hv>%a@ %a@])"
      Builtin.pp b (Util.pp_list (pp_inner depth)) l
  | Bind {ty_var;binder;body} ->
    Fmt.fprintf out "@[%a (@[Y%d:%a@]).@ %a@]"
      Binder.pp binder
      depth (pp_rec depth) ty_var
      (pp_rec @@ depth+1) body
  | Ite (a,b,c) ->
    Fmt.fprintf out "@[<hv2>ite %a@ %a@ %a@]"
      (pp_inner depth) a
      (pp_inner depth) b
      (pp_inner depth) c
and pp_inner depth out t = match view t with
  | App _ | Bind _ | AppBuiltin (_,_::_) | Arrow _ | Ite _ ->
    Fmt.fprintf out "(%a)@{<Black>/%d@}" (pp_rec depth) t t.id
  | Type | Const _ | Var _ | AppBuiltin (_,[]) ->
    Fmt.fprintf out "%a@{<Black>/%d@}" (pp_rec depth) t t.id
and pp_infix_ depth b out l = match l with
  | [] -> assert false
  | [t] -> pp_inner depth out t
  | t :: l' ->
    Format.fprintf out "@[%a@]@ %a %a"
      (pp_inner depth) t Builtin.pp b (pp_infix_ depth b) l'

let pp = pp_rec 0

let[@inline] mk_ view ty : t =
  let t = {view; ty; id= -1; } in
  H_cons.hashcons t

let t_type = mk_ Type None
let[@inline] var v = mk_ (Var v) (Some (HVar.ty v))
let[@inline] const ~ty id = mk_ (Const id) (Some ty)

let[@inline] is_type t : bool = match ty t with
  | Some ty -> ty == t_type
  | None -> false

let ite a b c =
  begin match ty b, ty c with
    | Some ty1, Some ty2 when equal ty1 ty2 ->
      mk_ (Ite (a,b,c)) (ty b)
    | _ ->
      errorf "type error:@ cannot build `@[ite %a %a %a@]`@ incompatible types"
        pp a pp b pp c
  end

let[@inline] app_ f x ~ty = mk_ (App (f,x)) (Some ty)
let[@inline] arrow_ a b = mk_ (Arrow (a,b)) (Some t_type)

let[@inline] bind ~ty binder ~ty_var body = mk_ (Bind {binder;ty_var;body}) (Some ty)

let norm_builtin_ b l = match b, l with
  | Builtin.Eq, [a;b] when compare a b > 0 -> Builtin.Eq, [b;a]
  | _ -> b, l

let[@inline] app_builtin ~ty b l =
  let b, l = norm_builtin_ b l in
  mk_ (AppBuiltin (b,l)) (Some ty)

let[@inline] builtin ~ty b = app_builtin ~ty b []

let[@inline] map ~f ~bind:f_bind b_acc t = match view t with
  | Type -> t_type
  | Var v -> var (HVar.update_ty v ~f:(f b_acc))
  | Const id -> const ~ty:(f b_acc @@ ty_exn t) id
  | App (hd,a) -> app_ (f b_acc hd) (f b_acc a) ~ty:(f b_acc (ty_exn t))
  | Arrow (a,b) -> arrow_ (f b_acc a) (f b_acc b)
  | Bind b ->
    let b_acc' = f_bind b_acc in
    bind b.binder ~ty:(f b_acc @@ ty_exn t) ~ty_var:(f b_acc b.ty_var)
      (f b_acc' b.body)
  | AppBuiltin (b,l) ->
    app_builtin ~ty:(f b_acc @@ ty_exn t) b (List.map (f b_acc) l)
  | Ite (a,b,c) ->
    ite (f b_acc a) (f b_acc b) (f b_acc c)

(* shift DB indices by [n] *)
let db_shift n (t:t) : t =
  let rec aux k t = match view t with
    | Var v ->
      if HVar.id v >= k then var (HVar.make (HVar.id v+n) ~ty:(HVar.ty v))
      else t
    | _ ->
      map ~f:aux ~bind:succ k t
  in
  aux 0 t

(* replace DB 0 by [sub] in [t] *)
let db_eval ~(sub:t) (t:t) : t =
  let rec aux k t = match view t with
    | Var v ->
      if HVar.id v = k then (
        (* shift [sub] by the number of binders added since binding point *)
        db_shift k sub
      ) else if (HVar.id v > k) then (
        (* shift down by 1, to account for the vanished binder *)
        var (HVar.make (HVar.id v-1) ~ty:(HVar.ty v))
      )
      else t
    | _ ->
      map ~f:aux ~bind:succ k t
  in
  aux 0 t

let app_ f x = match ty f, ty x with
  | Some {view=Arrow (a,b);_}, Some a' when equal a a' -> app_ f x ~ty:b
  | Some {view=Bind{binder=Binder.ForallTy;ty_var;body};_}, Some ty_x ->
    (* polymorphic type *)
    if not (equal ty_x t_type) then (
      errorf "cannot apply@ `@[<2>%a@ : %a@]`@ to non-type `@[<2>%a@ : %a@]`"
        pp f (Fmt.opt pp) (ty f) pp x (Fmt.opt pp) (ty x)
    );
    if not (equal ty_var t_type) then (
      errorf "ill-formed polymorphic type@ `%a`" pp (ty_exn f);
    );
    let ty = db_eval ~sub:x body in
    app_ f x ~ty
  | _ ->
    errorf "type error: cannot apply `@[<2>%a@ : %a@]`@ to `@[<2>%a@ : %a@]`"
      pp f (Fmt.opt pp) (ty f) pp x (Fmt.opt pp) (ty x)

let app f x = match view f with
  | Bind {binder=Binder.Lambda;ty_var;body} ->
    (* β-reduction *)
    begin match ty x with
      | Some ty_x when equal ty_var ty_x ->
        db_eval ~sub:x body
      | Some ty_x ->
        errorf "type error: cannot apply `%a`@ to `%a : %a`" pp f pp x pp ty_x
      | None -> errorf "type error: cannot apply `%a`@ to `%a : none`" pp f pp x
    end
  | _ -> app_ f x

let rec app_l f = function
  | [] -> f
  | a :: tail -> app_l (app f a) tail

let rec returns (t:t) : ty = match view t with
  | Arrow (_, ret) -> returns ret
  | _ -> t

let arrow a b = match ty a, ty b with
  | Some {view=Type;_}, Some {view=Type;_} -> arrow_ a b
  | _ when a == t_type && returns b == t_type -> arrow_ a b (* type constructor *)
  | _ ->
    errorf "type error: cannot make arrow between non-types@ :from `%a`@ :to `%a`"
      pp a pp b

let rec arrow_l l ret = match l with
  | [] -> ret
  | a :: tail -> arrow a (arrow_l tail ret)

let bool = builtin ~ty:t_type Builtin.Prop
let box_opaque t = app_builtin ~ty:(ty_exn t) Builtin.Box_opaque [t]

let lambda ~ty_var body =
  bind Binder.Lambda ~ty:(arrow ty_var @@ ty_exn body) ~ty_var body

module Form = struct
  type t = term
  type view =
    | True
    | False
    | Or of t list
    | And of t list
    | Not of t
    | Equiv of t * t
    | Xor of t * t
    | Imply of t * t
    | Atom of t
    | Eq of t * t
    | Neq of t * t
    | Forall of {ty_var: ty; body: t}
    | Exists of {ty_var: ty; body: t}

  let pp = pp

  let view t = match view t with
    | AppBuiltin (Builtin.True,[]) -> True
    | AppBuiltin (Builtin.False,[]) -> False
    | AppBuiltin (Builtin.And, l) -> And l
    | AppBuiltin (Builtin.Or, l) -> Or l
    | AppBuiltin (Builtin.Not, [t]) -> Not t
    | AppBuiltin (Builtin.Eq, [t;u]) -> Eq(t,u)
    | AppBuiltin (Builtin.Neq, [t;u]) -> Neq(t,u)
    | AppBuiltin (Builtin.Imply, [t;u]) -> Imply(t,u)
    | AppBuiltin (Builtin.Equiv, [t;u]) -> Equiv(t,u)
    | AppBuiltin (Builtin.Xor, [t;u]) -> Xor(t,u)
    | Bind {binder=Binder.Forall; ty_var; body; _} -> Forall {ty_var;body}
    | Bind {binder=Binder.Exists; ty_var; body; _} -> Exists {ty_var;body}
    | _ -> Atom t

  let true_ = builtin ~ty:bool Builtin.True
  let false_ = builtin ~ty:bool Builtin.False
  let eq a b = app_builtin ~ty:(ty_exn a) Builtin.Eq [a;b]
  let neq a b = app_builtin ~ty:bool Builtin.Neq [a;b]
  let and_ a = app_builtin ~ty:bool Builtin.And a
  let or_ a = app_builtin ~ty:bool Builtin.Or a
  let equiv a b = app_builtin ~ty:(ty_exn a) Builtin.Equiv [a;b]
  let imply a b = app_builtin ~ty:(ty_exn a) Builtin.Imply [a;b]
  let xor a b = app_builtin ~ty:(ty_exn a) Builtin.Xor [a;b]
  let forall ~ty_var body = bind Binder.Forall ~ty:bool ~ty_var body
  let exists ~ty_var body = bind Binder.Exists ~ty:bool ~ty_var body

  let not_ a = match view a with
    | Eq (a,b) -> neq a b
    | Neq (a,b) -> eq a b
    | Not f -> f
    | _ -> app_builtin ~ty:bool Builtin.Not [a]
end

module As_key = struct
  type t = term
  let compare = compare
end

module Set = CCSet.Make(As_key)

module Conv = struct
  module T = TypedSTerm

  type ctx = {
    depth: int; (* depth *)
    vars: (T.t,int * ty) Var.Subst.t; (* depth at binding site, + type *)
  }

  let create() : ctx = {depth=0; vars=Var.Subst.empty}

  let pp_ctx out (c:ctx) : unit =
    Fmt.fprintf out "(@[ctx@ :depth %d@ :vars %a@])"
      c.depth (Var.Subst.pp (Fmt.map fst Fmt.int)) c.vars

  let rec of_term (ctx:ctx) (t:T.t): t = match T.view t with
    | T.Var v ->
      begin match Var.Subst.find ctx.vars v with
        | None -> errorf "cannot find var `%a`@ in %a" Var.pp v pp_ctx ctx
        | Some (i,ty) ->
          assert (ctx.depth > i);
          let ty = db_shift (ctx.depth-i) ty in
          var (HVar.make (ctx.depth-i-1) ~ty)
      end
    | T.Const id ->
      let ty = of_term ctx (T.ty_exn t) in
      const id ~ty
    | T.App (f, l) ->
      let f = of_term ctx f in
      let l = List.map (of_term ctx) l in
      app_l f l
    | T.Ite (a,b,c) ->
      let a = of_term ctx a in
      let b = of_term ctx b in
      let c = of_term ctx c in
      ite a b c
    | T.Bind (b,var,body) ->
      let ty = of_term ctx (T.ty_exn t) in
      let ty_var = of_term ctx (Var.ty var) in
      let ctx = {
        depth=ctx.depth+1;
        vars=Var.Subst.add ctx.vars var (ctx.depth,ty_var)
      } in
      bind b ~ty_var ~ty (of_term ctx body)
    | T.AppBuiltin (Builtin.TType, []) -> t_type
    | T.AppBuiltin (Builtin.Arrow, ret::l) ->
      let ret = of_term ctx ret in
      let l = List.map (of_term ctx) l in
      arrow_l l ret
    | T.AppBuiltin (b, l) ->
      let ty = of_term ctx (T.ty_exn t) in
      let l = List.map (of_term ctx) l in
      app_builtin ~ty b l
    | T.Let _ -> assert false (* FIXME *)
    | T.Match _ -> assert false (* FIXME? *)
    | T.Multiset _ -> assert false (* FIXME? *)
    | T.Record _ -> assert false (* FIXME? *)
    | T.Meta _ -> assert false
end
