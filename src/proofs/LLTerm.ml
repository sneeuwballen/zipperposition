
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Terms For Proofs} *)

open Logtk

type t = {
  view: view;
  ty: t option;
  mutable id: int; (** unique ID *)
}

and view =
  | Type
  | Const of ID.t
  | App of t * t list
  | Var of var (** bound var *)
  | Bind of {
      binder: Binder.t;
      ty_var: t;
      body: t;
    }
  | AppBuiltin of Builtin.t * t list

and var = t HVar.t
type term = t

let[@inline] view t = t.view
let[@inline] ty t = t.ty
let[@inline] ty_exn t = match t.ty with Some ty -> ty | None -> assert false
let[@inline] equal (a:t) (b:t) : bool = a == b
let[@inline] hash (a:t) : int = a.id
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
        | App (f1,l1), App (f2,l2) -> equal f1 f2 && CCList.equal equal l1 l2
        | Var v1, Var v2 -> HVar.equal equal v1 v2
        | Bind b1, Bind b2 ->
          Binder.equal b1.binder b2.binder &&
          equal b1.ty_var b2.ty_var &&
          equal b1.body b2.body
        | AppBuiltin (b1,l1), AppBuiltin (b2,l2) ->
          Builtin.equal b1 b2 &&
          CCList.equal equal l1 l2
        | Type, _
        | Const _, _
        | App _, _
        | Var _, _
        | Bind _, _
        | AppBuiltin _, _
          -> false
      end

    let hash (a:t) : int = match a.view with
      | Type -> 1
      | Const id -> CCHash.combine2 10 (ID.hash id)
      | Var v -> CCHash.combine2 20 (HVar.hash v)
      | App (f,l) -> CCHash.combine3 30 (hash f) (CCHash.list hash l)
      | Bind b ->
        CCHash.combine4 40 (Binder.hash b.binder) (hash b.ty_var) (hash b.body)
      | AppBuiltin (b,l) ->
        CCHash.combine3 50 (Builtin.hash b) (CCHash.list hash l)

    let tag (i:int) (t:t) : unit =
      assert (t.id = -1);
      t.id <- i
  end)

let[@inline] mk_ view ty : t =
  let t = {view; ty; id= -1; } in
  H_cons.hashcons t

let t_type = mk_ Type None
let var v = mk_ (Var v) (Some (HVar.ty v))
let const ~ty id = mk_ (Const id) (Some ty)
let app ~ty f l = match l with
  | [] -> f
  | _ -> mk_ (App (f,l)) (Some ty)
let bind ~ty binder ~ty_var body = mk_ (Bind {binder;ty_var;body}) (Some ty)
let app_builtin ~ty b l = mk_ (AppBuiltin (b,l)) (Some ty)
let builtin ~ty b = app_builtin ~ty b []

