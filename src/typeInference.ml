(*
Copyright (c) 2013, Simon Cruanes
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  Redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(* {1 Type inference} *)

(** Reference:
    https://en.wikipedia.org/wiki/Hindley-Milner
*)

module T = Term

(** {2 Typing context} *)

module Ctx = struct

  type t = {
    offset : int;
    db : Type.t list;               (* types of bound variables (stack) *)
    varsubst : Type.Subst.t;        (* binding of type variables *)
    signature : Signature.t;        (* symbol -> type *)
  }

  let empty = {
    offset = 0;
    db = [];
    varsubst = Type.Subst.empty;
    signature = Signature.empty;
  }

  let add_signature ctx signature =
    { ctx with signature = Signature.merge ctx.signature signature; }

  (* allocate a new type variable *)
  let _new_var ctx =
    let ty = Type.var ctx.offset in
    { ctx with offset = ctx.offset + 1; }, ty

  (* push variable on  the bound vars stack *)
  let enter_binder ctx =
    let ctx, v = _new_var ctx in
    let ctx = { ctx with db = v :: ctx.db; } in
    ctx, v
      
  (* pop variable from the bound var stack *)
  let exit_binder ctx =
    match ctx.db with
    | [] -> failwith "TypeInference.Ctx.exit_binder: empty binding stack"
    | _::db' -> { ctx with db = db'; }

  let unify ctx t1 t2 =
    let varsubst = Type.unify ~subst:ctx.varsubst t1 t2 in
    { ctx with varsubst; }

  let rename_type ctx ty =
    let mv = Type.max_var ty in
    let ty' = Type.rename ctx.offset ty in
    let offset = ctx.offset + mv in
    { ctx with offset; }, ty'

  let rename_var ctx ty i =
    assert false

  let eval_type ctx ty =
    Type.Subst.apply ctx.varsubst ty

  let type_of_symbol ctx s =
    try
      let ty = eval_type ctx (Signature.find ctx.signature s) in
      let ctx, ty = rename_type ctx ty in
      ctx, ty
    with Not_found ->
      (* give a new type variable to this symbol *)
      let ctx, ty = _new_var ctx in
      let ctx = { ctx with signature = Signature.declare ctx.signature s ty; } in
      ctx, ty

  let to_signature ctx =
    let signature = ctx.signature in
    let signature = Signature.map signature (fun _ ty -> eval_type ctx ty) in
    let signature = Signature.filter signature (fun _ ty -> not (Type.is_var ty)) in
    signature
end

(** {2 Hindley-Milner} *)

(* infer a type for [t], possibly updating [ctx] *)
let rec infer_update ctx t =
  let open Type.Infix in
  match t.T.type_ with
  | Some ty -> ctx, ty
  | None -> 
    begin match t.T.term with
    | T.Var _ -> ctx, Type.i
    | T.BoundVar i when i < 0 || i > List.length ctx.Ctx.db ->
      failwith "TypeInference: bound variable out of scope"
    | T.BoundVar i -> ctx, List.nth ctx.Ctx.db i
    | T.At (t1, t2) ->
      let ctx, ty1 = infer_update ctx t1 in
      let ctx, ty2 = infer_update ctx t2 in
      (* t1 : ty1, t2 : ty2. Now we must also have
         ty1 = a -> b, with t2 = a, and the resulting type is b. *)
      begin match ty1 with
      | Type.Fun (ty1_ret, [ty1_arg]) -> 
        let ctx = Ctx.unify ctx ty1_arg ty2 in
        ctx, ty1_ret
      | _ ->
        let msg =
          Util.sprintf "%a should have type 'a -> 'b but has type %a"
          T.pp t1 Type.pp ty1
        in
        raise (Type.Error msg)
      end
    | T.Bind (s, t') ->
      let ctx, ty_s = Ctx.type_of_symbol ctx s in
      let ctx, v = Ctx.enter_binder ctx in
      let ctx, ty_t' = infer_update ctx t' in
      let ctx = Ctx.exit_binder ctx in
      (* now, the bound variable has type [v], [s] has type [ty_s]
          and should also have type [(v -> ty_t') -> ty_t'].
          The resulting type is [ty_t'] *)
      let ctx = Ctx.unify ctx ty_s (ty_t' <=. (ty_t' <=. v)) in
      ctx, ty_t'
    | T.Node (s, l) ->
      let ctx, ty_s = Ctx.type_of_symbol ctx s in
      let ctx, ty_l = List.fold_right
        (fun t' (ctx, ty_l) ->
          let ctx, ty_t' = infer_update ctx t' in
          ctx, ty_t' :: ty_l)
        l (ctx, [])
      in
      (* [s] has type [ty_s], but must also have type [ty_l -> 'a].
          The result is 'a. *)
      begin match ty_s with
      | Type.Fun (ty_ret, ty_l') when List.length ty_l' = List.length l ->
        let ctx = List.fold_left2 Ctx.unify ctx ty_l ty_l' in
        ctx, ty_ret
      | _ ->
        let msg =
          Util.sprintf "%a should be a %d-ary function, but has type %a"
          Symbol.pp s (List.length l) Type.pp ty_s
        in
        raise (Type.Error msg)
      end
    end

let infer ctx t =
  let _, ty = infer_update ctx t in
  ty

let check_type ctx t ty =
  failwith "check_type: not implemented"

(*

(* recursive type inference for this term, in the given typing environment.
    It returns a type, and an offset (scope) that binds the variables
    in this type. *)
let rec infer_env ~newvars env t =
  match t.T.type_, t.T.term with
  | Some ty, _ -> ty, mk_scope env
  | None, T.Var _ -> T.Types.Type.i  (* if not specified, default is individual *)
  | None, T.BoundVar _ -> get_bound_type env
  | None, T.At (t1, t2) ->
    let o = mk_scope env in
    let ty1, o1 = infer_env env t1 in
    let ty2, o2 = infer_env env t2 in
    (* unify ty1 with (v1 <= v2), then unify ty2 with v2, and return v1 *)
    let v1 = T.Types.ty_var 0 in
    let v2 = T.Types.ty_var 1 in
    let ty1' = T.Types.(v1 <=. v2) in
    env.subst <- Unif.unification env.subst ty1 o1 ty1' o;
    env.subst <- Unif.unification env.subst ty2 o2 v2 o;
    v1, o
  | None, T.Node (s, l) ->
    begin try
      Signature.find env.signature s
    with Not_found ->
      (* build an arrow type for [s], with arguments' types *)
      let v = T.Types.ty_var 0 in
      let args = List.map (infer_env ~newvars env) l in
      let ty = T.Types.(v <== args) in
      let o = mk_scope env in
      (* remember to save this symbol's type afterwards! *)
      (if not (List.mem_assq s !newvars)
        then newvars := (s, ty) :: !newvars);
      ty, o
    end
  | None, T.Bind (s, t') ->

let infer signature t =
  let env = mk_env signature in
  let ty, offset = infer_env env t in
  (* rename variables *)
  let renaming = Substs.Renaming.create 5 in
  Substs.apply_subst ~recursive:true ~renaming env.subst ty offset

let infer_update signature t =
  failwith "Typing.infer_update: not implemented"
*)
