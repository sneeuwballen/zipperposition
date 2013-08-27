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

  let of_signature signature = {
    offset = 0;
    db = [];
    varsubst = Type.Subst.empty;
    signature;
  }

  let add_signature ctx signature =
    { ctx with signature = Signature.merge ctx.signature signature; }

  (* allocate a new type variable *)
  let new_var ctx =
    let ty = Type.var ctx.offset in
    { ctx with offset = ctx.offset + 1; }, ty

  (* allocate [n] new type variables *)
  let rec new_vars ctx n =
    if n = 0
      then ctx, []
      else
        let ctx, l = new_vars ctx (n-1) in
        let ctx, v = new_var ctx in
        ctx, v :: l

  (* push variable on  the bound vars stack *)
  let enter_binder ctx =
    let ctx, v = new_var ctx in
    let ctx = { ctx with db = v :: ctx.db; } in
    ctx, v
      
  (* pop variable from the bound var stack *)
  let exit_binder ctx =
    match ctx.db with
    | [] -> failwith "TypeInference.Ctx.exit_binder: empty binding stack"
    | _::db' -> { ctx with db = db'; }

  let unify ctx t1 t2 =
    Util.printf "Ctx.unify %a %a\n" Type.pp t1 Type.pp t2;
    let varsubst = Type.unify ~subst:ctx.varsubst t1 t2 in
    { ctx with varsubst; }

  let rename_type ctx ty =
    Util.printf "Ctx.rename_type %a\n" Type.pp ty;
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
      ctx, ty
    with Not_found ->
      (* give a new type variable to this symbol *)
      let ctx, ty = new_var ctx in
      Util.printf "Ctx: new type %a for symbol %a\n" Type.pp ty Symbol.pp s;
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
         ty1 = ty2 -> ty1_ret, ty1_ret being the result type *)
      let ctx, ty1_ret = Ctx.new_var ctx in
      let ctx = Ctx.unify ctx ty1 (ty1_ret <=. ty2) in
      ctx, ty1_ret
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
      let ctx, ty_ret = Ctx.new_var ctx in
      let ctx = Ctx.unify ctx ty_s (ty_ret <== ty_l) in
      ctx, ty_ret
    end

let infer ctx t =
  let _, ty = infer_update ctx t in
  ty

let infer_sig signature t =
  let ctx = Ctx.of_signature signature in
  infer ctx t

let constrain_term_term ctx t1 t2 =
  let ctx, ty1 = infer_update ctx t1 in
  let ctx, ty2 = infer_update ctx t2 in
  let ctx = Ctx.unify ctx ty1 ty2 in
  ctx

let constrain_term_type ctx t ty =
  let ctx, ty' = infer_update ctx t in
  let ctx = Ctx.unify ctx ty ty' in
  ctx

let check_term_type ctx t ty =
  let ctx, ty_t = infer_update ctx t in
  try
    ignore (Ctx.unify ctx ty_t ty);
    true
  with Type.Error _ ->
    false

let check_type_type ctx ty1 ty2 =
  try
    ignore (Ctx.unify ctx ty1 ty2);
    true
  with Type.Error _ ->
    false

let check_term_term ctx t1 t2 =
  try
    ignore
      (match t1.T.type_, t2.T.type_ with
      | Some ty1, Some ty2 -> Ctx.unify ctx ty1 ty2
      | Some ty1, None ->
        let ctx, ty2 = infer_update ctx t2 in
        Ctx.unify ctx ty1 ty2
      | None, Some ty2 ->
        let ctx, ty1 = infer_update ctx t1 in
        Ctx.unify ctx ty1 ty2
      | None, None ->
        let ctx, ty1 = infer_update ctx t1 in
        let ctx, ty2 = infer_update ctx t2 in
        Ctx.unify ctx ty1 ty2);
    true
  with Type.Error _ ->
    false
