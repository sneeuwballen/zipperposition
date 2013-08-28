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
    st : Type.Stack.t;              (* bindings stack *)
    mutable db : Type.t list;       (* types of bound variables (stack) *)
    mutable signature : Signature.t;(* symbol -> type *)
  }

  let create () = {
    st = Type.Stack.create ();
    db = [];
    signature = Signature.empty;
  }

  let of_signature signature = {
    st = Type.Stack.create ();
    db = [];
    signature;
  }

  let add_signature ctx signature =
    ctx.signature <- Signature.merge ctx.signature signature;
    ()

  let within_binder ctx f =
    let gvar = Type.new_gvar () in
    ctx.db <- gvar :: ctx.db;
    try
      let x = f gvar in
      ctx.db <- List.tl ctx.db;
      x
    with e ->
      ctx.db <- List.tl ctx.db;
      raise e

  let db_type ctx i =
    if i >= 0 && i < List.length ctx.db
      then List.nth ctx.db i
      else
        failwith (Printf.sprintf "TypeInference.Ctx.db_type: idx %d not bound" i)

  let unify ctx ty1 ty2 =
    Util.debug 5 "Ctx.unify %a %a" Type.pp ty1 Type.pp ty2;
    Type.unify ctx.st ty1 ty2;
    ()

  let type_of_symbol ctx s =
    try
      let ty = Signature.find ctx.signature s in
      let ty = Type.deref ty in
      Type.instantiate ty
    with Not_found ->
      (* give a new type variable to this symbol *)
      let ty = Type.new_gvar () in
      Util.debug 5 "Ctx: new type %a for symbol %a" Type.pp ty Symbol.pp s;
      ctx.signature <- Signature.declare ctx.signature s ty;
      ty

  let declare ctx s ty =
    assert (Type.is_closed ty);
    ctx.signature <- Signature.declare ctx.signature s ty;
    ()

  let to_signature ctx =
    let signature = ctx.signature in
    let signature = Signature.map signature (fun _ ty -> Type.deref ty) in
    let signature = Signature.filter signature (fun _ ty -> Type.is_closed ty) in
    signature

  let unwind_protect ctx f = Type.Stack.unwind_protect ctx.st f

  let protect ctx f = Type.Stack.protect ctx.st f
end

(** {2 Hindley-Milner} *)

(* infer a type for [t], possibly updating [ctx]. If [check] is true,
   will type-check recursively in the term even if it's not needed to
   compute its type. *)
let rec infer_rec ~check ctx t =
  let open Type.Infix in
  match t.T.type_ with
  | Some ty -> ty
  | None -> 
    begin match t.T.term with
    | T.Var _ -> Type.i
    | T.BoundVar i -> Ctx.db_type ctx i
    | T.At (t1, t2) ->
      let ty1 = infer_rec ~check ctx t1 in
      let ty2 = infer_rec ~check ctx t2 in
      (* t1 : ty1, t2 : ty2. Now we must also have
         ty1 = ty2 -> ty1_ret, ty1_ret being the result type *)
      let ty1_ret = Type.new_gvar () in
      Ctx.unify ctx ty1 (ty1_ret <=. ty2);
      ty1_ret
    | T.Bind (s, t') ->
      let ty_s = Ctx.type_of_symbol ctx s in
      Ctx.within_binder ctx
        (fun v ->
          let ty_t' = infer_rec ~check ctx t' in
          (* now, the bound variable has type [v], [s] has type [ty_s]
              and should also have type [(v -> ty_t') -> ty_t'].
              The resulting type is [ty_t'] *)
          Ctx.unify ctx ty_s (ty_t' <=. (ty_t' <=. v));
          ty_t')
    | T.Node (s, l) ->
      let ty_s = Ctx.type_of_symbol ctx s in
      begin match ty_s with
      | Type.Fun (ret, _) when (not check) && Type.is_ground ret ->
        ret  (* no need to recurse *)
      | Type.App (_, _) when (not check) && Type.is_ground ty_s ->
        ty_s (* no need to recurse *)
      | _ ->
        let ty_l = List.fold_right
          (fun t' ty_l -> infer_rec ~check ctx t' :: ty_l) l [] in
        (* [s] has type [ty_s], but must also have type [ty_l -> 'a].
            The result is 'a. *)
        let ty_ret = Type.new_gvar () in
        Ctx.unify ctx ty_s (ty_ret <== ty_l);
        ty_ret
      end
    end

let infer ctx t =
  let check = true in
  let ty = Ctx.unwind_protect ctx (fun () -> infer_rec ~check ctx t) in
  Type.deref ty

let infer_sig signature t =
  let ctx = Ctx.of_signature signature in
  let check = true in
  let ty = Ctx.unwind_protect ctx (fun () -> infer_rec ~check ctx t) in
  Type.deref ty

let infer_no_check ctx t =
  let check = false in
  let ty = Ctx.unwind_protect ctx (fun () -> infer_rec ~check ctx t) in
  Type.deref ty

let default_to_i ctx =
  let signature = ctx.Ctx.signature in
  Symbol.SMap.iter
    (fun s ty ->
      let ty = Type.deref ty in
      let gvars = Type.free_vars ty in
      List.iter (fun gvar -> Type.bind gvar Type.i) gvars)
    signature;
  ()

let constrain_term_term ctx t1 t2 =
  let check = true in
  Ctx.unwind_protect ctx
    (fun () ->
      let ty1 = infer_rec ~check ctx t1 in
      let ty2 = infer_rec ~check ctx t2 in
      Ctx.unify ctx ty1 ty2)

let constrain_term_type ctx t ty =
  let check = true in
  Ctx.unwind_protect ctx
    (fun () ->
      let ty' = infer_rec ~check ctx t in
      Ctx.unify ctx ty ty')

let check_term_type ctx t ty =
  let check = false in
  Ctx.protect ctx
    (fun () ->
      let ty_t = infer_rec ~check ctx t in
      try
        Ctx.unify ctx ty_t ty;
        true
      with Type.Error _ ->
        false)

let check_type_type ctx ty1 ty2 =
  Ctx.protect ctx
    (fun () ->
      try
        Ctx.unify ctx ty1 ty2;
        true
      with Type.Error _ ->
        false)

let check_term_term ctx t1 t2 =
  let check = false in
  Ctx.protect ctx
    (fun () ->
      try
        (match t1.T.type_, t2.T.type_ with
        | Some ty1, Some ty2 -> Ctx.unify ctx ty1 ty2
        | Some ty1, None ->
          let ty2 = infer_rec ~check ctx t2 in
          Ctx.unify ctx ty1 ty2
        | None, Some ty2 ->
          let ty1 = infer_rec ~check ctx t1 in
          Ctx.unify ctx ty1 ty2
        | None, None ->
          let ty1 = infer_rec ~check ctx t1 in
          let ty2 = infer_rec ~check ctx t2 in
          Ctx.unify ctx ty1 ty2);
        true
      with Type.Error _ ->
        false)
