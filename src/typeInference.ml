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

let prof_infer = Util.mk_profiler "TypeInference.infer"

(** {2 Typing context} *)

module Ctx = struct
  type t = {
    st : Type.Stack.t;              (* bindings stack *)
    default : Type.t;               (* default type *)
    mutable db : Type.t list;       (* types of bound variables (stack) *)
    mutable signature : Signature.t;(* symbol -> type *)
  }

  let create ?(default=Type.i) ?(base=true) () =
    let signature = if base then Signature.base else Signature.empty in
    {
      st = Type.Stack.create ();
      default;
      db = [];
      signature;
    }

  let of_signature ?(default=Type.i) signature = {
    st = Type.Stack.create ();
    default;
    db = [];
    signature;
  }

  let add_signature ctx signature =
    ctx.signature <- Signature.merge ctx.signature signature;
    ()

  let within_binder ctx ~ty f =
    ctx.db <- ty :: ctx.db;
    try
      let x = f () in
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

  let type_of_fun ctx s =
    match s with
    | Symbol.Int _
    | Symbol.Rat _
    | Symbol.Real _ -> Symbol.Arith.typeof s
    | Symbol.Const _ ->
      begin try
        let ty = Signature.find ctx.signature s in
        Type.instantiate ty
      with Not_found ->
        (* give a new type variable to this symbol *)
        let ty = Type.new_gvar () in
        Util.debug 5 "Ctx: new type %a for function %a" Type.pp ty Symbol.pp s;
        ctx.signature <- Signature.declare ctx.signature s ty;
        ty
      end

  let declare ctx s ty =
    assert (Type.is_closed ty);
    ctx.signature <- Signature.declare ctx.signature s ty;
    ()

  let to_signature ctx =
    let signature = ctx.signature in
    let signature = Signature.map signature
      (fun _ ty ->
        (* bind all remaining free variables to [ctx.default] *)
        let vars = Type.free_gvars ty in
        List.iter (fun gv -> Type.bind gv ctx.default) vars;
        (* dereference the type *)
        let ty = Type.deref ty in
        assert (Type.is_closed ty);
        ty)
    in
    signature

  let unwind_protect ctx f = Type.Stack.unwind_protect ctx.st f

  let protect ctx f = Type.Stack.protect ctx.st f
end

(** {2 Hindley-Milner} *)

let check_type_type ctx ty1 ty2 =
  Type.unifiable ty1 ty2

let check_type_type_sig signature ty1 ty2 =
  let ctx = Ctx.of_signature signature in
  check_type_type ctx ty1 ty2

module type S = sig
  type term

  val infer : Ctx.t -> term -> Type.t
    (** Infer the type of this term under the given signature.  This updates
        the context's typing environment!
        @raise Type.Error if the types are inconsistent *)

  val infer_sig : Signature.t -> term -> Type.t
    (** Inference from a signature (shortcut) *)

  val infer_no_check : Ctx.t -> term -> Type.t
    (** Infer the type of the term, but does not recurse if it's not needed. *)

  (** {3 Constraining types} *)

  val constrain_term_term : Ctx.t -> term -> term -> unit
    (** Force the two terms to have the same type
        @raise Type.Error if an inconsistency is detected *)

  val constrain_term_type : Ctx.t -> term -> Type.t -> unit
    (** Force the term to have the given type.
        @raise Type.Error if an inconsistency is detected *)

  (** {3 Checking compatibility} *)

  val check_term_type : Ctx.t -> term -> Type.t -> bool
    (** Check whether this term can be used with this type *)

  val check_term_term : Ctx.t -> term -> term -> bool
    (** Can we unify the terms' types? *)

  val check_term_term_sig : Signature.t -> term -> term -> bool

  val check_term_type_sig : Signature.t -> term -> Type.t -> bool

  (** {3 Handy shortcuts for type inference} *)

  module Quick : sig
    (* type constraints *)
    type constr =
      | WellTyped of term
      | SameType of term * term
      | HasType of term * Type.t

    val constrain : ?ctx:Ctx.t -> constr list -> Ctx.t
    
    val constrain_seq : ?ctx:Ctx.t -> constr Sequence.t -> Ctx.t

    val signature : ?signature:Signature.t -> constr list -> Signature.t

    val signature_seq : ?signature:Signature.t -> constr Sequence.t -> Signature.t
  end
end

(* build an instance of {!S}, given the function that infers the type
    of a term. *)
module Make(T : sig
  type term
  val infer_rec : check:bool -> Ctx.t -> term -> Type.t
end) = struct
  type term = T.term

  (* wrapper to [infer_rec], with profiling *)
  let infer_type_of ~check ctx t =
    Util.enter_prof prof_infer;
    try
      let res = T.infer_rec ~check ctx t in
      Util.exit_prof prof_infer;
      res
    with e ->
      Util.exit_prof prof_infer;
      raise e

  let infer ctx t =
    let check = true in
    let ty = Ctx.unwind_protect ctx (fun () -> infer_type_of ~check ctx t) in
    Type.deref ty

  let infer_sig signature t =
    let ctx = Ctx.of_signature signature in
    let check = true in
    let ty = Ctx.unwind_protect ctx (fun () -> infer_type_of ~check ctx t) in
    Type.deref ty

  let infer_no_check ctx t =
    let check = false in
    let ty = Ctx.unwind_protect ctx (fun () -> infer_type_of ~check ctx t) in
    Type.deref ty

  let constrain_term_term ctx t1 t2 =
    let check = true in
    Ctx.unwind_protect ctx
      (fun () ->
        let ty1 = infer_type_of ~check ctx t1 in
        let ty2 = infer_type_of ~check ctx t2 in
        Ctx.unify ctx ty1 ty2)

  let constrain_term_type ctx t ty =
    let check = true in
    Ctx.unwind_protect ctx
      (fun () ->
        let ty' = infer_type_of ~check ctx t in
        Ctx.unify ctx ty' (Type.instantiate ty))

  let check_term_type ctx t ty =
    let check = false in
    Ctx.protect ctx
      (fun () ->
        let ty_t = infer_type_of ~check ctx t in
        Type.unifiable ty ty_t)

  let check_term_term ctx t1 t2 =
    let check = false in
    Ctx.protect ctx
      (fun () ->
        let ty1 = infer_type_of ~check ctx t1 in
        let ty2 = infer_type_of ~check ctx t2 in
        Type.unifiable ty1 ty2)

  let check_term_term_sig signature t1 t2 =
    let ctx = Ctx.of_signature signature in
    check_term_term ctx t1 t2

  let check_term_type_sig signature t1 ty2 =
    let ctx = Ctx.of_signature signature in
    check_term_type ctx t1 ty2

  module Quick = struct
    type constr =
      | WellTyped of term
      | SameType of term * term
      | HasType of term * Type.t

    let constrain_seq ?(ctx=Ctx.create ()) seq =
      Sequence.iter
        (function
        | WellTyped t -> ignore (infer ctx t)
        | SameType (t1, t2) -> constrain_term_term ctx t1 t2
        | HasType (t, ty) -> constrain_term_type ctx t ty)
        seq;
      ctx

    let constrain ?ctx l = constrain_seq ?ctx (Sequence.of_list l)

    let signature_seq ?(signature=Signature.empty) seq =
      let ctx = Ctx.of_signature signature in
      ignore (constrain_seq ~ctx seq);
      Ctx.to_signature ctx

    let signature ?signature l = signature_seq ?signature (Sequence.of_list l)
  end
end

module FO = Make(struct
  module T = FOTerm

  type term = T.t

  (* infer a type for [t], possibly updating [ctx]. If [check] is true,
     will type-check recursively in the term even if it's not needed to
     compute its type. *)
  let rec infer_rec ~check ctx t =
    let open Type.Infix in
    match t.T.term with
    | T.Var _ ->
      let ty = T.get_type t in
      Type.instantiate ty
    | T.BoundVar i -> Ctx.db_type ctx i
    | T.Node (s, []) -> Ctx.type_of_fun ctx s
    | T.Node (s, l) ->
      let ty_s = Ctx.type_of_fun ctx s in
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
end)

module HO = Make(struct
  module T = HOTerm
  type term = T.t

  (* infer a type for [t], possibly updating [ctx]. If [check] is true,
     will type-check recursively in the term even if it's not needed to
     compute its type. *)
  let rec infer_rec ~check ctx t =
    let open Type.Infix in
    match t.T.term with
    | T.Var _ ->
      let ty = T.get_type t in
      Type.instantiate ty
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
      let ty = Type.instantiate (T.get_type t) in
      let vars = Type.free_gvars ty in
      Ctx.within_binder ~ty ctx
        (fun () ->
          let ty_t' = infer_rec ~check ctx t' in
          (* re-generalize type variables *)
          List.iter Type.close_var vars;
          ty_t')
    | T.Const s -> Ctx.type_of_fun ctx s
end)
