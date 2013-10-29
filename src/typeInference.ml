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

module Ty = Type
module S = Substs.Ty
module Unif = TypeUnif

module STbl = Symbol.Tbl

type scope = Substs.scope

let prof_infer = Util.mk_profiler "TypeInference.infer"

(** {2 Typing context}

The scope maintained by the typing context starts at 1.
Scope 0 should be used for ground types.
*)

module Ctx = struct
  type t = {
    default : Type.t;               (* default type *)
    mutable scope : int;            (* next scope *)
    mutable var : int;              (* generate fresh vars *)
    mutable db : (Type.t * scope) list;  (* types of bound variables (stack) *)
    mutable signature : Signature.t;(* symbol -> type *)
    mutable subst : S.t;            (* variable bindings *)
    symbols : (Type.t * scope) STbl.t; (* symbol -> instantiated type *)
  }

  let create ?(default=Type.i) ?(base=true) () =
    let signature = if base then Signature.base else Signature.empty in
    let ctx = {
      default;
      scope = 1;
      var = ~-1;
      db = [];
      signature;
      subst = S.create 17;
      symbols = STbl.create 7;
    } in
    ctx

  let of_signature ?(default=Type.i) signature =
    let ctx = {
      default;
      scope = 1;
      var = ~-1;
      db = [];
      signature;
      subst = S.create 17;
      symbols = STbl.create 7;
    } in
    ctx

  let clear ctx =
    ctx.scope <- 0;
    ctx.var <- ~-1;
    ctx.db <- [];
    ctx.subst <- S.empty;
    STbl.clear ctx.symbols;
    ctx.signature <- Signature.empty;
    ()

  let add_signature ctx signature =
    ctx.signature <- Signature.merge ctx.signature signature;
    ()

  let set_signature ctx signature =
    ctx.signature <- signature;
    ()

  let _new_scope ctx =
    let n = ctx.scope in
    ctx.scope <- n+1;
    n

  let _new_var ctx =
    let n = ctx.var in
    ctx.var <- n -1 ;
    Type.__var n

  (* unify within the context's substitution *)
  let unify ctx ty1 s1 ty2 s2 =
    Unif.unify ~subst:ctx.subst ty1 s1 ty2 s2

  (* same as {!unify}, but also updates the ctx's substitution *)
  let unify_and_set ctx ty1 s1 ty2 s2 =
    let subst = unify ctx ty1 s1 ty2 s2 in
    ctx.subst <- subst

  let within_binder ctx ~ty f =
    let scope = _new_scope ctx in
    ctx.db <- (ty, scope) :: ctx.db;
    try
      (* compute within scope *)
      let x = f ty scope in
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

  let type_of_fun ctx s =
    match s with
    | Symbol.Int _
    | Symbol.Rat _
    | Symbol.Real _ -> Symbol.Arith.typeof s, 0  (* ground *)
    | Symbol.Const _ ->
      begin try
        let ty = Signature.find ctx.signature s in
        ty, _new_scope ctx
      with Not_found ->
        (* give a new type variable to this symbol. The symbol will not
          be able to be polymorphic (need to declare it!). *)
        try
          STbl.find ctx.symbols s
        with Not_found ->
          let v = Type.var 0 in
          let scope = _new_scope ctx in
          STbl.add ctx.symbols s (v, scope);
          v, scope
      end

  let declare ctx s ty =
    ctx.signature <- Signature.declare ctx.signature s ty;
    ()

  let eval_type ?(renaming=S.Renaming.create 4) ctx ty s_ty =
    S.apply ctx.subst ~renaming ty s_ty

  let to_signature ctx =
    let signature = ctx.signature in
    (* enrich signature with new symbols *)
    STbl.fold
      (fun s (ty,scope) signature ->
        (* evaluate type (no renaming needed because we don't care
            about collisions, all variables will be grounded) *)
        let ty = S.apply_no_renaming ctx.subst ty scope in
        (* bind all remaining free variables to [ctx.default] *)
        let vars = Ty.free_vars ty in
        let subst' = S.of_list
          (List.map (fun v -> v, 1, ctx.default, 0) vars)
        in
        let ty = S.apply_no_renaming subst' ty scope in
        (* add to signature *)
        Signature.declare signature s ty)
      ctx.symbols signature
end

(** {2 Hindley-Milner} *)

module type S = sig
  type term

  val infer : Ctx.t -> term -> scope -> Type.t
    (** Infer the type of this term under the given signature. This updates
        the context's typing environment! The resulting type
        is to be used within the same scope as the last argument.
        @raise TypeUnif.Error if the types are inconsistent *)

  val infer_eval : ?renaming:Substs.Ty.Renaming.t ->
                    Ctx.t -> term -> scope -> Type.t
    (** Infer the type of the given term, and then evaluate the type
        in the given renaming (desambiguate scopes). *)

  (** {3 Constraining types} *)

  val constrain_term_term : Ctx.t -> term -> scope -> term -> scope -> unit
    (** Force the two terms to have the same type in this context
        @raise TypeUnif.Error if an inconsistency is detected *)

  val constrain_term_type : Ctx.t -> term -> scope -> Type.t -> scope -> unit
    (** Force the term to have the given type in the given scope.
        @raise TypeUnif.Error if an inconsistency is detected *)

  (** {3 Checking compatibility} *)

  val check_term_type : Ctx.t -> term -> scope -> Type.t -> scope -> bool
    (** Check whether this term can be used with this type. *)

  val check_term_term : Ctx.t -> term -> scope -> term -> scope -> bool
    (** Can we unify the terms' types? *)

  val check_term_term_sig : Signature.t -> term -> scope -> term -> scope -> bool

  val check_term_type_sig : Signature.t -> term -> scope -> Type.t -> scope -> bool

  (** {3 Handy shortcuts for type inference}
  This module provides an easy way to specify constraints. Every term and
  type is assumed to live in the scope 0. *)

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
  val infer_rec : Ctx.t -> term -> scope -> Type.t
end) = struct
  type term = T.term

  (* wrapper to [infer_rec], with profiling *)
  let infer_rec ctx t s_t =
    Util.enter_prof prof_infer;
    try
      let res = T.infer_rec ctx t s_t in
      Util.exit_prof prof_infer;
      res
    with e ->
      Util.exit_prof prof_infer;
      raise e

  let infer ctx t s_t = infer_rec ctx t s_t

  let infer_eval ?(renaming=S.Renaming.create 5) ctx t s_t =
    let ty = infer_rec ctx t s_t in
    S.apply ctx.Ctx.subst ~renaming ty s_t

  let constrain_term_term ctx t1 s1 t2 s2 =
    let ty1 = infer_rec ctx t1 s1 in
    let ty2 = infer_rec ctx t2 s2 in
    Ctx.unify_and_set ctx ty1 s1 ty2 s2

  let constrain_term_type ctx t s_t ty s_ty =
    let ty1 = infer_rec ctx t s_t in
    Ctx.unify_and_set ctx ty1 s_t ty s_ty

  let check_term_type ctx t s_t ty s_ty =
    let ty' = infer_rec ctx t s_t in
    try
      ignore (Ctx.unify ctx ty' s_t ty (Ctx._new_scope ctx));
      true
    with Unif.Error _ ->
      false

  let check_term_term ctx t1 s1 t2 s2 =
    let ty1 = infer_rec ctx t1 s1 in
    let ty2 = infer_rec ctx t2 s2 in
    try
      ignore (Ctx.unify ctx ty1 s1 ty2 s2);
      true
    with Unif.Error _ ->
      false

  let check_term_term_sig signature t1 s1 t2 s2 =
    let ctx = Ctx.of_signature signature in
    check_term_term ctx t1 s1 t2 s2

  let check_term_type_sig signature t1 s1 ty2 s2 =
    let ctx = Ctx.of_signature signature in
    check_term_type ctx t1 s1 ty2 s2

  module Quick = struct
    type constr =
      | WellTyped of term
      | SameType of term * term
      | HasType of term * Type.t

    let constrain_seq ?(ctx=Ctx.create ()) seq =
      Sequence.iter
        (function
        | WellTyped t -> ignore (infer_rec ctx t 0)
        | SameType (t1, t2) -> constrain_term_term ctx t1 0 t2 0
        | HasType (t, ty) -> constrain_term_type ctx t 0 ty 0)
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

module FO = struct
  module TI = Make(struct
    module T = FOTerm

    type term = T.t

    (* return a fresh var in the current scope *)
    let _get_sym ctx s scope =
      let ty', s' = Ctx.type_of_fun ctx s in
      let v = Ctx._new_var ctx in
      Ctx.unify_and_set ctx v scope ty' s';
      v

    let _get_db ctx i scope =
      let ty', s' = Ctx.db_type ctx i in
      let v = Ctx._new_var ctx in
      Ctx.unify_and_set ctx v scope ty' s';
      v

    (* infer a type for [t], possibly updating [ctx]. *)
    let rec infer_rec ctx t s_t =
      match t.T.term with
      | T.Var _ -> T.get_type t
      | T.BoundVar i -> _get_db ctx i s_t
      | T.Node (s, []) -> _get_sym ctx s s_t
      | T.Node (s, l) ->
        let ty_s = _get_sym ctx s s_t in
        let ty_l = List.fold_right
          (fun t' ty_l -> infer_rec ctx t' s_t :: ty_l) l [] in
        (* [s] has type [ty_s], but must also have type [ty_l -> 'a].
            We generate a fresh variable 'a, which is also the result. *)
        let ty_ret = Ctx._new_var ctx in
        Ctx.unify_and_set ctx ty_s s_t (Type.mk_fun ty_ret ty_l) s_t;
        ty_ret
  end)
  
  include TI

  module F = FOFormula

  let rec constrain_form ctx f s_f = match f.F.form with
    | F.True
    | F.False -> ()
    | F.Atom p -> constrain_term_type ctx p s_f Type.o s_f
    | F.Equal (t1, t2) ->
      constrain_term_term ctx t1 s_f t2 s_f
    | F.And l
    | F.Or l -> List.iter (fun f' -> constrain_form ctx f' s_f) l
    | F.Forall (ty,f')
    | F.Exists (ty,f') ->
      Ctx.within_binder ctx ~ty (fun _ _ -> constrain_form ctx f' s_f)
    | F.Equiv (f1, f2)
    | F.Imply (f1, f2) -> constrain_form ctx f1 s_f; constrain_form ctx f2 s_f
    | F.Not f' -> constrain_form ctx f' s_f

  let signature_forms ?(signature=Signature.empty) seq =
    let ctx = Ctx.of_signature signature in
    Sequence.iter
      (fun f -> constrain_form ctx f 0)
      seq;
    Ctx.to_signature ctx
end

module HO = Make(struct
  module T = HOTerm
  type term = T.t

  (* return a fresh var in the current scope *)
  let _get_sym ctx s scope =
    let ty', s' = Ctx.type_of_fun ctx s in
    let v = Ctx._new_var ctx in
    Ctx.unify_and_set ctx v scope ty' s';
    v

  let _get_db ctx i scope =
    let ty', s' = Ctx.db_type ctx i in
    let v = Ctx._new_var ctx in
    Ctx.unify_and_set ctx v scope ty' s';
    v

  (* infer a type for [t], possibly updating [ctx]. *)
  let rec infer_rec ctx t s_t =
    match t.T.term with
    | T.Var _ -> T.get_type t
    | T.BoundVar i -> _get_db ctx i s_t
    | T.At (t1, t2) ->
      let ty1 = infer_rec ctx t1 s_t in
      let ty2 = infer_rec ctx t2 s_t in
      (* t1 : ty1, t2 : ty2. Now we must also have
         ty1 = ty2 -> ty1_ret, ty1_ret being the result type *)
      let ty1_ret = Ctx._new_var ctx in
      Ctx.unify_and_set ctx ty1 s_t Type.(ty1_ret <=. ty2) s_t;
      ty1_ret
    | T.Bind (s, t') ->
      let ty = T.get_type t in
      Ctx.within_binder ctx ~ty (fun _ _ -> infer_rec ctx t' s_t)
    | T.Const s -> _get_sym ctx s s_t
end)
