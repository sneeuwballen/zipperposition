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
    tyctx : Type.ctx;               (* convert types *)
    vars : (string, (int * Type.t)) Hashtbl.t;  (* var name -> number + type *)
  }

  let create ?(default=Type.i) ?(base=true) () =
    let signature = if base then Signature.base else Signature.empty in
    let ctx = {
      default;
      scope = 1;
      var = 0;
      db = [];
      signature;
      subst = S.create 17;
      symbols = STbl.create 7;
      tyctx = Type.create_ctx ();
      vars = Hashtbl.create 13;
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
      tyctx = Type.create_ctx ();
      vars = Hashtbl.create 13;
    } in
    ctx

  let clear ctx =
    ctx.scope <- 0;
    ctx.var <- ~-1;
    ctx.db <- [];
    ctx.subst <- S.empty;
    STbl.clear ctx.symbols;
    ctx.signature <- Signature.empty;
    Hashtbl.clear ctx.tyctx;
    ()

  let add_signature ctx signature =
    ctx.signature <- Signature.merge ctx.signature signature;
    ()

  let set_signature ctx signature =
    ctx.signature <- signature;
    ()

  let _cur_scope ctx = ctx.scope

  let _new_scope ctx =
    let n = ctx.scope in
    ctx.scope <- n+1;
    n

  (* generate fresh vars. Those should always live in their own scope *)
  let _new_var ctx =
    let n = ctx.var in
    ctx.var <- n + 1 ;
    Type.var n

  let _of_parsed ctx ty =
    Type.of_parsed ~ctx:ctx.tyctx ty

  (* variable number and type, for the given name. An optional type can
    be provided.*)
  let _get_var ?ty ctx name =
    try
      let n, ty' = Hashtbl.find ctx.vars name in
      match ty with
      | None -> n, ty'
      | Some ty ->
        (* check same type *)
        if Type.eq ty ty'
          then n, ty'
          else failwith
            (Util.sprintf "type mismatch for var %s: %a and %a"
              name Type.pp ty Type.pp ty')
    with Not_found ->
      let n = Hashtbl.length ctx.vars in
      let ty = match ty with
        | None -> ctx.default
        | Some ty -> ty
      in
      Hashtbl.add ctx.vars name (n,ty);
      n, ty

  (* unify within the context's substitution *)
  let unify ctx ty1 s1 ty2 s2 =
    Unif.unify ~subst:ctx.subst ty1 s1 ty2 s2

  (* same as {!unify}, but also updates the ctx's substitution *)
  let unify_and_set ctx ty1 s1 ty2 s2 =
    let subst = unify ctx ty1 s1 ty2 s2 in
    ctx.subst <- subst

  (* Provides a context, corresponding to a term binding environment.
     Within the local context, the bound De Bruijn variable will
     have the (type,scope) that are passed as arguments to the closure. *)
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

  (* The type of the bound variable of De Bruijn index [i].
     {!within_binder} must have been used enough times before, so
     that a type is attributed to the [i]-th bound variable.
     @raise Invalid_argument if the [i]-th variable is not bound.*)
  let db_type ctx i =
    if i >= 0 && i < List.length ctx.db
      then List.nth ctx.db i
      else
        failwith (Printf.sprintf "TypeInference.Ctx.db_type: idx %d not bound" i)

  (* If the function symbol has an unknown type, a fresh variable
     (in a fresh scope, that is) is returned. Otherwise the known
     type of the symbol is returned along with a new scope,
     so that, for instance, "nil" (the empty list) can have several
     concrete types (using distinct scopes).
     
     @param ret the expected return type (if symbol not declared). Must be ground
     @arity the expected arity (if not declared)
  *)
  let type_of_fun ?ret ~arity ctx s =
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
          let vars = Util.list_range 0 (arity-1) in
          let vars = List.map Type.var vars in
          let ret = match ret with
            | None -> ctx.default
            | Some ty -> assert (Type.is_ground ty); ty
          in
          let ty = Type.(ret <== vars) in
          let scope = _new_scope ctx in
          STbl.add ctx.symbols s (ty, scope);
          ty, scope
      end

  let declare ctx s ty =
    ctx.signature <- Signature.declare ctx.signature s ty;
    ()

  let declare_parsed ctx s ty =
    let ty = _of_parsed ctx ty in
    declare ctx s ty

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

type 'a closure = Substs.Ty.Renaming.t -> Substs.Ty.t -> 'a
  (** Function that returns a ['a] value if provided with a proper
      type substitution *)

module type S = sig
  type untyped (* untyped term *)
  type typed   (* typed term *)

  val infer : ?pred:bool -> Ctx.t -> untyped -> scope -> Type.t * typed closure
    (** Infer the type of this term under the given signature. This updates
        the context's typing environment! The resulting type's variables
        belong to the given scope.

        @param ctx the context
        @param untyped the untyped term whose type must be inferred
        @param scope where the term's type variables live
        @param pred true if we expect a predicate (return type {!Type.o})

        @return the inferred type of the untyped term (possibly a type var)
          along with a closure to produce a typed term once every
          constraint has been solved
        @raise TypeUnif.Error if the types are inconsistent *)

  (** {3 Constraining types}
  
  This section is mostly useful for inferring a signature without
  converting untyped_terms into typed_terms. *)

  val constrain_term_term : Ctx.t -> untyped -> scope -> untyped -> scope -> unit
    (** Force the two terms to have the same type in this context
        @raise TypeUnif.Error if an inconsistency is detected *)

  val constrain_term_type : Ctx.t -> untyped -> scope -> Type.t -> scope -> unit
    (** Force the term's type and the given type to be the same.
        @raise TypeUnif.Error if an inconsistency is detected *)
end

module FO = struct
  module UT = Untyped.FO
  module T = FOTerm
  module UF = Untyped.Form
  module F = FOFormula

  type untyped = UT.t
  type typed = T.t

  let _get_sym ?ret ~arity ctx s scope  =
    let ty', s' = Ctx.type_of_fun ?ret ~arity ctx s in
    let v = Ctx._new_var ctx in
    Ctx.unify_and_set ctx v scope ty' s';
    v

  let _get_db ctx i scope =
    let ty', s' = Ctx.db_type ctx i in
    let v = Ctx._new_var ctx in
    Ctx.unify_and_set ctx v scope ty' s';
    v

  (* convert type *)
  let _get_ty ctx ty =
    Ctx._of_parsed ctx ty

  (* infer a type for [t], possibly updating [ctx]. Also returns a
    continuation to build a typed term.
    @param pred true if the term is a predicate *)
  let rec infer_rec ~pred ctx t s_t =
    let ret = if pred then Some Type.o else None in
    match t with
    | UT.Var (name, ty) ->
      let ty = _get_ty ctx ty in
      let i, ty = Ctx._get_var ctx ~ty name in
      let closure renaming subst =
        let ty = Substs.Ty.apply ~renaming subst ty s_t in
        T.mk_var ~ty i
      in
      ty, closure
    | UT.App (s, []) ->
      let ty = _get_sym ?ret ~arity:0 ctx s s_t in
      let closure renaming subst =
        let ty = Substs.Ty.apply ~renaming subst ty s_t in
        T.mk_const ~ty s
      in
      ty, closure
    | UT.App (s, l) ->
      let ty_s = _get_sym ?ret ~arity:(List.length l) ctx s s_t in
      let sub = List.map (fun t' -> infer_rec ~pred:false ctx t' s_t) l in
      let ty_l, closure_l = List.split sub in
      (* [s] has type [ty_s], but must also have type [ty_l -> 'a].
          We generate a fresh variable 'a, which is also the result. *)
      let ty_ret = Ctx._new_var ctx in
      Ctx.unify_and_set ctx ty_s s_t (Type.mk_fun ty_ret ty_l) s_t;
      (* now to produce the closure, that first creates subterms *)
      let closure renaming subst =
        let l' = List.map (fun closure' -> closure' renaming subst) closure_l in
        let ty = Substs.Ty.apply ~renaming subst ty_ret s_t in
        T.mk_node ~ty s l'
      in
      ty_ret, closure

  let infer ?(pred=false) ctx t s_t =
    Util.enter_prof prof_infer;
    try
      let ty, k = infer_rec ~pred ctx t s_t in
      if pred then Ctx.unify_and_set ctx ty s_t Type.o s_t; (* check *)
      Util.exit_prof prof_infer;
      ty, k
    with e ->
      Util.exit_prof prof_infer;
      raise e

  let constrain_term_term ctx t1 s1 t2 s2 =
    let ty1, _ = infer ctx t1 s1 in
    let ty2, _ = infer ctx t2 s2 in
    Ctx.unify_and_set ctx ty1 s1 ty2 s2

  let constrain_term_type ctx t s_t ty s_ty =
    let ty1, _ = infer ctx t s_t in
    Ctx.unify_and_set ctx ty1 s_t ty s_ty

  let rec infer_form ctx f s_f = match f with
    | UF.Bool b ->
      let closure renaming subst = if b then F.mk_true else F.mk_false in
      closure
    | UF.Nary (op, l) ->
      (* closures of sub formulas *)
      let l' = List.map (fun f' -> infer_form ctx f' s_f) l in
      let closure renaming subst =
        let l'' = List.map (fun f' -> f' renaming subst) l' in
        match op with
        | UF.And -> F.mk_and l''
        | UF.Or -> F.mk_or l''
      in
      closure
    | UF.Binary (op, f1, f2) ->
      let closure_f1 = infer_form ctx f1 s_f in
      let closure_f2 = infer_form ctx f2 s_f in
      let closure renaming subst =
        let f1' = closure_f1 renaming subst in
        let f2' = closure_f2 renaming subst in
        match op with
        | UF.Imply -> F.mk_imply f1' f2'
        | UF.Equiv -> F.mk_equiv f1' f2'
      in
      closure
    | UF.Not f' ->
      let closure_f' = infer_form ctx f' s_f in
      (fun renaming subst -> F.mk_not (closure_f' renaming subst))
    | UF.Atom p ->
      let ty, clos = infer ~pred:true ctx p s_f in
      Ctx.unify_and_set ctx ty s_f Type.o s_f;  (* must return Type.o *)
      let closure renaming subst =
        F.mk_atom (clos renaming subst)
      in
      closure
    | UF.Equal (t1, t2) ->
      let ty1, c1 = infer ctx t1 s_f in
      let ty2, c2 = infer ctx t2 s_f in
      Ctx.unify_and_set ctx ty1 s_f ty2 s_f;  (* must have same type *)
      let closure renaming subst =
        F.mk_eq (c1 renaming subst) (c2 renaming subst)
      in
      closure
    | UF.Quant (op, vars, f') ->
      let _, clos_vars = List.split (List.map (fun t -> infer ctx t s_f) vars) in
      let clos_f = infer_form ctx f' s_f in
      let closure renaming subst =
        let vars' = List.map (fun c -> c renaming subst) clos_vars in
        let f' = clos_f renaming subst in
        match op with
        | UF.Forall -> F.mk_forall_list vars' f'
        | UF.Exists -> F.mk_exists_list vars' f'
      in
      closure

  let constrain_form ctx f =
    let _ = infer_form ctx f 0 in
    ()

  let signature_forms signature seq =
    let ctx = Ctx.of_signature signature in
    Sequence.iter (constrain_form ctx) seq;
    Ctx.to_signature ctx

  let convert ~ctx f =
    let closure = infer_form ctx f 0 in
    let renaming = Substs.Ty.Renaming.create 13 in
    let subst = ctx.Ctx.subst in
    closure renaming subst

  let convert_clause ~ctx c =
    let closures = List.map (fun lit -> infer_form ctx lit 0) c in
    let renaming = Substs.Ty.Renaming.create 13 in
    let subst = ctx.Ctx.subst in
    List.map (fun c' -> c' renaming subst) closures

  let convert_seq ~ctx forms =
    (* build closures, inferring all types *)
    let closures = Sequence.map
      (fun f -> infer_form ctx f 0)
      forms
    in
    let closures = Sequence.to_rev_list closures in
    let renaming = Substs.Ty.Renaming.create 13 in
    let subst = ctx.Ctx.subst in
    (* apply closures to the final substitution *)
    List.rev_map
      (fun closure ->
        let f = closure renaming subst in
        Substs.Ty.Renaming.clear renaming;
        f)
      closures
end

module HO = struct
  module UT = Untyped.HO
  module T = HOTerm

  type untyped = UT.t
  type typed = T.t

  let _get_sym ?ret ~arity ctx s scope =
    let ty', s' = Ctx.type_of_fun ?ret ~arity ctx s in
    let v = Ctx._new_var ctx in
    Ctx.unify_and_set ctx v scope ty' s';
    v

  let _get_db ctx i scope =
    let ty', s' = Ctx.db_type ctx i in
    let v = Ctx._new_var ctx in
    Ctx.unify_and_set ctx v scope ty' s';
    v

  (* convert type *)
  let _get_ty ctx ty =
    Ctx._of_parsed ctx ty

  (* infer a type for [t], possibly updating [ctx]. Also returns a
    continuation to build a typed term
    @param pred true if we expect a proposition
    @param arity expected number of arguments *)
  let rec infer_rec ?(arity=0) ~pred ctx t s_t =
    let ret = if pred then Some Type.o else None in
    match t with
    | UT.Var (name, ty) ->
      let ty = _get_ty ctx ty in
      let i, ty = Ctx._get_var ctx ~ty name in
      let closure renaming subst =
        let ty = Substs.Ty.apply ~renaming subst ty s_t in
        T.mk_var ~ty i
      in
      ty, closure
    | UT.Const s ->
      let ty = _get_sym ?ret ~arity ctx s s_t in
      let closure renaming subst =
        let ty = Substs.Ty.apply ~renaming subst ty s_t in
        T.mk_const ~ty s
      in
      ty, closure
    | UT.App (_, []) -> assert false
    | UT.App (t, l) ->
      let ty_t, clos_t = infer_rec ~pred ~arity:(List.length l) ctx t s_t in
      let ty_l, clos_l = List.split
        (List.map (fun t' -> infer_rec ~pred:false ctx t' s_t) l) in
      let ty_ret = Ctx._new_var ctx in
      (* t:ty_t, l:ty_l. now we must have  ty_t = (ty_l -> ty_ret) *)
      Ctx.unify_and_set ctx ty_t s_t Type.(ty_ret <== ty_l) s_t;
      let closure renaming subst =
        let t' = clos_t renaming subst in
        let l' = List.map (fun c -> c renaming subst) clos_l in
        T.mk_at t' l'
      in
      ty_ret, closure
    | UT.Lambda (v, t) ->
      let ty_t, clos_t = infer_rec ~pred:false ctx t s_t in
      let ty_v, clos_v = infer_rec ~pred:false ctx v s_t in
      (* type is ty_v -> ty_t *)
      let ty = Type.(ty_t <=. ty_v) in
      let closure renaming subst =
        let t' = clos_t renaming subst in
        let v' = clos_v renaming subst in
        T.mk_lambda_var [v'] t'
      in
      ty, closure 

  let infer ?(pred=false) ctx t s_t =
    Util.enter_prof prof_infer;
    try
      let ty, k = infer_rec ~pred ctx t s_t in
      if pred then Ctx.unify_and_set ctx ty s_t Type.o s_t; (* check *)
      Util.exit_prof prof_infer;
      ty, k
    with e ->
      Util.exit_prof prof_infer;
      raise e

  let constrain_term_term ctx t1 s1 t2 s2 =
    let ty1, _ = infer ctx t1 s1 in
    let ty2, _ = infer ctx t2 s2 in
    Ctx.unify_and_set ctx ty1 s1 ty2 s2

  let constrain_term_type ctx t s_t ty s_ty =
    let ty1, _ = infer ctx t s_t in
    Ctx.unify_and_set ctx ty1 s_t ty s_ty

  let constrain ~ctx t =
    let _ = infer ~pred:true ctx t 0 in
    ()

  let convert ~ctx t =
    let _, closure = infer ctx t 0 in
    let renaming = Substs.Ty.Renaming.create 13 in
    let subst = ctx.Ctx.subst in
    closure renaming subst

  let convert_seq ~ctx terms =
    let closures = Sequence.map
      (fun t ->
        let _, closure = infer ~pred:true ctx t 0 in
        closure)
      terms
    in
    (* evaluate *)
    let closures = Sequence.to_rev_list closures in
    let renaming = Substs.Ty.Renaming.create 13 in
    let subst = ctx.Ctx.subst in
    List.rev_map
      (fun c ->
        Substs.Ty.Renaming.clear renaming;
        c renaming subst)
      closures
end
