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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBBTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BBT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBBTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BBT NOT LIMITED TO, PROCUREMENT OF SUBSTITBTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OBT OF THE USE
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

(** {2 Closures} *)

module Closure = struct
  type 'a t = Substs.Ty.Renaming.t -> Substs.Ty.t -> 'a
    (** Function that returns a ['a] value if provided with a proper
        type substitution *)

  let return x = fun _ _ -> x

  let map clos f =
    fun renaming subst ->
      f (clos renaming subst)

  let (>>=) clos f =
    fun renaming subst ->
      let x = clos renaming subst in
      f x renaming subst

  let pure_ty ty s_ty =
    fun renaming subst -> Substs.Ty.apply subst ~renaming ty s_ty
end

module TraverseClosure = Monad.Traverse(Closure)

(** {2 Typing context}

The scope maintained by the typing context starts at 1.
Scope 0 should be used for ground types.
*)

module Ctx = struct
  type t = {
    default : Type.t;               (* default type *)
    mutable scope : int;            (* next scope *)
    mutable var : int;              (* generate fresh vars *)
    mutable signature : Signature.t;(* symbol -> type *)
    mutable subst : S.t;            (* variable bindings *)
    mutable to_bind : (Type.t * scope) list;  (* constructor variables to bind *)
    renaming : Substs.Ty.Renaming.t;
    symbols : (Type.t * scope) STbl.t; (* symbol -> instantiated type *)
    tyctx : TypeConversion.ctx;     (* convert types *)
    vars : (string, (int * Type.t)) Hashtbl.t;  (* var name -> number + type *)
  }

  let create ?(default=Type.i) ?(base=true) () =
    let signature = if base then Signature.base else Signature.empty in
    let ctx = {
      default;
      scope = ~-1;
      var = ~-1;
      signature;
      subst = S.empty;
      to_bind = [];
      renaming = Substs.Ty.Renaming.create 7;
      symbols = STbl.create 7;
      tyctx = TypeConversion.create_ctx ();
      vars = Hashtbl.create 7;
    } in
    ctx

  let of_signature ?(default=Type.i) signature =
    let ctx = create ~base:false ~default () in
    { ctx with signature; }

  let clear ctx =
    ctx.scope <- 0;
    ctx.var <- ~-1;
    ctx.subst <- S.empty;
    ctx.to_bind <- [];
    STbl.clear ctx.symbols;
    ctx.signature <- Signature.empty;
    TypeConversion.clear ctx.tyctx;
    Hashtbl.clear ctx.vars;
    ()

  let exit_scope ctx =
    Hashtbl.clear ctx.vars

  let add_signature ctx signature =
    ctx.signature <- Signature.merge ctx.signature signature;
    ()

  let set_signature ctx signature =
    ctx.signature <- signature;
    ()

  let _cur_scope ctx = ctx.scope

  let _new_scope ctx =
    let n = ctx.scope in
    ctx.scope <- n-1;
    n

  (* generate fresh vars. Those should always live in their own scope *)
  let _new_var ctx =
    let n = ctx.var in
    ctx.var <- n - 1 ;
    Type.__var n

  let _of_quantified ctx ty =
    TypeConversion.of_quantified ~ctx:ctx.tyctx ty

  let _of_ty ctx ty =
    TypeConversion.of_basic ~ctx:ctx.tyctx ty

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

  (* enter new scope for the variable with this name *)
  let _enter_var_scope ctx name ty =
    let n = Hashtbl.length ctx.vars in
    Hashtbl.add ctx.vars name (n,ty);
    n

  let _exit_var_scope ctx name =
    Hashtbl.remove ctx.vars name

  (* unify within the context's substitution *)
  let unify ctx ty1 s1 ty2 s2 =
    Unif.unify ~subst:ctx.subst ty1 s1 ty2 s2

  (* same as {!unify}, but also updates the ctx's substitution *)
  let unify_and_set ctx ty1 s1 ty2 s2 =
    let subst = unify ctx ty1 s1 ty2 s2 in
    ctx.subst <- subst

  (* If the function symbol has an unknown type, a fresh variable
     (in a fresh scope, that is) is returned. Otherwise the known
     type of the symbol is returned along with a new scope,
     so that, for instance, "nil" (the empty list) can have several
     concrete types (using distinct scopes).
     
     @arity the expected arity (if not declared)
  *)
  let type_of_fun ~arity ctx s =
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
          let ret = Type.var 0 in
          let vars = Util.list_range 1 (arity+1) in
          let vars = List.map Type.var vars in
          let ty = Type.(ret <== vars) in
          let scope = _new_scope ctx in
          STbl.add ctx.symbols s (ty, scope);
          ctx.to_bind <- (ret, scope) :: ctx.to_bind;
          ty, scope
      end

  let declare ctx s ty =
    ctx.signature <- Signature.declare ctx.signature s ty;
    ()

  let declare_basic ctx s ty =
    let ty = _of_quantified ctx ty in
    declare ctx s ty

  let eval_type ?(renaming=S.Renaming.create 4) ctx ty s_ty =
    S.apply ctx.subst ~renaming ty s_ty

  let to_signature ctx =
    let signature = ctx.signature in
    (* enrich signature with new symbols *)
    STbl.fold
      (fun s (ty,scope) signature ->
        (* evaluate type. if variables remain, they are generalized *)
        let renaming = S.Renaming.create 4 in
        let ty = S.apply ~renaming ctx.subst ty scope in
        (* add to signature *)
        Signature.declare signature s ty)
      ctx.symbols signature

  let bind_to_default ctx =
    List.iter
      (fun (v, s_v) ->
        (* try to bind the variable. Will fail if already bound to
            something else, which is fine. *)
        try unify_and_set ctx v s_v ctx.default s_v
        with TypeUnif.Error _ -> ())
      ctx.to_bind;
    ctx.to_bind <- []

  let generalize ctx =
    (* keep constructor variables as they are *)
    ctx.to_bind <- []

  (* clear and return the renaming *)
  let renaming_clear ctx =
    let renaming = ctx.renaming in
    Substs.Ty.Renaming.clear renaming;
    renaming

  let apply_closure ?(default=true) ?renaming ctx closure =
    if default then bind_to_default ctx;
    let renaming = match renaming with
      | None -> renaming_clear ctx
      | Some r -> r
    in
    closure renaming ctx.subst
end

(** {2 Hindley-Milner} *)

module type S = sig
  type untyped (* untyped term *)
  type typed   (* typed term *)

  val infer : Ctx.t -> untyped -> scope -> Type.t * typed Closure.t
    (** Infer the type of this term under the given signature. This updates
        the context's typing environment! The resulting type's variables
        belong to the given scope.

        @param ctx the context
        @param untyped the untyped term whose type must be inferred
        @param scope where the term's type variables live

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
  module BT = Basic.FO
  module T = FOTerm
  module BF = Basic.Form
  module F = FOFormula

  type untyped = BT.t
  type typed = T.t

  let _get_sym ~arity ctx s scope  =
    let ty', s' = Ctx.type_of_fun ~arity ctx s in
    let v = Ctx._new_var ctx in
    Ctx.unify_and_set ctx v scope ty' s';
    v

  (* convert type *)
  let _get_ty ctx ty =
    Ctx._of_ty ctx ty

  let _get_quantified ctx ty =
    Ctx._of_quantified ctx ty

  (* infer a type for [t], possibly updating [ctx]. Also returns a
    continuation to build a typed term. *)
  let rec infer_rec ctx t s_t =
    let ty, closure = match t.BT.term with
    | BT.Var name ->
      let ty = _get_ty ctx (BT.get_ty t) in
      let i, ty = Ctx._get_var ctx ~ty name in
      let closure renaming subst =
        let ty = Substs.Ty.apply ~renaming subst ty s_t in
        T.mk_var ~ty i
      in
      ty, closure
    | BT.App (s, []) ->
      let ty = _get_sym ~arity:0 ctx s s_t in
      let closure renaming subst =
        let ty = Substs.Ty.apply ~renaming subst ty s_t in
        T.mk_const ~ty s
      in
      ty, closure
    | BT.App (s, l) ->
      let ty_s = _get_sym ~arity:(List.length l) ctx s s_t in
      let sub = List.map (fun t' -> infer_rec ctx t' s_t) l in
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
    in
    (* ensure consistency of type and type annotation *)
    match t.BT.ty with
    | None -> ty, closure
    | Some ty' ->
      Ctx.unify_and_set ctx ty s_t (Ctx._of_ty ctx ty') s_t;
      ty, closure

  let infer_var_scope ctx t s_t = match t.BT.term with
    | BT.Var name ->
      let ty = _get_ty ctx (BT.get_ty t) in
      let i = Ctx._enter_var_scope ctx name ty in
      let closure renaming subst =
        let ty = Substs.Ty.apply ~renaming subst ty s_t in
        T.mk_var ~ty i
      in
      closure
    | _ -> assert false

  let exit_var_scope ctx t = match t.BT.term with
    | BT.Var name -> Ctx._exit_var_scope ctx name
    | _ -> assert false

  let infer ctx t s_t =
    Util.enter_prof prof_infer;
    Util.debug 5 "infer_term %a" BT.pp t;
    try
      let ty, k = infer_rec ctx t s_t in
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

  let rec infer_form_rec ctx f s_f = match f.BF.form with
    | BF.Bool b ->
      let closure renaming subst = if b then F.mk_true else F.mk_false in
      closure
    | BF.Nary (op, l) ->
      (* closures of sub formulas *)
      let l' = List.map (fun f' -> infer_form_rec ctx f' s_f) l in
      let closure renaming subst =
        let l'' = List.map (fun f' -> f' renaming subst) l' in
        match op with
        | BF.And -> F.mk_and l''
        | BF.Or -> F.mk_or l''
      in
      closure
    | BF.Binary (op, f1, f2) ->
      let closure_f1 = infer_form_rec ctx f1 s_f in
      let closure_f2 = infer_form_rec ctx f2 s_f in
      let closure renaming subst =
        let f1' = closure_f1 renaming subst in
        let f2' = closure_f2 renaming subst in
        match op with
        | BF.Imply -> F.mk_imply f1' f2'
        | BF.Equiv -> F.mk_equiv f1' f2'
      in
      closure
    | BF.Not f' ->
      let closure_f' = infer_form_rec ctx f' s_f in
      (fun renaming subst -> F.mk_not (closure_f' renaming subst))
    | BF.Atom p ->
      let ty, clos = infer ctx p s_f in
      Ctx.unify_and_set ctx ty s_f Type.o s_f;  (* must return Type.o *)
      let closure renaming subst =
        F.mk_atom (clos renaming subst)
      in
      closure
    | BF.Equal (t1, t2) ->
      let ty1, c1 = infer ctx t1 s_f in
      let ty2, c2 = infer ctx t2 s_f in
      Ctx.unify_and_set ctx ty1 s_f ty2 s_f;  (* must have same type *)
      let closure renaming subst =
        F.mk_eq (c1 renaming subst) (c2 renaming subst)
      in
      closure
    | BF.Quant (op, vars, f') ->
      let clos_vars = List.map (fun t -> infer_var_scope ctx t s_f) vars in
      let clos_f =
        Util.finally
          ~h:(fun () -> List.iter (fun t -> exit_var_scope ctx t) vars)
          ~f:(fun () -> infer_form_rec ctx f' s_f)
      in
      let closure renaming subst =
        let vars' = List.map (fun c -> c renaming subst) clos_vars in
        let f' = clos_f renaming subst in
        match op with
        | BF.Forall -> F.mk_forall_list vars' f'
        | BF.Exists -> F.mk_exists_list vars' f'
      in
      closure

  let infer_form ctx f s_f =
    Util.debug 5 "infer_form %a" BF.pp f;
    let c_f = infer_form_rec ctx f s_f in
    c_f

  let constrain_form ctx f =
    let _ = infer_form ctx f 0 in
    ()

  let signature_forms signature seq =
    let ctx = Ctx.of_signature signature in
    Sequence.iter (constrain_form ctx) seq;
    Ctx.to_signature ctx

  let convert ?(generalize=false) ~ctx t =
    let _, closure = infer ctx t 0 in
    if generalize then Ctx.generalize ctx else Ctx.bind_to_default ctx;
    Ctx.apply_closure ctx closure

  let convert_form ?(generalize=false) ~ctx f =
    let closure = infer_form ctx f 0 in
    if generalize then Ctx.generalize ctx else Ctx.bind_to_default ctx;
    Ctx.apply_closure ctx closure

  let convert_clause ?(generalize=false) ~ctx c =
    let closures = List.map (fun lit -> infer_form ctx lit 0) c in
    Ctx.exit_scope ctx;
    (* use same renaming for all formulas, to keep
      a consistent scope *)
    let renaming = Ctx.renaming_clear ctx in
    if generalize then Ctx.generalize ctx else Ctx.bind_to_default ctx;
    List.map (fun c' -> Ctx.apply_closure ~renaming ctx c') closures

  let convert_seq ?(generalize=false) ~ctx forms =
    (* build closures, inferring all types *)
    let closures = Sequence.map (fun f -> infer_form ctx f 0) forms in
    let closures = Sequence.to_rev_list closures in
    if generalize then Ctx.generalize ctx else Ctx.bind_to_default ctx;
    (* apply closures to the final substitution *)
    List.rev_map (Ctx.apply_closure ctx) closures
end

module HO = struct
  module BT = Basic.HO
  module T = HOTerm

  type untyped = BT.t
  type typed = T.t

  let _get_sym ~arity ctx s scope =
    let ty', s' = Ctx.type_of_fun ~arity ctx s in
    let v = Ctx._new_var ctx in
    Ctx.unify_and_set ctx v scope ty' s';
    v

  (* convert type *)
  let _get_ty ctx ty =
    Ctx._of_ty ctx ty

  let _get_quantified ctx ty =
    Ctx._of_quantified ctx ty

  let infer_var_scope ctx t s_t = match t.BT.term with
    | BT.Var name ->
      let ty = _get_ty ctx (BT.get_ty t) in
      let i = Ctx._enter_var_scope ctx name ty in
      let closure renaming subst =
        let ty = Substs.Ty.apply ~renaming subst ty s_t in
        T.mk_var ~ty i
      in
      ty, closure
    | _ -> assert false

  let exit_var_scope ctx t = match t.BT.term with
    | BT.Var name -> Ctx._exit_var_scope ctx name
    | _ -> assert false

  (* infer a type for [t], possibly updating [ctx]. Also returns a
    continuation to build a typed term
    @param pred true if we expect a proposition
    @param arity expected number of arguments *)
  let rec infer_rec ?(arity=0) ctx t s_t =
    let ty, closure = match t.BT.term with
    | BT.Var name ->
      let ty = _get_ty ctx (BT.get_ty t) in
      let i, ty = Ctx._get_var ctx ~ty name in
      let closure renaming subst =
        let ty = Substs.Ty.apply ~renaming subst ty s_t in
        T.mk_var ~ty i
      in
      ty, closure
    | BT.Const s ->
      let ty = _get_sym ~arity ctx s s_t in
      let closure renaming subst =
        let ty = Substs.Ty.apply ~renaming subst ty s_t in
        T.mk_const ~ty s
      in
      ty, closure
    | BT.App (_, []) -> assert false
    | BT.App (t, l) ->
      let ty_t, clos_t = infer_rec ~arity:(List.length l) ctx t s_t in
      let ty_l, clos_l = List.split
        (List.map (fun t' -> infer_rec ctx t' s_t) l) in
      let ty_ret = Ctx._new_var ctx in
      (* t:ty_t, l:ty_l. now we must have  ty_t = (ty_l -> ty_ret) *)
      Ctx.unify_and_set ctx ty_t s_t Type.(ty_ret <== ty_l) s_t;
      let closure renaming subst =
        let t' = clos_t renaming subst in
        let l' = List.map (fun c -> c renaming subst) clos_l in
        T.mk_at t' l'
      in
      ty_ret, closure
    | BT.Lambda (v, t) ->
      let ty_v, clos_v = infer_var_scope ctx v s_t in
      let ty_t, clos_t = Util.finally
        ~f:(fun () -> infer_rec ctx t s_t)
        ~h:(fun () -> exit_var_scope ctx v)
      in
      (* type is ty_v -> ty_t *)
      let ty = Type.(ty_t <=. ty_v) in
      let closure renaming subst =
        let t' = clos_t renaming subst in
        let v' = clos_v renaming subst in
        T.mk_lambda_var [v'] t'
      in
      ty, closure 
    in
    (* ensure consistency of type and type annotation *)
    match t.BT.ty with
    | None -> ty, closure
    | Some ty' ->
      Ctx.unify_and_set ctx ty s_t (Ctx._of_ty ctx ty') s_t;
      ty, closure

  let infer ctx t s_t =
    Util.enter_prof prof_infer;
    Util.debug 5 "infer_term %a" BT.pp t;
    try
      let ty, k = infer_rec ctx t s_t in
      Ctx.exit_scope ctx;
      Util.exit_prof prof_infer;
      ty, k
    with e ->
      Ctx.exit_scope ctx;
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
    let ty, _ = infer ctx t 0 in
    Ctx.unify_and_set ctx ty 0 Type.o 0;
    ()

  let convert ?(generalize=false) ?(ret=Type.o) ~ctx t =
    let ty, closure = infer ctx t 0 in
    Ctx.unify_and_set ctx ty 0 ret 0;
    if generalize then Ctx.generalize ctx else Ctx.bind_to_default ctx;
    Ctx.apply_closure ctx closure

  let convert_seq ?(generalize=false) ~ctx terms =
    let closures = Sequence.map
      (fun t ->
        let ty, closure = infer ctx t 0 in
        Ctx.unify_and_set ctx ty 0 Type.o 0;
        closure)
      terms
    in
    (* evaluate *)
    let closures = Sequence.to_rev_list closures in
    if generalize then Ctx.generalize ctx else Ctx.bind_to_default ctx;
    List.rev_map
      (fun c -> Ctx.apply_closure ctx c)
      closures
end
