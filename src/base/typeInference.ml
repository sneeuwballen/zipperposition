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
module S = Substs
module Sym = Symbol

let prof_infer = Util.mk_profiler "TypeInference.infer"

(** {2 Default Types} *)

type default_types = {
  default_i : Type.t;
  default_prop : Type.t;
  default_int : Type.t;
  default_rat : Type.t;
}

let tptp_default = {
  default_i = Type.TPTP.i;
  default_prop = Type.TPTP.o;
  default_int = Type.TPTP.int;
  default_rat = Type.TPTP.rat;
}

(** {2 Typing context}

The scope maintained by the typing context starts at 1.
Scope 0 should be used for ground types.
*)

module Ctx = struct
  type t = {
    default : default_types;        (* default types *)
    mutable var : int;              (* generate fresh vars *)
    mutable signature : Signature.t;(* symbol -> type *)
    mutable subst : S.t;            (* variable bindings *)
    mutable to_bind : Type.t list;  (* list of variables to eventually bind (close) *)
    mutable locs : Location.t list; (* stack of locations *)
    renaming : Substs.Renaming.t;
    symbols : Type.t Symbol.Tbl.t;  (* symbol -> instantiated type *)
    tyvars : (string, int) Hashtbl.t;            (* type variable -> number *)
    vars : (string, (int * Type.t)) Hashtbl.t;  (* var name -> number + type *)
  }

  let create ?(default=tptp_default) signature =
    let ctx = {
      default;
      var = ~-1;
      signature;
      subst = S.empty;
      to_bind = [];
      locs = [];
      renaming = Substs.Renaming.create ();
      symbols = Symbol.Tbl.create 7;
      tyvars = Hashtbl.create 6;
      vars = Hashtbl.create 7;
    } in
    ctx

  let copy t = { t with
    renaming = Substs.Renaming.create ();
    symbols = Symbol.Tbl.copy t.symbols;
    tyvars = Hashtbl.copy t.tyvars;
    vars = Hashtbl.copy t.vars;
  }

  let clear ctx =
    ctx.var <- ~-1;
    ctx.subst <- S.empty;
    ctx.to_bind <- [];
    ctx.locs <- [];
    Symbol.Tbl.clear ctx.symbols;
    ctx.signature <- Signature.empty;
    Hashtbl.clear ctx.vars;
    ()

  let exit_scope ctx =
    Hashtbl.clear ctx.vars

  let add_signature ctx signature =
    ctx.signature <- Signature.merge ctx.signature signature;
    ()

  let declare ctx sym ty =
    ctx.signature <- Signature.declare ctx.signature sym ty

  (* generate fresh vars. Those should always live in their own scope *)
  let _new_var ctx =
    let n = ctx.var in
    ctx.var <- n - 1 ;
    Type.__var n

  (* generate [n] fresh vars *)
  let rec _new_vars ctx n =
    if n = 0
      then []
      else _new_var ctx :: _new_vars ctx (n-1)

  (* convert a prolog term into a type *)
  let _of_ty ctx ty =
    Type.Conv.of_prolog ~ctx:ctx.tyvars ty

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
        | None -> ctx.default.default_i
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

  let with_loc ctx ~loc f =
    let old_locs = ctx.locs in
    ctx.locs <- loc::old_locs;
    Util.finally
      ~h:(fun () -> ctx.locs <- old_locs)
      ~f

  (* unify within the context's substitution. scopes are 0 *)
  let unify ctx ty1 ty2 =
    Unif.Ty.unification ~subst:ctx.subst ty1 0 ty2 0

  (* same as {!unify}, but also updates the ctx's substitution *)
  let unify_and_set ctx ty1 ty2 =
    Util.debug 5 "unify %a and %a (ctx: %a)"
      Type.pp ty1 Type.pp ty2 Substs.pp ctx.subst;
    let subst = unify ctx ty1 ty2 in
    ctx.subst <- subst

  (* If the function symbol has an unknown type, a fresh variable
     is returned. Otherwise the known type of the symbol is returned.

     @arity the expected arity (if not declared) *)
  let type_of_fun ~arity ctx s =
    match s with
    | Sym.Int _ -> ctx.default.default_int
    | Sym.Rat _ -> ctx.default.default_rat
    | Sym.Conn _ -> assert false
    | Sym.Cst _ ->
      begin match Signature.find ctx.signature s with
      | Some ty -> ty
      | None ->
        (* give a new type variable to this symbol. The symbol will not
          be able to be polymorphic (need to declare it!). *)
        try
          let ty = Sym.Tbl.find ctx.symbols s in
          ty
        with Not_found ->
          let ret = _new_var ctx in
          let new_vars = _new_vars ctx arity in
          let ty = Type.(ret <== new_vars) in
          Sym.Tbl.add ctx.symbols s ty;
          ctx.to_bind <- ret :: new_vars @ ctx.to_bind;
          ty
      end

  let to_signature ctx =
    let signature = ctx.signature in
    (* enrich signature with new symbols *)
    Sym.Tbl.fold
      (fun s ty signature ->
        (* evaluate type. if variables remain, they are generalized *)
        let ty = S.Ty.apply_no_renaming ctx.subst ty 0 in
        (* generalize free vars, if any *)
        let ty = Type.close_forall ty in
        (* add to signature *)
        Signature.declare signature s ty)
      ctx.symbols signature

  let bind_to_default ctx =
    List.iter
      (fun v ->
        (* try to bind the variable. Will fail if already bound to
            something else, which is fine. *)
        try unify_and_set ctx v ctx.default.default_i
        with Unif.Fail -> ())
      ctx.to_bind;
    ctx.to_bind <- []

  let generalize ctx =
    (* keep constructor variables as they are, they will be generalized
        if {!to_signature} is called. *)
    ctx.to_bind <- []

  (* clear and return the renaming *)
  let renaming_clear ctx =
    let renaming = ctx.renaming in
    Substs.Renaming.clear renaming;
    renaming

  let reset_renaming ctx =
    Substs.Renaming.clear ctx.renaming

  (* apply substitution to type *)
  let apply_ty ctx ty =
    Substs.Ty.apply ~renaming:ctx.renaming ctx.subst ty 0

  (* apply substitution to term *)
  let apply_fo ctx t =
    Substs.FO.apply ~renaming:ctx.renaming ctx.subst t 0

  (* apply substitution to ho term *)
  let apply_ho ctx t =
    Substs.HO.apply ~renaming:ctx.renaming ctx.subst t 0
end

(** {2 Closures} *)

module Closure = Monad.Fun(Ctx)

(** {2 Hindley-Milner} *)

module type S = sig
  type untyped (* untyped term *)
  type typed   (* typed term *)

  val infer : Ctx.t -> untyped -> Type.t * typed Closure.t
    (** Infer the type of this term under the given signature. This updates
        the context's typing environment!

        @param ctx the context
        @param untyped the untyped term whose type must be inferred

        @return the inferred type of the untyped term (possibly a type var)
          along with a closure to produce a typed term once every
          constraint has been solved
        @raise Error if the types are inconsistent *)

  (** {3 Constraining types}

  This section is mostly useful for inferring a signature without
  converting untyped_terms into typed_terms. *)

  val constrain_term_term : Ctx.t -> untyped -> untyped -> unit
    (** Force the two terms to have the same type in this context
        @raise Error if an inconsistency is detected *)

  val constrain_term_type : Ctx.t -> untyped -> Type.t -> unit
    (** Force the term's type and the given type to be the same.
        @raise Error if an inconsistency is detected *)
end

exception BadArity

(* split [l] into a pair [(l1, l2)]. [l2] has length = arity.
    @raise Failure if [length l < arity] *)
let _split_arity arity l =
  let n = List.length l in
  if n < arity then raise BadArity;
  let rec drop n pre post =
    match n, post with
    | 0, _ -> List.rev pre, post
    | _, [] -> assert false
    | _, x::post' -> drop (n-1) (x::pre) post'
  in
  drop (n - arity) [] l

(* error-raising function *)
let __error ctx msg =
  let b = Buffer.create 15 in
  (* print closest location *)
  begin match ctx.Ctx.locs with
  | [] -> ()
  | loc::_ -> Printf.bprintf b "at %a:" Location.pp loc
  end;
  Printf.bprintf b "error during type inference: ";
  Printf.kbprintf
    (fun b -> raise (Type.Error (Buffer.contents b)))
    b msg

module FO = struct
  module PT = PrologTerm
  module T = FOTerm
  module F = Formula.FO

  type untyped = PT.t
  type typed = T.t

  (* convert (and possibly complete) this list of args *)
  let rec _complete_type_args ctx arity args =
    if arity < 0 then raise BadArity;
    match args with
    | [] -> Ctx._new_vars ctx arity  (* add variables *)
    | a::args' ->
      (* convert [a] into a type *)
      match Ctx._of_ty ctx a with
      | None -> __error ctx "term %a is not a type" PT.pp a
      | Some ty -> ty :: _complete_type_args ctx (arity-1) args'

  (* infer a type for [t], possibly updating [ctx]. Also returns a
    continuation to build a typed term. *)
  let rec infer_rec ctx t =
    match t with
    | PT.Location (t', loc) ->
      Ctx.with_loc ctx ~loc (fun () -> infer_rec ctx t')
    | PT.Column (PT.Var name, ty) ->
      (* typed var *)
      let ty = match Ctx._of_ty ctx ty with
        | Some ty -> ty
        | None -> __error ctx "expected type, got %a" PT.pp ty
      in
      let i, ty = Ctx._get_var ctx ~ty name in
      let closure ctx =
        let ty = Ctx.apply_ty ctx ty in
        T.var ~ty i
      in
      ty, closure
    | PT.Var name ->
      (* (possibly) untyped var *)
      let i, ty = Ctx._get_var ctx name in
      let closure ctx =
        let ty = Ctx.apply_ty ctx ty in
        T.var ~ty i
      in
      ty, closure
    | PT.Const s ->
      let ty_s = Ctx.type_of_fun ~arity:0 ctx s in
      Util.debug 5 "type of symbol %a: %a" Sym.pp s Type.pp ty_s;
      let closure ctx =
        let ty = Ctx.apply_ty ctx ty_s in
        T.const ~ty s
      in
      ty_s, closure
    | PT.App (PT.Const s, l) ->
      (* use type of [s] *)
      let ty_s = Ctx.type_of_fun ~arity:(List.length l) ctx s in
      Util.debug 5 "type of symbol %a: %a" Sym.pp s Type.pp ty_s;
      let n_tyargs, n_args = Type.arity ty_s in
      if n_args > List.length l
        then __error ctx "expected %d arguments, got only %d" n_args (List.length l);
      (* separation between type arguments and proper term arguments,
          based on the expected arity of the symbol.
          We split [l] into the list [tyargs], containing [n_tyargs] types,
          and [args], containing [n_args] terms. *)
      let tyargs, args = _split_arity n_args l in
      let tyargs = _complete_type_args ctx n_tyargs tyargs in
      let ty_s' = Type.apply ty_s tyargs in
      Util.debug 5 "applied type for %a: %a" Sym.pp s Type.pp ty_s';
      (* create sub-closures, by inferring the type of [args] *)
      let l = List.map (fun t' -> infer_rec ctx t') args in
      let ty_of_args, closure_args = List.split l in
      let closure_args = Closure.seq closure_args in
      (* [s] has type [ty_s] once applied to polymorphic type arguments,
          but must also have type [ty_l -> 'a].
          We generate a fresh variable 'a (named [ty_ret]),
          which is also the result. *)
      let ty_ret = Ctx._new_var ctx in
      Ctx.unify_and_set ctx ty_s' (Type.mk_fun ty_ret ty_of_args);
      (* now to produce the closure, that first creates subterms *)
      let closure ctx =
        let args' = closure_args ctx in
        let tyargs' = List.map (Ctx.apply_ty ctx) tyargs in
        let ty_s' = Type.close_forall (Ctx.apply_ty ctx ty_s) in
        Util.debug 5 "final type for %a: %a" Sym.pp s Type.pp ty_s';
        T.app ~tyargs:tyargs' (T.const ~ty:ty_s' s) args'
      in
      ty_ret, closure
    | PT.Int _
    | PT.Rat _
    | PT.List _
    | PT.Column _
    | PT.App _
    | PT.Bind _ -> __error ctx "expected first-order term"

  let infer_var_scope ctx t = match t with
    | PT.Column (PT.Var name, ty) ->
      let ty = match Ctx._of_ty ctx ty with
        | Some ty -> ty
        | None -> __error ctx "expected type, got %a" PT.pp ty
      in
      let i = Ctx._enter_var_scope ctx name ty in
      let closure ctx =
        let ty = Ctx.apply_ty ctx ty in
        T.var ~ty i
      in closure
    | PT.Var name ->
      let ty = ctx.Ctx.default.default_i in
      let i = Ctx._enter_var_scope ctx name ty in
      let closure ctx =
        let ty = Ctx.apply_ty ctx ty in
        T.var ~ty i
      in closure
    | _ -> assert false

  let exit_var_scope ctx t = match t with
    | PT.Column (PT.Var name, _)
    | PT.Var name -> Ctx._exit_var_scope ctx name
    | _ -> assert false

  let infer ctx t =
    Util.enter_prof prof_infer;
    Util.debug 5 "infer_term %a" PT.pp t;
    try
      let ty, k = infer_rec ctx t in
      Util.exit_prof prof_infer;
      ty, k
    with (* error handling: return a nice message *)
    | BadArity ->
      Util.exit_prof prof_infer;
      __error ctx "bad arity when trying to type %a" PT.pp t
    | e ->
      Util.exit_prof prof_infer;
      raise e

  let constrain_term_term ctx t1 t2 =
    let ty1, _ = infer ctx t1 in
    let ty2, _ = infer ctx t2 in
    Ctx.unify_and_set ctx ty1 ty2

  let constrain_term_type ctx t ty =
    let ty1, _ = infer ctx t in
    Ctx.unify_and_set ctx ty1 ty

  let rec infer_form_rec ctx f = match f with
    | PT.Location (f,loc) ->
      Ctx.with_loc ctx ~loc (fun () -> infer_form_rec ctx f)
    | PT.Const (Sym.Conn ((Sym.True | Sym.False) as b)) ->
      fun _ ->
        begin match b with
        | Sym.True -> F.Base.true_
        | Sym.False -> F.Base.false_
        | _ -> assert false
        end
    | PT.App (PT.Const (Sym.Conn Sym.And), l) ->
      let l' = List.map (fun f' -> infer_form_rec ctx f') l in
      fun ctx ->
        let l' = (Closure.seq l') ctx in
        F.Base.and_ l'
    | PT.App (PT.Const (Sym.Conn Sym.Or), l) ->
      let l' = List.map (fun f' -> infer_form_rec ctx f') l in
      fun ctx ->
        let l' = (Closure.seq l') ctx in
        F.Base.or_ l'
    | PT.App (PT.Const (Sym.Conn ((Sym.Equiv | Sym.Xor | Sym.Imply) as conn)), [a;b]) ->
      let a' = infer_form_rec ctx a  and b' = infer_form_rec ctx b in
      fun ctx ->
        let a = a' ctx and b = b' ctx in
        begin match conn with
        | Sym.Equiv -> F.Base.equiv a b
        | Sym.Xor -> F.Base.xor a b
        | Sym.Imply -> F.Base.imply a b
        | _ -> assert false
        end
    | PT.App (PT.Const (Sym.Conn Sym.Not), [a]) ->
      let a' = infer_form_rec ctx a in
      fun ctx -> F.Base.not_ (a' ctx)
    | PT.Bind(Sym.Conn ((Sym.Forall | Sym.Exists) as conn), vars, f') ->
      let vars' = Closure.seq (List.map (infer_var_scope ctx) vars) in
      let f' =
        Util.finally
          ~h:(fun () -> List.iter (exit_var_scope ctx) (List.rev vars))
          ~f:(fun () -> infer_form_rec ctx f')
      in
      fun ctx ->
        begin match conn with
        | Sym.Forall -> F.Base.forall (vars' ctx) (f' ctx)
        | Sym.Exists -> F.Base.exists (vars' ctx) (f' ctx)
        | _ -> assert false
        end
    | PT.App (PT.Const (Sym.Conn ((Sym.Eq | Sym.Neq) as conn)), [a;b]) ->
      (* a ?= b *)
      let tya, a = infer ctx a in
      let tyb, b = infer ctx b in
      Ctx.unify_and_set ctx tya tyb;
      fun ctx ->
        begin match conn with
        | Sym.Eq -> F.Base.eq (a ctx) (b ctx)
        | Sym.Neq -> F.Base.neq (a ctx) (b ctx)
        | _ -> assert false
        end
    | PT.Const _
    | PT.App _ ->
      (* atoms *)
      let tyt, t = infer ctx f in
      Ctx.unify_and_set ctx tyt ctx.Ctx.default.default_prop;
      fun ctx -> F.Base.atom (t ctx)
    | PT.Var _
    | PT.Column _
    | PT.List _
    | PT.Int _
    | PT.Bind _
    | PT.Rat _ -> __error ctx "expected formula, got %a" PT.pp f

  let infer_form ctx f =
    Util.debug 5 "infer_form %a" PT.pp f;
    let c_f = infer_form_rec ctx f in
    c_f

  let constrain_form ctx f =
    let _ = infer_form ctx f in
    ()

  let signature_forms signature seq =
    let ctx = Ctx.create signature in
    Sequence.iter (constrain_form ctx) seq;
    Ctx.to_signature ctx

  let convert ?(generalize=false) ~ctx t =
    let _, closure = infer ctx t in
    if generalize then Ctx.generalize ctx else Ctx.bind_to_default ctx;
    closure ctx

  let convert_form ?(generalize=false) ~ctx f =
    let closure = infer_form ctx f in
    if generalize then Ctx.generalize ctx else Ctx.bind_to_default ctx;
    closure ctx

  let convert_clause ?(generalize=false) ~ctx c =
    let closures = List.map (fun lit -> infer_form ctx lit) c in
    Ctx.exit_scope ctx;
    (* use same renaming for all formulas, to keep
      a consistent scope *)
    Ctx.reset_renaming ctx;
    if generalize then Ctx.generalize ctx else Ctx.bind_to_default ctx;
    List.map (fun c' -> c' ctx) closures

  let convert_seq ?(generalize=false) ~ctx forms =
    (* build closures, inferring all types *)
    let closures = Sequence.map (fun f -> infer_form ctx f) forms in
    let closures = Sequence.to_rev_list closures in
    if generalize then Ctx.generalize ctx else Ctx.bind_to_default ctx;
    (* apply closures to the final substitution *)
    List.rev_map (fun c -> c ctx) closures
end

(*
module HO = struct
  module PT = PrologTerm
  module T = HOTerm

  type untyped = PT.t
  type typed = T.t

  (* convert (and possibly complete) this list of args *)
  let rec _complete_type_args ctx arity args =
    if arity < 0 then raise BadArity;
    match args with
    | [] -> Ctx._new_vars ctx arity  (* add variables *)
    | a::args' ->
      (* convert [a] into a type *)
      let ty = Ctx._of_ty ctx (BT.as_ty a) in
      ty :: _complete_type_args ctx (arity-1) args'

  let infer_var_scope ctx t = match t.BT.term with
    | BT.Var name ->
      let ty = Ctx._of_ty ctx (BT.get_ty t) in
      let i = Ctx._enter_var_scope ctx name ty in
      let closure renaming subst =
        let ty = Substs.Ty.apply ~renaming subst ty 0 in
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
  let rec infer_rec ?(arity=0) ctx t =
    let ty, closure = match t.BT.term with
    | BT.Var name ->
      let ty = Ctx._of_ty ctx (BT.get_ty t) in
      let i, ty = Ctx._get_var ctx ~ty name in
      let closure renaming subst =
        let ty = Substs.Ty.apply ~renaming subst ty 0 in
        T.mk_var ~ty i
      in
      ty, closure
    | BT.Lambda (v, t) ->
      let ty_v, clos_v = infer_var_scope ctx v in
      let ty_t, clos_t = Util.finally
        ~f:(fun () -> infer_rec ctx t)
        ~h:(fun () -> exit_var_scope ctx v)
      in
      (* type is ty_v -> ty_t *)
      let ty = Type.(ty_t <=. ty_v) in
      let closure renaming subst =
        let t' = clos_t renaming subst in
        let v' = clos_v renaming subst in
        T.mk_lambda [v'] t'
      in
      ty, closure 
    | BT.Const s ->
      (* recover the type *)
      let ty = Ctx.type_of_fun ~arity ctx s in
      let closure renaming subst =
        let ty = Type.close_forall (Substs.Ty.apply ~renaming subst ty 0) in
        let s = Symbol.of_basic ~ty s in
        T.mk_const s
      in
      ty, closure
    | BT.App (t, l) ->
      (* we are going to assume that the type of [t], as inferred, is a forall
          or a function (or a constant iff [l] is empty *)
      let ty_t, clos_t = infer_rec ~arity:(List.length l) ctx t in
      let n_tyargs, n_args = Type.arity ty_t in
      (* separation between type arguments and proper term arguments,
          based on the expected arity of the head [t].
          We split [l] into the list [tyargs], containing [n_tyargs] types,
          and [args], containing [n_args] terms. *)
      let tyargs, args = _split_arity n_args l in
      let tyargs = _complete_type_args ctx n_tyargs tyargs in
      let ty_t' = Type.apply ty_t tyargs in
      (* create sub-closures, by inferring the type of [args] *)
      let l = List.map (fun t' -> infer_rec ctx t') args in
      let ty_of_args, closure_args = List.split l in
      (* [s] has type [ty_s] once applied to polymorphic type arguments,
          but must also have type [ty_l -> 'a].
          We generate a fresh variable 'a (named [ty_ret]),
          which is also the result. *)
      let ty_ret = Ctx._new_var ctx in
      Ctx.unify_and_set ctx ty_t' (Type.mk_fun ty_ret ty_of_args);
      (* closure *)
      let closure renaming subst =
        let t' = clos_t renaming subst in
        let tyargs' = List.map (fun ty -> Substs.Ty.apply ~renaming subst ty 0) tyargs in
        let l' = List.map (fun c -> c renaming subst) closure_args in
        T.mk_at ~tyargs:tyargs' t' l'
      in
      ty_ret, closure
    in
    (* ensure consistency of type and type annotation *)
    match t.BT.ty with
    | None -> ty, closure
    | Some ty' ->
      Ctx.unify_and_set ctx ty (Ctx._of_ty ctx ty');
      ty, closure

  let infer ctx t =
    Util.enter_prof prof_infer;
    Util.debug 5 "infer_term %a" BT.pp t;
    try
      let ty, k = infer_rec ctx t in
      Ctx.exit_scope ctx;
      Util.exit_prof prof_infer;
      ty, k
    with e ->
      Ctx.exit_scope ctx;
      Util.exit_prof prof_infer;
      raise e

  let constrain_term_term ctx t1 t2 =
    let ty1, _ = infer ctx t1 in
    let ty2, _ = infer ctx t2 in
    Ctx.unify_and_set ctx ty1 ty2 

  let constrain_term_type ctx t ty =
    let ty1, _ = infer ctx t in
    Ctx.unify_and_set ctx ty1 ty

  let constrain ~ctx t =
    let ty, _ = infer ctx t in
    Ctx.unify_and_set ctx ty Type.o;
    ()

  let convert ?(generalize=false) ?(ret=Type.o) ~ctx t =
    let ty, closure = infer ctx t in
    Ctx.unify_and_set ctx ty ret;
    if generalize then Ctx.generalize ctx else Ctx.bind_to_default ctx;
    Ctx.apply_closure ctx closure

  let convert_seq ?(generalize=false) ~ctx terms =
    let closures = Sequence.map
      (fun t ->
        let ty, closure = infer ctx t in
        Ctx.unify_and_set ctx ty Type.o;
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
*)
