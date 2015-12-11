
(* This file is free software, part of Logtk. See file "license" for more details. *)

(* {1 Type inference} *)

(** Reference:
    https://en.wikipedia.org/wiki/Hindley-Milner
*)

module PT = STerm
module T = TypedSTerm
module Loc = ParseLocation
module Err = CCError
module Subst = Var.Subst

let prof_infer = Util.mk_profiler "TypeInference.infer"
let section = Util.Section.(make ~parent:logtk "ty_infer")

type 'a or_error = [`Error of string | `Ok of 'a]

type type_ = TypedSTerm.t
type signature = type_ ID.Tbl.t
type untyped = STerm.t (** untyped term *)
type typed = TypedSTerm.t (** typed term *)

exception Error of string

let () = Printexc.register_printer
  (function
    | Error msg ->
        Some (CCFormat.sprintf "@[<2>type inference error:@ %s@]" msg)
    | _ -> None)

(* error-raising function *)
let error_ ?loc msg =
  CCFormat.ksprintf msg
    ~f:(fun msg ->
      let msg = match loc with
        | None -> msg
        | Some l -> CCFormat.sprintf "@[<hv>%s@ at %a@]" msg Loc.pp l
      in
      raise (Error msg))

(* unify within the context's substitution. Wraps {!Unif.Ty.unification}
   by returning a nicer exception in case of failure *)
let unify ?loc ty1 ty2 =
  Util.debugf ~section 5 "@[<hv2>unify types@ @[%a@] and@ @[%a@]@]"
    (fun k-> k T.pp ty1 T.pp ty2);
  try
    T.unify ty1 ty2
  with T.UnifyFailure _ as e ->
    error_ ?loc "@[%s@]" (Printexc.to_string e)

(** {2 Typing context}

    The scope maintained by the typing context starts at 1.
    Scope 0 should be used for ground types.
*)

module Ctx = struct
  type env = (string, [`Var of type_ Var.t | `ID of ID.t * type_]) Hashtbl.t

  type t = {
    default: type_;
    env : env;
      (* map names to variables or IDs *)
    sigma : signature;
      (* ID.t -> instantiated type *)
    mutable to_set_to_default : T.meta_var list;
      (* variables that should be set to [default] is they are not bound *)
    mutable to_generalize : T.meta_var list;
      (* variables that should be generalized in the global scope *)
    mutable local_vars: T.t Var.t list;
      (* free variables in the local scope *)
  }

  let create ?(default=T.Ty.term) ?(sigma=ID.Tbl.create 32) () =
    let ctx = {
      default;
      env = Hashtbl.create 32;
      sigma;
      to_set_to_default = [];
      to_generalize = [];
      local_vars = [];
    } in
    ctx

  let copy t =
    { t with
      sigma =ID.Tbl.copy t.sigma;
      env = Hashtbl.copy t.env;
    }

  let to_signature t = t.sigma

  (* enter new scope for the variable with this name *)
  let with_var ctx v ~f =
    let name = Var.name v in
    Hashtbl.add ctx.env name (`Var v);
    try
      let x = f () in
      Hashtbl.remove ctx.env name;
      x
    with e ->
      Hashtbl.remove ctx.env name;
      raise e

  let rec with_vars ctx vars ~f = match vars with
    | [] -> f()
    | v :: vars' -> with_var ctx v ~f:(fun () -> with_vars ctx vars' ~f)

  let exit_scope ctx =
    List.iter (fun v -> Hashtbl.remove ctx.env (Var.name v)) ctx.local_vars;
    ctx.local_vars <- [];
    ()

  let declare ctx sym ty =
    if ID.Tbl.mem ctx.sigma sym
    then error_ "@[<2>symbol %a is already declared@]" ID.pp sym;
    ID.Tbl.add ctx.sigma sym ty

  (* generate fresh type var. *)
  let fresh_ty_meta_var () : T.meta_var =
    Var.gensym ~ty:T.tType (), ref None

  (* generate [n] fresh type meta vars *)
  let rec fresh_ty_meta_vars n =
    if n = 0
    then []
    else fresh_ty_meta_var () :: fresh_ty_meta_vars (n-1)

  let add_to_set_to_default ctx v =
    ctx.to_set_to_default <- v :: ctx.to_set_to_default

  (* Fresh function type with [arity] arguments *)
  let fresh_fun_ty ~arity ctx =
    let ret = fresh_ty_meta_var () in
    let new_vars =
      List.map (fun v -> T.Ty.meta v) (fresh_ty_meta_vars arity) in
    let ty = T.Ty.fun_ new_vars (T.Ty.meta ret) in
    (* only need to specialize the return type, because arguments will be
       unified to other types *)
    add_to_set_to_default ctx ret;
    ty

  (* find identifier in env *)
  let find_env_ ?loc ctx name =
    try Hashtbl.find ctx.env name
    with Not_found ->
      error_ ?loc "@[<2>unknown identifier %s" name

  let get_id_ ?loc ~arity ctx name =
    try match Hashtbl.find ctx.env name with
      | `ID (id, ty) -> id, ty
      | `Var _ -> error_ ?loc "@[<2>expected %s to be a constant, not a variable@]" name
    with Not_found ->
      Util.debugf ~section 2
        "@[<2>unknown constant %s,@ will infer a default type@]" (fun k->k name);
      let ty = fresh_fun_ty ~arity ctx in
      let id = ID.make name in
      Hashtbl.add ctx.env name (`ID (id, ty));
      ID.Tbl.add ctx.sigma id ty;
      id, ty

  let get_var_ ?loc ctx v =
    try match find_env_ ?loc ctx v with
      | `ID _ ->
          error_ ?loc "@[<2>expected %s to be a variable,@ not a constant@]" v
      | `Var v -> v
    with Not_found ->
      let ty_v = fresh_ty_meta_var() in
      add_to_set_to_default ctx ty_v;
      let v' = Var.of_string ~ty:(T.Ty.meta ty_v) v in
      ctx.local_vars <- v' :: ctx.local_vars;
      Hashtbl.add ctx.env v (`Var v');
      v'

  (* only specialize variable if it's not bound *)
  let specialize_meta ctx (v, r) =
    match !r with
    | None ->
        let ty = ctx.default in
        Util.debugf ~section 5 "@[<2>specialize type meta_var %a to@ @[%a@]@]"
          (fun k->k Var.pp v T.pp ty);
        r := Some ty
    | Some _ -> ()

  let bind_to_default ctx =
    (* try to bind the variable. Will fail if already bound to
       something else, which is fine. *)
    List.iter (specialize_meta ctx) ctx.to_set_to_default;
    ctx.to_set_to_default <- [];
    ()

  let generalize_meta (v, r) =
    match !r with
    | None ->
        let v' = Var.copy v in
        Util.debugf ~section 5 "@[<2>generalize type meta_var %a@]"
          (fun k->k Var.pp v);
        r := Some (T.var v')
    | Some _ -> ()

  let generalize ctx =
    List.iter generalize_meta ctx.to_generalize;
    ctx.to_generalize <- [];
    ()
end

(** {2 Hindley-Milner} *)

(* convert a prolog term into a type *)
let infer_ty_ ?loc ctx ty =
  let rec aux ty = match PT.view ty with
    | PT.AppBuiltin (Builtin.TyInt, []) -> T.Ty.int
    | PT.AppBuiltin (Builtin.TyRat, []) -> T.Ty.rat
    | PT.AppBuiltin (Builtin.Term, []) -> T.Ty.term
    | PT.AppBuiltin (Builtin.Prop, []) -> T.Ty.prop
    | PT.AppBuiltin (Builtin.TType, []) -> T.Ty.tType
    | PT.AppBuiltin (Builtin.Arrow, ret :: args) ->
        let ret = aux ret in
        let args = List.map aux args in
        T.Ty.fun_ ?loc args ret
    | PT.Var v ->
        begin match Ctx.find_env_ ?loc ctx v with
          | `ID (id, ty) -> T.const ~ty id
          | `Var v -> T.Ty.var v
        end
    | PT.Const f ->
        (* constant type *)
        let id, ty = Ctx.get_id_ ctx ~arity:0 f in
        unify ty T.Ty.tType;
        T.Ty.const id
    | PT.App (f, l) ->
        begin match PT.view f with
        | PT.Const name ->
            let id, ty = Ctx.get_id_ ctx ~arity:(List.length l) name in
            unify ?loc (T.Ty.returns ty) T.Ty.tType;
            let l = List.map aux l in
            T.Ty.app id l
        | _ -> error_ ?loc "@[<2>cannot apply non-constant@ `@[%a@]`@]" PT.pp f
        end
    | PT.Bind (Binder.ForallTy, vars, body) ->
        (* create new variables for each element of [vars] *)
        let vars' = List.map
          (fun (v,o) ->
            CCOpt.iter
              (fun ty ->
                let ty = aux ty in
                if not (T.Ty.is_tType ty)
                  then error_ ?loc "@[<2>variable %s should have type TType@]" v)
              o;
            Var.of_string ~ty:T.tType v)
          vars
        in
        (* convert [body] to a type, in a scope where [vars] are
           available. *)
        Ctx.with_vars ctx vars'
          ~f:(fun () ->
              let body' = aux body in
              T.Ty.forall_l vars' body')
    | _ ->
        error_ ?loc "@[<2>`@[%a@]`@ is not a valid type@]" PT.pp ty
  in
  aux ty

let infer_ty_exn ctx ty = infer_ty_ ctx ty

let infer_ty ctx ty =
  try Err.return (infer_ty_exn ctx ty)
  with e -> Err.of_exn_trace e

(* create a new variable for [v], and specialize its type if needed.
   To be used for binders *)
let var_of_typed_var ?loc ctx (v,o) =
  let ty = match o with
    | None -> T.Ty.meta (Ctx.fresh_ty_meta_var ())
    | Some ty -> infer_ty_ ?loc ctx ty
  in
  Var.of_string ~ty v

(* infer a type for [t], possibly updating [ctx]. Also returns a
   continuation to build a typed term. *)
let rec infer_rec ctx t =
  let loc = PT.loc t in
  match PT.view t with
  | PT.Var name ->
      (* var or const, actually *)
      let v = Ctx.get_var_ ?loc ctx name in
      T.var v
  | PT.Const s ->
      let id, ty = Ctx.get_id_ ?loc ~arity:0 ctx s in
      T.const ~ty id
  | PT.App ({PT.term=PT.Const s; _}, l) ->
      let id, ty_s = Ctx.get_id_ ?loc ~arity:(List.length l) ctx s in
      (* infer types for arguments *)
      let l = List.map (infer_rec ctx) l in
      let ty = T.apply_unify ty_s l in
      Util.debugf ~section 5 "type of symbol %a: %a"
        (fun k->k ID.pp id T.pp ty_s);
      T.app ?loc ~ty (T.const ?loc ~ty:ty_s id) l
  | PT.App (f,l) ->
      (* higher order application *)
      let f = infer_rec ctx f in
      let l = List.map (infer_rec ctx) l in
      let ty = T.apply_unify (T.ty_exn f) l in
      T.app ?loc ~ty f l
  | PT.List [] ->
      let v = Ctx.fresh_ty_meta_var () in
      let ty = T.Ty.multiset (T.Ty.meta v) in
      Ctx.add_to_set_to_default ctx v;
      T.multiset ?loc ~ty []
  | PT.List (t::l) ->
      let t = infer_rec ctx t in
      let l = List.map (infer_rec ctx) l in
      let ty = T.Ty.multiset (T.ty_exn t) in
      T.multiset ?loc ~ty (t::l)
  | PT.Record (l,rest) ->
      (* infer types of fields *)
      let ty_l, l =
        List.map
         (fun (n,t) ->
           let t' = infer_rec ctx t in
           (n, T.ty_exn t'), (n, t'))
         l
        |> List.split
      in
      let rest =
        CCOpt.map
          (fun s -> Ctx.get_var_ ?loc ctx s)
          rest
      in
      let ty = T.Ty.record_flatten ty_l ~rest:(CCOpt.map Var.ty rest) in
      T.record ~ty ?loc l ~rest
  | PT.AppBuiltin (Builtin.Wildcard, []) ->
      (* make a new TYPE variable *)
      let v = Ctx.fresh_ty_meta_var () in
      T.Ty.meta v
  | PT.AppBuiltin (Builtin.Arrow, ret :: args) ->
      let ret = infer_ty_exn ctx ret in
      let args = List.map (infer_ty_exn ctx) args in
      T.Ty.fun_ ?loc args ret
  | PT.AppBuiltin (Builtin.True, []) -> T.Form.true_
  | PT.AppBuiltin (Builtin.False, []) -> T.Form.false_
  | PT.AppBuiltin (Builtin.And, l) ->
      let l = List.map (infer_prop_exn ctx) l in
      T.Form.and_ ?loc l
  | PT.AppBuiltin (Builtin.Or, l) ->
      let l = List.map (infer_prop_exn ctx) l in
      T.Form.or_ ?loc l
  | PT.AppBuiltin (((Builtin.Equiv | Builtin.Xor | Builtin.Imply) as conn), [a;b]) ->
      let a = infer_prop_exn ctx a
      and b = infer_prop_exn ctx b in
      begin match conn with
        | Builtin.Equiv -> T.Form.equiv ?loc a b
        | Builtin.Xor -> T.Form.xor ?loc a b
        | Builtin.Imply -> T.Form.imply ?loc a b
        | _ -> assert false
      end
  | PT.AppBuiltin (Builtin.Not, [a]) ->
      let a = infer_prop_exn ctx a in
      T.Form.not_ ?loc a
  | PT.AppBuiltin ((Builtin.Eq | Builtin.Neq) as conn, [a;b]) ->
      (* a ?= b *)
      let a = infer_rec ctx a in
      let b = infer_rec ctx b in
      unify ?loc (T.ty_exn a) (T.ty_exn b);
      begin match conn with
        | Builtin.Eq -> T.Form.eq a b
        | Builtin.Neq -> T.Form.neq a b
        | _ -> assert false
      end
  | PT.Bind(((Binder.Forall | Binder.Exists) as binder), vars, f') ->
      let vars = List.map (var_of_typed_var ?loc ctx) vars in
      let f' = Ctx.with_vars ctx vars
        ~f:(fun () -> infer_prop_exn ctx f') in
      begin match binder with
        | Binder.Forall -> T.Form.forall_l vars f'
        | Binder.Exists -> T.Form.exists_l vars f'
        | _ -> assert false
      end
  | PT.Bind(Binder.Lambda, vars, t') ->
      let vars = List.map (var_of_typed_var ?loc ctx) vars in
      let t' = Ctx.with_vars ctx vars
        ~f:(fun () -> infer_rec ctx t') in
      let ty = T.Ty.fun_ ?loc (List.map Var.ty vars) (T.ty_exn t') in
      T.bind_list ?loc ~ty Binder.Lambda vars t'
  | PT.Bind (Binder.ForallTy, vars, t') ->
      let vars = List.map (var_of_typed_var ?loc ctx) vars in
      List.iter (fun v -> unify T.Ty.tType (Var.ty v)) vars;
      let t' = Ctx.with_vars ctx vars
        ~f:(fun () -> infer_rec ctx t') in
      unify T.Ty.tType (T.ty_exn t');
      T.Ty.forall_l ?loc vars t'
  | PT.AppBuiltin _ ->
      error_ ?loc
        "@[<2>unexpected builtin in@ `@[%a@]`, expected term@]" PT.pp t

(* infer a term, and force its type to [prop] *)
and infer_prop_exn ctx t =
  let t = infer_rec ctx t in
  unify (T.ty_exn t) T.Ty.prop;
  t

let infer_exn ctx t =
  Util.enter_prof prof_infer;
  Util.debugf ~section 5 "@[<2>infer type of@ `@[%a@]`@]" (fun k->k PT.pp t);
  try
    let t = infer_rec ctx t in
    Util.exit_prof prof_infer;
    t
  with e ->
    Util.exit_prof prof_infer;
    raise e

let infer ctx t =
  try Err.return (infer_exn ctx t)
  with e ->
    Err.of_exn_trace e

let infer_clause_exn ctx c =
  let c = List.map (infer_prop_exn ctx) c in
  Ctx.exit_scope ctx;
  c

let constrain_term_term_exn ctx t1 t2 =
  let t1 = infer_exn ctx t1 in
  let t2 = infer_exn ctx t2 in
  unify (T.ty_exn t1) (T.ty_exn t2)

let constrain_term_term ctx t1 t2 =
  try Err.return (constrain_term_term_exn ctx t1 t2)
  with e -> Err.of_exn_trace e

let constrain_term_type_exn ctx t ty =
  let t = infer_exn ctx t in
  unify ty (T.ty_exn t)

let constrain_term_type ctx t ty =
  try Err.return (constrain_term_type_exn ctx t ty)
  with e -> Err.of_exn_trace e
