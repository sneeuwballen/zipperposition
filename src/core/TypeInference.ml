
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

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
let section = Util.Section.(make ~parent:zip "ty_infer")

type 'a or_error = [`Error of string | `Ok of 'a]

type type_ = TypedSTerm.t
type untyped = STerm.t (** untyped term *)
type typed = TypedSTerm.t (** typed term *)
type loc = ParseLocation.t

exception Error of string

let () = Printexc.register_printer
  (function
    | Error msg ->
        Some (CCFormat.sprintf "@[<2>@{<Red>type inference error@}:@ %s@]" msg)
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
  Util.debugf ~section 5 "@[<hv2>unify types@ `@[%a@]`@ and `@[%a@]`@]"
    (fun k-> k T.pp ty1 T.pp ty2);
  try
    T.unify ~allow_open:true ?loc ty1 ty2
  with T.UnifyFailure _ as e ->
    error_ ?loc "@[%s@]" (Printexc.to_string e)

module TyBuiltin = struct
  let a = Var.of_string ~ty:T.Ty.tType "α"
  let a_ = T.Ty.var a
  let prop2 = T.Ty.([prop; prop] ==> prop)
  let prop1 = T.Ty.([prop] ==> prop)
  let prop2poly = T.Ty.(forall a ([a_; a_] ==> prop))
  let ty_1_to_int = T.Ty.(forall a ([a_] ==> int))
  let ty2op = T.Ty.(forall a ([a_; a_] ==> a_))
  let ty1op = T.Ty.(forall a ([a_] ==> a_))
  let ty2op_to_i = T.Ty.([int;int] ==> int)
  let hobinder = T.Ty.(forall a ([[a_] ==> prop] ==> prop))

  let ty_exn = function
    | Builtin.True -> T.Ty.prop
    | Builtin.False -> T.Ty.prop
    | Builtin.Eq -> prop2poly
    | Builtin.Neq -> prop2poly
    | Builtin.Not -> prop1
    | Builtin.Imply -> prop2
    | Builtin.And -> prop2
    | Builtin.Or -> prop2
    | Builtin.Equiv -> prop2
    | Builtin.Xor -> prop2
    | Builtin.ForallConst -> hobinder
    | Builtin.ExistsConst -> hobinder
    | Builtin.Less -> prop2poly
    | Builtin.Lesseq -> prop2poly
    | Builtin.Greater -> prop2poly
    | Builtin.Greatereq -> prop2poly
    | Builtin.Uminus -> ty1op
    | Builtin.Sum -> ty2op
    | Builtin.Difference -> ty2op
    | Builtin.Product -> ty2op
    | Builtin.Quotient -> ty2op
    | Builtin.Quotient_e -> ty2op_to_i
    | Builtin.Quotient_f -> ty2op_to_i
    | Builtin.Quotient_t -> ty2op_to_i
    | Builtin.Remainder_e -> ty2op_to_i
    | Builtin.Remainder_f -> ty2op_to_i
    | Builtin.Remainder_t -> ty2op_to_i
    | Builtin.Floor -> ty_1_to_int
    | Builtin.Ceiling -> ty_1_to_int
    | Builtin.Round -> ty_1_to_int
    | Builtin.Truncate -> ty_1_to_int
    | Builtin.To_int -> T.Ty.(forall a ([a_] ==> int))
    | Builtin.To_rat -> T.Ty.(forall a ([a_] ==> rat))
    | Builtin.Is_int -> T.Ty.(forall a ([a_] ==> prop))
    | Builtin.Is_rat -> T.Ty.(forall a ([a_] ==> prop))
    | _ -> invalid_arg "TyBuiltin.ty_exn"

  let ty x = try Some (ty_exn x) with _ -> None
end

(** {2 Typing context} *)

module Ctx = struct
  type env = (string, [`Var of type_ Var.t | `ID of ID.t * type_]) Hashtbl.t

  type t = {
    default: type_;
    env : env;
      (* map names to variables or IDs *)
    mutable to_set_to_default : T.meta_var list;
      (* variables that should be set to [default] is they are not bound *)
    mutable to_generalize : T.meta_var list;
      (* variables that should be generalized in the global scope *)
    mutable local_vars: T.t Var.t list;
      (* free variables in the local scope *)
    mutable new_types: (ID.t * type_) list;
      (* list of symbols whose type has been inferred recently *)
  }

  let create ?(default=T.Ty.term) () =
    let ctx = {
      default;
      env = Hashtbl.create 32;
      to_set_to_default = [];
      to_generalize = [];
      local_vars = [];
      new_types = [];
    } in
    ctx

  let copy t =
    { t with
      env = Hashtbl.copy t.env;
    }

  (* enter new scope for the variables with those names *)
  let with_vars ctx vs ~f =
    let names =
      List.map
        (fun v ->
          let name = Var.name v in
          Hashtbl.add ctx.env name (`Var v);
          name)
        vs
    in
    try
      let x = f () in
      List.iter (Hashtbl.remove ctx.env) names;
      x
    with e ->
      List.iter (Hashtbl.remove ctx.env) names;
      raise e

  let with_var ctx v ~f = with_vars ctx [v] ~f

  let exit_scope ctx =
    List.iter (fun v -> Hashtbl.remove ctx.env (Var.name v)) ctx.local_vars;
    ctx.local_vars <- [];
    ()

  let declare ctx s ty =
    let name = ID.name s in
    Util.debugf ~section 3 "@{<yellow>declare@} %a:@ @[%a@]" (fun k->k ID.pp s T.pp ty);
    if Hashtbl.mem ctx.env name
    then Util.warnf "@[<2>shadowing identifier %s@]" name;
    Hashtbl.add ctx.env name (`ID (s,ty))

  let add_to_set_to_default ctx v =
    ctx.to_set_to_default <- v :: ctx.to_set_to_default

  (* generate fresh type var. *)
  let fresh_ty_meta_var ?(dest=`ToDefault) ctx : T.meta_var =
    let v = Var.gensym ~ty:T.tType () in
    let r = ref None in
    begin match dest with
      | `ToDefault -> add_to_set_to_default ctx (v,r);
      | `ToGeneralize -> ctx.to_generalize <- (v,r) :: ctx.to_generalize;
      | `ToNowhere -> ()
    end;
    v, r

  (* generate [n] fresh type meta vars *)
  let rec fresh_ty_meta_vars ?dest ctx n =
    if n = 0
    then []
    else fresh_ty_meta_var ?dest ctx :: fresh_ty_meta_vars ?dest ctx (n-1)

  (* Fresh function type with [arity] arguments *)
  let fresh_fun_ty ?(dest=`ToDefault) ~arity ctx =
    let ret = fresh_ty_meta_var ~dest ctx in
    let new_vars = fresh_ty_meta_vars ~dest ctx arity in
    let ty = T.Ty.fun_ (List.map (fun v->T.Ty.meta v) new_vars) (T.Ty.meta ret) in
    ty

  let get_id_ ?loc ~arity ctx name =
    try match Hashtbl.find ctx.env name with
      | `ID (id, ty) -> id, ty
      | `Var _ -> error_ ?loc "@[<2>expected %s to be a constant, not a variable@]" name
    with Not_found ->
      let ty = fresh_fun_ty ~arity ctx in
      Util.debugf ~section 2
        "@[<2>unknown constant %s,@ will infer a default type @[%a@]@]"
        (fun k->k name T.pp ty);
      let id = ID.make name in
      Hashtbl.add ctx.env name (`ID (id, ty));
      ctx.new_types <- (id, ty) :: ctx.new_types;
      id, ty

  (* in ZF, variables might be constant, there is no syntactic difference *)
  let get_var_ ctx v =
    try Hashtbl.find ctx.env v
    with Not_found ->
      let ty_v = fresh_ty_meta_var ~dest:`ToDefault ctx in
      let v' = Var.of_string ~ty:(T.Ty.meta ty_v) v in
      ctx.local_vars <- v' :: ctx.local_vars;
      Hashtbl.add ctx.env v (`Var v');
      `Var v'

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

  let pop_new_types ctx =
    let l = ctx.new_types in
    ctx.new_types <- [];
    l
end

(** {2 Hindley-Milner} *)

(* TODO: check, afterwards, that types:
  - do not contain metas variables
  - are closed
*)

(* convert the typed variables into proper variables [vars'], call [f vars'],
   and then exit the scope of [vars'] *)
let with_typed_vars_ ?loc ~infer_ty ctx vars ~f =
  let rec aux acc l = match l with
    | [] -> f (List.rev acc)
    | (v,o) :: l' ->
        let ty = match o with
          | None -> T.Ty.meta (Ctx.fresh_ty_meta_var ~dest:`ToDefault ctx)
          | Some ty -> infer_ty ?loc ctx ty
        in
        let v = Var.of_string ~ty v in
        (* enter [v] before dealing with next variables, for they might depend
           on it (e.g. [forall (a:type)(l:list a). ...]) *)
        Ctx.with_var ctx v ~f:(fun () -> aux (v :: acc) l')
  in
  aux [] vars

(* convert a prolog term into a type *)
let rec infer_ty_ ?loc ctx ty =
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
        begin match Ctx.get_var_ ctx v with
        | `Var v ->
            unify ?loc (Var.ty v) T.Ty.tType;
            T.Ty.var ?loc v
        | `ID (id, ty) ->
            unify ?loc ty T.Ty.tType;
            T.Ty.const id
        end
    | PT.Const f ->
        (* constant type *)
        let id, ty = Ctx.get_id_ ctx ~arity:0 f in
        unify ?loc ty T.Ty.tType;
        T.Ty.const id
    | PT.App (f, l) ->
        begin match PT.view f with
        | PT.Var name
        | PT.Const name ->
            let id, ty = Ctx.get_id_ ctx ~arity:(List.length l) name in
            unify ?loc (T.Ty.returns ty) T.Ty.tType;
            let l = List.map aux l in
            (* ensure that the type is well-typed (!) *)
            let ty_res = T.apply_unify ?loc ~allow_open:false ty l in
            unify ?loc ty_res T.Ty.tType;
            T.Ty.app id l
        | _ -> error_ ?loc "@[<2>cannot apply non-constant@ `@[%a@]`@]" PT.pp f
        end
    | PT.Bind (Binder.ForallTy, vars, body) ->
        with_typed_vars_ ?loc ~infer_ty:infer_ty_ ctx vars
          ~f:(fun vars' ->
              (* be sure those are type variables *)
              List.iter (fun v -> unify T.tType (Var.ty v)) vars';
              let body' = aux body in
              T.Ty.forall_l vars' body')
    | _ ->
        error_ ?loc "@[<2>`@[%a@]`@ is not a valid type@]" PT.pp ty
  in
  aux ty

(* XXX: hack: need to define {!with_typed_vars_} before {!infer_ty_}
   so that it generalizes return type properly *)
let with_typed_vars ?loc ctx vars ~f =
  with_typed_vars_ ?loc ~infer_ty:infer_ty_ ctx vars ~f

let infer_ty_exn ctx ty = infer_ty_ ?loc:(PT.loc ty) ctx ty

let infer_ty ctx ty =
  try Err.return (infer_ty_exn ctx ty)
  with e -> Err.of_exn_trace e

(* infer a type for [t], possibly updating [ctx]. Also returns a
   continuation to build a typed term. *)
let rec infer_rec ctx t =
  let loc = PT.loc t in
  let t' = match PT.view t with
  | PT.Var name ->
      begin match Ctx.get_var_ ctx name with
      | `Var v -> T.var v
      | `ID (id, ty) -> T.const ~ty id
      end
  | PT.Const s ->
      let id, ty = Ctx.get_id_ ?loc ~arity:0 ctx s in
      T.const ~ty id
  | PT.App ({PT.term=PT.Const s; _}, l) ->
      let id, ty_s = Ctx.get_id_ ?loc ~arity:(List.length l) ctx s in
      (* infer types for arguments *)
      let l = List.map (infer_rec ctx) l in
      Util.debugf ~section 5 "@[<2>apply@ @[<2>%a:@,%a@]@ to [@[<2>@[%a@]]:@,[@[%a@]@]]@]"
        (fun k->k ID.pp id T.pp ty_s (Util.pp_list T.pp) l
         (Util.pp_list T.pp) (List.map T.ty_exn l));
      let ty = T.apply_unify ?loc ~allow_open:true ty_s l in
      T.app ?loc ~ty (T.const ?loc ~ty:ty_s id) l
  | PT.App (f,l) ->
      (* higher order application *)
      let f = infer_rec ctx f in
      let l = List.map (infer_rec ctx) l in
      Util.debugf ~section 5 "@[<2>apply@ @[<2>%a:@,%a@]@ to [@[<2>@[%a@]]:@,[@[%a@]@]]@]"
        (fun k->k T.pp f T.pp (T.ty_exn f) (Util.pp_list T.pp) l
         (Util.pp_list T.pp) (List.map T.ty_exn l));
      let ty = T.apply_unify ?loc ~allow_open:true (T.ty_exn f) l in
      T.app ?loc ~ty f l
  | PT.List [] ->
      let v = Ctx.fresh_ty_meta_var ~dest:`ToDefault ctx in
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
          (fun s -> match Ctx.get_var_ ctx s with
            | `Var v -> v
            | `ID (id,_) -> error_ ?loc "row variable cannot be a constant %a" ID.pp id)
          rest
      in
      let ty = T.Ty.record_flatten ty_l ~rest:(CCOpt.map Var.ty rest) in
      T.record ~ty ?loc l ~rest
  | PT.AppBuiltin (Builtin.Wildcard, []) ->
      (* make a new TYPE variable *)
      let v = Ctx.fresh_ty_meta_var ~dest:`ToDefault ctx in
      T.Ty.meta v
  | PT.AppBuiltin (Builtin.Arrow, ret :: args) ->
      let ret = infer_ty_exn ctx ret in
      let args = List.map (infer_ty_exn ctx) args in
      T.Ty.fun_ ?loc args ret
  | PT.AppBuiltin (Builtin.True, []) -> T.Form.true_
  | PT.AppBuiltin (Builtin.False, []) -> T.Form.false_
  | PT.AppBuiltin (Builtin.And, l) ->
      let l = List.map (infer_prop_ ctx) l in
      T.Form.and_ ?loc l
  | PT.AppBuiltin (Builtin.Or, l) ->
      let l = List.map (infer_prop_ ctx) l in
      T.Form.or_ ?loc l
  | PT.AppBuiltin (((Builtin.Equiv | Builtin.Xor | Builtin.Imply) as conn), [a;b]) ->
      let a = infer_prop_ ctx a
      and b = infer_prop_ ctx b in
      begin match conn with
        | Builtin.Equiv -> T.Form.equiv ?loc a b
        | Builtin.Xor -> T.Form.xor ?loc a b
        | Builtin.Imply -> T.Form.imply ?loc a b
        | _ -> assert false
      end
  | PT.AppBuiltin (Builtin.Not, [a]) ->
      let a = infer_prop_ ctx a in
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
      with_typed_vars ?loc ctx vars
        ~f:(fun vars' ->
            let f' = infer_prop_ ctx f' in
            match binder with
              | Binder.Forall -> T.Form.forall_l vars' f'
              | Binder.Exists -> T.Form.exists_l vars' f'
              | _ -> assert false)
  | PT.Bind(Binder.Lambda, vars, t') ->
      with_typed_vars ?loc ctx vars
        ~f:(fun vars' ->
            let t' = infer_rec ctx t' in
            let ty = T.Ty.fun_ ?loc (List.map Var.ty vars') (T.ty_exn t') in
            T.bind_list ?loc ~ty Binder.Lambda vars' t')
  | PT.Bind (Binder.ForallTy, vars, t') ->
      with_typed_vars ?loc ctx vars
        ~f:(fun vars' ->
            let t' = infer_rec ctx t' in
            T.Ty.forall_l ?loc vars' t')
  | PT.AppBuiltin (Builtin.Int _ as b, []) -> T.builtin ~ty:T.Ty.int b
  | PT.AppBuiltin (Builtin.Rat _ as b, []) -> T.builtin ~ty:T.Ty.rat b
  | PT.AppBuiltin (Builtin.TyInt, []) -> T.Ty.int
  | PT.AppBuiltin (Builtin.TyRat, []) -> T.Ty.rat
  | PT.AppBuiltin (Builtin.Term, []) -> T.Ty.term
  | PT.AppBuiltin (Builtin.Prop, []) -> T.Ty.prop
  | PT.AppBuiltin (Builtin.TType, []) -> T.Ty.tType
  | PT.AppBuiltin (b,l) ->
      begin match TyBuiltin.ty b with
      | None ->
          error_ ?loc
            "@[<2>unexpected builtin in@ `@[%a@]`, expected term@]" PT.pp t
      | Some ty_b ->
          let i,j = T.Ty.arity ty_b in
          (* some builtin are ad-hoc polymorphic (eq, $less, ...) so
             we need to add wildcards *)
          let l = List.map (infer_rec ctx) l in
          let l =
            if i>0 && List.length l = j
            then (
              Util.debugf ~section 5
                "@[<2>add %d implicit type arguments to@ `@[<1>%a@ (%a)@]`@]"
                (fun k->k i Builtin.pp b (Util.pp_list T.pp) l);
              let metas = Ctx.fresh_ty_meta_vars ~dest:`ToGeneralize ctx i in
              let metas = List.map (T.Ty.meta ?loc) metas in
              metas @ l
            ) else l
          in
          let ty = T.apply_unify ~allow_open:true ?loc ty_b l in
          T.app_builtin ?loc ~ty b l
      end
  in
  Util.debugf ~section 5 "@[<hv2>typing of@ `@[%a@]`@ yields `@[%a@]`@ : `@[%a@]`@]"
    (fun k->k PT.pp t T.pp t' T.pp (T.ty_exn t'));
  t'

(* infer a term, and force its type to [prop] *)
and infer_prop_ ctx t =
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
  Util.enter_prof prof_infer;
  try
    let c = List.map (infer_prop_ ctx) c in
    Ctx.exit_scope ctx;
    Util.exit_prof prof_infer;
    c
  with e ->
    Util.exit_prof prof_infer;
    raise e

let infer_prop_exn ctx t =
  let t' = infer_exn ctx t in
  unify ?loc:(PT.loc t) (T.ty_exn t') T.prop;
  t'

let constrain_term_term_exn ?loc ctx t1 t2 =
  let t1 = infer_exn ctx t1 in
  let t2 = infer_exn ctx t2 in
  unify ?loc (T.ty_exn t1) (T.ty_exn t2)

let constrain_term_term ?loc ctx t1 t2 =
  try Err.return (constrain_term_term_exn ?loc ctx t1 t2)
  with e -> Err.of_exn_trace e

let constrain_term_type_exn ?loc ctx t ty =
  let t = infer_exn ctx t in
  unify ?loc ty (T.ty_exn t)

let constrain_term_type ?loc ctx t ty =
  try Err.return (constrain_term_type_exn ?loc ctx t ty)
  with e -> Err.of_exn_trace e

(** {2 Statements} *)

type typed_statement = (typed, typed, type_, UntypedAST.attrs) Statement.t

module A = UntypedAST
module Stmt = Statement

let infer_statement_exn ctx st =
  Util.debugf ~section 3 "@[<2>infer types for @{<yellow>statement@}@ `@[%a@]`@]"
    (fun k->k A.pp_statement st);
  (* auxiliary statements *)
  let src = st.A.attrs in
  let loc = st.A.loc in
  let st = match st.A.stmt with
    | A.Decl (s,ty) ->
        (* new type
           TODO: warning if it shadows? *)
        let id = ID.make s in
        let ty = infer_ty_exn ctx ty in
        Ctx.declare ctx id ty;
        Stmt.ty_decl ~src id ty
    | A.Def (s,ty,t) ->
        let id = ID.make s in
        let ty = infer_ty_exn ctx ty in
        let t = infer_exn ctx t in
        Ctx.declare ctx id ty;
        Stmt.def ~src id ty t
    | A.Data l ->
        (* declare the inductive types *)
        let data_types =
          List.map
            (fun d ->
              let data_ty = ID.make d.A.data_name in
              (* the type [data_ty : type -> type -> ... -> type] *)
              let ty_of_data_ty =
                T.Ty.fun_ (List.map (fun _ -> T.Ty.tType) d.A.data_vars) T.Ty.tType
              in
              Ctx.declare ctx data_ty ty_of_data_ty;
              data_ty, ty_of_data_ty)
            l
        in
        (* now we can infer the types of each constructor *)
        let l' =
          List.map2
            (fun d (data_ty,ty_of_data_ty) ->
              (* locally, declare type variables *)
              with_typed_vars ?loc ctx
                (List.map (fun v->v, Some PT.tType) d.A.data_vars)
                ~f:(fun ty_vars ->
                  (* return type of every constructor: [data_ty ty_vars] *)
                  let ty_ret =
                    T.Ty.app data_ty (List.map (T.Ty.var ?loc:None) ty_vars)
                  in
                  (* infer type of constructors *)
                  let cstors =
                    List.map
                      (fun (name, args) ->
                        let c_id = ID.make name in
                        (* type of c: forall ty_vars. ty_args -> ty_ret *)
                        let ty_args = List.map (infer_ty_exn ctx) args in
                        let ty_c =
                          T.Ty.forall_l ty_vars (T.Ty.fun_ ty_args ty_ret)
                        in
                        Ctx.declare ctx c_id ty_c;
                        (* TODO: check absence of other type variables in ty_c *)
                        c_id, ty_c)
                      d.A.data_cstors
                  in
                  Stmt.mk_data data_ty ty_of_data_ty ~args:ty_vars cstors
                ))
            l
            data_types
        in
        Ctx.exit_scope ctx;
        Stmt.data ~src l'
    | A.Assert t ->
        let t = infer_prop_exn ctx t in
        Stmt.assert_ ~src t
    | A.Goal t ->
        let t = infer_prop_exn ctx t in
        Stmt.goal ~src t
  in
  (* be sure to bind the remaining meta variables *)
  Ctx.bind_to_default ctx;
  let aux =
    Ctx.pop_new_types ctx
    |> List.map (fun (id,ty) -> Stmt.ty_decl ~src:A.default_attrs id ty)
  in
  st, aux

let infer_statements_exn ?(ctx=Ctx.create ()) seq =
  let res = CCVector.create () in
  Sequence.iter
    (fun st ->
      (* add declarations first *)
      let st, aux = infer_statement_exn ctx st in
      List.iter (CCVector.push res) aux;
      CCVector.push res st)
    seq;
  CCVector.freeze res

let infer_statements ?ctx seq =
  try CCError.return (infer_statements_exn ?ctx seq)
  with e -> CCError.of_exn_trace e

