
(* This file is free software, part of Logtk. See file "license" for more details. *)

(* {1 Type inference} *)

(** Reference:
    https://en.wikipedia.org/wiki/Hindley-Milner
*)

module PT = STerm
module T = TypedSTerm
module Loc = ParseLocation
module Err = CCResult
module Subst = Var.Subst
module Fmt = CCFormat

let prof_infer = Util.mk_profiler "TypeInference.infer"
let section = Util.Section.(make "ty-infer")

type 'a or_error = ('a, string) CCResult.t

type type_ = TypedSTerm.t
type untyped = STerm.t (** untyped term *)
type typed = TypedSTerm.t (** typed term *)
type loc = ParseLocation.t

exception Error of string

let () = Printexc.register_printer
    (function
      | Error msg ->
        Some (CCFormat.sprintf "@[@{<Red>type inference error@}:@ %s@]" msg)
      | _ -> None)

let error_on_incomplete_match_ = ref false
let () =
  Options.add_opts
    [ "--require-exhaustive-matches", Arg.Set error_on_incomplete_match_,
      " fail if pattern matches are not exhaustive";
      "--no-require-exhautive-matches", Arg.Clear error_on_incomplete_match_,
      " accept non-exhaustive pattern matches";
    ]

(* error-raising function *)
let error_ ?loc msg =
  CCFormat.ksprintf msg
    ~f:(fun msg ->
      let msg = match loc with
        | None -> msg
        | Some l -> Util.err_spf "@[<hv>%s@ at %a@]" msg Loc.pp l
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
    def_as_rewrite: bool;
    (* if true, definitions are rewrite rules *)
    on_var: [`Default | `Infer];
    (* what to do for variables without type annotation *)
    on_undef: [`Warn | `Fail | `Guess];
    (* what to do when we meet an undefined symbol *)
    on_shadow: [`Warn | `Ignore];
    (* what to do when an identifier is re-declared *)
    implicit_ty_args: bool;
    (* add implicit type arguments in applications? *)
    mutable new_metas: T.meta_var list;
    (* variables that should be generalized in the global scope
       or bound to [default], if they are not bound *)
    mutable local_vars: T.t Var.t list;
    (* free variables in the local scope *)
    mutable datatypes: (ID.t * type_ * (type_ * (ID.t * type_)) list) list ID.Tbl.t;
    (* datatype ID -> list of cstors *)
    mutable new_types: (ID.t * type_) list;
    (* list of symbols whose type has been inferred recently *)
  }

  let create
      ?(def_as_rewrite=true) ?(default=T.Ty.term)
      ?(on_var=`Infer) ?(on_undef=`Guess)
      ?(on_shadow=`Warn)
      ~implicit_ty_args
      () =
    let ctx = {
      default;
      def_as_rewrite;
      on_var;
      on_undef;
      on_shadow;
      implicit_ty_args;
      env = Hashtbl.create 32;
      datatypes = ID.Tbl.create 32;
      new_metas=[];
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

  (* only specialize/generalize variable if it's not bound *)
  let bind_meta ctx (v, r, k) =
    match !r, k with
      | None, `BindDefault ->
        let ty = ctx.default in
        Util.debugf ~section 5 "@[<2>specialize type meta_var %a to@ @[%a@]@]"
          (fun k->k Var.pp v T.pp ty);
        r := Some ty
      | None, `Generalize ->
        let v' = Var.copy v in
        Util.debugf ~section 5 "@[<2>generalize type meta_var %a@]"
          (fun k->k Var.pp v);
        r := Some (T.var v')
      | None, `NoBind -> assert false
      | Some _, _ -> ()

  let exit_scope ctx =
    List.iter (fun v -> Hashtbl.remove ctx.env (Var.name v)) ctx.local_vars;
    ctx.local_vars <- [];
    (* try to bind the variable (generalizing or binding to default).
       Will fail if already bound to something else, which is fine. *)
    List.iter (bind_meta ctx) ctx.new_metas;
    ctx.new_metas <- [];
    ()

  let declare ?loc ctx s ty =
    let name = ID.name s in
    let doit = match CCHashtbl.get ctx.env name with
      | None -> true
      | Some (`ID (_,ty_old) | `Var {Var.ty=ty_old;_}) ->
        begin match ctx.on_shadow with
          | `Ignore ->
            (* ignore decl, but ensure the two types are the same *)
            Util.debugf ~section 5 "ignore duplicate declaration of `%a`"
              (fun k->k ID.pp s);
            T.unify ?loc ty_old ty;
            false
          | `Warn ->
            Util.warnf "@[<2>shadowing identifier %s@]" name;
            true
        end
    in
    if doit then (
      Util.debugf ~section 3 "@{<yellow>declare@} %a:@ @[%a@]"
        (fun k->k ID.pp s T.pp ty);
      Hashtbl.add ctx.env name (`ID (s,ty))
    )

  let default_dest ctx = match ctx.on_var with
    | `Default -> `BindDefault
    | `Infer -> `Generalize

  (* generate fresh type var. *)
  let fresh_ty_meta_var ctx ?(dest=default_dest ctx) () : T.meta_var =
    let v = Var.gensym ~ty:T.tType () in
    let r = ref None in
    let meta = v, r, dest in
    ctx.new_metas <- meta :: ctx.new_metas;
    meta

  (* generate [n] fresh type meta vars *)
  let rec fresh_ty_meta_vars ?dest ctx n =
    if n = 0
    then []
    else fresh_ty_meta_var ?dest ctx () :: fresh_ty_meta_vars ?dest ctx (n-1)

  (* Fresh function type with [arity] arguments. Type meta-vars should
     not be generalized but bound to default. *)
  let fresh_fun_ty ?(dest=`BindDefault) ~arity ctx =
    let ret = fresh_ty_meta_var ctx ~dest () in
    let new_vars = fresh_ty_meta_vars ~dest ctx arity in
    let ty = T.Ty.fun_ (List.map (fun v->T.Ty.meta v) new_vars) (T.Ty.meta ret) in
    ty

  let find_close_names ctx (s:string): string list =
    CCHashtbl.keys ctx.env
    |> Sequence.filter
      (fun s' -> CCString.edit_distance s s' < 2)
    |> Sequence.to_rev_list
    |> CCList.sort_uniq ~cmp:String.compare

  let pp_names out = function
    | [] -> ()
    | l ->
      Fmt.fprintf out " (did you mean any of [@[%a@]]?)" (Util.pp_list Fmt.string) l

  (* Does the identifier represent a (TPTP) distinct object? *)
  let is_distinct_ s =
    String.length s > 2 && s.[0] = '"' && s.[String.length s-1] = '"'

  let get_id_ ?loc ~arity ctx name =
    try match Hashtbl.find ctx.env name with
      | `ID (id, ty) -> id, ty
      | `Var _ -> error_ ?loc "@[<2>expected `%s` to be a constant, not a variable@]" name
    with Not_found ->
      let ty = fresh_fun_ty ~arity ctx in
      begin match ctx.on_undef with
        | `Fail -> error_ ?loc "unknown identifier %s" name
        | `Guess -> ()
        | `Warn ->
          Util.warnf
            "@[<2>unknown constant %s@,%a,@ will create one with type @[%a@]@]"
            name pp_names (find_close_names ctx name) T.pp ty;
      end;
      let id = ID.make name in
      if is_distinct_ name then ID.set_payload id ID.Attr_distinct;
      Hashtbl.add ctx.env name (`ID (id, ty));
      ctx.new_types <- (id, ty) :: ctx.new_types;
      id, ty

  (* in ZF, variables might be constant, there is no syntactic difference *)
  let get_var_ ?loc ctx v =
    let mk_fresh v =
      let dest = default_dest ctx in
      let ty_v = fresh_ty_meta_var ~dest ctx () in
      let v' = Var.of_string ~ty:(T.Ty.meta ty_v) v in
      ctx.local_vars <- v' :: ctx.local_vars;
      v'
    in
    match v with
      | PT.Wildcard -> `Var (mk_fresh "_")
      | PT.V v ->
        try Hashtbl.find ctx.env v
        with Not_found ->
          begin match ctx.on_undef with
            | `Fail -> error_ ?loc "unknown variable %s@,%a" v pp_names (find_close_names ctx v)
            | `Guess -> ()
            | `Warn ->
              Util.warnf
                "@[<2>create implicit variable %s@,%a%a@]"
                v pp_names (find_close_names ctx v) Loc.pp_opt loc;
          end;
          let v' = mk_fresh v in
          Hashtbl.add ctx.env v (`Var v');
          `Var v'

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
        | None -> T.Ty.meta (Ctx.fresh_ty_meta_var ~dest:`Generalize ctx ())
        | Some ty -> infer_ty ?loc ctx ty
      in
      let v = match v with
        | PT.Wildcard -> Var.of_string ~ty "_"
        | PT.V v -> Var.of_string ~ty v
      in
      (* enter [v] before dealing with next variables, for they might depend
         on it (e.g. [forall (a:type)(l:list a). ...]) *)
      Ctx.with_var ctx v ~f:(fun () -> aux (v :: acc) l')
  in
  aux [] vars

let with_typed_var_ ?loc ~infer_ty ctx v ~f =
  with_typed_vars_ ?loc ~infer_ty ctx [v]
    ~f:(function
      | [v] -> f v
      | _ -> assert false)

let apply_unify ~allow_open ?loc ctx ty l =
  T.apply_unify ty l
    ?loc ~allow_open
    ~gen_fresh_meta:(Ctx.fresh_ty_meta_var ctx ~dest:`Generalize)

(* convert a prolog term into a type *)
let rec infer_ty_ ?loc ctx ty =
  let rec aux ty = match PT.view ty with
    | PT.AppBuiltin (Builtin.TyInt, []) -> T.Ty.int
    | PT.AppBuiltin (Builtin.TyRat, []) -> T.Ty.rat
    | PT.AppBuiltin (Builtin.TyReal, []) -> T.Ty.real
    | PT.AppBuiltin (Builtin.Term, []) -> T.Ty.term
    | PT.AppBuiltin (Builtin.Prop, []) -> T.Ty.prop
    | PT.AppBuiltin (Builtin.TType, []) -> T.Ty.tType
    | PT.AppBuiltin (Builtin.Arrow, ret :: args) ->
      let ret = aux ret in
      let args = List.map aux args in
      T.Ty.fun_ ?loc args ret
    | PT.AppBuiltin (Builtin.HasType, [t;ty]) ->
      (* cast *)
      let t = aux t in
      let ty = aux ty in
      unify ?loc ty T.Ty.tType;
      unify ?loc (T.ty_exn t) ty;
      t
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
      let id, ty = Ctx.get_id_ ?loc ctx ~arity:0 f in
      unify ?loc ty T.Ty.tType;
      T.Ty.const id
    | PT.App (f, l) ->
      begin match PT.view f with
        | PT.Var PT.Wildcard -> error_ ?loc "wildcard function: not supported"
        | PT.Var (PT.V name)
        | PT.Const name ->
          let id, ty = Ctx.get_id_ ?loc ctx ~arity:(List.length l) name in
          aux_app id ty l
        | _ -> error_ ?loc "@[<2>cannot apply non-constant@ `@[%a@]`@]" PT.pp f
      end
    | PT.Bind (Binder.ForallTy, vars, body) ->
      with_typed_vars_ ?loc ~infer_ty:infer_ty_ ctx vars
        ~f:(fun vars' ->
          (* be sure those are type variables *)
          List.iter (fun v -> unify T.tType (Var.ty v)) vars';
          let body' = aux body in
          T.Ty.forall_l vars' body')
    | PT.AppBuiltin (Builtin.Wildcard,[]) ->
      Ctx.fresh_ty_meta_var ctx ~dest:`Generalize () |> T.meta
    | _ ->
      error_ ?loc "@[<2>`@[%a@]`@ is not a valid type@]" PT.pp ty
  and aux_app id ty l =
    unify ?loc (T.Ty.returns ty) T.Ty.tType;
    let l = List.map aux l in
    (* ensure that the type is well-typed (!) *)
    let ty_res = apply_unify ctx ~allow_open:false ?loc ty l in
    unify ?loc ty_res T.Ty.tType;
    T.Ty.app id l
  in
  aux ty

(* XXX: hack: need to define {!with_typed_vars_} before {!infer_ty_}
   so that it generalizes return type properly *)
let with_non_inferred_typed_vars ?loc ctx vars ~f =
  with_typed_vars_ ?loc ~infer_ty:infer_ty_ ctx vars ~f

let infer_ty_exn ctx ty = infer_ty_ ?loc:(PT.loc ty) ctx ty

let infer_ty ctx ty =
  try Err.return (infer_ty_exn ctx ty)
  with e -> Err.of_exn_trace e

(* add type variables if needed, to apply [some_fun:ty_fun] to [l] *)
let add_implicit_params ctx ty_fun l =
  if ctx.Ctx.implicit_ty_args then (
    let tyvars, args, _ = T.Ty.unfold ty_fun in
    let l' =
      if List.length l = List.length args
      then List.map (fun _ -> PT.wildcard) tyvars
      else []
    in
    l'@l
  ) else l

let mk_metas ctx n =
  CCList.init n
    (fun _ -> T.Ty.meta (Ctx.fresh_ty_meta_var ~dest:`Generalize ctx ()))

(* apply type to the relevant number of metas; return the resulting type *)
let apply_ty_to_metas ?loc ctx (ty:T.Ty.t): T.Ty.t list * T.Ty.t =
  let ty_vars, _, _ = T.Ty.unfold ty in
  let metas = mk_metas ctx (List.length ty_vars) in
  let ty = apply_unify ctx ~allow_open:true ?loc ty metas in
  metas, ty

(* infer a type for [t], possibly updating [ctx]. Also returns a
   continuation to build a typed term. *)
let rec infer_rec ?loc ctx t =
  let open Loc.Infix in
  let loc = PT.loc t <+> loc in
  let t' = match PT.view t with
    | PT.Var name ->
      begin match Ctx.get_var_ ctx name with
        | `Var v -> T.var v
        | `ID (id, ty_id) ->
          (* implicit parameters, e.g. for [nil] *)
          let l =
            add_implicit_params ctx ty_id [] |> List.map (infer_rec ?loc ctx)
          in
          let ty = apply_unify ctx ?loc ~allow_open:true ty_id l in
          T.app ?loc ~ty (T.const ?loc ~ty:ty_id id) l
      end
    | PT.Const s ->
      let id, ty_id = Ctx.get_id_ ?loc ~arity:0 ctx s in
      (* implicit parameters, e.g. for [nil] *)
      let l = add_implicit_params ctx ty_id [] |> List.map (infer_rec ?loc ctx) in
      let ty = apply_unify ctx ?loc ~allow_open:true ty_id l in
      T.app ?loc ~ty (T.const ?loc ~ty:ty_id id) l
    | PT.App ({PT.term=PT.Var v; _}, l) ->
      begin match Ctx.get_var_ ?loc ctx v with
        | `ID (id,ty) -> infer_app ?loc ctx id ty l
        | `Var v ->
          let l = add_implicit_params ctx (Var.ty v) l in
          (* infer types for arguments *)
          let l = List.map (infer_rec ?loc ctx) l in
          Util.debugf ~section 5 "@[<2>apply@ @[<2>%a:@,%a@]@ to [@[<2>@[%a@]]:@,[@[%a@]@]]@]"
            (fun k->k Var.pp v T.pp (Var.ty v) (Util.pp_list T.pp) l
                (Util.pp_list T.pp) (List.map T.ty_exn l));
          let ty = apply_unify ctx ?loc ~allow_open:true (Var.ty v) l in
          T.app ?loc ~ty (T.var ?loc v) l
      end
    | PT.App ({PT.term=PT.Const s; _}, l) ->
      let id, ty_s = Ctx.get_id_ ?loc ~arity:(List.length l) ctx s in
      infer_app ?loc ctx id ty_s l
    | PT.App (f,l) ->
      (* higher order application *)
      let f = infer_rec ?loc ctx f in
      let l = List.map (infer_rec ?loc ctx) l in
      Util.debugf ~section 5 "@[<2>apply@ @[<2>%a:@,%a@]@ to [@[<2>@[%a@]]:@,[@[%a@]@]]@]"
        (fun k->k T.pp f T.pp (T.ty_exn f) (Util.pp_list T.pp) l
            (Util.pp_list T.pp) (List.map T.ty_exn l));
      let ty = apply_unify ctx ?loc ~allow_open:true (T.ty_exn f) l in
      T.app ?loc ~ty f l
    | PT.Ite (a,b,c) ->
      let a = infer_prop_ ?loc ctx a in
      let b = infer_rec ?loc ctx b in
      let c = infer_rec ?loc ctx c in
      unify ?loc (T.ty_exn b)(T.ty_exn c);
      T.ite ?loc a b c
    | PT.Let (l, u) ->
      (* deal with pairs in [l] one by one *)
      let rec aux = function
        | [] -> infer_rec ?loc ctx u
        | (v,t) :: tail ->
          let t = infer_rec ?loc ctx t in
          with_typed_var_ ctx ?loc ~infer_ty:(fun ?loc:_ _ ty -> ty)
            (v, Some (T.ty_exn t))
            ~f:(fun v ->
              let body = aux tail in
              T.let_ ?loc [v, t] body)
      in
      aux l
    | PT.Match (u, l) ->
      let u = infer_rec ?loc ctx u in
      let ty_u = T.ty_exn u in
      (* find the datatype corresponding to [u] *)
      let data =
        try
          let ty_id = T.head_exn ty_u in
          ID.Tbl.find ctx.Ctx.datatypes ty_id
        with Not_found ->
          error_ ?loc "type `@[%a@]` is not a known datatype" T.pp ty_u
      in
      let l = infer_match ?loc ctx ~ty_matched:ty_u t data l in
      T.match_ ?loc u l
    | PT.List [] ->
      let v = Ctx.fresh_ty_meta_var ~dest:`Generalize ctx () in
      let ty = T.Ty.multiset (T.Ty.meta v) in
      T.multiset ?loc ~ty []
    | PT.List (t::l) ->
      let t = infer_rec ?loc ctx t in
      let l = List.map (infer_rec ?loc ctx) l in
      let ty = T.Ty.multiset (T.ty_exn t) in
      T.multiset ?loc ~ty (t::l)
    | PT.Record (l,rest) ->
      (* infer types of fields *)
      let ty_l, l =
        List.map
          (fun (n,t) ->
             let t' = infer_rec ?loc ctx t in
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
      let v = Ctx.fresh_ty_meta_var ~dest:`Generalize ctx () in
      T.Ty.meta v
    | PT.AppBuiltin (Builtin.Arrow, ret :: args) ->
      let ret = infer_ty_exn ctx ret in
      let args = List.map (infer_ty_exn ctx) args in
      T.Ty.fun_ ?loc args ret
    | PT.AppBuiltin (Builtin.True, []) -> T.Form.true_
    | PT.AppBuiltin (Builtin.False, []) -> T.Form.false_
    | PT.AppBuiltin (Builtin.And, l) ->
      let l = List.map (infer_prop_ ?loc ctx) l in
      T.Form.and_ ?loc l
    | PT.AppBuiltin (Builtin.Or, l) ->
      let l = List.map (infer_prop_ ?loc ctx) l in
      T.Form.or_ ?loc l
    | PT.AppBuiltin (((Builtin.Equiv | Builtin.Xor | Builtin.Imply) as conn), [a;b]) ->
      let a = infer_prop_ ?loc ctx a
      and b = infer_prop_ ?loc ctx b in
      begin match conn with
        | Builtin.Equiv -> T.Form.equiv ?loc a b
        | Builtin.Xor -> T.Form.xor ?loc a b
        | Builtin.Imply -> T.Form.imply ?loc a b
        | _ -> assert false
      end
    | PT.AppBuiltin (Builtin.Not, [a]) ->
      let a = infer_prop_ ?loc ctx a in
      T.Form.not_ ?loc a
    | PT.AppBuiltin ((Builtin.Eq | Builtin.Neq) as conn, [a;b]) ->
      (* a ?= b *)
      let a = infer_rec ?loc ctx a in
      let b = infer_rec ?loc ctx b in
      unify ?loc (T.ty_exn a) (T.ty_exn b);
      if T.Ty.returns_tType (T.ty_exn a)
      then error_ ?loc "(in)equation @[%a@] ?= @[%a@] between types is forbidden" T.pp a T.pp b;
      begin match conn with
        | Builtin.Eq ->
          if T.Ty.is_prop (T.ty_exn a) then T.Form.equiv a b else T.Form.eq a b
        | Builtin.Neq ->
          if T.Ty.is_prop (T.ty_exn a) then T.Form.xor a b else T.Form.neq a b
        | _ -> assert false
      end
    | PT.Bind(((Binder.Forall | Binder.Exists) as binder), vars, f') ->
      with_non_inferred_typed_vars ?loc ctx vars
        ~f:(fun vars' ->
          let f' = infer_prop_ ctx f' in
          match binder with
            | Binder.Forall -> T.Form.forall_l vars' f'
            | Binder.Exists -> T.Form.exists_l vars' f'
            | _ -> assert false)
    | PT.Bind(Binder.Lambda, vars, t') ->
      with_non_inferred_typed_vars ?loc ctx vars
        ~f:(fun vars' ->
          let t' = infer_rec ?loc ctx t' in
          let ty = T.Ty.fun_ ?loc (List.map Var.ty vars') (T.ty_exn t') in
          T.bind_list ?loc ~ty Binder.Lambda vars' t')
    | PT.Bind (Binder.ForallTy, vars, t') ->
      with_non_inferred_typed_vars ?loc ctx vars
        ~f:(fun vars' ->
          let t' = infer_rec ?loc ctx t' in
          T.Ty.forall_l ?loc vars' t')
    | PT.AppBuiltin (Builtin.Int _ as b, []) -> T.builtin ~ty:T.Ty.int b
    | PT.AppBuiltin (Builtin.Rat _ as b, []) -> T.builtin ~ty:T.Ty.rat b
    | PT.AppBuiltin (Builtin.Real _ as b, []) -> T.builtin ~ty:T.Ty.real b
    | PT.AppBuiltin (Builtin.TyInt, []) -> T.Ty.int
    | PT.AppBuiltin (Builtin.TyRat, []) -> T.Ty.rat
    | PT.AppBuiltin (Builtin.Term, []) -> T.Ty.term
    | PT.AppBuiltin (Builtin.Prop, []) -> T.Ty.prop
    | PT.AppBuiltin (Builtin.TType, []) -> T.Ty.tType
    | PT.AppBuiltin (Builtin.HasType, [t;ty]) ->
      (* cast *)
      let t = infer_rec ?loc ctx t in
      let ty = infer_ty_exn ctx ty in
      unify ?loc (T.ty_exn t) ty;
      t
    | PT.AppBuiltin (Builtin.HasType, l) ->
      error_ ?loc "ill-formed has_type@ [@[<hv>%a@]]" (Util.pp_list PT.pp) l
    | PT.AppBuiltin (b,l) ->
      begin match TyBuiltin.ty b with
        | None ->
          error_ ?loc
            "@[<2>unexpected builtin in@ `@[%a@]`, expected term@]" PT.pp t
        | Some ty_b ->
          let i,j = T.Ty.arity ty_b in
          (* some builtin are ad-hoc polymorphic (eq, $less, ...) so
             we need to add wildcards *)
          let l = List.map (infer_rec ?loc ctx) l in
          let l =
            if i>0 && List.length l = j
            then (
              Util.debugf ~section 5
                "@[<2>add %d implicit type arguments to@ `@[<1>%a@ (%a)@]`@]"
                (fun k->k i Builtin.pp b (Util.pp_list T.pp) l);
              let metas = Ctx.fresh_ty_meta_vars ~dest:`Generalize ctx i in
              let metas = List.map (T.Ty.meta ?loc) metas in
              metas @ l
            ) else l
          in
          let ty = apply_unify ctx ~allow_open:true ?loc ty_b l in
          T.app_builtin ?loc ~ty b l
      end
  in
  Util.debugf ~section 5 "@[<hv>typing of `@[%a@]`@ yields @[<2>`@[%a@]`@ : `@[%a@]`@]@]"
    (fun k->k PT.pp t T.pp t' T.pp (T.ty_exn t'));
  t'

and infer_app ?loc ctx id ty_id l =
  let l = add_implicit_params ctx ty_id l in
  (* infer types for arguments *)
  let l = List.map (infer_rec ?loc ctx) l in
  Util.debugf ~section 5 "@[<2>apply@ @[<2>%a:@,%a@]@ to [@[<2>@[%a@]]:@,[@[%a@]@]]@]"
    (fun k->k ID.pp id T.pp ty_id (Util.pp_list T.pp) l
        (Util.pp_list T.pp) (List.map T.ty_exn l));
  let ty = apply_unify ctx ?loc ~allow_open:true ty_id l in
  T.app ?loc ~ty (T.const ?loc ~ty:ty_id id) l

(* replace a match with possibly a "default" case into a completely
   defined match *)
and infer_match ?loc ctx ~ty_matched t data (l:PT.match_branch list)
  : (T.match_cstor * type_ Var.t list * T.t) list =
  let ty_ret = ref None in
  (* check consistency of types in every branch *)
  let check_ty ty : unit = match !ty_ret with
    | None -> ty_ret := Some ty
    | Some ty' -> unify ?loc ty' ty
  in
  let seen = ref [] in
  let seen_default = ref false in
  let l = CCList.flat_map
      (fun b ->
         begin match b with
           | PT.Match_default rhs ->
             seen_default := true;
             let rhs = infer_rec ?loc ctx rhs in
             check_ty (T.ty_exn rhs);
             (* now cover every missing case *)
             CCList.filter_map
               (fun (c_id,c_ty,_) ->
                  if List.exists (ID.equal c_id) !seen
                  then None
                  else (
                    let ty_params, c_ty_applied = apply_ty_to_metas ?loc ctx c_ty in
                    let _vars, ty_args, ty_ret = T.Ty.unfold c_ty_applied in
                    assert (_vars=[]);
                    unify ?loc ty_ret ty_matched;
                    let vars =
                      List.mapi (fun i ty -> Var.makef ~ty "x_%d" i) ty_args
                    in
                    let cstor =
                      { T.cstor_id=c_id; cstor_ty=c_ty; cstor_args=ty_params; }
                    in
                    Some (cstor, vars, rhs)
                  ))
               data
           | PT.Match_case (s, vars, rhs) ->
             let c_id, c_ty = Ctx.get_id_ ?loc ~arity:(List.length vars) ctx s in
             if List.exists (ID.equal c_id) !seen then (
               error_ ?loc "duplicate branch for constructor `%a`" ID.pp c_id
             );
             if List.for_all (fun (cstor,_,_) -> not (ID.equal c_id cstor)) data then (
               error_ ?loc "symbol `%a` not a suitable constructor" ID.pp c_id
             );
             seen := c_id :: !seen;
             (* apply [ty_s] to some meta variables *)
             let ty_params, c_ty_applied = apply_ty_to_metas ?loc ctx c_ty in
             let _vars, ty_s_args, ty_ret_s = T.Ty.unfold c_ty_applied in
             assert (_vars=[]);
             unify ?loc ty_ret_s ty_matched;
             if List.length ty_s_args <> List.length vars then (
               error_ ?loc "constructor `%a`@ expected %d arguments,@ got %d"
                 ID.pp c_id (List.length ty_s_args) (List.length vars);
             );
             with_typed_vars_ ?loc ~infer_ty:(fun ?loc:_ _ ty -> ty) ctx
               (List.map2 (fun v ty_arg -> v, Some ty_arg) vars ty_s_args)
               ~f:(fun vars ->
                 (* now we have everything in scope, we can convert [rhs] *)
                 let rhs = infer_rec ?loc ctx rhs in
                 check_ty (T.ty_exn rhs);
                 let cstor =
                   { T.cstor_id=c_id; cstor_ty=c_ty; cstor_args=ty_params; }
                 in
                 [cstor, vars, rhs])
         end)
      l
  in
  let missing =
    CCList.filter_map
      (fun (id,_,_) ->
         if List.exists (fun (c,_,_) -> ID.equal id c.T.cstor_id) l
         then None else Some id)
      data
  in
  begin match missing with
    | [] -> l
    | _::_ ->
      if !error_on_incomplete_match_ then (
        error_ ?loc "missing cases in match: (@[%a@])@ :in `%a`"
          (Util.pp_list ID.pp) missing PT.pp t
      ) else (
        Util.warnf "%a@,missing cases in match: (@[%a@])@ :in `%a`"
          Loc.pp_opt loc (Util.pp_list ID.pp) missing PT.pp t;
        l
      )
  end

(* infer a term, and force its type to [prop] *)
and infer_prop_ ?loc ctx t =
  let t = infer_rec ?loc ctx t in
  unify ?loc (T.ty_exn t) T.Ty.prop;
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

type typed_statement = (typed, typed, type_) Statement.t

module A = UntypedAST
module Stmt = Statement

let check_vars_rhs ?loc bound rhs =
  let vars_rhs = T.Seq.free_vars rhs |> Var.Set.of_seq in
  (* check that all variables of [rhs] are within [lhs] *)
  let only_in_rhs = Var.Set.diff vars_rhs bound in
  if not (Var.Set.is_empty only_in_rhs) then (
    error_ ?loc "variables @[%a@]@ occur in RHS/cond `@[%a@]`@ but are not bound"
      Var.Set.pp only_in_rhs T.pp rhs;
  )

(* check that [vars rhs] subseteq [vars lhs] *)
let check_vars_eqn ?loc bound lhs rhs =
  let vars_lhs = T.Seq.free_vars lhs |> Var.Set.of_seq in
  (* check that all variables in [lhs] are bound *)
  let not_bound = Var.Set.diff vars_lhs bound in
  if not (Var.Set.is_empty not_bound)
  then error_ ?loc "variables @[%a@] are not bound" Var.Set.pp not_bound;
  check_vars_rhs ?loc vars_lhs rhs;
  ()

(* decompose [t] as [forall vars. id args = rhs]
   or [forall vars. lhs <=> rhs]
   @return [id, ty, args, rhs] or [lhs,rhs]
   @param bound the set of bound variables so far *)
let rec as_def ?loc ?of_ bound t =
  let fail() =
    error_ ?loc "expected `forall <vars>. <lhs> =/<=> <rhs>`"
  and yield_term id ty args rhs =
    let vars =
      Sequence.of_list args
      |> Sequence.flat_map T.Seq.free_vars
      |> Var.Set.add_seq bound
      |> Var.Set.to_list
      |> T.sort_ty_vars_first
    in
    (* check that we talk about the same ID *)
    begin match of_ with
      | Some id' when not (ID.equal id id') ->
        error_ ?loc
          "rule `%a`@ for `%a` has head symbol `%a`@ \
           every rule in the definition of `%a` \
           must start with `%a`"
          T.pp t ID.pp id ID.pp id' ID.pp id ID.pp id;
      | _ -> ()
    end;
    if T.Ty.returns_tType ty then (
      error_ ?loc
        "in definition of %a,@ equality between types is forbidden" ID.pp id;
    );
    Stmt.Def_term {vars;id;ty;args;rhs;as_form=t}
  and yield_prop lhs rhs pol =
    let vars =
      SLiteral.to_seq lhs
      |> Sequence.flat_map T.Seq.free_vars
      |> Var.Set.add_seq bound
      |> Var.Set.to_list
      |> T.sort_ty_vars_first
    in
    assert (T.Ty.is_prop (T.ty_exn rhs));
    begin match lhs with
      | SLiteral.Atom (t,_) ->
        begin match T.head t, of_ with
          | Some id, Some id' when not (ID.equal id' id) ->
            error_ ?loc
              "rule `%a`@ must have `%a` as head symbol, not `%a`"
              T.pp t ID.pp id' ID.pp id
          | _ -> ()
        end
      | _ -> ()
    end;
    Stmt.Def_form {vars;lhs;rhs=[rhs];polarity=pol;as_form=[t]}
  in
  begin match T.view t with
    | T.Bind (Binder.Forall, v, t) ->
      as_def ?loc (Var.Set.add bound v) t
    | T.AppBuiltin ((Builtin.Equiv | Builtin.Imply) as op, [lhs;rhs]) ->
      (* check that LHS is a literal, and  that all free variables
         of RHS occur in LHS (bound variables are ok though) *)
      check_vars_eqn ?loc bound lhs rhs;
      let lhs = SLiteral.of_form lhs in
      let pol = if op=Builtin.Equiv then `Equiv else `Imply in
      yield_prop lhs rhs pol
    | T.AppBuiltin (Builtin.Eq, [lhs;rhs]) ->
      check_vars_eqn ?loc bound lhs rhs;
      begin match T.view lhs with
        | T.Const id ->
          let ty = T.ty_exn lhs in
          yield_term id ty [] rhs
        | T.App (f, args) ->
          begin match T.view f with
            | T.Const id ->
              let ty = T.ty_exn f in
              yield_term id ty args rhs
            | _ -> fail()
          end
        | _ -> fail()
      end
    | T.AppBuiltin (Builtin.Not, [lhs]) ->
      let rhs = T.Form.false_ in
      check_vars_eqn ?loc bound lhs rhs;
      let lhs = SLiteral.of_form lhs in
      yield_prop lhs rhs `Equiv
    | _ when T.Ty.is_prop (T.ty_exn t) ->
      let rhs = T.Form.true_ in
      check_vars_eqn ?loc bound t rhs;
      let lhs = SLiteral.of_form t in
      yield_prop lhs rhs `Equiv
    | _ -> fail()
  end

let infer_defs ?loc ctx (l:A.def list): (_,_,_) Stmt.def list =
  (* first, declare all *)
  let decls =
    List.map
      (fun d ->
         let id = ID.make d.A.def_id in
         let ty = infer_ty_exn ctx d.A.def_ty in
         (* cannot return [Type] *)
         if T.Ty.returns_tType ty then (
           error_ ?loc
             "in definition of %a,@ equality between types is forbidden"
             ID.pp id;
         );
         Ctx.declare ?loc ctx id ty;
         id, ty, d.A.def_rules)
      l
  in
  (* now, infer type of each definition *)
  List.map
    (fun (id, ty, rules) ->
       let rules =
         List.map
           (fun r ->
              let r = infer_prop_exn ctx r in
              as_def ?loc ~of_:id Var.Set.empty r)
           rules
       in
       Stmt.mk_def ~rewrite:ctx.Ctx.def_as_rewrite id ty rules)
    decls

(* see whether attributes contain some hints of notation for this ID *)
let set_notation id attrs: unit =
  List.iter
    (function
      | A.A_app ("infix", [A.A_quoted s]) -> ID.set_payload id (ID.Attr_infix s)
      | A.A_app ("prefix", [A.A_quoted s]) -> ID.set_payload id (ID.Attr_prefix s)
      | _ -> ())
    attrs

let read_attrs ~file attrs =
  let module A = UntypedAST in
  let attrs = Stmt.conv_attrs attrs
  and name =
    CCList.find_map
      (function
        | A.A_app ("name", [(A.A_quoted s | A.A_app (s,[]))]) -> Some s
        | _ -> None)
      attrs
  in
  Proof.Src.from_file ?name file, attrs

let infer_statement_exn ?(file="<no file>") ctx st =
  Util.debugf ~section 3 "@[<2>infer types for @{<yellow>statement@}@ `@[%a@]`@]"
    (fun k->k A.pp_statement st);
  (* auxiliary statements *)
  let src, attrs = read_attrs ~file st.A.attrs in
  let loc = st.A.loc in
  let st = match st.A.stmt with
    | A.Include _ ->
      error_ ?loc "remaining include statement"
    | A.Decl (s,ty) ->
      (* new type
         TODO: warning if it shadows? *)
      let id = ID.make s in
      let ty = infer_ty_exn ctx ty in
      Ctx.declare ?loc ctx id ty;
      set_notation id st.A.attrs;
      Stmt.ty_decl ~attrs ~proof:(Proof.Step.intro src Proof.R_decl) id ty
    | A.Def l ->
      let l = infer_defs ?loc ctx l in
      List.iter
        (fun d -> set_notation d.Stmt.def_id st.A.attrs)
        l;
      Stmt.def ~attrs ~proof:(Proof.Step.intro src Proof.R_def) l
    | A.Rewrite t ->
      let t =  infer_prop_ ctx t in
      let def = as_def ?loc Var.Set.empty t in
      Stmt.rewrite ~proof:(Proof.Step.intro src Proof.R_def) def
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
             Ctx.declare ?loc ctx data_ty ty_of_data_ty;
             data_ty, ty_of_data_ty)
          l
      in
      (* now we can infer the types of each constructor *)
      let l' =
        List.map2
          (fun d (data_ty,ty_of_data_ty) ->
             (* locally, declare type variables *)
             with_non_inferred_typed_vars ?loc ctx
               (List.map (fun v->PT.V v, Some PT.tType) d.A.data_vars)
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
                        let args =
                          List.mapi
                            (fun i (p,ty) ->
                               let ty = infer_ty_exn ctx ty in
                               (* type of projector *)
                               let p_ty =
                                 T.Ty.forall_l ty_vars (T.Ty.fun_ [ty_ret] ty)
                               and p_id = match p with
                                 | Some p -> ID.make p
                                 | None ->
                                   (* create projector *)
                                   ID.makef "proj_%a_%d" ID.pp c_id i
                               in
                               Ctx.declare ?loc ctx p_id p_ty;
                               ty, (p_id, p_ty))
                            args
                        in
                        let ty_args = List.map fst args in
                        let ty_c =
                          T.Ty.forall_l ty_vars (T.Ty.fun_ ty_args ty_ret)
                        in
                        Ctx.declare ?loc ctx c_id ty_c;
                        (* TODO: check absence of other type variables in ty_c *)
                        c_id, ty_c, args)
                     d.A.data_cstors
                 in
                 ID.Tbl.add ctx.Ctx.datatypes data_ty cstors;
                 Stmt.mk_data data_ty ty_of_data_ty ~args:ty_vars cstors
               ))
          l
          data_types
      in
      Ctx.exit_scope ctx;
      Stmt.data ~attrs ~proof:(Proof.Step.intro src Proof.R_def) l'
    | A.Assert t ->
      let t = infer_prop_exn ctx t in
      Stmt.assert_ ~attrs ~proof:(Proof.Step.intro src Proof.R_assert) t
    | A.Lemma t ->
      let t = infer_prop_exn ctx t in
      Stmt.lemma ~attrs ~proof:(Proof.Step.intro src Proof.R_lemma) [t]
    | A.Goal t ->
      let t = infer_prop_exn ctx t in
      Stmt.goal ~attrs ~proof:(Proof.Step.intro src Proof.R_goal) t
  in
  (* be sure to bind the remaining meta variables *)
  Ctx.exit_scope ctx;
  let aux =
    Ctx.pop_new_types ctx
    |> List.map
      (fun (id,ty) ->
         let proof = Proof.Step.intro (Proof.Src.internal []) Proof.R_decl in
         Stmt.ty_decl ~proof id ty)
  in
  st, aux

let infer_statements_exn
    ?def_as_rewrite ?on_var ?on_undef ?on_shadow
    ?ctx ?file ~implicit_ty_args seq =
  let ctx = match ctx with
    | None ->
      Ctx.create ?def_as_rewrite ?on_var ?on_undef ?on_shadow ~implicit_ty_args ()
    | Some c -> c
  in
  let res = CCVector.create () in
  Sequence.iter
    (fun st ->
       (* add declarations first *)
       let st, aux = infer_statement_exn ?file ctx st in
       List.iter (CCVector.push res) aux;
       CCVector.push res st)
    seq;
  CCVector.freeze res

let infer_statements
    ?def_as_rewrite ?on_var ?on_undef ?on_shadow
    ?ctx ?file ~implicit_ty_args seq =
  try
    Err.return
      (infer_statements_exn ?def_as_rewrite ?on_var ?on_undef
         ?on_shadow ?ctx ?file ~implicit_ty_args seq)
  with e -> Err.of_exn_trace e
