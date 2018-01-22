
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Types} *)

module T = InnerTerm

type t = T.t

type ty = t

type builtin = TType | Prop | Term | Rat | Int

let builtin_conv = function
  | TType -> Builtin.tType
  | Prop -> Builtin.prop
  | Term -> Builtin.term
  | Rat -> Builtin.ty_rat
  | Int -> Builtin.ty_int

let pp_builtin out b = Builtin.pp out (builtin_conv b)

type view =
  | Builtin of builtin
  | Var of t HVar.t
  | DB of int
  | App of ID.t * t list (** parametrized type *)
  | Fun of t list * t (** Function type (left to right, no left-nesting) *)
  | Forall of t (** explicit quantification using De Bruijn index *)

let view t = match T.view t with
  | T.Var v -> Var v
  | T.DB i -> DB i
  | T.Bind (Binder.ForallTy, varty, t') ->
    assert (T.equal varty T.tType);
    Forall t'
  | T.Const s -> App (s, [])
  | T.App (f, l) ->
    begin match T.view f with
      | T.Const id -> App (id, l)
      | _ -> assert false
    end
  | T.AppBuiltin (Builtin.Arrow, [_]) -> assert false
  | T.AppBuiltin (Builtin.Arrow, (ret :: l)) -> Fun (l, ret)
  | T.AppBuiltin (Builtin.Prop, []) -> Builtin Prop
  | T.AppBuiltin (Builtin.TType, []) -> Builtin TType
  | T.AppBuiltin (Builtin.Term, []) -> Builtin Term
  | T.AppBuiltin (Builtin.TyInt, []) -> Builtin Int
  | T.AppBuiltin (Builtin.TyRat, []) -> Builtin Rat
  | _ -> assert false

let hash = T.hash
let equal = T.equal
let compare = T.compare

let hash_mod_alpha = T.hash_mod_alpha

let is_tType ty = match view ty with | Builtin TType -> true | _ -> false
let is_var ty = match view ty with | Var _ -> true | _ -> false
let is_bvar ty = match view ty with | DB _ -> true | _ -> false
let is_app ty = match view ty with App _ -> true | _ -> false
let is_const ty = match view ty with App (_, []) -> true | _ -> false
let is_fun ty = match view ty with | Fun _ -> true | _ -> false
let is_forall ty = match view ty with | Forall _ -> true | _ -> false
let is_prop ty = match view ty with | Builtin Prop -> true | _ -> false

let tType = T.tType
let prop = T.builtin ~ty:tType Builtin.Prop
let term = T.builtin ~ty:tType Builtin.Term
let int = T.builtin ~ty:tType Builtin.TyInt
let rat = T.builtin ~ty:tType Builtin.TyRat

let builtin = function
  | TType -> tType
  | Prop -> prop
  | Term -> term
  | Int -> int
  | Rat -> rat

let var v = T.var v

let var_of_int i = T.var (HVar.make ~ty:tType i)

let arrow = T.arrow

let app s l =
  let ty_s = arrow (List.map (fun _ -> T.tType) l) T.tType in
  T.app ~ty:T.tType (T.const ~ty:ty_s s) l

let const s = T.const ~ty:T.tType s

let bvar i =
  T.bvar ~ty:T.tType i

let forall ty =
  T.bind ~ty:T.tType ~varty:T.tType Binder.forall_ty ty

let rec forall_n n ty =
  if n=0 then ty
  else forall (forall_n (n-1) ty)

let forall_fvars vars ty =
  if vars=[] then ty
  else (
    List.fold_right
      (fun v ty ->
         assert (is_tType (HVar.ty v));
         let body = T.DB.from_var ty ~var:(var v) in
         forall body)
      vars ty
  )

let (==>) = arrow

let of_term_unsafe t = t
let of_terms_unsafe l = l
let cast_var_unsafe v = v

(** {2 Definitions} *)

type def =
  | Def_unin of int (* number of type variables *)
  | Def_data of int * ty list (* data type with number of variables and cstors *)

exception Payload_def of def


let def id =
  ID.payload_find id
    ~f:(function
      | Payload_def d -> Some d
      | _ -> None)

let def_exn id = match def id with
  | Some d -> d
  | None -> raise Not_found

let set_def id d = ID.set_payload id (Payload_def d)

(** {2 Containers} *)

module Set = T.Set
module Map = T.Map
module Tbl = T.Tbl

module Seq = struct
  let vars = T.Seq.vars
  let sub = T.Seq.subterms
  let symbols = T.Seq.symbols
  let add_set = T.Seq.add_set
  let max_var = T.Seq.max_var
  let min_var = T.Seq.min_var
end

module VarMap = T.VarMap
module VarSet = T.VarSet
module VarTbl = T.VarTbl

let vars_set set t = VarSet.add_seq set (Seq.vars t)

let vars t = vars_set VarSet.empty t |> VarSet.elements

let close_forall ty =
  let vars = vars ty in
  T.bind_vars ~ty:prop Binder.Forall vars ty

type arity_result =
  | Arity of int * int
  | NoArity

let arity ty =
  (* n_forall: number of forall traversed so far *)
  let rec traverse n_forall ty = match view ty with
    | Fun (l,ty') ->
      assert (not (is_fun ty'));
      Arity (n_forall, List.length l)
    | Forall ty' ->
      traverse (n_forall+1) ty'
    | Var _ | Builtin _ -> NoArity
    | DB _
    | App _ -> Arity (n_forall, 0)
  in traverse 0 ty

let rec expected_args ty = match view ty with
  | Fun (args, ret) -> args @ expected_args ret
  | Forall ty' -> expected_args ty'
  | DB _ | Var _ | Builtin _ | App _ -> []

let expected_ty_vars t = T.expected_ty_vars t

let needs_args ty = expected_ty_vars ty>0 || expected_args ty<>[]

let order ty: int =
  let rec aux ty = match view ty with
    | Forall ty -> aux ty
    | Fun (l, ret) ->
      List.fold_left (fun o arg -> max o (1 + aux arg)) (aux ret) l
    | App (_, l) -> List.fold_left (fun o arg -> max o (aux arg)) 0 l
    | Var _ | DB _ | Builtin _ -> 0
  in
  max 1 (aux ty)  (* never less than 1 *)

let is_ground = T.is_ground

let size = T.size

let rec depth ty = match view ty with
  | Builtin _
  | Var _
  | DB _ -> 1
  | App (_, l) -> 1 + depth_l l
  | Forall ty' -> 1 + depth ty'
  | Fun (l,ret) -> 1 + max (depth ret) (depth_l l)
and depth_l l = List.fold_left (fun d t -> max d (depth t)) 0 l

let open_fun = T.open_fun
let open_poly_fun = T.open_poly_fun

let returns ty =
  let _, _, ret = open_poly_fun ty in
  ret

let returns_prop ty = is_prop (returns ty)
let returns_tType ty = is_tType (returns ty)

exception ApplyError of string

let () = Printexc.register_printer
    (function
      | ApplyError msg -> Some (Util.err_spf "@[<2>Type.ApplyError:@ %s@]" msg)
      | _ -> None)

let err_apply_ msg = raise (ApplyError msg)
let err_applyf_ msg = CCFormat.ksprintf msg ~f:err_apply_

(* apply a type to arguments. *)
let apply ty0 args0 =
  let rec aux ty args env = match T.view ty, args with
    | _, [] -> T.DB.eval env ty
    | T.AppBuiltin(Builtin.Arrow, (ret :: exp_args)), _::_ ->
      (* match expected types with actual types *)
      aux_l ret exp_args args env
    | T.Bind (Binder.ForallTy, _, ty'), arg :: args' ->
      let arg = T.DB.eval env arg in
      aux ty' args' (DBEnv.push env arg)
    | T.DB _, _ ->
      let ty = T.DB.eval env ty in
      aux ty args env
    | _ ->
      err_applyf_
        "@[<2>Type.apply:@ expected quantified or function type,@ but got @[%a@]"
        T.pp_zf ty
  and aux_l ty_ret exp_args args env = match exp_args, args with
    | [], [] -> T.DB.eval env ty_ret
    | _, [] ->
      T.DB.eval env (arrow exp_args ty_ret)
    | [], _ ->
      begin match T.view (T.DB.eval env ty_ret) with
        | T.AppBuiltin (Builtin.Arrow, (ty_ret'::exp_args')) ->
          (* [ty_ret = exp_args' -> ty_ret'], continue applying *)
          aux_l ty_ret' exp_args' args env
        | _ ->
          err_applyf_ "@[<2>Type.apply:@ unexpected arguments [@[%a@]]@]"
            (Util.pp_list T.pp_zf) args
      end
    | exp :: exp_args', a :: args' ->
      (* expected type: [exp];  [a]: actual value, whose type must match [exp] *)
      let exp' = T.DB.eval env exp in
      if T.equal exp' (T.ty_exn a)
      then aux_l ty_ret exp_args' args' env
      else
        err_applyf_
          "@[<2>Type.apply:@ wrong argument type,@ expected `@[_ : %a@]`@ \
           but got `@[%a : %a@]`@ when applying `%a` to@ [@[%a@]]@ in env [%a]@]"
          T.pp_zf exp' T.pp_zf a T.pp_zf (T.ty_exn a) T.pp_zf ty0 (Util.pp_list T.pp_zf) args0
          (DBEnv.pp T.pp_zf) env
  in
  aux ty0 args0 DBEnv.empty

let apply1 ty a = apply ty [a]

let apply_unsafe = apply

let is_unifiable = T.type_is_unifiable

type print_hook = int -> (CCFormat.t -> t-> unit) -> CCFormat.t -> t-> bool




module TPTP = struct
  let i = term
  let o = prop
  let int = int
  let rat = rat
  let real = const (ID.make "$real")

  type print_hook = int -> (CCFormat.t -> t -> unit) -> CCFormat.t -> t -> bool

  let rec pp_tstp_rec depth out t = match view t with
    | Builtin Prop -> CCFormat.string out "$o"
    | Builtin TType -> CCFormat.string out "$tType"
    | Builtin Term -> CCFormat.string out "$i"
    | Builtin Int -> CCFormat.string out "$int"
    | Builtin Rat -> CCFormat.string out "$rat"
    | Var v -> Format.fprintf out "X%d" (HVar.id v)
    | DB i -> Format.fprintf out "Tb%d" (depth-i-1)
    | App (p, []) -> ID.pp_tstp out p
    | App (p, args) ->
      Format.fprintf out "@[<2>%a(%a)@]" ID.pp_tstp p
        (Util.pp_list (pp_tstp_rec depth)) args
    | Fun (args, ret) ->
      Format.fprintf out "%a > %a" (pp_l depth) args (pp_tstp_rec depth) ret
    | Forall ty' ->
      Format.fprintf out "!>[Tb%d:$tType]: %a" depth (pp_inner (depth+1)) ty'
  and pp_inner depth out t = match view t with
    | Fun _ -> Format.fprintf out "(@[%a@])" (pp_tstp_rec depth) t
    | _ -> pp_tstp_rec depth out t
  and pp_l depth out l = match l with
    | [] -> assert false
    | [ty] -> pp_tstp_rec depth out ty
    | _ ->
      Format.fprintf out "(@[%a@])"
        (Util.pp_list ~sep:" * " (pp_tstp_rec depth)) l

  let pp out t = pp_tstp_rec 0 out t

  let pp_depth ?hooks:_ depth out t = pp_tstp_rec depth out t

  let pp_typed_var out v = match view (HVar.ty v) with
    | Builtin Term -> HVar.pp out v (* implicit *)
    | _ -> Format.fprintf out "@[%a : %a@]" HVar.pp_tstp v pp (HVar.ty v)

  let to_string = CCFormat.to_string pp
end

let pp_typed_var_gen ~pp_ty out v = match view (HVar.ty v) with
  | Builtin TType -> Format.fprintf out "A%d" (HVar.id v)
  | Builtin Term -> HVar.pp out v
  | Builtin Int -> Format.fprintf out "I%d" (HVar.id v)
  | Builtin Rat -> Format.fprintf out "Q%d" (HVar.id v)
  | Builtin Prop -> Format.fprintf out "P%d" (HVar.id v)
  | Forall _ | Fun _ -> Format.fprintf out "(@[F%d:%a@])" (HVar.id v) pp_ty (HVar.ty v)
  | _ -> Format.fprintf out "(@[%a:%a@])" HVar.pp v pp_ty (HVar.ty v)

module ZF = struct
  let pp = T.pp_zf
  let to_string = CCFormat.to_string pp

  let pp_typed_var out v = pp_typed_var_gen ~pp_ty:pp out v
end

(** {2 IO} *)

let rec pp_rec depth out t = match view t with
  | Builtin b -> pp_builtin out b
  | Var v ->
    let ty = HVar.ty v in
    if is_tType ty
    then Format.fprintf out "A%d" (HVar.id v)
    else HVar.pp out v
  | DB i -> Format.fprintf out "T%i" (depth-i-1)
  | App (p, []) -> ID.pp out p
  | App (p, args) ->
    Format.fprintf out "@[<2>%a %a@]"
      ID.pp p (Util.pp_list ~sep:" " (pp_inner_app depth)) args
  | Fun (args, ret) ->
    Format.fprintf out "@[%a →@ %a@]"
      (Util.pp_list ~sep:" → " (pp_inner_fun depth)) args (pp_rec depth) ret
  | Forall ty' ->
    Format.fprintf out "@[Π T%i.@ %a@]" depth (pp_inner_fun (depth+1)) ty'
and pp_inner_fun depth out t = match view t with
  | Fun _ -> Format.fprintf out "(@[%a@])" (pp_rec depth) t
  | _ -> pp_rec depth out t
and pp_inner_app depth out t = match view t with
  | Fun _ | App (_,_::_) -> Format.fprintf out "(@[%a@])" (pp_rec depth) t
  | _ -> pp_rec depth out t

let pp_depth ?hooks:_ depth out t = pp_rec depth out t

let pp out t = pp_rec 0 out t
let pp_surrounded out t = (pp_inner_app 0) out t

let to_string = CCFormat.to_string pp

let pp_in = function
  | Output_format.O_zf -> ZF.pp
  | Output_format.O_tptp -> TPTP.pp
  | Output_format.O_normal -> pp
  | Output_format.O_none -> CCFormat.silent

(* keep synchro with {!InnerTerm.pp_var} *)
let pp_typed_var out v = pp_typed_var_gen ~pp_ty:pp out v

let mangle (ty:t): string =
  let add_id buf id =
    let s =
      ID.name id
      |> CCString.filter (function '#' | '_' -> false | _ -> true)
    in
    Buffer.add_string buf s
  in
  let rec aux buf t = match view t with
    | Builtin TType -> Buffer.add_string buf "ty"
    | Builtin Int -> Buffer.add_string buf "int"
    | Builtin Rat -> Buffer.add_string buf "rat"
    | Builtin Prop -> Buffer.add_string buf "prop"
    | Builtin Term -> Buffer.add_string buf "i"
    | Var _ -> Buffer.add_string buf "_"
    | DB i -> Printf.bprintf buf "A%d" i
    | App (f,[]) -> add_id buf f
    | App (f,l) ->
      add_id buf f;
      List.iter (fun sub -> Buffer.add_char buf '_'; aux buf sub) l
    | Fun (args,ret) ->
      List.iter (fun sub -> aux buf sub; Buffer.add_string buf "_to_") args;
      aux buf ret;
    | Forall f -> Printf.bprintf buf "pi_%a" aux f
  in
  let buf = Buffer.create 32 in
  aux buf ty;
  Buffer.contents buf

let pp_mangle out ty = CCFormat.string out (mangle ty)

(** {2 Conversions} *)

module Conv = struct
  module PT = TypedSTerm

  (* context used to map free variables to free variables *)
  type ctx = {
    mutable vars: (PT.t, t HVar.t) Var.Subst.t;
    mutable n: int;  (* counter for free vars *)
    mutable hvars: PT.t Var.t VarMap.t;
  }

  let create () = { vars=Var.Subst.empty; n=0; hvars=VarMap.empty; }

  let copy t = {t with vars=t.vars; }

  let clear ctx =
    ctx.vars <- Var.Subst.empty;
    ctx.n <- 0;
    ()

  let fresh_ty_var ctx =
    let n = ctx.n in
    ctx.n <- n+1;
    HVar.make ~ty:tType n

  exception Error of TypedSTerm.t

  let () = Printexc.register_printer
      (function
        | Error t ->
          Some (Util.err_spf "@[<2>Type.Conv.Error on `@[%a@]`@]" PT.pp t)
        | _ -> None)

  let of_simple_term_exn ctx t =
    let rec aux depth v2db t = match PT.view t with
      | PT.Var v ->
        begin match Var.Subst.find v2db v with
          | Some i ->
            (* i was the level when [v] was bound, [depth] is the current
               level, therefore there are [depth-i] binders in between *)
            bvar (depth - i - 1)
          | None -> var (aux_var v)
        end
      | PT.AppBuiltin (Builtin.Wildcard, []) ->
        (* make a fresh variable, but do not remember it *)
        var (fresh_ty_var ctx)
      | PT.Const id -> const id
      | PT.AppBuiltin (Builtin.Arrow, ret::args) ->
        let ret = aux depth v2db ret in
        assert (not (is_fun ret || is_forall ret));
        let args = List.map (aux depth v2db) args in
        arrow args ret
      | PT.AppBuiltin (Builtin.Term,[]) -> term
      | PT.AppBuiltin (Builtin.Prop,[]) -> prop
      | PT.AppBuiltin (Builtin.TType,[]) -> tType
      | PT.AppBuiltin (Builtin.TyInt,[]) -> int
      | PT.AppBuiltin (Builtin.TyRat,[]) -> rat
      | PT.App (f, l) ->
        begin match PT.view f with
          | PT.Const hd ->
            let l = List.map (aux depth v2db) l in
            app hd l
          | _ -> raise (Error t)
        end
      | PT.Bind (Binder.ForallTy, v, t') ->
        let v2db = Var.Subst.add v2db v depth in
        let t' = aux (depth+1) v2db t' in
        forall t'
      | PT.Record _ -> failwith "cannot convert record-type into type"
      | PT.Meta (_,{contents=Some ty'},_) -> aux depth v2db ty'
      | PT.AppBuiltin (Builtin.TyReal,[]) -> failwith "cannot handle type `real`"
      | PT.Bind _
      | PT.AppBuiltin _
      | PT.Meta _
      | PT.Ite _
      | PT.Match _
      | PT.Let _
      | PT.Multiset _ -> raise (Error t)
    and aux_var v = match Var.Subst.find ctx.vars v with
      | Some v -> v
      | None ->
        (* free variable *)
        let v' = fresh_ty_var ctx in
        ctx.vars <- Var.Subst.add ctx.vars v v';
        v'
    in
    aux 0 Var.Subst.empty t

  let var_of_simple_term ctx v = match Var.Subst.find ctx.vars v with
    | Some v' -> v'
    | None ->
      let ty = of_simple_term_exn ctx (Var.ty v) in
      let v' = HVar.make ~ty ctx.n in
      ctx.n <- ctx.n + 1;
      ctx.vars <- Var.Subst.add ctx.vars v v';
      v'

  let of_simple_term ctx t =
    try Some (of_simple_term_exn ctx t)
    with Error _ -> None

  let rec to_simple_term ?(env=DBEnv.empty) ctx t =
    let rec aux env t = match view t with
      | Builtin Prop -> PT.builtin ~ty:PT.tType Builtin.Prop
      | Builtin TType -> PT.builtin ~ty:PT.tType Builtin.TType
      | Builtin Term -> PT.builtin ~ty:PT.tType Builtin.Term
      | Builtin Int -> PT.builtin ~ty:PT.tType Builtin.TyInt
      | Builtin Rat -> PT.builtin ~ty:PT.tType Builtin.TyRat
      | Var v ->
        let v = aux_var v in
        PT.var v
      | DB i -> PT.var (DBEnv.find_exn env i)
      | App (s,l) ->
        (* s : type -> type -> ... -> type *)
        let ty_s = PT.Ty.fun_ (List.map (fun _ -> PT.tType) l) PT.tType in
        PT.app ~ty:PT.tType (PT.const ~ty:ty_s s) (List.map (aux env) l)
      | Fun (args,ret) ->
        let args = List.map (aux env) args in
        let ret = aux env ret in
        PT.Ty.fun_ args ret
      | Forall t' ->
        let v = Var.of_string ~ty:PT.tType
            (CCFormat.sprintf "V%d" (DBEnv.size env)) in
        let t' = aux (DBEnv.push env v) t' in
        PT.bind ~ty:PT.tType Binder.forall_ty v t'
    and aux_var v =
      var_to_simple_var ~prefix:"A" ctx v
    in
    aux env t

  and var_to_simple_var ?(prefix="A") ctx v =
    try VarMap.find v ctx.hvars
    with Not_found ->
      let v' = Var.of_string ~ty:(to_simple_term ctx (HVar.ty v))
          (CCFormat.sprintf "%s%d" prefix (HVar.id v)) in
      ctx.hvars <- VarMap.add v v' ctx.hvars;
      v'
end

let rebuild_rec ?(env=[]) (t:t) : t =
  let rec aux env t = match T.ty t with
    | T.NoType -> assert (t == tType); t
    | T.HasType ty ->
      begin match view t with
        | Var v -> var (HVar.cast ~ty v)
        | DB i ->
          assert (if i >= 0 && i < List.length env then true
            else (Format.printf "%d not in %a@." i (CCFormat.Dump.list pp) env; false));
          assert (if equal ty (List.nth env i) then true
            else (Format.printf "%a:%a or %a@." pp t pp ty pp (List.nth env i); false));
          bvar i
        | App (f, l) -> app f (List.map (aux env) l)
        | Fun (args, ret) -> arrow (List.map (aux env) args) (aux env ret)
        | Builtin b -> builtin b
        | Forall body ->
          let body' = aux (tType :: env) body in
          forall body'
      end
  in
  aux env t

let unsafe_eval_db env t : t =
  if CCList.is_empty env then t
  else (
    let env = List.fold_right (fun ty env -> DBEnv.push env ty) env DBEnv.empty in
    of_term_unsafe (T.DB.eval env t)
  )
