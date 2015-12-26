
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Types} *)

module T = InnerTerm

type t = T.t

type ty = t

type builtin = TType | Prop | Term | Rat | Int

type view =
  | Builtin of builtin
  | Var of t HVar.t
  | DB of int
  | App of ID.t * t list (** parametrized type *)
  | Fun of t list * t (** Function type (left to right, no left-nesting) *)
  | Record of (string*t) list * t HVar.t option (** Record type (+ variable) *)
  | Multiset of t
  | Forall of t (** explicit quantification using De Bruijn index *)

let view t = match T.view t with
  | T.Var v -> Var v
  | T.DB i -> DB i
  | T.Bind (Binder.ForallTy, varty, t') ->
      assert (T.equal varty T.tType);
      Forall t'
  | T.Const s -> App (s, [])
  | T.AppBuiltin (Builtin.Arrow, (ret :: l)) -> Fun (l, ret)
  | T.AppBuiltin (Builtin.Multiset, [t]) -> Multiset t
  | T.AppBuiltin (Builtin.Prop, []) -> Builtin Prop
  | T.AppBuiltin (Builtin.TType, []) -> Builtin TType
  | T.AppBuiltin (Builtin.Term, []) -> Builtin Term
  | T.AppBuiltin (Builtin.TyInt, []) -> Builtin Int
  | T.AppBuiltin (Builtin.TyRat, []) -> Builtin Rat
  | T.Record (l, rest) -> Record (l, rest)
  | _ -> assert false

let hash_fun = T.hash_fun
let hash = T.hash
let equal = T.equal
let compare = T.compare

let is_var ty = match view ty with | Var _ -> true | _ -> false
let is_bvar ty = match view ty with | DB _ -> true | _ -> false
let is_app ty = match view ty with App _ -> true | _ -> false
let is_fun ty = match view ty with | Fun _ -> true | _ -> false
let is_forall ty = match view ty with | Forall _ -> true | _ -> false
let is_record ty = match view ty with | Record _ -> true | _ -> false

let tType = T.tType
let prop = T.builtin ~ty:tType Builtin.Prop
let term = T.builtin ~ty:tType Builtin.Term
let int = T.builtin ~ty:tType Builtin.TyInt
let rat = T.builtin ~ty:tType Builtin.TyRat

let var v = T.var v

let var_of_int i = T.var (HVar.make ~ty:tType i)

let app s l = T.app ~ty:T.tType (T.const ~ty:T.tType s) l

let const s = T.const ~ty:T.tType s

let arrow_ l r = T.app_builtin ~ty:T.tType Builtin.arrow (r :: l)

let arrow l r = match view r with
  | Fun (l', ret) -> arrow_ (l @ l') ret
  | _ -> arrow_ l r

let record l ~rest = T.record ~ty:T.tType l ~rest

let record_flatten l ~rest = T.record_flatten ~ty:tType l ~rest

let bvar i =
  T.bvar ~ty:T.tType i

let forall ty =
  T.bind ~ty:T.tType ~varty:T.tType Binder.forall_ty ty

let multiset ty = T.app_builtin ~ty:T.tType Builtin.multiset [ty]

let (==>) = arrow
let (@@) = app

let of_term_unsafe t = t
let of_terms_unsafe l = l
let cast_var_unsafe v = v

(** {2 Containers} *)

module Set = T.Set
module Map = T.Map
module Tbl = T.Tbl

module Seq = struct
  let vars = T.Seq.vars
  let sub = T.Seq.subterms
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
  let rec traverse i j ty = match view ty with
    | Fun (_,ty') ->
        traverse i (j+1) ty'
    | Forall ty' ->
        traverse (i+1) j ty'
    | Multiset _
    | Record _
    | Var _ | Builtin _ -> NoArity
    | DB _
    | App _ -> Arity (i, j)
  in traverse 0 0 ty

let rec expected_args ty = match view ty with
  | Fun (args, ret) -> args @ expected_args ret
  | Forall ty' -> expected_args ty'
  | DB _ | Var _ | Builtin _ | Record _ | Multiset _ | App _ -> []

let is_ground = T.is_ground

let size = T.size

let rec depth ty = match view ty with
  | Builtin _
  | Var _
  | DB _ -> 1
  | App (_, l) -> 1 + depth_l l
  | Forall ty' -> 1 + depth ty'
  | Fun (l,ret) -> 1 + max (depth ret) (depth_l l)
  | Multiset t -> 1 + depth t
  | Record (r,rest) ->
      let d = CCOpt.maybe (fun _ -> 1) 0 rest in
      List.fold_left
        (fun d (_,ty) -> max d (depth ty))
        d r
and depth_l l = List.fold_left (fun d t -> max d (depth t)) 0 l

let rec open_fun ty = match view ty with
  | Fun (args, ret) ->
      let xs, ret' = open_fun ret in
      args @ xs, ret'
  | _ -> [], ty

exception ApplyError of string

let () = Printexc.register_printer
  (function
    | ApplyError msg -> Some (CCFormat.sprintf "@[<2>Type.ApplyError:@ %s@]" msg)
    | _ -> None)

let err_apply_ msg = raise (ApplyError msg)
let err_applyf_ msg = CCFormat.ksprintf msg ~f:err_apply_

(* apply a type to arguments. *)
let apply ty args =
  let rec aux ty args env = match T.view ty, args with
    | _, [] ->
        if DBEnv.is_empty env then ty else T.DB.eval env ty
    | T.AppBuiltin(Builtin.Arrow, (ret :: exp_args)), _::_ ->
        (* match expected types with actual types *)
        aux_l ret exp_args args env
    | T.Bind (Binder.ForallTy, _, ty'), arg :: args' ->
        aux ty' args' (DBEnv.push env arg)
    | _ ->
        err_applyf_
          "@[<2>Type.apply:@ expected quantified or function type,@ but got @[%a@]"
          T.pp ty
  and aux_l ty exp_args args env = match exp_args, args with
  | _, [] ->
      if DBEnv.is_empty env then ty else T.DB.eval env ty
  | [], _ ->
      err_applyf_ "@[<2>Type.apply:@ unexpected arguments @[%a@]@]"
        (CCFormat.list T.pp) args
  | exp :: exp_args', a :: args' ->
      (* expected type: [exp];  [a]: actual value, whose type must match [exp] *)
      if T.equal (T.DB.eval env exp) (T.ty_exn a)
      then aux_l ty exp_args' args' env
      else
        err_applyf_
          "@[<2>Type.apply:@ wrong argument type, expected @[%a@]@ but got @%a@]"
          T.pp exp T.pp (T.ty_exn a)
  in
  aux ty args DBEnv.empty

let apply1 ty a = apply ty [a]

let apply_unsafe = apply

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
    | App (p, []) -> ID.pp out p
    | App (p, args) ->
        Format.fprintf out "@[<2>%a(%a)@]" ID.pp p
          (Util.pp_list (pp_tstp_rec depth)) args
    | Fun (args, ret) ->
        Format.fprintf out "%a > %a" (pp_l depth) args (pp_tstp_rec depth) ret
    | Record _ -> failwith "cannot print record types in TPTP"
    | Multiset _ -> failwith "cannot print multiset types in TPTP"
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

  let to_string = CCFormat.to_string pp
end

(** {2 IO} *)

let rec pp_rec depth out t = match view t with
  | Builtin Prop -> CCFormat.string out "prop"
  | Builtin TType -> CCFormat.string out "type"
  | Builtin Term -> CCFormat.string out "ι"
  | Builtin Int -> CCFormat.string out "int"
  | Builtin Rat -> CCFormat.string out "rat"
  | Var v -> HVar.pp out v
  | DB i -> Format.fprintf out "T%i" (depth-i-1)
  | Multiset t ->
      Format.fprintf out "@[<2>multiset@ %a@]" (pp_rec depth) t
  | App (p, []) -> ID.pp out p
  | App (p, args) ->
      Format.fprintf out "@[<2>%a %a@]"
        ID.pp p (Util.pp_list ~sep:" " (pp_rec depth)) args
  | Fun (args, ret) ->
      Format.fprintf out "@[%a →@ %a@]" (pp_l depth) args (pp_rec depth) ret
  | Record (l, None) ->
      Format.fprintf out "{@[<hv>%a@]}"
        (Util.pp_list (fun out (n, t) ->
             Format.fprintf out "@[%s: %a@]" n (pp_rec depth) t)) l
  | Record (l, Some v) ->
      Format.fprintf out "{@[<hv>%a@ | %a@]}"
        (Util.pp_list
           (fun buf (n, t) -> Format.fprintf buf "@[%s: %a@]" n (pp_rec depth) t))
        l HVar.pp v
  | Forall ty' ->
      Format.fprintf out "@[Π T%i.@ %a@]" depth (pp_inner (depth+1)) ty'
and pp_inner depth out t = match view t with
  | Fun _ -> Format.fprintf out "(@[%a@])" (pp_rec depth) t
  | _ -> pp_rec depth out t
and pp_l depth out l = match l with
  | [] -> assert false
  | [ty] -> pp_rec depth out ty
  | ty :: l' ->
      Format.fprintf out "@[<2>%a →@ @[<hv>%a@]@]"
        (pp_rec depth) ty (pp_l depth) l'

let pp_depth ?hooks:_ depth out t = pp_rec depth out t

let pp buf t = pp_rec 0 buf t
let pp_surrounded buf t = (pp_inner 0) buf t

let to_string = CCFormat.to_string pp

(** {2 Conversions} *)

module Conv = struct
  module PT = TypedSTerm

  (* context used to map free variables to free variables *)
  type ctx = {
    mutable vars: (PT.t, t HVar.t) Var.Subst.t;
    mutable n: int;  (* counter for free vars *)
  }

  let create () = { vars=Var.Subst.empty; n=0; }

  let copy t = {t with vars=t.vars; }

  let clear ctx =
    ctx.vars <- Var.Subst.empty;
    ctx.n <- 0;
    ()

  let fresh_ty_var ctx =
    let n = ctx.n in
    ctx.n <- n+1;
    HVar.make ~ty:tType n

  exception Error

  let of_simple_term_exn ctx t =
    let rec aux depth v2db t = match PT.view t with
      | PT.Var v ->
          begin match Var.Subst.find v2db v with
            | Some i ->
                (* i was the level when [v] was bound, [depth] is the current
                   level, therefore there are [depth-i] binders in between *)
                bvar (depth - i)
            | None -> var (aux_var v)
          end
      | PT.AppBuiltin (Builtin.Wildcard, []) ->
          (* make a fresh variable, but do not remember it *)
          var (fresh_ty_var ctx)
      | PT.Const id -> const id
      | PT.AppBuiltin (Builtin.Arrow, ret::args) ->
          let ret = aux depth v2db ret in
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
          | _ -> raise Error
          end
      | PT.Bind (Binder.ForallTy, v, t') ->
          let v2db = Var.Subst.add v2db v depth in
          let t' = aux (depth+1) v2db t' in
          forall t'
      | PT.Record (l, rest) ->
          let rest = CCOpt.map
            (fun t -> match PT.view t with
              | PT.Var v -> aux_var v
              | _ -> raise Error) rest
          in
          let l = List.map (fun (n,t) -> n, aux depth v2db t) l in
          record l ~rest
      | PT.Bind _
      | PT.AppBuiltin _
      | PT.Meta _
      | PT.Multiset _ -> raise Error
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
        ctx.vars <- Var.Subst.add ctx.vars v v';
        v'

  let of_simple_term ctx t =
    try Some (of_simple_term_exn ctx t)
    with Error -> None

  let to_simple_term ?(env=DBEnv.empty) t =
    let tbl = ref VarMap.empty in
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
      | Record (l, rest) ->
          let rest = CCOpt.map (fun v -> aux_var v) rest in
          PT.record ~ty:PT.tType (List.map (fun (n,ty) -> n, aux env ty) l) ~rest
      | Multiset t ->
          PT.app_builtin ~ty:PT.tType Builtin.multiset [aux env t]
      | Forall t' ->
          let v = Var.of_string ~ty:PT.tType
            (CCFormat.sprintf "V%d" (DBEnv.size env)) in
          let t' = aux (DBEnv.push env v) t' in
          PT.bind ~ty:PT.tType Binder.forall_ty v t'
    and aux_var v =
      try VarMap.find v !tbl
      with Not_found ->
        let v' = Var.of_string ~ty:PT.tType
          (CCFormat.sprintf "A%d" (HVar.id v)) in
        tbl := VarMap.add v v' !tbl;
        v'
    in
    aux env t
end
