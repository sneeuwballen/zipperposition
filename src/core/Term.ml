
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Lambda-free Terms} *)

module PB = Position.Build
module PW = Position.With
module T = InnerTerm

let prof_app = Util.mk_profiler "term.app"
let prof_ac_normal_form = Util.mk_profiler "term.AC_normal_form"

(** {2 Term} *)

type t = T.t

type term = t
type var = Type.t HVar.t

type view =
  | AppBuiltin of Builtin.t * t list
  | DB of int (** Bound variable (De Bruijn index) *)
  | Var of var (** Term variable *)
  | Const of ID.t (** Typed constant *)
  | App of t * t list (** Application to a list of terms (cannot be left-nested) *)

let view t = match T.view t with
  | T.AppBuiltin (b,l) -> AppBuiltin (b,l)
  | T.Var v -> Var (Type.cast_var_unsafe v)
  | T.DB i -> DB i
  | T.App (_, []) -> assert false
  | T.App (f, l) -> App (f, l)
  | T.Const s -> Const s
  | _ -> assert false

(** {2 Comparison, equality, containers} *)

let subterm ~sub t =
  let rec check t =
    T.equal sub t ||
    match T.view t with
      | T.Var _ | T.DB _ | T.Const _ -> false
      | T.App (f, l) -> check f || List.exists check l
      | T.AppBuiltin (_,l) -> List.exists check l
      | _ -> false
  in
  check t

let equal = T.equal
let hash = T.hash
let compare = T.compare
let ty t = match T.ty t with
  | T.NoType -> assert false
  | T.HasType ty -> Type.of_term_unsafe ty

let hash_mod_alpha = T.hash_mod_alpha
let same_l = T.same_l

(* split list between types, terms.
   [ty] is the type of the function, [l] the arguments *)
let rec split_args_ ~ty l = match Type.view ty, l with
  | Type.Forall ty', x :: l' ->
    let l1, l2 = split_args_ ~ty:ty' l' in
    x :: l1, l2
  | _ -> [], l

module Classic = struct
  type view =
    | Var of var
    | DB of int
    | App of ID.t * t list (** covers Const and App *)
    | AppBuiltin of Builtin.t * t list
    | NonFO (** any other case *)

  let view t : view = match T.view t with
    | T.Var v -> Var (Type.cast_var_unsafe v)
    | T.DB i -> DB i
    | _ when not (Type.is_unifiable @@ ty t) -> NonFO
    | T.Const s -> App (s,[])
    | T.AppBuiltin (b,l) -> AppBuiltin (b,l)
    | T.App (f, l) ->
      begin match T.view f with
        | T.Const id -> App (id, l)
        | _ -> NonFO
      end
    | _ -> assert false
end

(** {2 Containers} *)

module Tbl = T.Tbl
module Set = T.Set
module Map = T.Map

module VarSet = Type.VarSet
module VarMap = Type.VarMap
module VarTbl = Type.VarTbl

(** {2 Smart constructors} *)

(** In this section, term smart constructors are defined. They perform
    hashconsing, and precompute some properties (flags). *)

let var = (T.var :> var -> T.t)

let var_of_int ~ty i =
  let ty = (ty : Type.t :> T.t) in
  T.var (HVar.make ~ty i)

let builtin ~ty b = T.builtin ~ty:(ty : Type.t :> T.t) b

let app_builtin ~ty b l = T.app_builtin ~ty:(ty : Type.t :> T.t) b l

let bvar ~ty i =
  assert (i >= 0);
  T.bvar ~ty:(ty : Type.t :> T.t) i

let const ~ty s =
  T.const ~ty:(ty : Type.t :> T.t) s

let tyapp t args = match args with
  | [] -> t
  | _::_ ->
    let args' = (args : Type.t list :> T.t list) in
    let ty = (Type.apply (ty t) args : Type.t :> T.t) in
    T.app ~ty t args'

let app f l = match l with
  | [] -> f
  | _::_ ->
    Util.enter_prof prof_app;
    (* first; compute type *)
    let ty_result = Type.apply_unsafe (ty f) l in
    (* apply constant to type args and args *)
    let res = T.app ~ty:(ty_result : Type.t :> T.t) f l in
    Util.exit_prof prof_app;
    res

let app_full f tyargs l =
  let l = (tyargs : Type.t list :> T.t list) @ l in
  app f l

let true_ = builtin ~ty:Type.prop Builtin.True
let false_ = builtin ~ty:Type.prop Builtin.False

let grounding ty = builtin ~ty Builtin.Grounding

let is_var t = match T.view t with
  | T.Var _ -> true
  | _ -> false

let is_bvar t = match T.view t with
  | T.DB _ -> true
  | _ -> false

let is_const t = match T.view t with
  | T.Const _ -> true
  | _ -> false

let is_app t = match T.view t with
  | T.Const _
  | T.App _ -> true
  | _ -> false

let is_type t = Type.equal Type.tType (ty t)

let as_const_exn t = match T.view t with
  | T.Const c -> c
  | _ -> invalid_arg "as_const_exn"

let as_const t = try Some (as_const_exn t) with Invalid_argument _ -> None

let as_var_exn t = match T.view t with
  | T.Var v -> (Type.cast_var_unsafe v)
  | _ -> invalid_arg "as_var_exn"

let as_var t = try Some (as_var_exn t) with Invalid_argument _ -> None

let as_app t = match view t with
  | App (f,l) -> f, l
  | _ -> t, []

let head_term t = fst (as_app t)
let args t = snd (as_app t)

let is_ho_var t = match view t with
  | Var v -> Type.needs_args (HVar.ty v)
  | _ -> false

let as_ho_app t =
  let hd, args = as_app t in
  begin match as_var hd with
    | Some v when args<> [] -> Some (v, args)
    | _ -> None
  end

let is_ho_app t = CCOpt.is_some (as_ho_app t)

let is_ho_pred t = is_ho_app t && Type.is_prop (ty t)

let is_ho_at_root t = is_ho_var t || is_ho_app t

module Seq = struct
  let vars t k =
    let rec aux t =
      Type.Seq.vars (ty t) k;
      aux_term t;
    and aux_term t = match view t with
      | Var v -> k v
      | Const _
      | DB _ -> ()
      | App (f, l) ->
        aux f;
        List.iter aux l
      | AppBuiltin (_,l) -> List.iter aux l
    in
    aux t

  let subterms t k =
    let rec aux t =
      k t;
      match view t with
        | AppBuiltin _
        | Const _
        | Var _
        | DB _ -> ()
        | App (f, l) -> aux f; List.iter aux l
    in
    aux t

  let subterms_depth t k =
    let rec recurse depth t =
      k (t, depth);
      match view t with
        | Const _
        | DB _
        | Var _ -> ()
        | AppBuiltin (_, l) -> List.iter (recurse (depth+1)) l
        | App (_, l) ->
          let depth' = depth + 1 in
          List.iter (recurse depth') l
    in
    recurse 0 t

  let symbols t k =
    let rec aux t = match view t with
      | AppBuiltin (_,l) -> List.iter aux l
      | Const s -> k s
      | Var _
      | DB _ -> ()
      | App (f, l) -> aux f; List.iter aux l
    in
    aux t

  let max_var = Type.Seq.max_var
  let min_var = Type.Seq.min_var

  let add_set set xs =
    Sequence.fold (fun set x -> Set.add x set) set xs

  let ty_vars t =
    subterms t
    |> Sequence.flat_map (fun t -> Type.Seq.vars (ty t))

  let typed_symbols t =
    subterms t
    |> Sequence.filter_map
      (fun t -> match T.view t with
         | T.Const s -> Some (s, ty t)
         | _ -> None)
end

let var_occurs ~var t =
  Sequence.exists (HVar.equal Type.equal var) (Seq.vars t)

let rec size t = match view t with
  | Var _
  | DB _ -> 1
  | AppBuiltin (_,l)
  | App (_, l) -> List.fold_left (fun s t' -> s + size t') 1 l
  | Const _ -> 1

let weight ?(var=1) ?(sym=fun _ -> 1) t =
  let rec weight t = match view t with
    | Var _
    | DB _ -> var
    | AppBuiltin (_,l)
    | App (_, l) -> List.fold_left (fun s t' -> s + weight t') 1 l
    | Const s -> sym s
  in weight t

let is_ground t = T.is_ground t

let monomorphic t = Sequence.is_empty (Seq.ty_vars t)

let max_var set = VarSet.to_seq set |> Seq.max_var

let min_var set = VarSet.to_seq set |> Seq.min_var

let add_vars tbl t = Seq.vars t (fun v -> VarTbl.replace tbl v ())

let vars ts = Seq.vars ts |> VarSet.of_seq

let vars_prefix_order t =
  Seq.vars t
  |> Sequence.fold (fun l x -> if not (List.memq x l) then x::l else l) []
  |> List.rev

let depth t = Seq.subterms_depth t |> Sequence.map snd |> Sequence.fold max 0

let rec head_exn t = match T.view t with
  | T.Const s -> s
  | T.App (hd,_) -> head_exn hd
  | _ -> invalid_arg "Term.head"

let head t =
  try Some (head_exn t)
  with Invalid_argument _-> None

let ty_vars t = Seq.ty_vars t |> Type.VarSet.of_seq

let of_term_unsafe t = t
let of_term_unsafe_l l = l

let of_ty t = (t : Type.t :> T.t)

(** {2 Subterms and positions} *)

module Pos = struct
  let at t pos = of_term_unsafe (T.Pos.at (t :> T.t) pos)

  let replace t pos ~by =
    assert (Type.equal (at t pos |> ty) (ty by));
    of_term_unsafe (T.Pos.replace (t:>T.t) pos ~by:(by:>T.t))
end

let replace t ~old ~by =
  assert (Type.equal (ty by) (ty old));
  of_term_unsafe (T.replace (t:t:>T.t) ~old:(old:t:>T.t) ~by:(by:t:>T.t))

let replace_m t m =
  of_term_unsafe (T.replace_m (t:t:>T.t) (m:t Map.t:>T.t T.Map.t))

let symbols ?(init=ID.Set.empty) t =
  ID.Set.add_seq init (Seq.symbols t)

(** Does t contains the symbol f? *)
let contains_symbol f t =
  Sequence.exists (ID.equal f) (Seq.symbols t)

(** {2 Fold} *)

let all_positions ?(vars=false) ?(ty_args=true) ?(pos=Position.stop) t f =
  let rec aux pb t = match view t with
    | Var _ | DB _ ->
      if vars && (ty_args || not (Type.is_tType (ty t)))
      then f (PW.make t (PB.to_pos pb))
    | Const _ ->
      if ty_args || not (Type.is_tType (ty t))
      then f (PW.make t (PB.to_pos pb))
    | AppBuiltin (_,tl)
    | App (_, tl) ->
      if ty_args || not (Type.is_tType (ty t)) then (
        f (PW.make t (PB.to_pos pb));
      );
      List.iteri
        (fun i t' ->
           (* if [t'] is a type parameter and [not ty_args], ignore *)
           if ty_args || not (Type.is_tType (ty t'))
           then aux (PB.arg i pb) t')
        tl
  in
  aux (PB.of_pos pos) t

(** {2 Some AC-utils} *)

module type AC_SPEC = sig
  val is_ac : ID.t -> bool
  val is_comm : ID.t -> bool
end

module AC(A : AC_SPEC) = struct
  let flatten f l =
    let rec flatten acc l = match l with
      | [] -> acc
      | x::l' -> flatten (deconstruct acc x) l'
    and deconstruct acc t = match T.view t with
      | T.App (f', l') ->
        begin match head f' with
          | Some id when ID.equal id f ->
            let _, args = split_args_ ~ty:(ty f') l' in
            flatten acc args
          | Some _ | None -> t::acc
        end
      | _ -> t::acc
    in flatten [] l

  let normal_form t =
    Util.enter_prof prof_ac_normal_form;
    let rec normalize t = match T.view t with
      | T.Const _
      | T.Var _
      | T.DB _ -> t
      | T.App (f, l) when T.is_const f && A.is_ac (head_exn f) ->
        let l = flatten (head_exn f) l in
        let tyargs, l = split_args_ ~ty:(ty f) l in
        let l = List.map normalize l in
        let l = List.sort compare l in
        begin match l with
          | x::l' ->
            let ty = T.ty_exn t in
            let tyargs = (tyargs :> T.t list) in
            List.fold_left
              (fun subt x -> T.app ~ty f (tyargs@[x;subt]))
              x l'
          | [] -> assert false
        end
      | T.App (f, l) when T.is_const f && A.is_comm (head_exn f) ->
        let tyargs, l = split_args_ ~ty:(ty f) l in
        begin match l with
          | [a;b] ->
            let a' = normalize a in
            let b' = normalize b in
            if compare a' b' > 0
            then T.app ~ty:(ty t :>T.t) f (tyargs @ [b'; a'])
            else if T.equal a a' && T.equal b b' then t
            else T.app ~ty:(ty t :>T.t) f (tyargs @ [a'; b'])
          | _ -> t  (* partially applied *)
        end
      | T.App (f, l) ->
        let l = List.map normalize l in
        T.app ~ty:(T.ty_exn t) f l
      | T.AppBuiltin (b,l) ->
        let l = List.map normalize l in
        T.app_builtin ~ty:(T.ty_exn t) b l
      | _ -> assert false
    in
    let t' = normalize t in
    Util.exit_prof prof_ac_normal_form;
    t'

  let equal t1 t2 =
    let t1' = normal_form t1
    and t2' = normal_form t2 in
    equal t1' t2'

  let seq_symbols t =
    Seq.symbols t
    |> Sequence.filter A.is_ac

  let symbols seq =
    seq
    |> Sequence.flat_map seq_symbols
    |> ID.Set.add_seq ID.Set.empty
end

(** {2 Printing/parsing} *)

let print_all_types = ref false

type print_hook = int -> (CCFormat.t -> t -> unit) -> CCFormat.t -> t -> bool

(* lightweight printing *)
let pp_depth = T.pp_depth

let pp_var out (v:Type.t HVar.t) = T.pp_var out (v :> T.t HVar.t)

let add_hook = T.add_default_hook
let default_hooks = T.default_hooks

let pp out t = pp_depth 0 out t

let to_string = CCFormat.to_string pp

(** {2 Form} *)

module Form = struct
  let pp_hook _depth pp_rec out t =
    match Classic.view t with
      | Classic.AppBuiltin (Builtin.Not, [a]) ->
        Format.fprintf out "(@[<1>¬@ %a@])" pp_rec a; true
      | _ -> false  (* default *)

  let () = add_hook pp_hook

  let not_ t: t =
    assert (Type.is_prop (ty t));
    match view t with
      | AppBuiltin (Builtin.Not, [u]) -> u
      | _ -> app_builtin ~ty:Type.prop Builtin.not_ [t]

  let eq a b =
    assert (Type.equal (ty a)(ty b));
    app_builtin ~ty:Type.prop Builtin.eq [of_ty (ty a); a; b]

  let neq a b =
    assert (Type.equal (ty a)(ty b));
    app_builtin ~ty:Type.prop Builtin.neq [of_ty (ty a); a; b]

  let and_ a b =
    assert (Type.is_prop (ty a) && Type.is_prop (ty b));
    app_builtin ~ty:Type.prop Builtin.and_ [a; b]

  let or_ a b =
    assert (Type.is_prop (ty a) && Type.is_prop (ty b));
    app_builtin ~ty:Type.prop Builtin.or_ [a; b]

  let and_l = function
    | [] -> true_
    | [t] -> t
    | a :: tail -> List.fold_left and_ a tail
  let or_l = function
    | [] -> false_
    | [t] -> t
    | a :: tail -> List.fold_left or_ a tail
end

(** {2 Arith} *)

module Arith = struct
  let ty1 = Type.(forall ([int] ==> bvar 0))

  let floor = builtin ~ty:ty1 Builtin.Arith.floor
  let ceiling = builtin ~ty:ty1 Builtin.Arith.ceiling
  let truncate = builtin ~ty:ty1 Builtin.Arith.truncate
  let round = builtin ~ty:ty1 Builtin.Arith.round

  let prec = builtin ~ty:Type.([int] ==> int) Builtin.Arith.prec
  let succ = builtin ~ty:Type.([int] ==> int) Builtin.Arith.succ

  let ty2 = Type.(forall ([bvar 0; bvar 0] ==> bvar 0))
  let ty2i = Type.([int;int] ==> int)

  let sum = builtin ~ty:ty2 Builtin.Arith.sum
  let difference = builtin ~ty:ty2 Builtin.Arith.difference
  let uminus = builtin ~ty:ty2 Builtin.Arith.uminus
  let product = builtin ~ty:ty2 Builtin.Arith.product
  let quotient = builtin ~ty:ty2 Builtin.Arith.quotient

  let quotient_e = builtin ~ty:ty2i Builtin.Arith.quotient_e
  let quotient_t = builtin ~ty:ty2i Builtin.Arith.quotient_t
  let quotient_f = builtin ~ty:ty2i Builtin.Arith.quotient_f
  let remainder_e = builtin ~ty:ty2i Builtin.Arith.remainder_e
  let remainder_t = builtin ~ty:ty2i Builtin.Arith.remainder_t
  let remainder_f = builtin ~ty:ty2i Builtin.Arith.remainder_f

  let ty2o = Type.(forall ([bvar 0; bvar 0] ==> prop))

  let less = builtin ~ty:ty2o Builtin.Arith.less
  let lesseq = builtin ~ty:ty2o Builtin.Arith.lesseq
  let greater = builtin ~ty:ty2o Builtin.Arith.greater
  let greatereq = builtin ~ty:ty2o Builtin.Arith.greatereq

  (* hook that prints arithmetic expressions *)
  let pp_hook _depth pp_rec out t =
    let pp_surrounded buf t = match view t with
      | AppBuiltin (s, [_;_]) when Builtin.is_infix s ->
        Format.fprintf buf "(@[<hv>%a@])" pp_rec t
      | _ -> pp_rec buf t
    in
    match Classic.view t with
      | Classic.Var v when Type.equal (ty t) Type.int ->
        Format.fprintf out "I%d" (HVar.id v); true
      | Classic.Var v when Type.equal (ty t) Type.rat ->
        Format.fprintf out "Q%d" (HVar.id v); true
      | Classic.AppBuiltin (Builtin.Less, [_; a; b]) ->
        Format.fprintf out "%a < %a" pp_surrounded a pp_surrounded b; true
      | Classic.AppBuiltin (Builtin.Lesseq, [_;a; b]) ->
        Format.fprintf out "%a ≤ %a" pp_surrounded a pp_surrounded b; true
      | Classic.AppBuiltin (Builtin.Greater, [_;a; b]) ->
        Format.fprintf out "%a > %a" pp_surrounded a pp_surrounded b; true
      | Classic.AppBuiltin (Builtin.Greatereq, [_;a; b]) ->
        Format.fprintf out "%a ≥ %a" pp_surrounded a pp_surrounded b; true
      | Classic.AppBuiltin (Builtin.Sum, [_;a; b]) ->
        Format.fprintf out "%a + %a" pp_surrounded a pp_surrounded b; true
      | Classic.AppBuiltin (Builtin.Difference, [_;a; b]) ->
        Format.fprintf out "%a - %a" pp_surrounded a pp_surrounded b; true
      | Classic.AppBuiltin (Builtin.Product, [_;a; b]) ->
        Format.fprintf out "%a × %a" pp_surrounded a pp_surrounded b; true
      | Classic.AppBuiltin (Builtin.Quotient, [_;a; b]) ->
        Format.fprintf out "%a / %a" pp_surrounded a pp_surrounded b; true
      | Classic.AppBuiltin (Builtin.Quotient_e, [_;a; b]) ->
        Format.fprintf out "%a // %a" pp_surrounded a pp_surrounded b; true
      | Classic.AppBuiltin (Builtin.Uminus, [_;a]) ->
        Format.fprintf out "-%a" pp_surrounded a; true;
      | Classic.AppBuiltin (Builtin.Remainder_e, [_;a;b]) ->
        Format.fprintf out "%a mod %a" pp_surrounded a pp_surrounded b; true;
      | _ -> false  (* default *)

  let () = add_hook pp_hook
end

let debugf = pp

(** {2 TPTP} *)

module TPTP = struct
  let pp_depth ?hooks:_ depth out t =
    let depth = ref depth in
    (* recursive printing *)
    let rec pp_rec out t = match view t with
      | DB i ->
        Format.fprintf out "Y%d" (!depth - i - 1);
        (* print type of term *)
        if !print_all_types && not (Type.equal (ty t) Type.TPTP.i)
        then Format.fprintf out ":%a" (Type.TPTP.pp_depth !depth) (ty t)
      | AppBuiltin (b,[]) -> Builtin.TPTP.pp out b
      | AppBuiltin (b, ([t;u] | [_;t;u])) when Builtin.TPTP.is_infix b ->
        Format.fprintf out "(@[%a %a@ %a@])" pp_rec t Builtin.TPTP.pp b pp_rec u
      | AppBuiltin (b, l) when Builtin.TPTP.fixity b = Builtin.Infix_nary ->
        Format.fprintf out "(@[%a@])"
          (Util.pp_list ~sep:(Builtin.TPTP.to_string b) pp_rec) l
      | AppBuiltin (b,l) ->
        Format.fprintf out "(@[<hov2>%a@ %a@])" Builtin.TPTP.pp b (Util.pp_list pp_rec) l
      | Const s -> ID.pp_tstp out s
      | App (f, l) ->
        Format.fprintf out "@[<hov2>%a(@,%a)@]" pp_rec f
          (Util.pp_list ~sep:", " pp_rec) l
      | Var i ->
        Format.fprintf out "X%d" (HVar.id i);
        (* print type of term *)
        if !print_all_types && not (Type.equal (ty t) Type.TPTP.i)
        then Format.fprintf out ":%a" (Type.TPTP.pp_depth !depth) (ty t)
    in
    pp_rec out t

  let pp buf t = pp_depth 0 buf t
  let to_string = CCFormat.to_string pp
end

module ZF = struct
  let pp = T.pp_zf
  let to_string = CCFormat.to_string pp
end

(** {2 Conversions} *)

module Conv = struct
  module PT = TypedSTerm

  type ctx = Type.Conv.ctx
  let create = Type.Conv.create

  let var_to_simple_var = Type.Conv.var_to_simple_var

  let of_simple_term_exn ctx t =
    let rec aux t = match PT.view t with
      | PT.Var v -> var (Type.Conv.var_of_simple_term ctx v)
      | PT.AppBuiltin (Builtin.Wildcard, []) ->
        (* fresh type variable *)
        var (Type.Conv.fresh_ty_var ctx)
      | PT.Const id ->
        let ty = Type.Conv.of_simple_term_exn ctx (PT.ty_exn t) in
        const ~ty id
      | PT.Bind (Binder.ForallTy, _, _)
      | PT.AppBuiltin (Builtin.Arrow, _)
      | PT.AppBuiltin (Builtin.Term,[])
      | PT.AppBuiltin (Builtin.Prop,[])
      | PT.AppBuiltin (Builtin.TType,[])
      | PT.AppBuiltin (Builtin.TyInt,[])
      | PT.AppBuiltin (Builtin.TyRat,[]) ->
        let t = Type.Conv.of_simple_term_exn ctx t in
        of_ty t
      | PT.App (f, l) ->
        let f = aux f in
        let l = List.map aux l in
        app f l
      | PT.AppBuiltin (b, l) ->
        let ty = Type.Conv.of_simple_term_exn ctx (PT.ty_exn t) in
        let l = List.map aux l in
        app_builtin ~ty b l
      | PT.Bind _
      | PT.Meta _
      | PT.Record _
      | PT.Ite _
      | PT.Let _
      | PT.Match _
      | PT.Multiset _ -> raise (Type.Conv.Error t)
    in
    aux t

  let of_simple_term ctx t =
    try Some (of_simple_term_exn ctx t)
    with Type.Conv.Error _ -> None

  let to_simple_term ?(env=DBEnv.empty) ctx t =
    let module ST = TypedSTerm in
    let rec to_simple_term t =
      match view t with
        | Var i -> ST.var (aux_var i)
        | DB i -> ST.var (DBEnv.find_exn env i)
        | Const id -> ST.const ~ty:(aux_ty (ty t)) id
        | App (f,l) ->
          ST.app ~ty:(aux_ty (ty t))
            (to_simple_term f) (List.map to_simple_term l)
        | AppBuiltin (b,l) ->
          ST.app_builtin ~ty:(aux_ty (ty t))
            b (List.map to_simple_term l)
    and aux_var v =
      Type.Conv.var_to_simple_var ~prefix:"X" ctx v
    and aux_ty ty =
      Type.Conv.to_simple_term ~env ctx ty
    in
    to_simple_term t
end
