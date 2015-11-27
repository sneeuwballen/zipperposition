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
SERVICES; LOSS OF USE, DATA, OR PROFICst; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 First-order terms} *)

module PB = Position.Build
module T = ScopedTerm

let prof_mk_node = Util.mk_profiler "Term.mk_node"
let prof_ac_normal_form = Util.mk_profiler "ac_normal_form"

type symbol = Symbol.t

(** {2 Term} *)

type t = T.t

type term = t

type view =
  | Builtin of Builtin.t
  | Var of int                (** Term variable *)
  | BVar of int               (** Bound variable (De Bruijn index) *)
  | Const of Symbol.t         (** Typed constant *)
  | TyApp of t * Type.t       (** Application to type *)
  | App of t * t list        (** Application to a list of terms (cannot be left-nested) *)

let is_ty_ t = match T.view t with
  | T.AppBuiltin (Builtin.LiftType, [_]) -> true
  | _ -> false

let as_ty_ t = match T.view t with
  | T.AppBuiltin (Builtin.LiftType, [ty]) -> Some (Type.of_term_unsafe ty)
  | _ -> None

(* split list between types, terms *)
let rec _split_types l = match l with
  | [] -> [], []
  | x::l' ->
      match as_ty_ x with
      | None -> [], l
      | Some ty ->
          let l1, l2 = _split_types l' in
          ty::l1, l2

let view t = match T.view t with
  | T.AppBuiltin (b,[]) -> Builtin b
  | T.Var i -> Var i
  | T.BVar i -> BVar i
  | T.App (_, []) -> assert false
  | T.At (l, r) ->
      begin match T.view r with
      | T.AppBuiltin (Builtin.LiftType, [ty]) ->
          (* type argument *)
          TyApp (l, Type.of_term_unsafe ty)
      | _ -> App (l, [r])
      end
  | T.App (hd, args) -> App (hd, args)
  | T.Const s -> Const s
  | _ -> assert false

let open_app t =
  let rec collect_head_tyargs acc t =
    match T.view t with
    | T.At (f, ty) -> collect_head_tyargs (Type.of_term_unsafe ty::acc) f
    | _ -> t, acc
  in
  match T.view t with
  | T.App (f, l) ->
      let f', tyargs = collect_head_tyargs [] f in
      f', tyargs, l
  | T.At _ ->
      let f', tyargs = collect_head_tyargs [] t in
      f', tyargs, []
  | _ -> t, [], []

module Classic = struct
  let rec _drop_types l = match l with
    | [] -> []
    | t::l' ->
        if is_ty_ t then _drop_types l' else l

  type view =
    | Var of int
    | BVar of int
    | App of symbol * Type.t list * t list  (** covers Const and App *)
    | NonFO   (* any other case *)

  let view t =
    let hd, tyargs, l = open_app t in
    match T.view hd, tyargs, l with
    | T.Var i, [], [] -> Var i
    | T.BVar i, [], [] -> BVar i
    | T.Const s, _, _ -> App (s, tyargs, l)
    | _ -> NonFO
end

(** {2 Comparison, equality, containers} *)

let subterm ~sub t =
  let rec check t =
    T.equal sub t ||
    match T.view t with
    | T.Var _ | T.BVar _ -> false
    | T.App (f, l) -> check f || List.exists check l
    | _ -> false
  in
  check t

let equal = T.equal
let hash_fun = T.hash_fun
let hash = T.hash
let compare = T.compare
let ty t = match T.ty t with
  | T.NoType -> assert false
  | T.HasType ty -> Type.of_term_unsafe ty

module TermHASH = struct
  type t = term
  let equal = equal
  let hash = hash
end

module Tbl = T.Tbl

module Set = T.Set
module Map = T.Map

module TCache = Cache.Replacing(TermHASH)
module T2Cache = Cache.Replacing2(TermHASH)(TermHASH)

(** {2 Typing} *)

let cast ~(ty:Type.t) t =
  match T.view t with
  | T.Var _ | T.BVar _ -> T.cast ~ty:(ty :> T.t) t
  | T.App _ -> raise (Invalid_argument "FOTerm.cast")
  | _ -> assert false

(** {2 Smart constructors} *)

(** In this section, term smart constructors are defined. They perform
    hashconsing, and precompute some properties (flags).

    TODO: flag_monomorphic *)

let var ~(ty:Type.t) i =
  assert (i >= 0);
  T.var ~ty:(ty :> T.t) i

let builtin ~(ty:Type.t) b = T.builtin ~ty:(ty :> T.t) b

let bvar ~(ty:Type.t) i =
  assert (i >= 0);
  T.bvar ~ty:(ty :> T.t) i

let const ~(ty:Type.t) s =
  T.const ~ty:(ty :> T.t) s

let tylift_ (t:Type.t) =
  T.app_builtin ~ty:T.tType Builtin.LiftType [(t:>T.t)]

let tyapp t tyarg =
  let ty = Type.apply (ty t) tyarg in
  T.at ~ty:(ty:>T.t) t (tylift_ tyarg)

let app f l =
  Util.enter_prof prof_mk_node;
  (* first; compute type *)
  let ty_result = List.fold_left
    (fun ty_fun t -> Type.apply ty_fun (ty t))
    (ty f) l
  in
  (* apply constant to type args and args *)
  let res = T.app ~ty:(ty_result:>T.t) f l in
  Util.exit_prof prof_mk_node;
  res

let app_full f tyargs l =
  app (List.fold_left tyapp f tyargs) l

let is_var t = match T.view t with
  | T.Var _ -> true
  | _ -> false

let is_bvar t = match T.view t with
  | T.BVar _ -> true
  | _ -> false

let is_const t = match T.view t with
  | T.Const _ -> true
  | _ -> false

let is_app t = match T.view t with
  | T.Const _
  | T.App _ -> true
  | _ -> false

let is_tyapp t = match T.view t with
  | T.At _ -> true
  | _ -> false

module Seq = struct
  let rec vars t k =
    if T.ground t then ()
    else match T.view t with
    | T.Var _ -> k t
    | T.BVar _ -> ()
    | T.App (f, l) ->
        vars f k;
        List.iter (fun t -> vars t k) l
    | T.At (l,r) -> vars l k; vars r k
    | _ -> ()
  and _vars_list l k = match l with
    | [] -> ()
    | t::l' -> vars t k; _vars_list l' k

  let rec subterms t k =
    k t;
    match T.view t with
    | T.AppBuiltin _
    | T.Const _
    | T.Var _
    | T.BVar _ -> ()
    | T.App (f, l) -> subterms f k; List.iter (fun t' -> subterms t' k) l
    | T.At (l,r) -> subterms l k; subterms r k
    | _ -> assert false

  let subterms_depth t k =
    let rec recurse depth t =
      k (t, depth);
      match T.view t with
      | T.App (_, ((_::_) as l)) ->
        let depth' = depth + 1 in
        List.iter (fun t' -> recurse depth' t') l
      | T.At (l, r) -> recurse depth l; recurse depth r
      | _ -> ()
    in
    recurse 0 t

  let rec symbols t k =
    match view t with
    | Const s -> k s
    | Builtin _
    | Var _
    | BVar _ -> ()
    | App (f, l) ->
        symbols f k;
        _symbols_list l k
    | TyApp (l, _) -> symbols l k
  and _symbols_list l k = match l with
    | [] -> ()
    | t::l' -> symbols t k; _symbols_list l' k

  let max_var seq =
    let r = ref 0 in
    seq (fun t -> match T.view t with
      | T.Var i -> r := max i !r
      | _ -> ());
    !r

  let min_var seq =
    let r = ref 0 in
    seq (fun t -> match T.view t with
      | T.Var i -> r := min i !r
      | _ -> ());
    !r

  let add_set set xs =
    Sequence.fold (fun set x -> Set.add x set) set xs

  let ty_vars t =
    subterms t |> Sequence.flatMap (fun t -> Type.Seq.vars (ty t))

  let typed_symbols t =
    subterms t
      |> Sequence.fmap
        (fun t -> match T.view t with
          | T.Const s -> Some (s, ty t)
          | _ -> None)
end

let var_occurs ~var t =
  Sequence.exists (equal var) (Seq.vars t)

let rec size t = match view t with
  | Builtin _
  | Var _
  | BVar _ -> 1
  | App (_, l) -> List.fold_left (fun s t' -> s + size t') 1 l
  | Const _ -> 1
  | TyApp (l,_) -> size l

let weight ?(var=1) ?(sym=fun _ -> 1) t =
  let rec weight t = match view t with
    | Var _
    | BVar _ -> var
    | App (_, l) -> List.fold_left (fun s t' -> s + weight t') 1 l
    | Const s -> sym s
    | TyApp (l,_) -> weight l
    | Builtin _ -> 1
  in weight t

let is_ground t = T.ground t

let monomorphic t = Sequence.is_empty (Seq.ty_vars t)

let max_var set = Set.to_seq set |> Seq.max_var

let min_var set = Set.to_seq set |> Seq.min_var

let add_vars tbl t = Seq.vars t (fun x -> Tbl.replace tbl x ())

let vars ts = Sequence.flatMap Seq.vars ts |> Seq.add_set Set.empty

let vars_prefix_order t =
  Seq.vars t
    |> Sequence.fold (fun l x -> if not (List.memq x l) then x::l else l) []
    |> List.rev


let depth t = Seq.subterms_depth t |> Sequence.map snd |> Sequence.fold max 0

let rec head_exn t = match T.view t with
  | T.Const s -> s
  | T.At (hd, _)
  | T.App (hd,_) -> head_exn hd
  | _ -> invalid_arg "FOTerm.head"

let head t =
  try Some (head_exn t)
  with Invalid_argument _-> None

let ty_vars t = Seq.ty_vars t |> Type.Seq.add_set Type.Set.empty

let of_term_unsafe t = t

(** {2 Subterms and positions} *)

module Pos = struct
  let at t pos = of_term_unsafe (T.Pos.at (t :> T.t) pos)

  let replace t pos ~by = of_term_unsafe (T.Pos.replace (t:>T.t) pos ~by:(by:>T.t))

  (** get subterm by its compact position *)
  let at_cpos t pos =
    let rec recurse t pos =
      match T.view t, pos with
      | _, 0 -> t
      | T.App (_, l), _ -> get_subpos l (pos - 1)
      | _ -> failwith "bad compact position"
    and get_subpos l pos =
      match l, pos with
      | t::l', _ ->
          let st = size t in
          if st > pos
            then recurse t pos  (* search inside the term *)
            else get_subpos l' (pos - st) (* continue to next term *)
      | [], _ -> assert false
    in recurse t pos

  let max_cpos t = size t - 1
end

let replace t ~old ~by =
  of_term_unsafe (T.replace (t:>T.t) ~old:(old:>T.t) ~by:(by:>T.t))

let symbols ?(init=Symbol.Set.empty) t =
  Symbol.Seq.add_set init (Seq.symbols t)

(** Does t contains the symbol f? *)
let contains_symbol f t =
  Sequence.exists (Symbol.equal f) (Seq.symbols t)

(** {2 Fold} *)

let rec _all_pos_rec f vars acc pb t =
  match view t with
  | Var _ | BVar _ ->
    if vars then f acc t (PB.to_pos pb) else acc
  | Builtin _
  | Const _ -> f acc t (PB.to_pos pb)
  | TyApp (_, _) -> f acc t (PB.to_pos pb)
  | App (_, tl) ->
    let acc = f acc t (PB.to_pos pb) in  (* apply to term itself *)
    _all_pos_rec_list f vars acc pb tl 0
and _all_pos_rec_list f vars acc pb l i = match l with
  | [] -> acc
  | t::l' when is_ty_ t ->
    _all_pos_rec_list f vars acc pb l' (i+1)
  | t::l' ->
    let acc = _all_pos_rec f vars acc (PB.arg i pb) t in
    _all_pos_rec_list f vars acc pb l' (i+1)

let all_positions ?(vars=false) ?(pos=Position.stop) t acc f =
  _all_pos_rec f vars acc (PB.of_pos pos) t

(** {2 Some AC-utils} *)

module type AC_SPEC = sig
  val is_ac : Symbol.t -> bool
  val is_comm : Symbol.t -> bool
end

module AC(A : AC_SPEC) = struct
  let flatten f l =
    let rec flatten acc l = match l with
    | [] -> acc
    | x::l' when is_ty_ x -> flatten acc l' (* ignore type args *)
    | x::l' -> flatten (deconstruct acc x) l'
    and deconstruct acc t = match T.view t with
    | T.App (f', l') when Symbol.equal (head_exn f') f ->
      flatten acc l'
    | _ -> t::acc
    in flatten [] l

  let normal_form t =
    Util.enter_prof prof_ac_normal_form;
    let rec normalize t = match T.view t with
      | _ when is_ty_ t -> t
      | T.Var _ -> t
      | T.BVar _ -> t
      | T.App (f, l) when A.is_ac (head_exn f) ->
        let l = flatten (head_exn f) l in
        let tyargs, l = _split_types l in
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
      | T.App (f, [a;b]) when A.is_comm (head_exn f) ->
        (* FIXME: doesn't handle polymorphic commutative operators *)
        let a = normalize a in
        let b = normalize b in
        if compare a b > 0
          then T.app ~ty:(ty t :>T.t) f [b; a]
          else t
      | T.App (f, l) ->
        let l = List.map normalize l in
        T.app ~ty:(T.ty_exn t) f l
      | _ -> assert false
    in
    let t' = normalize t in
    Util.exit_prof prof_ac_normal_form;
    t'

  let equal t1 t2 =
    let t1' = normal_form t1
    and t2' = normal_form t2 in
    equal t1' t2'

  let symbols seq =
    Sequence.flatMap Seq.symbols seq
      |> Sequence.filter A.is_ac
      |> Symbol.Seq.add_set Symbol.Set.empty
end

(** {2 Conversions} *)

let to_simple_term ?(depth=0) t =
  let module ST = STerm in
  let rec to_simple_term t =
    let ty = ty t in
    match view t with
    | Var i ->
        ST.column
          (ST.var (CCFormat.sprintf "X%d" i))
          (Type.Conv.to_simple_term ~depth ty)
    | BVar i -> ST.var (CCFormat.sprintf "Y%d" (depth-i-1))
    | TyApp _
    | App _ -> gather_left [] t
    | Builtin b -> ST.builtin b
    | Const f -> ST.const f
  and gather_left acc t = match view t with
    | TyApp (f, ty) -> gather_left (Type.Conv.to_simple_term ~depth ty :: acc) f
    | App (f, l) -> gather_left (List.map to_simple_term l @ acc) f
    | _ -> ST.app (to_simple_term t) acc
  in to_simple_term t

(** {2 Printing/parsing} *)

let print_all_types = ref false

type print_hook = int -> (CCFormat.t -> t -> unit) -> CCFormat.t -> t -> bool

(* lightweight printing *)
let pp_depth ?(hooks=[]) depth out t =
  let depth = ref depth in
  (* recursive printing *)
  let rec pp_rec out t =
    if not (List.exists (fun hook -> hook !depth pp_rec out t) hooks)
    then begin match view t with
    | Builtin b -> Builtin.pp out b
    | BVar i -> Format.fprintf out "Y%d" (!depth - i - 1)
    | TyApp (f, ty) ->
        pp_rec out f;
        CCFormat.char out ' ';
        Type.pp_depth !depth out ty
    | App (f, args) ->
        assert (args <> []);
        Format.fprintf out
          "@[<2>%a@ %a@]" pp_rec f
            (Util.pp_list ~sep:" " pp_inner) args
    | Const s -> Symbol.pp out s
    | Var i ->
      if not !print_all_types && not (Type.equal (ty t) Type.TPTP.i)
        then Format.fprintf out "@[X%d:%a@]" i (Type.pp_depth !depth) (ty t)
        else Format.fprintf out "X%d" i
    end;
    (* print type of term? *)
    if !print_all_types
      then Format.fprintf out ":%a" (Type.pp_depth !depth) (ty t)
  and pp_inner out t = match view t with
    | TyApp _
    | App _ -> CCFormat.char out '('; pp_rec out t; CCFormat.char out ')'
    | _ -> pp_rec out t
  in
  pp_rec out t

let __hooks = ref []
let add_hook h = __hooks := h :: !__hooks
let default_hooks () = !__hooks

let pp out t = pp_depth ~hooks:!__hooks 0 out t

let to_string = CCFormat.to_string pp

let rec debugf out t =
  begin match view t with
  | Builtin b -> Builtin.pp out b
  | Var i -> Format.fprintf out "X%d" i
  | BVar i -> Format.fprintf out "Y%d" i
  | Const s -> Symbol.pp out s
  | TyApp (f, ty) ->
    Format.fprintf out "(%a %a)" debugf f Type.pp ty
  | App (s, l) ->
    Format.fprintf out "(%a %a)" debugf s (Util.pp_list debugf) l
  end;
  Format.fprintf out ":%a" Type.pp (ty t)

(** {2 TPTP} *)

module TPTP = struct
  let true_ = builtin ~ty:Type.TPTP.o Builtin.true_
  let false_ = builtin ~ty:Type.TPTP.o Builtin.false_

  let pp_depth ?hooks:_ depth out t =
    let depth = ref depth in
    (* recursive printing *)
    let rec pp_rec out t = match view t with
    | BVar i ->
        Format.fprintf out "Y%d" (!depth - i - 1);
        (* print type of term *)
        if !print_all_types || not (Type.equal (ty t) Type.TPTP.i)
          then Format.fprintf out ":%a" (Type.TPTP.pp_depth !depth) (ty t)
    | Builtin b -> Builtin.TPTP.pp out b
    | Const s -> Symbol.pp out s
    | App _
    | TyApp _ ->
        let f, tyargs, args = open_app t in
        Format.fprintf out "@[<2>%a(@," pp_rec f;
        Util.pp_list ~sep:", "
          (Type.TPTP.pp_depth !depth) out tyargs;
        begin match tyargs, args with
          | _::_, _::_ -> CCFormat.string out ", "
          | _ -> ();
        end;
        Util.pp_list ~sep:"," pp_rec out args;
        CCFormat.fprintf out ")@]"
    | Var i ->
        Format.fprintf out "X%d" i;
        (* print type of term *)
        if !print_all_types || not (Type.equal (ty t) Type.TPTP.i)
          then Format.fprintf out ":%a" (Type.TPTP.pp_depth !depth) (ty t)
    in
    pp_rec out t

  let pp buf t = pp_depth 0 buf t
  let to_string = CCFormat.to_string pp

  module Arith = struct
    let term_pp_depth = pp_depth

    open Type.TPTP

    let x = Type.var 0

    let ty1 = Type.(forall [x] (int <=. x))

    let floor = const ~ty:ty1 Symbol.TPTP.Arith.floor
    let ceiling = const ~ty:ty1 Symbol.TPTP.Arith.ceiling
    let truncate = const ~ty:ty1 Symbol.TPTP.Arith.truncate
    let round = const ~ty:ty1 Symbol.TPTP.Arith.round

    let prec = const ~ty:Type.(int <=. int) Symbol.TPTP.Arith.prec
    let succ = const ~ty:Type.(int <=. int) Symbol.TPTP.Arith.succ

    let ty2 = Type.(forall [x] (x <== [x;x]))
    let ty2i = Type.(int <== [int;int])

    let sum = const ~ty:ty2 Symbol.TPTP.Arith.sum
    let difference = const ~ty:ty2 Symbol.TPTP.Arith.difference
    let uminus = const ~ty:ty2 Symbol.TPTP.Arith.uminus
    let product = const ~ty:ty2 Symbol.TPTP.Arith.product
    let quotient = const ~ty:ty2 Symbol.TPTP.Arith.quotient

    let quotient_e = const ~ty:ty2i Symbol.TPTP.Arith.quotient_e
    let quotient_t = const ~ty:ty2i Symbol.TPTP.Arith.quotient_t
    let quotient_f = const ~ty:ty2i Symbol.TPTP.Arith.quotient_f
    let remainder_e = const ~ty:ty2i Symbol.TPTP.Arith.remainder_e
    let remainder_t = const ~ty:ty2i Symbol.TPTP.Arith.remainder_t
    let remainder_f = const ~ty:ty2i Symbol.TPTP.Arith.remainder_f

    let ty2o = Type.(forall [x] (o <== [x;x]))

    let less = const ~ty:ty2o Symbol.TPTP.Arith.less
    let lesseq = const ~ty:ty2o Symbol.TPTP.Arith.lesseq
    let greater = const ~ty:ty2o Symbol.TPTP.Arith.greater
    let greatereq = const ~ty:ty2o Symbol.TPTP.Arith.greatereq

    (* hook that prints arithmetic expressions *)
    let arith_hook _depth pp_rec out t =
      let module SA = Symbol.TPTP.Arith in
      let pp_surrounded buf t = match Classic.view t with
      | Classic.App (s, _, [_;_]) when
        Symbol.equal s SA.sum ||
        Symbol.equal s SA.product ||
        Symbol.equal s SA.difference ||
        Symbol.equal s SA.quotient ->
        CCFormat.char buf '(';
        pp_rec buf t;
        CCFormat.char buf ')'
      | _ -> pp_rec buf t
      in
      match Classic.view t with
      | Classic.Var i when Type.equal (ty t) Type.TPTP.int ->
        Format.fprintf out "I%d" i; true
      | Classic.Var i when Type.equal (ty t) Type.TPTP.rat ->
        Format.fprintf out "Q%d" i; true
      | Classic.App (s, _,[a; b]) when Symbol.equal s SA.less ->
        Format.fprintf out "%a < %a" pp_surrounded a pp_surrounded b; true
      | Classic.App (s, _,[a; b]) when Symbol.equal s SA.lesseq ->
        Format.fprintf out "%a ≤ %a" pp_surrounded a pp_surrounded b; true
      | Classic.App (s, _,[a; b]) when Symbol.equal s SA.greater ->
        Format.fprintf out "%a > %a" pp_surrounded a pp_surrounded b; true
      | Classic.App (s, _,[a; b]) when Symbol.equal s SA.greatereq ->
        Format.fprintf out "%a ≥ %a" pp_surrounded a pp_surrounded b; true
      | Classic.App (s, _,[a; b]) when Symbol.equal s SA.sum ->
        Format.fprintf out "%a + %a" pp_surrounded a pp_surrounded b; true
      | Classic.App (s, _,[a; b]) when Symbol.equal s SA.difference ->
        Format.fprintf out "%a - %a" pp_surrounded a pp_surrounded b; true
      | Classic.App (s, _,[a; b]) when Symbol.equal s SA.product ->
        Format.fprintf out "%a × %a" pp_surrounded a pp_surrounded b; true
      | Classic.App (s, _,[a; b]) when Symbol.equal s SA.quotient ->
        Format.fprintf out "%a / %a" pp_surrounded a pp_surrounded b; true
      | Classic.App (s, _,[a; b]) when Symbol.equal s SA.quotient_e ->
        Format.fprintf out "%a // %a" pp_surrounded a pp_surrounded b; true
      | Classic.App (s, _,[a]) when Symbol.equal s SA.uminus ->
        Format.fprintf out "-%a" pp_surrounded a; true;
      | Classic.App (s, _,[a;b]) when Symbol.equal s SA.remainder_e ->
        Format.fprintf out "%a mod %a" pp_surrounded a pp_surrounded b; true;
      | _ -> false  (* default *)

    let pp_debugf buf t =
      term_pp_depth ~hooks:(arith_hook:: !__hooks) 0 buf t
  end
end
