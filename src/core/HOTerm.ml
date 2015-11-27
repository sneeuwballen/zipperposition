
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

(** {1 Higher Order Terms} *)

module T = ScopedTerm
module FOT = FOTerm

(** {2 Type Definitions} *)

type symbol = Symbol.t

type t = ScopedTerm.t

type term = t

type view =
  | Builtin of Builtin.t
  | Var of int                  (** variable *)
  | BVar of int                 (** bound variable (De Bruijn index) *)
  | Lambda of Type.t * t   (** lambda abstraction over one variable. *)
  | Forall of Type.t * t   (** Forall quantifier (commutes with other forall) *)
  | Exists of Type.t * t   (** Exists quantifier (commutes with other exists) *)
  | Const of symbol             (** Typed constant *)
  | At of t * t                 (** Curried application *)
  | TyLift of Type.t       (** Lift a type to a term *)
  | Multiset of Type.t * t list
  | Record of (string*t) list * t option (** Record of terms *)

let ty t = match T.ty t with
  | T.NoType -> assert false
  | T.HasType ty -> Type.of_term_unsafe ty

let __get_ty = ty

let view t = match T.view t with
  | T.Var i -> Var i
  | T.BVar i -> BVar i
  | T.Bind (Binder.Lambda, varty, t') ->
    Lambda (Type.of_term_unsafe varty, t')
  | T.Bind (Binder.Forall, varty, t') ->
    Forall (Type.of_term_unsafe varty, t')
  | T.Bind (Binder.Exists, varty, t') ->
    Exists (Type.of_term_unsafe varty, t')
  | T.Const s -> Const s
  | T.At (l,r) -> At (l, r)
  | T.Multiset l ->
      begin match Type.view (ty t) with
        | Type.Multiset tau -> Multiset (tau, l)
        | _ -> assert false
      end
  | T.Record (l, rest) -> Record (l, rest)
  | T.AppBuiltin (Builtin.LiftType, [ty]) ->
    let ty = Type.of_term_unsafe ty in
    TyLift ty
  | T.AppBuiltin (b, []) -> Builtin b
  | T.RecordGet _
  | T.RecordSet _
  | T.RigidVar _
  | T.Bind _
  | T.App _
  | T.SimpleApp _
  | T.AppBuiltin _ -> assert false

let of_term_unsafe t = t

(** {2 Comparison, equality, containers} *)

let open_at t =
  let rec collect types args t =
    match T.view t with
    | T.At (f, a) ->
      begin match view a with
        | TyLift ty -> collect (ty::types) args f
        | _ -> collect types (a::args) f
      end
    | _ -> t, types, args
  in
  (* inline first call *)
  match T.view t with
  | T.At _ -> collect [] [] t
  | _ -> t, [], []

let subterm ~sub t =
  let rec check t =
    T.equal sub t ||
    match T.view t with
    | T.Var _ | T.BVar _ | T.App (_, []) -> false
    | T.App (_, args) -> List.exists check args
    | _ -> false
  in
  check t

let equal = T.equal
let compare = T.compare
let hash_fun = T.hash_fun
let hash = T.hash

module TermHASH = struct
  type t = term
  let equal = equal
  let hash = hash
end

module Set = T.Set
module Map = T.Map
module Tbl = T.Tbl

module Cache = Cache.Replacing(TermHASH)

(** {2 Typing} *)

let cast ~ty t = T.cast ~ty:(ty : Type.t :> T.t) t

let lambda_var_ty t = match T.view t with
  | T.Bind (Binder.Lambda, varty, _) ->
      Type.of_term_unsafe varty
  | _ -> invalid_arg "lambda_var_ty: expected lambda term"

(** {2 Smart constructors} *)

(** In this section, term smart constructors are defined. They perform
    hashconsing, and precompute some properties (flags) *)

let var ~(ty:Type.t) i =
  T.var ~ty:(ty :> T.t) i

let bvar ~(ty:Type.t) i =
  T.bvar ~ty:(ty :> T.t) i

let tylift (ty:Type.t) =
  T.app_builtin ~ty:(ty :> T.t) Builtin.lift_type [(ty:>T.t)]

let at l r =
  let ty_ret = Type.apply (ty l) (ty r) in
  T.at ~ty:(ty_ret :>T.t) l r

let tyat t tyarg = at t (tylift tyarg)

let rec tyat_list t l = match l with
  | [] -> t
  | ty::l' -> tyat_list (tyat t ty) l'

let rec at_list f l = match l with
  | [] -> f
  | t::l' -> at_list (at f t) l'

let at_full ?(tyargs=[]) f l =
  match tyargs with
  | [] -> at_list f l
  | _::_ -> at_list (tyat_list f tyargs) l

let const ~ty symbol =
  T.const ~ty:(ty : Type.t :> T.t) symbol

let builtin ~ty b =
  T.builtin ~ty:(ty : Type.t :> T.t) b

let multiset ~ty l =
  if List.exists (fun t -> not (Type.equal ty (__get_ty t))) l
    then raise (Type.Error "type mismatch when building a multiset");
  (* all elements are of type [ty], the result has type [multiset ty] *)
  let ty_res = Type.multiset ty in
  T.multiset ~ty:(ty_res:>T.t) l

let record l ~rest =
  (* build record type! *)
  let ty_l = List.map (fun (n,t) -> n, ty t) l in
  let ty_rest = match rest with
    | None -> None
    | Some r ->
      let ty_r = ty r in
      (* r must be a record type! *)
      begin match Type.view ty_r with
      | Type.Record _ -> Some ty_r
      | _ ->
        raise (Type.Error "the type of a row in a record must be a record type")
      end
  in
  let ty = Type.record ty_l ~rest:ty_rest in
  (* flattening done by ScopedTerm. *)
  T.record ~ty:(ty:>T.t) l ~rest

let __mk_lambda ~varty t' =
  let ty = Type.arrow varty (ty t') in
  T.bind ~ty:(ty :> T.t) ~varty:(varty:Type.t:>T.t) Binder.lambda t'

let __mk_forall ~varty t' =
  let ty = ty t' in
  T.bind ~ty:(ty :> T.t) ~varty:(varty:Type.t:>T.t) Binder.forall t'

let __mk_exists ~varty t' =
  let ty = ty t' in
  T.bind ~ty:(ty :> T.t) ~varty:(varty:Type.t:>T.t) Binder.exists t'

let mk_binder_l f vars t =
  List.fold_right
    (fun v t ->
      let t' = T.DB.replace (T.DB.shift 1 t) ~sub:v in
      f ~varty:(ty v) t')
    vars t

let lambda vars t = mk_binder_l __mk_lambda vars t
let forall vars t = mk_binder_l __mk_forall vars t
let exists vars t = mk_binder_l __mk_exists vars t

let is_var t = match T.view t with | T.Var _ -> true | _ -> false
let is_bvar t = match T.view t with | T.BVar _ -> true | _ -> false
let is_const t = match T.view t with | T.Const _ -> true | _ -> false
let is_at t = match view t with | At _ -> true | _ -> false
let is_tylift t = match view t with | TyLift _ -> true | _ -> false
let is_lambda t = match view t with | Lambda _ -> true | _ -> false
let is_forall t = match view t with | Forall _ -> true | _ -> false
let is_exists t = match view t with | Exists _ -> true | _ -> false
let is_multiset t = match T.view t with | T.Multiset _ -> true | _ -> false
let is_record t = match T.view t with | T.Record _ -> true | _ -> false

(** {2 Sequences} *)

module Seq = struct
  let subterms t = T.Seq.subterms t
  let vars t = T.Seq.vars t
  let subterms_depth = T.Seq.subterms_depth
  let symbols t = T.Seq.symbols t
  let max_var = T.Seq.max_var
  let min_var = T.Seq.min_var
  let ty_vars t =
    subterms t |> Sequence.flatMap (fun t -> Type.Seq.vars (ty t))
  let add_set set ts =
    Sequence.fold (fun set t -> Set.add t set) set ts
end

(** {2 Subterms and positions} *)

module Pos = struct
  let at = T.Pos.at
  let replace = T.Pos.replace
end

let replace = T.replace

let rec size t = match view t with
  | Builtin _
  | Var _
  | BVar _ -> 1
  | Const _ -> 1
  | Forall (_, t')
  | Exists (_, t')
  | Lambda (_, t') -> 1+ size t'
  | At (l,r) -> 1 + size l + size r
  | Record (l, rest) ->
      let s = match rest with None -> 0 | Some r -> size r in
      List.fold_left (fun acc (_,t') -> acc+size t') s l
  | TyLift _ -> 0
  | Multiset (_,l) -> 1 + List.fold_left (fun acc t->acc + size t) 0 l


let is_ground t = Seq.vars t |> Sequence.is_empty

let var_occurs ~var t = Seq.vars t |> Sequence.exists (equal var)

let monomorphic t = Seq.ty_vars t |> Sequence.is_empty

let max_var set = Set.to_seq set |> Seq.max_var

let min_var set = Set.to_seq set |> Seq.min_var

let add_vars tbl t = Seq.vars t (fun x -> Tbl.replace tbl x ())

let vars ts = Sequence.flatMap Seq.vars ts |> Seq.add_set Set.empty

let vars_prefix_order t =
  Seq.vars t
    |> Sequence.fold (fun l x -> if not (List.memq x l) then x::l else l) []
    |> List.rev

let ty_vars t =
  Seq.ty_vars t |> Type.Seq.add_set Type.Set.empty

let depth t = Seq.subterms_depth t |> Sequence.map snd |> Sequence.fold max 0

let rec head t = match view t with
  | Const s -> s
  | At (t,_) -> head t
  | TyLift _
  | Builtin _
  | BVar _
  | Var _
  | Forall _
  | Exists _
  | Lambda _
  | Multiset _
  | Record _ -> invalid_arg "Term.head"

(** {2 High-level operations} *)

let symbols ?(init=Symbol.Set.empty) t =
  Seq.symbols t |> Symbol.Seq.add_set init

let contains_symbol s t =
  Seq.symbols t |> Sequence.exists (Symbol.equal s)

(** {2 FO conversion} *)

(* Curry all subterms *)
let rec curry t =
  let ty = FOT.ty t in
  match FOT.view t with
  | FOT.Var i -> var ~ty i
  | FOT.BVar i -> bvar ~ty i
  | FOT.Const s -> const ~ty s
  | FOT.TyApp (f, ty) -> tyat (curry f) ty
  | FOT.App (f, l) -> at_list (curry f) (List.map curry l)
  | FOT.Builtin b -> builtin ~ty b

let uncurry t =
  let rec uncurry t =
    let ty = ty t in
    match view t with
    | Builtin b -> FOT.builtin ~ty b
    | Var i -> FOT.var ~ty i
    | BVar i -> FOT.bvar ~ty i
    | At _ ->
        let f, tyargs, l = open_at t in
        let f' = uncurry f in
        let l' = List.map uncurry l in
        FOT.app_full f' tyargs l'
    | Const s -> FOT.const ~ty s
    | TyLift _
    | Forall _
    | Exists _
    | Lambda _
    | Record _
    | Multiset _ -> raise Exit
  in try Some (uncurry t)
  with Exit -> None

let rec is_fo t = match view t with
  | Builtin _
  | Var _
  | BVar _ -> true
  | At _ ->
      let f, _, l = open_at t in
      is_fo f && List.for_all is_fo l
  | Const _ -> true
  | Record _
  | Multiset _
  | TyLift _
  | Forall _
  | Exists _
  | Lambda _ -> false

(** {2 Various operations} *)

let close_forall t =
  let vars = Seq.vars t |> T.Set.of_seq |> T.Set.elements in
  forall vars t

let close_exists t =
  let vars = Seq.vars t |> T.Set.of_seq |> T.Set.elements in
  exists vars t

let open_forall ?(offset=0) f =
  let offset = max offset (Seq.max_var (Seq.vars f)) + 1 in
  (* open next forall, replacing it with a fresh var *)
  let rec open_one offset env f = match view f with
  | Forall (varty,f') ->
    let v = var ~ty:varty offset in
    let env' = DBEnv.push env v in
    open_one (offset+1) env' f'
  | _ ->
    of_term_unsafe (T.DB.eval env f)  (* replace *)
  in
  open_one offset DBEnv.empty f

(** {2 IO} *)

let print_all_types = ref false

type print_hook = int -> (CCFormat.t -> t -> unit) -> CCFormat.t -> t -> bool

let binder_to_str t = match view t with
  | Lambda _ -> "λ"
  | Forall _ -> "∀"
  | Exists _ -> "∃"
  | _ -> assert false

let pp_depth ?hooks:_ depth out t =
  let depth = ref depth in
  (* recursive printing *)
  let rec pp_rec out t = match view t with
  | Builtin b -> Builtin.pp out b
  | BVar i -> Format.fprintf out "Y%d" (!depth - i - 1)
  | Lambda (varty,t') | Forall (varty,t') | Exists (varty,t') ->
    Format.fprintf out "@[<2>%s%a:@[%a@].@ "
      (binder_to_str t) pp_bvar () Type.pp_surrounded varty;
    incr depth;
    pp_surrounded out t';
    decr depth;
    Format.fprintf out "@]"
  | Const s -> Symbol.pp out s
  | Var i ->
      if not !print_all_types
      then Format.fprintf out "X%d:@[%a@]" i Type.pp_surrounded (ty t)
      else Format.fprintf out "X%d" i
  | At (l,r) ->
    Format.fprintf out "@[<2>@[%a@]@ @[%a@]@]" pp_rec l pp_surrounded r
  | TyLift ty -> Format.fprintf out "@@%a" Type.pp_surrounded ty
  | Record ([], None) ->
    CCFormat.string out "{}"
  | Record ([], Some r) ->
    Format.fprintf out "@[{ | %a}@]" pp_rec r
  | Record (l, None) ->
    Format.fprintf out "{@[<hv>%a@]}"
      (Util.pp_list (fun buf (n, t) -> Format.fprintf buf "@[%s=%a@]" n pp_rec t))
      l
  | Record (l, Some r) ->
    Format.fprintf out "{@[<hv>%a@ | %a@]}"
      (Util.pp_list ~sep:", " (fun out (n, t) -> Format.fprintf out "@[%s=%a@]" n pp_rec t))
      l pp_rec r
  | Multiset (_, l) ->
    Format.fprintf out "[@[<hv>%a@]]" (Util.pp_list pp_rec) l
  and pp_surrounded buf t = match view t with
  | Lambda _ | At _ ->
    CCFormat.char buf '('; pp_rec buf t;  CCFormat.char buf ')'
  | _ -> pp_rec buf t
  and pp_bvar buf () =  Format.fprintf buf "Y%d" !depth in
  pp_rec out t

let __hooks = ref []
let add_hook h = __hooks := h :: !__hooks

let pp buf t = pp_depth ~hooks:!__hooks 0 buf t

let to_string = CCFormat.to_string pp

let rec debugf out t = match view t with
  | Builtin b -> Builtin.pp out b
  | Var i ->
      Format.fprintf out "X%d:%a" i Type.pp (ty t)
  | BVar i -> Format.fprintf out "Y%d" i
  | Lambda (varty,t') ->
      Format.fprintf out "(@[<2>lambda@ %a@ %a@])" Type.pp varty debugf t'
  | Forall (varty,t') ->
      Format.fprintf out "(@[<2>forall@ %a@ %a@])" Type.pp varty debugf t'
  | Exists (varty,t') ->
      Format.fprintf out "(@[<2>exists@ %a@ %a@])" Type.pp varty debugf t'
  | Const s -> Symbol.pp out s
  | TyLift ty -> Type.pp out ty
  | At (l, r) ->
      Format.fprintf out "(@[<2>%a@ %a@])" debugf l debugf r
  | Multiset (_, l) ->
      Format.fprintf out "{| @[<hv>%a@] |}" (CCList.print debugf) l
  | Record (l, None) ->
      Format.fprintf out "{ @[<hv>%a@] }"
        (Util.pp_list (fun fmt (n,t) -> Format.fprintf fmt "%s: %a" n debugf t)) l
  | Record (l, Some r) ->
      Format.fprintf out "{ @[<hv>%a@ | %a@] }"
        (Util.pp_list (fun fmt (n,t) -> Format.fprintf fmt "%s: %a" n debugf t))
        l debugf r

module TPTP = struct
  let true_ = builtin ~ty:Type.TPTP.o Builtin.true_
  let false_ = builtin ~ty:Type.TPTP.o Builtin.false_

  (** Easy constructors for formulas *)

  let not_ = builtin ~ty:Type.(TPTP.o <=. TPTP.o) Builtin.not_
  let and_ = builtin ~ty:Type.(TPTP.o <== [TPTP.o; TPTP.o]) Builtin.and_
  let or_ = builtin ~ty:Type.(TPTP.o <== [TPTP.o; TPTP.o]) Builtin.or_
  let imply = builtin ~ty:Type.(TPTP.o <== [TPTP.o; TPTP.o]) Builtin.imply
  let equiv = builtin ~ty:Type.(TPTP.o <== [TPTP.o; TPTP.o]) Builtin.equiv
  let xor = builtin ~ty:Type.(TPTP.o <== [TPTP.o; TPTP.o]) Builtin.xor

  let eq = builtin
    ~ty:Type.(forall [var 0] (TPTP.o <== [var 0; var 0]))
    Builtin.eq
  let neq = builtin
    ~ty:Type.(forall [var 0] (TPTP.o <== [var 0; var 0]))
    Builtin.neq
  let forall = const
    ~ty:Type.(forall [var 0] (TPTP.o <=. (TPTP.o <=. var 0)))
    Symbol.TPTP.forall_fun
  let exists = const
    ~ty:Type.(forall [var 0] (TPTP.o <=. (TPTP.o <=. var 0)))
    Symbol.TPTP.exists_fun

  let mk_not t = at not_ t
  let mk_and a b = at_list and_ [a; b]
  let mk_or a b = at_list or_ [a; b]
  let mk_imply a b = at_list imply [a; b]
  let mk_equiv a b = at_list equiv [a; b]
  let mk_xor a b = at_list xor [a; b]
  let mk_eq a b = at_list (tyat eq (ty a)) [a; b]   (* use type of left arg *)
  let mk_neq a b = at_list (tyat neq (ty a)) [a; b]

  let rec mk_and_list l = match l with
    | [] -> true_
    | [x] -> x
    | x::l' -> mk_and x (mk_and_list l')

  let rec mk_or_list l = match l with
    | [] -> false_
    | [x] -> x
    | x::l' -> mk_or x (mk_or_list l')

  let tptp_binder_to_str t = match view t with
    | Lambda _ -> "^"
    | Forall _ -> "!"
    | Exists _ -> "?"
    | _ -> assert false

  let pp_depth ?hooks:_ depth out t =
    let depth = ref depth in
    (* recursive printing *)
    let rec pp_rec out t = match view t with
    | Builtin b -> Builtin.TPTP.pp out b
    | BVar i -> Format.fprintf out "Y%d" (!depth - i - 1)
    | Lambda (varty,t') | Forall (varty,t') | Exists (varty,t') ->
      Format.fprintf out "%s[%a:%a]: " (tptp_binder_to_str t)
        pp_bvar () Type.pp varty;
      incr depth;
      pp_surrounded out t';
      decr depth
    | Const s -> Symbol.pp out s
    | Var i ->
        if not !print_all_types && not (Type.equal (ty t) Type.TPTP.i)
        then Format.fprintf out "X%d" i
        else Format.fprintf out "X%d:%a" i Type.pp (ty t)
    | At (l,r) ->
      pp_surrounded out l; CCFormat.string out " @ ";
      pp_rec out r
    | TyLift ty -> Format.fprintf out "@%a" (Type.pp_depth !depth) ty
    | Multiset _ -> failwith "cannot print multiset in TPTP"
    | Record _ -> failwith "cannot print records in TPTP"
    and pp_surrounded buf t = match view t with
    | At _ | Lambda _ ->
      CCFormat.char buf '('; pp_rec buf t; CCFormat.char buf ')'
    | _ -> pp_rec buf t
    and pp_bvar buf () =  Format.fprintf buf "Y%d" !depth in
    pp_rec out t

  let pp buf t = pp_depth 0 buf t

  let to_string = CCFormat.to_string pp
end
