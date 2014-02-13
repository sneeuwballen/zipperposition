
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
  | Var of int                  (** variable *)
  | BVar of int                 (** bound variable (De Bruijn index) *)
  | Lambda of Type.t * t        (** lambda abstraction over one variable. *)
  | Const of symbol             (** Typed constant *)
  | App of t * Type.t list * t list
    (** HO function application. Invariant: first term is not a {!App}. *)

type sourced_term =
  t * string * string           (** Term + file,name *)

let ty t = match T.ty t with
  | T.NoType -> assert false
  | T.HasType ty -> Type.of_term_exn ty

(* split list between types, terms *)
let rec _split_types l = match l with
  | [] -> [], []
  | x::l' when Type.is_type x ->
      let l1, l2 = _split_types l' in
      (Type.of_term_exn x)::l1, l2
  | _ -> [], l

let view t = match T.view t with
  | T.Var i -> Var i
  | T.BVar i -> BVar i
  | T.Bind (Symbol.Conn Symbol.Lambda, varty, t') ->
    Lambda (Type.of_term_exn varty, t')
  | T.Const s -> Const s
  | T.App (hd, l) ->
      let tyargs, args = _split_types l in
      App (hd, tyargs, args)
  | _ -> assert false

let kind = T.Kind.HOTerm

let is_term t = match T.kind t with T.Kind.HOTerm -> true | _ -> false
let of_term t =
  if is_term t then Some t else None
let of_term_exn t =
  if not (is_term t) then raise (Invalid_argument "of_term_exn");
  t

(** {2 Comparison, equality, containers} *)

let subterm ~sub t =
  let rec check t =
    T.eq sub t ||
    match T.view t with
    | T.Var _ | T.BVar _ | T.App (_, []) -> false
    | T.App (_, args) -> List.exists check args
    | _ -> false
  in
  check t

let eq = T.eq
let cmp = T.cmp
let hash = T.hash

module TermHASH = struct
  type t = term
  let equal = eq
  let hash = hash
end

module Tbl = struct
  include Hashtbl.Make(TermHASH)

  let add_list tbl l =
    List.iter (fun x -> replace tbl x ()) l

  let add_seq tbl seq =
    Sequence.iter (fun x -> replace tbl x ()) seq

  let to_list set = fold (fun x _ acc -> x :: acc) set []

  let from_list l =
    let tbl = create 13 in
    add_list tbl l;
    tbl

  let to_seq set = fun k -> iter (fun x () -> k x) set

  let from_seq seq =
    let tbl = create 13 in
    add_seq tbl seq;
    tbl
end

(* FIXME: does   module Set = T.Set   work here? *)
module Set = Sequence.Set.Make(struct
  type t = term
  let compare = compare
end)

module Map = Sequence.Map.Make(struct
  type t = term
  let compare = compare
end)

module Cache = Cache.Replacing(TermHASH)

(** {2 Typing} *)

let cast ~ty t = T.cast ~ty:(ty : Type.t :> T.t) t

let lambda_var_ty t = match T.view t with
  | T.Bind (Symbol.Conn Symbol.Lambda, varty, _) ->
      Type.of_term_exn varty
  | _ -> raise (Invalid_argument "lambda_var_ty: expected lambda term")

(** {2 Smart constructors} *)

(** In this section, term smart constructors are defined. They perform
    hashconsing, and precompute some properties (flags) *)

let var ~(ty:Type.t) i =
  T.var ~kind ~ty:(ty :> T.t) i

let bvar ~(ty:Type.t) i =
  T.bvar ~kind ~ty:(ty :> T.t) i

(* compute type of s(tyargs  *)
let _compute_ty ty_fun tyargs l =
  let ty' = Type.apply ty_fun tyargs in
  Type.apply ty' (List.map ty l)

let app ?(tyargs=[]) hd args =
  (* first; compute type *)
  let ty = _compute_ty (ty hd) tyargs args in
  (* apply constant to type args and args *)
  let res = T.app ~kind ~ty:(ty:>T.t) hd
    ((tyargs : Type.t list :> t list) @ args) in
  res

let const ?(tyargs=[]) ~(ty:Type.t) symbol =
  let c = T.const ~kind ~ty:(ty :> T.t) symbol in
  match tyargs with
  | [] -> c
  | _::_ -> app ~tyargs c []

let __mk_lambda ~varty t' =
  let ty = Type.mk_fun (ty t') [varty] in
  T.bind ~kind ~ty:(ty :> T.t) ~varty:(varty:Type.t:>T.t) Symbol.Base.lambda t'

let mk_lambda vars t =
  List.fold_right
    (fun v t ->
      let t' = T.DB.replace (T.DB.shift 1 t) ~sub:v in
      __mk_lambda ~varty:(ty v) t')
    vars t

let is_var t = match T.view t with | T.Var _ -> true | _ -> false
let is_bvar t = match T.view t with | T.BVar _ -> true | _ -> false
let is_const t = match T.view t with | T.Const _ -> true | _ -> false
let is_app t = match T.view t with | T.App _ -> true | _ -> false
let is_lambda t = match T.view t with | T.Bind _ -> true | _ -> false

(** {2 Sequences} *)

module Seq = struct
  let subterms = T.Seq.subterms
  let vars = T.Seq.vars
  let subterms_depth = T.Seq.subterms_depth
  let symbols t =
    T.Seq.symbols t |>
      Sequence.filter (fun s -> not (Symbol.eq s Symbol.Base.lambda))
  let max_var = T.Seq.max_var
  let min_var = T.Seq.min_var
  let ty_vars t =
    subterms t |> Sequence.flatMap (fun t -> Type.Seq.vars (ty t))
  let add_set set ts =
    Sequence.fold (fun set t -> Set.add t set) set ts
end

let var_occurs ~var t =
  Seq.vars t |> Sequence.exists (eq t)

(** {2 Subterms and positions} *)

module Pos = struct
  let at = T.Pos.at
  let replace = T.Pos.replace
end

let replace = T.replace

let rec size t = match T.view t with
  | T.Var _
  | T.BVar _ -> 1
  | T.Const _ -> 1
  | T.Bind (_, _, t') -> 1+ size t'
  | T.Record l -> List.fold_left (fun acc (_,t') -> acc+size t') 0 l
  | T.App (f, l) -> List.fold_left (fun s t' -> s + size t') (1+size f) l

let is_ground t = Seq.vars t |> Sequence.is_empty

let var_occurs ~var t = Seq.vars t |> Sequence.exists (eq var)

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

let rec head t = match T.view t with
  | T.Const s -> s
  | T.App (t, _) -> head t
  | T.BVar _
  | T.Bind _ -> raise (Invalid_argument "Term.head: lambda")
  | T.Record _ -> raise (Invalid_argument "Term.head: record")
  | T.Var _ -> raise (Invalid_argument "Term.head: variable")

module AC = struct
  let flatten f l =
    let rec flatten acc l = match l with
    | [] -> acc
    | x::l' -> flatten (deconstruct acc x) l'
    and deconstruct acc t = match T.view t with
    | T.App (hd, l) ->
      begin match T.view hd with
        | T.Const f' when Symbol.eq f f' -> flatten acc l
        | _ -> t::acc
      end
    | _ -> t::acc
    in flatten [] l
end

(** {2 High-level operations} *)

let symbols ?(init=Symbol.Set.empty) t =
  Seq.symbols t |> Symbol.Seq.add_set init 

let contains_symbol s t =
  Seq.symbols t |> Sequence.exists (Symbol.eq s)

(** {2 High level operations} *)

(*
(* Curry all subterms *)
let rec curry t =
  let ty = FOT.ty t in
  match FOT.view t with
  | FOT.Var i -> var ~ty i
  | FOT.BVar i -> bvar ~ty i
  | FOT.App (f, tyargs, l) ->
    let ty_hd = FOT.Cst.ty f in
    let sym_hd = FOT.Cst.sym f in
    let c = const ~ty:ty_hd sym_hd in
    app ~tyargs c (List.map curry l)

let rec uncurry t =
  let ty = ty t in
  match T.view t with
  | T.Var i -> FOT.var ~ty i
  | T.BVar i -> FOT.bvar ~ty i
  | T.Bind (Symbol.Conn Symbol.Lambda, t') -> failwith "cannot uncurry lambda"
  | T.Const s -> FOT.const (FOT.Cst.make ~ty s)
  | T.App (hd, l) ->
    let l = List.map uncurry l in
    FOT.app ~tyargs (FOT.Cst.make ~ty s) l
  | _ -> failwith "cannot uncurry higher-order application"

let rec is_fo t = match t.term with
  | Var _ -> true
  | BoundVar _ -> false
  | Lambda _ -> false
  | At ({term=Const _}, _, l) -> List.for_all is_fo l
  | At _ -> false (* (X|lambda t) @ _ is not first-order  *)
  | Const _ -> true
*)

(** {2 IO} *)

let print_all_types = ref false

type print_hook = int -> (Buffer.t -> t -> unit) -> Buffer.t -> t -> bool

let pp_depth ?(hooks=[]) depth buf t =
  let depth = ref depth in
  (* recursive printing *)
  let rec pp_rec buf t = match view t with
  | BVar i -> Printf.bprintf buf "Y%d" (!depth - i - 1)
  | Lambda (varty,t') ->
    Printf.bprintf buf "λ%a:%a. " pp_bvar () Type.pp varty;
    incr depth;
    pp_surrounded buf t';
    decr depth
  | Const s -> Symbol.pp buf s
  | Var i ->
      if not !print_all_types
      then Printf.bprintf buf "X%d:%a" i Type.pp (ty t)
      else Printf.bprintf buf "X%d" i
  | App (hd, tyargs, l) ->
    (* try to use some hook *)
    if List.exists (fun hook -> hook !depth pp_rec buf t) hooks
    then ()
    else (* default case for nodes *)
      begin match tyargs, l with
      | [], [] -> pp_rec buf hd
      | _ ->
        Printf.bprintf buf "%a " pp_surrounded hd;
        Util.pp_list ~sep:" " Type.TPTP.pp buf tyargs;
        begin match tyargs, l with
        | _::_, _::_ -> Buffer.add_string buf " "
        | _ -> ()
        end;
        Util.pp_list ~sep:" " pp_rec buf l
      end
  and pp_surrounded buf t = match view t with
  | App (_, _, _) | Lambda _ ->
    Buffer.add_char buf '('; pp_rec buf t; Buffer.add_char buf ')'
  | _ -> pp_rec buf t
  and pp_bvar buf () =  Printf.bprintf buf "Y%d" !depth in
  pp_rec buf t

let __hooks = ref []
let add_hook h = __hooks := h :: !__hooks

let pp buf t = pp_depth ~hooks:!__hooks 0 buf t

let pp_debug buf t = pp_depth 0 buf t

let to_string = Util.on_buffer pp

let fmt fmt t = Format.pp_print_string fmt (to_string t)

let rec debug fmt t = match view t with
  | Var i ->
    Format.fprintf fmt "X%d:%a" i Type.fmt (ty t)
  | BVar i -> Format.fprintf fmt "Y%d" i
  | Lambda (varty,t') ->
    Format.fprintf fmt "(lambda %a %a)" Type.fmt varty debug t'
  | Const s -> Symbol.fmt fmt s
  | App (t, tyargs, l) ->
    Format.fprintf fmt "(%a %a %a)" debug t
      (Sequence.pp_seq ~sep:" " Type.fmt) (Sequence.of_list tyargs)
      (Sequence.pp_seq ~sep:" " debug) (Sequence.of_list l)

(*
let bij =
  let open Bij in
  let (!!!) = Lazy.force in
  fix
    (fun bij ->
      let bij_lam = lazy (pair Type.bij !!!bij) in
      let bij_var = pair int_ Type.bij in
      let bij_cst = Symbol.bij in
      let bij_at = lazy (triple !!!bij (list_ Type.bij) (list_ !!!bij)) in
      switch
        ~inject:(fun t -> match t.term with
        | BoundVar i -> "bv", BranchTo (bij_var, (i,t.ty))
        | Var i -> "v", BranchTo (bij_var, (i, t.ty))
        | Const s -> "c", BranchTo (bij_cst, s)
        | Lambda t' -> "lam", BranchTo (!!!bij_lam, (lambda_var_ty t, t'))
        | At (t, tyargs, l) -> "at", BranchTo (!!!bij_at, (t, tyargs, l)))
        ~extract:(function
        | "bv" -> BranchFrom (bij_var, fun (i,ty) -> __mk_bound_var ~ty i)
        | "v" -> BranchFrom (bij_var, fun (i,ty) -> mk_var ~ty i)
        | "c" -> BranchFrom (bij_cst, fun s -> const s)
        | "lam" -> BranchFrom (!!!bij_lam, fun (varty,t') -> __mk_lambda ~varty t')
        | "at" -> BranchFrom (!!!bij_at, fun (t, tyargs, l) -> app ~tyargs t l)
        | _ -> raise (DecodingError "expected Term")))
*)

module TPTP = struct
  let true_ = const ~ty:Type.TPTP.o Symbol.Base.true_
  let false_ = const ~ty:Type.TPTP.o Symbol.Base.false_

  (** Easy constructors for formulas *)

  let not_ = const ~ty:Type.(TPTP.o <=. TPTP.o) Symbol.Base.not_
  let and_ = const ~ty:Type.(TPTP.o <== [TPTP.o; TPTP.o]) Symbol.Base.and_
  let or_ = const ~ty:Type.(TPTP.o <== [TPTP.o; TPTP.o]) Symbol.Base.or_
  let imply = const ~ty:Type.(TPTP.o <== [TPTP.o; TPTP.o]) Symbol.Base.imply
  let equiv = const ~ty:Type.(TPTP.o <== [TPTP.o; TPTP.o]) Symbol.Base.equiv
  let xor = const ~ty:Type.(TPTP.o <== [TPTP.o; TPTP.o]) Symbol.Base.xor

  let eq = const
    ~ty:Type.(forall [var 0] (TPTP.o <== [var 0; var 0]))
    Symbol.Base.eq
  let neq = const
    ~ty:Type.(forall [var 0] (TPTP.o <== [var 0; var 0]))
    Symbol.Base.neq
  let forall = const
    ~ty:Type.(forall [var 0] (TPTP.o <=. (TPTP.o <=. var 0)))
    Symbol.Base.forall
  let exists = const
    ~ty:Type.(forall [var 0] (TPTP.o <=. (TPTP.o <=. var 0)))
    Symbol.Base.exists

  let mk_forall vars f =
    List.fold_right
      (fun v f -> app forall [mk_lambda [v] f])
      vars f

  let mk_exists vars f =
    List.fold_right
      (fun v f -> app exists [mk_lambda [v] f])
      vars f

  let mk_not t = app not_ [t]
  let mk_and a b = app and_ [a; b]
  let mk_or a b = app or_ [a; b]
  let mk_imply a b = app imply [a; b]
  let mk_equiv a b = app equiv [a; b]
  let mk_xor a b = app xor [a; b]
  let mk_eq a b = app ~tyargs:[ty a] eq [a; b]   (* use type of left arg *)
  let mk_neq a b = app ~tyargs:[ty a] neq [a; b]

  let rec mk_and_list l = match l with
    | [] -> true_
    | [x] -> x
    | x::l' -> mk_and x (mk_and_list l')

  let rec mk_or_list l = match l with
    | [] -> false_
    | [x] -> x
    | x::l' -> mk_or x (mk_or_list l')

  let close_forall t =
    let vars = vars (Sequence.singleton t) |> Set.elements in
    mk_forall vars t

  let close_exists t =
    let vars = vars (Sequence.singleton t) |> Set.elements in
    mk_exists vars t

  let pp_depth ?(hooks=[]) depth buf t =
    let depth = ref depth in
    (* recursive printing *)
    let rec pp_rec buf t = match view t with
    | BVar i -> Printf.bprintf buf "Y%d" (!depth - i - 1)
    | Lambda (varty,t') ->
      Printf.bprintf buf "^[%a:%a]: " pp_bvar () Type.pp varty;
      incr depth;
      pp_surrounded buf t';
      decr depth
    | Const s -> Symbol.pp buf s
    | Var i ->
        if not !print_all_types
        then Printf.bprintf buf "X%d:%a" i Type.pp (ty t)
        else Printf.bprintf buf "X%d" i
    | App (hd, tyargs, l) ->
      begin match tyargs, l with
      | [], [] -> pp_rec buf hd
      | _ ->
        Printf.bprintf buf "%a " pp_surrounded hd;
        Util.pp_list ~sep:" @ " Type.TPTP.pp buf tyargs;
        begin match tyargs, l with
        | _::_, _::_ -> Buffer.add_string buf " @ "
        | _ -> ()
        end;
        Util.pp_list ~sep:" @ " pp_rec buf l
      end
    and pp_surrounded buf t = match view t with
    | App (_, _, _) | Lambda _ ->
      Buffer.add_char buf '('; pp_rec buf t; Buffer.add_char buf ')'
    | _ -> pp_rec buf t
    and pp_bvar buf () =  Printf.bprintf buf "Y%d" !depth in
    pp_rec buf t

  let pp buf t = pp_depth 0 buf t

  let to_string = Util.on_buffer pp

  let fmt fmt t = Format.pp_print_string fmt (to_string t)
end
