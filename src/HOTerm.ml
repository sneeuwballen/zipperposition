
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
  | BVar of int             (** bound variable (De Bruijn index) *)
  | Lambda of t                 (** lambda abstraction over one variable. *)
  | Const of symbol             (** Typed constant *)
  | App of t * Type.t list * t list
    (** HO function application. Invariant: first term is not a {!App}. *)

type sourced_term =
  t * string * string           (** Term + file,name *)

let ty t = Type.of_term_exn (T.ty t)

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
  | T.Bind (Symbol.Conn Symbol.Lambda, t') -> Lambda t'
  | T.Const s -> Const s
  | T.App (hd, l) ->
      let tyargs, args = _split_types l in
      App (hd, tyargs, args)
  | _ -> assert false

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

let cast ~ty t = T.cast ~ty:(ty :> T.t) t

let lambda_var_ty t = match T.view t with
  | T.Bind (Symbol.Conn Symbol.Lambda, _) ->
      let ty = Type.of_term_exn (T.ty t) in
      begin match Type.view ty with
      | Type.Fun (_, arg::_) -> arg
      | _ -> raise (Invalid_argument "lambda_var_ty: expected function type")
      end
  | _ -> raise (Invalid_argument "lambda_var_ty: expected lambda term")

(** {2 Smart constructors} *)

(** In this section, term smart constructors are defined. They perform
    hashconsing, and precompute some properties (flags) *)

let var ~(ty:Type.t) i =
  T.var ~kind:T.Kind.HOTerm ~ty:(ty :> T.t) i

let bvar ~(ty:Type.t) i =
  T.bvar ~kind:T.Kind.HOTerm ~ty:(ty :> T.t) i

(* compute type of s(tyargs  *)
let _compute_ty ty_fun tyargs l =
  let ty' = Type.apply ty_fun tyargs in
  Type.apply ty' (List.map ty l)

let app ?(tyargs=[]) hd args =
  (* first; compute type *)
  let ty = _compute_ty (ty hd) tyargs args in
  (* apply constant to type args and args *)
  let res = T.app ~kind:T.Kind.HOTerm ~ty:(ty:>T.t) hd ((tyargs :> t list) @ args) in
  res

let const ?(tyargs=[]) ~(ty:Type.t) symbol =
  let c = T.const ~kind:T.Kind.HOTerm ~ty:(ty :> T.t) symbol in
  match tyargs with
  | [] -> c
  | _::_ -> app ~tyargs c []

let __mk_lambda ~varty t' =
  let ty = Type.mk_fun (ty t') [varty] in
  T.bind ~kind:T.Kind.HOTerm ~ty:(ty :> T.t) Symbol.Base.lambda t'

let mk_lambda vars t =
  List.fold_right
    (fun v t ->
      let t' = T.DB.replace t ~sub:v in
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
  let ty_vars t = T.Seq.types t |> Sequence.flatMap vars
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
  | T.Bind (_, t') -> 1+ size t'
  | T.App (f, l) -> List.fold_left (fun s t' -> s + size t') (1+size f) l
  | _ -> assert false

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

let depth t = Seq.subterms_depth t |> Sequence.map snd |> Sequence.fold max 0

let rec head t = match T.view t with
  | T.Const s -> s
  | T.App (t, _) -> head t
  | T.BVar _
  | T.Bind _ -> raise (Invalid_argument "Term.head: lambda")
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
  | T.Const s -> FOT.const (FOT.Cst.s ~ty)
  | T.App (hd, l) ->
    let l = List.map uncurry l in
    FOT.mk_node ~tyargs s l
  | _ -> failwith "cannot uncurry higher-order application"

let rec is_fo t = match t.term with
  | Var _ -> true
  | BoundVar _ -> false
  | Lambda _ -> false
  | At ({term=Const _}, _, l) -> List.for_all is_fo l
  | At _ -> false (* (X|lambda t) @ _ is not first-order  *)
  | Const _ -> true

(** {2 IO} *)

let print_all_types = ref false

let pp_tstp_depth depth buf t =
  let depth = ref depth in
  (* recursive printing *)
  let rec pp_rec buf t = match t.term with
  | BoundVar i -> Printf.bprintf buf "Y%d" (!depth - i - 1)
  | Lambda t' ->
    let varty = lambda_var_ty t in
    Printf.bprintf buf "^[%a:%a]: " pp_bvar () Type.pp varty;
    incr depth;
    pp_surrounded buf t';
    decr depth
  | Const s -> Symbol.pp buf s
  | Var i -> Printf.bprintf buf "X%d" i
  | At (t, tyargs, l) ->
    pp_surrounded buf t;  Buffer.add_string buf " @ ";
    Util.pp_list ~sep:" @ " Type.pp_tstp buf tyargs;
    (if l<>[] && tyargs<>[] then Buffer.add_string buf " @ ");
    Util.pp_list ~sep:" @ " pp_surrounded buf l
  and pp_surrounded buf t = match t.term with
  | At (_, _, _) ->
    Buffer.add_char buf '('; pp_rec buf t; Buffer.add_char buf ')'
  | _ -> pp_rec buf t
  and pp_bvar buf () =  Printf.bprintf buf "Y%d" !depth in
  pp_rec buf t

(* lightweight printing *)
let rec pp_depth depth buf t =
  let depth = ref depth in
  (* recursive printing *)
  let rec pp_rec buf t =
    begin match t.term with
    | BoundVar i -> Printf.bprintf buf "Y%d" (!depth - i - 1)
    | Lambda t' ->
      let varty = lambda_var_ty t in
      Printf.bprintf buf "λ%a:%a. " pp_bvar () Type.pp varty;
      incr depth;
      pp_surrounded buf t';
      decr depth
    | Const s -> Symbol.pp buf s
    | Var i ->
      if Type.eq t.ty Type.i
        then Printf.bprintf buf "X%d" i
        else Printf.bprintf buf "X%d:%a" i Type.pp t.ty
    | At (t, tyargs, l) ->
      pp_surrounded buf t;  Buffer.add_string buf " @ ";
      Util.pp_list ~sep:" @ " Type.pp buf tyargs;
      (if l<>[] && tyargs<>[] then Buffer.add_string buf " @ ");
      Util.pp_list ~sep:" @ " pp_surrounded buf l
    end;
    if !print_all_types then Printf.bprintf buf ":%a" Type.pp t.ty
  and pp_surrounded buf t = match t.term with
  | At _ ->
    Buffer.add_char buf '('; pp_rec buf t; Buffer.add_char buf ')'
  | _ -> pp_rec buf t
  and pp_bvar buf () = Printf.bprintf buf "Y%d" !depth in
  pp_rec buf t

let pp_debug buf t = pp_depth 0 buf t

let pp_tstp buf t = pp_tstp_depth 0 buf t

let __default_pp = ref pp_debug

let pp buf t = !__default_pp buf t

let set_default_pp pp = __default_pp := pp

let to_string t = Util.sprintf "%a" pp t

let fmt fmt t = Format.pp_print_string fmt (to_string t)

let rec debug fmt t = match view t with
  | Var i ->
    Format.fprintf fmt "X%d:%a" i Type.fmt (ty t)
  | BVar i -> Format.fprintf fmt "Y%d" i
  | Lambda t' -> 
    let varty = lambda_var_ty t in
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
        | "c" -> BranchFrom (bij_cst, fun s -> mk_const s)
        | "lam" -> BranchFrom (!!!bij_lam, fun (varty,t') -> __mk_lambda ~varty t')
        | "at" -> BranchFrom (!!!bij_at, fun (t, tyargs, l) -> mk_at ~tyargs t l)
        | _ -> raise (DecodingError "expected Term")))
*)

module TPTP = struct
  let true_ = const ~ty:Type.TPTP.o Symbol.Base.true_
  let false_ = const ~ty:Type.TPTP.o Symbol.Base.false_

  (** Easy constructors for formulas *)

  let not_ = const ~ty:Type.(TPTP.o <=. TPTP.o) Symbol.Base.not_
  let not_term = mk_const Symbol.not_symbol
  let and_term = mk_const Symbol.and_symbol
  let or_term = mk_const Symbol.or_symbol
  let imply_term = mk_const Symbol.imply_symbol
  let equiv_term = mk_const Symbol.equiv_symbol

  let eq_term = mk_const Symbol.eq_symbol
  let forall_term = mk_const Symbol.forall_symbol
  let exists_term = mk_const Symbol.exists_symbol

  let mk_not t = mk_at not_term [t]
  let mk_and a b = mk_at and_term [a; b]
  let mk_or a b = mk_at or_term [a; b]
  let mk_imply a b = mk_at imply_term [a; b]
  let mk_equiv a b = mk_at equiv_term [a; b]
  let mk_xor a b = mk_not (mk_equiv a b)
  let mk_eq a b = mk_at ~tyargs:[a.ty] eq_term [a; b]   (* use type of left arg *)
  let mk_neq a b = mk_not (mk_eq a b)

  let rec mk_and_list l = match l with
    | [] -> true_term
    | [x] -> x
    | x::l' -> mk_and x (mk_and_list l')

  let rec mk_or_list l = match l with
    | [] -> false_term
    | [x] -> x
    | x::l' -> mk_or x (mk_or_list l')
end
