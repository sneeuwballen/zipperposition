
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
  | RigidVar of int             (** rigid variable, only targets other variables *)
  | BVar of int                 (** bound variable (De Bruijn index) *)
  | Lambda of Type.t * t        (** lambda abstraction over one variable. *)
  | Const of symbol             (** Typed constant *)
  | At of t * t                 (** Curried application *)
  | TyAt of t * Type.t          (** Curried application to a type *)
  | Multiset of t list
  | Record of (string*t) list * t option (** Record of terms *)

type sourced_term =
  t * string * string           (** Term + file,name *)

let ty t = match T.ty t with
  | T.NoType -> assert false
  | T.HasType ty -> Type.of_term_exn ty

let __get_ty = ty

(* split list between types, terms *)
let rec _split_types l = match l with
  | [] -> [], []
  | x::l' when Type.is_type x ->
      let l1, l2 = _split_types l' in
      (Type.of_term_exn x)::l1, l2
  | _ -> [], l

let view t = match T.view t with
  | T.Var i -> Var i
  | T.RigidVar i -> RigidVar i
  | T.BVar i -> BVar i
  | T.Bind (Symbol.Conn Symbol.Lambda, varty, t') ->
    Lambda (Type.of_term_exn varty, t')
  | T.Const s -> Const s
  | T.At (l,r) ->
      begin match Type.of_term r with
      | None -> At (l, r)
      | Some ty -> TyAt (l, ty)
      end
  | T.Multiset l -> Multiset l
  | T.Record (l, rest) -> Record (l, rest)
  | T.Bind _
  | T.App _
  | T.SimpleApp _ -> assert false

let kind = T.Kind.HOTerm

let is_term t = match T.kind t with T.Kind.HOTerm -> true | _ -> false
let of_term t =
  if is_term t then Some t else None
let of_term_exn t =
  if not (is_term t) then raise (Invalid_argument "of_term_exn");
  t

(** {2 Comparison, equality, containers} *)

let open_at t =
  let rec collect types args t =
    match T.view t with
    | T.At (f, a) ->
        begin match Type.of_term a with
        | None -> collect types (a::args) f
        | Some ty -> collect (ty::types) args f
        end
    | _ -> t, types, args
  in
  (* inline first call *)
  match T.view t with
  | T.At _ -> collect [] [] t
  | _ -> t, [], []

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

let rigid_var ~ty i =
  T.rigid_var ~kind ~ty:(ty : Type.t :> T.t) i

let bvar ~(ty:Type.t) i =
  T.bvar ~kind ~ty:(ty :> T.t) i

let tyat t tyarg =
  let ty = Type.apply (ty t) tyarg in
  T.at ~kind ~ty:(ty:>T.t) t (tyarg:Type.t:>T.t)

let rec tyat_list t l = match l with
  | [] -> t
  | ty::l' -> tyat_list (tyat t ty) l'

let at l r =
  let ty_ret = Type.apply (ty l) (ty r) in
  T.at ~kind ~ty:(ty_ret :>T.t) l r

let rec at_list f l = match l with
  | [] -> f
  | t::l' -> at_list (at f t) l'

let at_full ?(tyargs=[]) f l =
  match tyargs with
  | [] -> at_list f l
  | _::_ -> at_list (tyat_list f tyargs) l

let const ~ty symbol =
  T.const ~kind ~ty:(ty : Type.t :> T.t) symbol

let multiset ~ty l =
  if List.exists (fun t -> not (Type.eq ty (__get_ty t))) l
    then raise (Type.Error "type mismatch when building a multiset");
  (* all elements are of type [ty], the result has type [multiset ty] *)
  let ty_res = Type.multiset ty in
  T.multiset ~kind ~ty:(ty_res:>T.t) l

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
  T.record ~kind ~ty:(ty:>T.t) l ~rest

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
let is_at t = match view t with | At _ -> true | _ -> false
let is_tyat t = match view t with | TyAt _ -> true | _ -> false
let is_lambda t = match T.view t with | T.Bind _ -> true | _ -> false
let is_multiset t = match T.view t with | T.Multiset _ -> true | _ -> false
let is_record t = match T.view t with | T.Record _ -> true | _ -> false

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
  | T.At (l,r) -> 1 + size l + size r
  | T.Record (l, rest) ->
      let s = match rest with None -> 0 | Some r -> size r in
      List.fold_left (fun acc (_,t') -> acc+size t') s l
  | T.App (f, l) -> List.fold_left (fun s t' -> s + size t') (1+size f) l
  | T.RigidVar _ | T.Multiset _ | T.SimpleApp _ -> assert false

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

let rec head t =
  match T.view t with
  | T.Const s -> s
  | T.At (t,_) -> head t
  | T.BVar _
  | T.RigidVar _
  | T.Var _ -> raise (Invalid_argument "Term.head: variable")
  | T.Bind _ -> raise (Invalid_argument "Term.head: lambda")
  | T.Multiset _ -> raise (Invalid_argument "Term.head: record")
  | T.Record _ -> raise (Invalid_argument "Term.head: record")
  | T.App _
  | T.SimpleApp _ -> assert false

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
  | FOT.Const s -> const ~ty s
  | FOT.TyApp (f, ty) -> tyat (curry f) ty
  | FOT.App (f, l) -> at_list (curry f) (List.map curry l)

let uncurry t =
  let rec uncurry t =
    let ty = ty t in
    match T.view t with
    | T.Var i -> FOT.var ~ty i
    | T.BVar i -> FOT.bvar ~ty i
    | T.At _ ->
        let f, tyargs, l = open_at t in
        let f' = uncurry f in
        let l' = List.map uncurry l in
        FOT.app_full f' tyargs l'
    | T.Const s -> FOT.const ~ty s
    | T.Record _
    | T.Multiset _
    | T.RigidVar _ 
    | T.Bind (Symbol.Conn Symbol.Lambda, _, _) -> raise Exit
    | T.App _
    | T.SimpleApp _
    | T.Bind _ -> assert false
  in try Some (uncurry t)
  with Exit -> None

let rec is_fo t = match T.view t with
  | T.Var _
  | T.BVar _ -> true
  | T.At _ ->
      let f, tyargs, l = open_at t in
      is_fo f && List.for_all is_fo l
  | T.Const _ -> true
  | T.Record _
  | T.Multiset _
  | T.RigidVar _ 
  | T.Bind (Symbol.Conn Symbol.Lambda, _, _) -> false
  | T.App _
  | T.SimpleApp _
  | T.Bind _ -> assert false

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
  | RigidVar i -> Printf.bprintf buf "Z%d" i
  | Var i ->
      if not !print_all_types
      then Printf.bprintf buf "X%d:%a" i Type.pp (ty t)
      else Printf.bprintf buf "X%d" i
  | At (l,r) ->
    pp_rec buf l; Buffer.add_char buf ' ';
    pp_surrounded buf r
  | TyAt (l,r) ->
    pp_rec buf l; Buffer.add_char buf ' ';
    Type.pp buf r
  | Record ([], None) ->
    Buffer.add_string buf "{}"
  | Record ([], Some r) ->
    Printf.bprintf buf "{ | %a}" pp_rec r
  | Record (l, None) ->
    Buffer.add_char buf '{';
    Util.pp_list (fun buf (n, t) -> Printf.bprintf buf "%s: %a" n pp_rec t)
      buf l;
    Buffer.add_char buf '}'
  | Record (l, Some r) ->
    Buffer.add_char buf '{';
    Util.pp_list (fun buf (n, t) -> Printf.bprintf buf "%s: %a" n pp_rec t)
      buf l;
    Printf.bprintf buf " | %a}" pp_rec r
  | Multiset l ->
    Printf.bprintf buf "{| %a |}" (Util.pp_list pp_rec) l
  and pp_surrounded buf t = match view t with
  | Lambda _ | At _ | TyAt _ ->
    Buffer.add_char buf '('; pp_rec buf t;  Buffer.add_char buf ')'
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
  | RigidVar i ->
    Format.fprintf fmt "Z%d:%a" i Type.fmt (ty t)
  | BVar i -> Format.fprintf fmt "Y%d" i
  | Lambda (varty,t') ->
    Format.fprintf fmt "(lambda %a %a)" Type.fmt varty debug t'
  | Const s -> Symbol.fmt fmt s
  | TyAt (l, r) ->
    Format.fprintf fmt "(%a %a)" debug l Type.fmt r
  | At (l, r) ->
    Format.fprintf fmt "(%a %a)" debug l debug r
  | Multiset l ->
    Format.fprintf fmt "{| %a |}"
      (Sequence.pp_seq debug) (Sequence.of_list l)
  | Record (l, None) ->
    Format.fprintf fmt "{ %a }"
      (Sequence.pp_seq (fun fmt (n,t) -> Format.fprintf fmt "%s: %a" n debug t))
      (Sequence.of_list l)
  | Record (l, Some r) ->
    Format.fprintf fmt "{ %a | %a }"
      (Sequence.pp_seq (fun fmt (n,t) -> Format.fprintf fmt "%s: %a" n debug t))
      (Sequence.of_list l) debug r

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
      (fun v f -> at forall (mk_lambda [v] f))
      vars f

  let mk_exists vars f =
    List.fold_right
      (fun v f -> at exists (mk_lambda [v] f))
      vars f

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
    | RigidVar i -> Printf.bprintf buf "Y%d" (!depth - i - 1)
    | Lambda (varty,t') ->
      Printf.bprintf buf "^[%a:%a]: " pp_bvar () Type.pp varty;
      incr depth;
      pp_surrounded buf t';
      decr depth
    | Const s -> Symbol.pp buf s
    | Var i ->
        if not !print_all_types && not (Type.eq (ty t) Type.TPTP.i)
        then Printf.bprintf buf "X%d" i
        else Printf.bprintf buf "X%d:%a" i Type.pp (ty t)
    | At (l,r) ->
      pp_surrounded buf l; Buffer.add_string buf " @ ";
      pp_rec buf r
    | TyAt (l,r) ->
      pp_surrounded buf l; Buffer.add_string buf " @ ";
      Type.pp buf r
    | Multiset _ -> failwith "cannot print multiset in TPTP"
    | Record _ -> failwith "cannot print records in TPTP"
    and pp_surrounded buf t = match view t with
    | At _ | TyAt _ | Lambda _ ->
      Buffer.add_char buf '('; pp_rec buf t; Buffer.add_char buf ')'
    | _ -> pp_rec buf t
    and pp_bvar buf () =  Printf.bprintf buf "Y%d" !depth in
    pp_rec buf t

  let pp buf t = pp_depth 0 buf t

  let to_string = Util.on_buffer pp

  let fmt fmt t = Format.pp_print_string fmt (to_string t)
end
