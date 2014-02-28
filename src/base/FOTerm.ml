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

type sourced_term =
  t * string * string           (** Term + file,name *)

type view =
  | Var of int                (** Term variable *)
  | BVar of int               (** Bound variable (De Bruijn index) *)
  | Const of Symbol.t         (** Typed constant *)
  | TyApp of t * Type.t       (** Application to type *)
  | App of t  * t list        (** Application to a list of terms (cannot be left-nested) *)

let kind = T.Kind.FOTerm

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
  | T.App (_, []) -> assert false
  | T.At (l, r) ->
      let ty = Type.of_term_exn r in
      TyApp (l, ty)
  | T.App (hd, args) ->
    App (hd, args)
  | T.Const s -> Const s
  | _ -> assert false

let open_app t =
  let rec collect_head_tyargs acc t =
    match T.view t with
    | T.At (f, ty) -> collect_head_tyargs (Type.of_term_exn ty::acc) f
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
    | t::l' when Type.is_type t -> _drop_types l
    | _::_ -> l

  type view =
  | Var of int
  | BVar of int
  | App of symbol * t list  (** covers Const and App *)
  | NonFO   (* any other case *)

  let view t = match T.view t with
    | T.Var i -> Var i
    | T.BVar i -> BVar i
    | T.At(l, r) ->
        begin match T.head l with
        | None -> NonFO
        | Some s -> App (s, [])
        end
    | T.App (hd, l) ->
        begin match T.head hd with
        | None -> NonFO
        | Some s -> App (s, l)
        end
    | T.Const s -> App (s, [])
    | _ -> NonFO
end

(** {2 Comparison, equality, containers} *)

let subterm ~sub t =
  let rec check t =
    T.eq sub t ||
    match T.view t with
    | T.Var _ | T.BVar _ -> false
    | T.App (f, l) -> check f || List.exists check l
    | _ -> false
  in
  check t

let eq = T.eq
let hash = T.hash
let cmp = T.cmp
let ty t = match T.ty t with
  | T.NoType -> assert false
  | T.HasType ty -> Type.of_term_exn ty

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
  let compare = cmp
end)

module Map = Sequence.Map.Make(struct
  type t = term
  let compare = cmp
end)

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
  T.var ~kind ~ty:(ty :> T.t) i

let bvar ~(ty:Type.t) i =
  assert (i >= 0);
  T.bvar ~kind ~ty:(ty :> T.t) i

let const ~(ty:Type.t) s =
  T.const ~kind ~ty:(ty :> T.t) s

let tyapp t tyarg =
  let ty = Type.apply (ty t) tyarg in
  T.at ~kind ~ty:(ty:>T.t) t (tyarg : Type.t :> T.t)

let app f l =
  Util.enter_prof prof_mk_node;
  (* first; compute type *)
  let ty_result = List.fold_left
    (fun ty_fun t -> Type.apply ty_fun (ty t))
    (ty f) l
  in
  (* apply constant to type args and args *)
  let res = T.app ~kind ~ty:(ty_result:>T.t) f l in
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

let of_term t = match T.kind t with
  | T.Kind.FOTerm -> Some t
  | _ -> None

let of_term_exn t = match T.kind t with
  | T.Kind.FOTerm -> t
  | _ -> raise (Invalid_argument "Term.of_term_exn")

let is_term t = match T.kind t with
  | T.Kind.FOTerm -> true
  | _ -> false

module Seq = struct
  let rec vars t k =
    if T.ground t then ()
    else match T.kind t, T.view t with
    | T.Kind.FOTerm, T.Var _ -> k t
    | T.Kind.FOTerm, T.BVar _ -> ()
    | T.Kind.FOTerm, T.App (f, l) -> vars f k; List.iter (fun t -> vars t k) l
    | _ -> ()
  and _vars_list l k = match l with
    | [] -> ()
    | t::l' -> vars t k; _vars_list l' k

  let rec subterms t k =
    if is_term t then begin
      k t;
      match T.view t with
      | T.Var _
      | T.BVar _ -> ()
      | T.App (f, l) -> subterms f k; List.iter (fun t' -> subterms t' k) l
      | _ -> assert false
    end

  let subterms_depth t k =
    let rec recurse depth t =
      if is_term t then begin
        k (t, depth);
        match T.view t with
        | T.App (_, ((_::_) as l)) ->
          let depth' = depth + 1 in
          List.iter (fun t' -> recurse depth' t') l
        | _ -> ()
      end
    in
    recurse 0 t

  let rec symbols t k =
    match T.kind t, T.view t with
    | T.Kind.FOTerm, T.Var _
    | T.Kind.FOTerm, T.BVar _ -> ()
    | T.Kind.FOTerm, T.App (hd, l) ->
        begin match T.view hd with
        | T.Const s -> k s
        | _ -> ()
        end;
        _symbols_list l k
    | _ -> assert false
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
    let r = ref max_int in
    seq (fun t -> match T.view t with
      | T.Var i -> r := min i !r
      | _ -> ());
    !r

  let add_set set xs =
    Sequence.fold (fun set x -> Set.add x set) set xs

  let ty_vars t =
    subterms t |> Sequence.flatMap (fun t -> Type.Seq.vars (ty t))
end

let var_occurs ~var t =
  Sequence.exists (eq var) (Seq.vars t)

let rec size t = match T.view t with
  | T.Var _
  | T.BVar _ -> 1
  | T.App (_, l) -> List.fold_left (fun s t' -> s + size t') 1 l
  | _ -> assert false

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

let head t = match T.view t with
  | T.Const s -> s
  | T.App (hd,_) ->
      begin match T.view hd with
        | T.Const s -> s
        | _ -> raise (Invalid_argument "FOTerm.head")
      end
  | _ -> raise (Invalid_argument "FOTerm.head")

let ty_vars t = Seq.ty_vars t |> Type.Seq.add_set Type.Set.empty

(** {2 Subterms and positions} *)

module Pos = struct
  let at t pos = of_term_exn (T.Pos.at (t :> T.t) pos)

  let replace t pos ~by = of_term_exn (T.Pos.replace (t:>T.t) pos ~by:(by:>T.t))

  (** get subterm by its compact position *)
  let at_cpos t pos = 
    let rec recurse t pos =
      match T.view t, pos with
      | _, 0 -> t
      | T.App (_, l), _ -> get_subpos l (pos - 1)
      | _ -> failwith "bad compact position"
    and get_subpos l pos =
      match l, pos with
      | t::l', _ when size t > pos -> recurse t pos  (* search inside the term *)
      | t::l', _ -> get_subpos l' (pos - size t) (* continue to next term *)
      | [], _ -> assert false
    in recurse t pos

  let max_cpos t = size t - 1
end

let replace t ~old ~by =
  of_term_exn (T.replace (t:>T.t) ~old:(old:>T.t) ~by:(by:>T.t))

let symbols ?(init=Symbol.Set.empty) t =
  Symbol.Seq.add_set init (Seq.symbols t)

(** Does t contains the symbol f? *)
let contains_symbol f t =
  Sequence.exists (Symbol.eq f) (Seq.symbols t)

(** {2 Fold} *)

let rec _all_pos_rec f vars acc pb t = match T.view t with
  | T.Var _ | T.BVar _ ->
    if vars then f acc t (PB.to_pos pb) else acc
  | T.App (_, tl) ->
    let acc = f acc t (PB.to_pos pb) in  (* apply to term itself *)
    _all_pos_rec_list f vars acc pb tl 0
  | _ -> assert false
and _all_pos_rec_list f vars acc pb l i = match l with
  | [] -> acc
  | t::l' when Type.is_type t ->
    _all_pos_rec_list f vars acc pb l' (i+1)
  | t::l' ->
    assert (is_term t);
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
    | x::l' when Type.is_type x -> flatten acc l' (* ignore type args *)
    | x::l' -> flatten (deconstruct acc x) l'
    and deconstruct acc t = match T.view t with
    | T.App (f', l') when Symbol.eq (head f') f ->
      flatten acc l'
    | _ -> t::acc
    in flatten [] l

  let normal_form t =
    Util.enter_prof prof_ac_normal_form;
    let rec normalize t = match T.view t with
      | _ when Type.is_type t -> t
      | T.Var _ -> t
      | T.BVar _ -> t
      | T.App (f, l) when A.is_ac (head f) ->
        let l = flatten (head f) l in
        let tyargs, l = _split_types l in
        let l = List.map normalize l in
        let l = List.sort cmp l in
        begin match l with
          | x::l' ->
            let ty = T.ty_exn t in
            let tyargs = (tyargs :> T.t list) in
            List.fold_left
              (fun subt x -> T.app ~ty ~kind:T.Kind.FOTerm f (tyargs@[x;subt]))
              x l'
          | [] -> assert false
        end
      | T.App (f, [a;b]) when A.is_comm (head f) ->
        (* FIXME: doesn't handle polymorphic commutative operators *)
        let a = normalize a in
        let b = normalize b in
        if cmp a b > 0
          then T.app ~kind:T.Kind.FOTerm ~ty:(ty t :>T.t) f [b; a]
          else t
      | T.App (f, l) ->
        let l = List.map normalize l in
        T.app ~kind:T.Kind.FOTerm ~ty:(T.ty_exn t) f l 
      | _ -> assert false
    in
    let t' = normalize t in
    Util.exit_prof prof_ac_normal_form;
    t'

  let eq t1 t2 =
    let t1' = normal_form t1
    and t2' = normal_form t2 in
    eq t1' t2'

  let symbols seq =
    Sequence.flatMap Seq.symbols seq
      |> Sequence.filter A.is_ac
      |> Symbol.Seq.add_set Symbol.Set.empty
end

(** {2 Conversions} *)

let to_prolog ?(depth=0) t =
  let module PT = PrologTerm in
  let rec to_prolog t =
    let ty = ty t in
    match view t with
    | Var i -> PT.column (PT.var (Util.sprintf "X%d" i)) (Type.Conv.to_prolog ~depth ty)
    | BVar i -> PT.var (Util.sprintf "Y%d" (depth-i-1))
    | TyApp _
    | App _ -> gather_left [] t
    | Const f -> PT.const f
  and gather_left acc t = match view t with
    | TyApp (f, ty) -> gather_left (Type.Conv.to_prolog ~depth ty :: acc) f
    | App (f, l) -> gather_left (List.map to_prolog l @ acc) f
    | _ -> PT.app (to_prolog t) acc
  in to_prolog t

(** {2 Printing/parsing} *)

let print_all_types = ref false

type print_hook = int -> (Buffer.t -> t -> unit) -> Buffer.t -> t -> bool

(* lightweight printing *)
let pp_depth ?(hooks=[]) depth buf t =
  let depth = ref depth in
  (* recursive printing *)
  let rec pp_rec buf t =
    begin match view t with
    | BVar i -> Printf.bprintf buf "Y%d" (!depth - i - 1)
    | TyApp (f, ty) ->
        pp_rec buf f;
        Buffer.add_char buf ' ';
        Type.pp buf ty
    | App (f, args) ->
      (* try to use some hook *)
      if List.exists (fun hook -> hook !depth pp_rec buf t) hooks
      then ()
      else (* default case for nodes *)
        begin
          assert (args <> []);
          pp_rec buf f;
          Buffer.add_char buf ' ';
          Util.pp_list ~sep:" " pp_inner buf args
        end
    | Const s -> Symbol.pp buf s
    | Var i ->
      if not !print_all_types
        then Printf.bprintf buf "X%d:%a" i Type.pp (ty t)
        else Printf.bprintf buf "X%d" i
    end;
    (* print type of term *)
    if !print_all_types
      then Printf.bprintf buf ":%a" Type.pp (ty t)
  and pp_inner buf t = match view t with
    | TyApp _
    | App _ -> Buffer.add_char buf '('; pp_rec buf t; Buffer.add_char buf ')'
    | _ -> pp_rec buf t
  in
  pp_rec buf t

let __hooks = ref []
let add_hook h = __hooks := h :: !__hooks

let pp buf t = pp_depth ~hooks:!__hooks 0 buf t

let to_string = Util.on_buffer pp

let fmt fmt t = Format.pp_print_string fmt (to_string t)

let rec debug fmt t =
  begin match view t with
  | Var i -> Format.fprintf fmt "X%d" i
  | BVar i -> Format.fprintf fmt "Y%d" i
  | Const s -> Symbol.fmt fmt s
  | TyApp (f, ty) ->
    Format.fprintf fmt "(%a %a)" debug f Type.fmt ty
  | App (s, l) ->
    Format.fprintf fmt "(%a %a)" debug s
      (Sequence.pp_seq debug) (Sequence.of_list l)
  end;
  Format.fprintf fmt ":%a" Type.fmt (ty t)

(** {2 TPTP} *)

module TPTP = struct
  let true_ = const ~ty:Type.TPTP.o Symbol.Base.true_
  let false_ = const ~ty:Type.TPTP.o Symbol.Base.false_

  let pp_depth ?(hooks=[]) depth buf t =
    let depth = ref depth in
    (* recursive printing *)
    let rec pp_rec buf t = match view t with
    | BVar i -> Printf.bprintf buf "Y%d" (!depth - i - 1)
    | Const s -> Symbol.TPTP.pp buf s
    | App _
    | TyApp _ ->
        let f, tyargs, args = open_app t in
        Printf.bprintf buf "%a(" pp_rec f;
        Util.pp_list Type.TPTP.pp buf tyargs;
        begin match tyargs, args with
          | _::_, _::_ -> Buffer.add_string buf ", "
          | _ -> ();
        end;
        Util.pp_list pp_rec buf args;
        Buffer.add_string buf ")"
    | Var i -> Printf.bprintf buf "X%d" i
    in
    pp_rec buf t

  let pp buf t = pp_depth 0 buf t
  let to_string = Util.on_buffer pp
  let fmt fmt t = Format.pp_print_string fmt (to_string t)

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
    let arith_hook depth pp_rec buf t =
      let _eq_sym s cst =  match T.view cst with
        | T.Const s' -> Symbol.eq s s'
        | _ -> false
      in
      let pp_surrounded buf t = match view t with
      | App (s, [_;_]) when
        eq s sum ||
        eq s product ||
        eq s difference ||
        eq s quotient ->
        Buffer.add_char buf '(';
        pp_rec buf t;
        Buffer.add_char buf ')'
      | _ -> pp_rec buf t
      in
      match view t with
      | App (s, [a; b]) when eq s less ->
        Printf.bprintf buf "%a < %a" pp_surrounded a pp_surrounded b; true
      | App (s, [a; b]) when eq s lesseq ->
        Printf.bprintf buf "%a ≤ %a" pp_surrounded a pp_surrounded b; true
      | App (s, [a; b]) when eq s greater ->
        Printf.bprintf buf "%a > %a" pp_surrounded a pp_surrounded b; true
      | App (s, [a; b]) when eq s greatereq ->
        Printf.bprintf buf "%a ≥ %a" pp_surrounded a pp_surrounded b; true
      | App (s, [a; b]) when eq s sum ->
        Printf.bprintf buf "%a + %a" pp_surrounded a pp_surrounded b; true
      | App (s, [a; b]) when eq s difference ->
        Printf.bprintf buf "%a - %a" pp_surrounded a pp_surrounded b; true
      | App (s, [a; b]) when eq s product ->
        Printf.bprintf buf "%a × %a" pp_surrounded a pp_surrounded b; true
      | App (s, [a; b]) when eq s quotient ->
        Printf.bprintf buf "%a / %a" pp_surrounded a pp_surrounded b; true
      | App (s, [a; b]) when eq s quotient_e ->
        Printf.bprintf buf "%a // %a" pp_surrounded a pp_surrounded b; true
      | App (s, [a]) when eq s uminus ->
        Printf.bprintf buf "-%a" pp_surrounded a; true;
      | App (s, [a;b]) when eq s remainder_e ->
        Printf.bprintf buf "%a mod %a" pp_surrounded a pp_surrounded b; true;
      | _ -> false  (* default *)

    let pp_debug buf t =
      term_pp_depth ~hooks:(arith_hook:: !__hooks) 0 buf t
  end
end

(** {2 Misc} *)

let __var ~(ty:Type.t) i =
  T.var ~kind:T.Kind.FOTerm ~ty:(ty:>T.t) i


(*
let bij =
  let open Bij in
  let (!!!) = Lazy.force in
  fix
    (fun bij ->
      let bij_node = lazy (triple Symbol.bij (list_ Type.bij) (list_ !!!bij)) in
      let bij_var = pair int_ Type.bij in
      switch
        ~inject:(fun t -> match t.term with
        | BoundVar i -> "bv", BranchTo (bij_var, (i,t.ty))
        | Var i -> "v", BranchTo (bij_var, (i, t.ty))
        | Node (s, tyargs, l) -> "n", BranchTo (!!!bij_node, (s, tyargs, l)))
        ~extract:(function
        | "bv" -> BranchFrom (bij_var, fun (i,ty) -> __mk_bound_var ~ty i)
        | "v" -> BranchFrom (bij_var, fun (i,ty) -> mk_var ~ty i)
        | "n" -> BranchFrom (!!!bij_node, fun (s,tyargs,l) -> mk_node ~tyargs s l)
        | _ -> raise (DecodingError "expected Term")))
*)
