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

module PB = LogtkPosition.Build
module T = LogtkScopedTerm

let prof_mk_node = LogtkUtil.mk_profiler "Term.mk_node"
let prof_ac_normal_form = LogtkUtil.mk_profiler "ac_normal_form"

type symbol = LogtkSymbol.t

(** {2 Term} *)

type t = T.t

type term = t

type view =
  | Var of int                (** Term variable *)
  | BVar of int               (** Bound variable (De Bruijn index) *)
  | Const of LogtkSymbol.t         (** LogtkTyped constant *)
  | TyApp of t * LogtkType.t       (** Application to type *)
  | App of t  * t list        (** Application to a list of terms (cannot be left-nested) *)

let kind = T.Kind.FOTerm

(* split list between types, terms *)
let rec _split_types l = match l with
  | [] -> [], []
  | x::l' when LogtkType.is_type x ->
      let l1, l2 = _split_types l' in
      (LogtkType.of_term_exn x)::l1, l2
  | _ -> [], l

let view t = match T.view t with
  | T.Var i -> Var i
  | T.BVar i -> BVar i
  | T.App (_, []) -> assert false
  | T.At (l, r) ->
      let ty = LogtkType.of_term_exn r in
      TyApp (l, ty)
  | T.App (hd, args) ->
    App (hd, args)
  | T.Const s -> Const s
  | _ -> assert false

let open_app t =
  let rec collect_head_tyargs acc t =
    match T.view t with
    | T.At (f, ty) -> collect_head_tyargs (LogtkType.of_term_exn ty::acc) f
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
    | t::l' when LogtkType.is_type t -> _drop_types l
    | _::_ -> l

  type view =
  | Var of int
  | BVar of int
  | App of symbol * LogtkType.t list * t list  (** covers Const and App *)
  | NonFO   (* any other case *)

  let view t =
    let hd, tyargs, l = open_app t in
    match T.view hd, tyargs, l with
    | T.Var i, [], [] -> Var i
    | T.BVar i, [], [] -> BVar i
    | T.Const s, _, _ -> App (s, tyargs, l)
    | _ -> NonFO
end

(** {2 LogtkComparison, equality, containers} *)

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
let hash_fun = T.hash_fun
let hash = T.hash
let cmp = T.cmp
let ty t = match T.ty t with
  | T.NoType -> assert false
  | T.HasType ty -> LogtkType.of_term_exn ty

module TermHASH = struct
  type t = term
  let equal = eq
  let hash = hash
end

module Tbl = T.Tbl

module Set = T.Set
module Map = T.Map

module TLogtkCache = LogtkCache.Replacing(TermHASH)
module T2LogtkCache = LogtkCache.Replacing2(TermHASH)(TermHASH)

(** {2 Typing} *)

let cast ~(ty:LogtkType.t) t =
  match T.view t with
  | T.Var _ | T.BVar _ -> T.cast ~ty:(ty :> T.t) t
  | T.App _ -> raise (Invalid_argument "FOTerm.cast")
  | _ -> assert false

(** {2 Smart constructors} *)

(** In this section, term smart constructors are defined. They perform
    hashconsing, and precompute some properties (flags).

    TODO: flag_monomorphic *)

let var ~(ty:LogtkType.t) i =
  assert (i >= 0);
  T.var ~kind ~ty:(ty :> T.t) i

let bvar ~(ty:LogtkType.t) i =
  assert (i >= 0);
  T.bvar ~kind ~ty:(ty :> T.t) i

let const ~(ty:LogtkType.t) s =
  T.const ~kind ~ty:(ty :> T.t) s

let tyapp t tyarg =
  let ty = LogtkType.apply (ty t) tyarg in
  T.at ~kind ~ty:(ty:>T.t) t (tyarg : LogtkType.t :> T.t)

let app f l =
  LogtkUtil.enter_prof prof_mk_node;
  (* first; compute type *)
  let ty_result = List.fold_left
    (fun ty_fun t -> LogtkType.apply ty_fun (ty t))
    (ty f) l
  in
  (* apply constant to type args and args *)
  let res = T.app ~kind ~ty:(ty_result:>T.t) f l in
  LogtkUtil.exit_prof prof_mk_node;
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
    | T.Kind.FOTerm, T.App (f, l) ->
        vars f k;
        List.iter (fun t -> vars t k) l
    | T.Kind.FOTerm, T.At (l,r) -> vars l k; vars r k
    | _ -> ()
  and _vars_list l k = match l with
    | [] -> ()
    | t::l' -> vars t k; _vars_list l' k

  let rec subterms t k =
    if is_term t then begin
      k t;
      match T.view t with
      | T.Const _
      | T.Var _
      | T.BVar _ -> ()
      | T.App (f, l) -> subterms f k; List.iter (fun t' -> subterms t' k) l
      | T.At (l,r) -> subterms l k; subterms r k
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
        | T.At (l, r) -> recurse depth l; recurse depth r
        | _ -> ()
      end
    in
    recurse 0 t

  let rec symbols t k =
    match T.kind t, T.view t with
    | T.Kind.FOTerm, T.Const s -> k s
    | T.Kind.FOTerm, T.Var _
    | T.Kind.FOTerm, T.BVar _ -> ()
    | T.Kind.FOTerm, T.App (f, l) ->
        symbols f k;
        _symbols_list l k
    | T.Kind.FOTerm, T.At (l,r) -> symbols l k; symbols r k
    | T.Kind.Type, _ -> ()
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
    let r = ref 0 in
    seq (fun t -> match T.view t with
      | T.Var i -> r := min i !r
      | _ -> ());
    !r

  let add_set set xs =
    Sequence.fold (fun set x -> Set.add x set) set xs

  let ty_vars t =
    subterms t |> Sequence.flatMap (fun t -> LogtkType.Seq.vars (ty t))

  let typed_symbols t =
    subterms t
      |> Sequence.fmap
        (fun t -> match T.view t with
          | T.Const s -> Some (s, ty t)
          | _ -> None)
end

let var_occurs ~var t =
  Sequence.exists (eq var) (Seq.vars t)

let rec size t = match T.view t with
  | T.Var _
  | T.BVar _ -> 1
  | T.App (_, l) -> List.fold_left (fun s t' -> s + size t') 1 l
  | T.Const _ -> 1
  | T.At (l, ty) when LogtkType.is_type ty -> size l
  | _ -> assert false

let weight ?(var=1) ?(sym=fun _ -> 1) t =
  let rec weight t = match T.view t with
    | T.Var _
    | T.BVar _ -> var
    | T.App (_, l) -> List.fold_left (fun s t' -> s + weight t') 1 l
    | T.Const s -> sym s
    | T.At (l, ty) when LogtkType.is_type ty -> weight l
    | _ -> assert false
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
  | T.SimpleApp (s, _)
  | T.Const s -> s
  | T.At (hd, _)
  | T.App (hd,_) -> head_exn hd
  | _ -> raise (Invalid_argument "FOTerm.head")

let head t =
  try Some (head_exn t)
  with Invalid_argument _-> None

let ty_vars t = Seq.ty_vars t |> LogtkType.Seq.add_set LogtkType.Set.empty

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
  of_term_exn (T.replace (t:>T.t) ~old:(old:>T.t) ~by:(by:>T.t))

let symbols ?(init=LogtkSymbol.Set.empty) t =
  LogtkSymbol.Seq.add_set init (Seq.symbols t)

(** Does t contains the symbol f? *)
let contains_symbol f t =
  Sequence.exists (LogtkSymbol.eq f) (Seq.symbols t)

(** {2 Fold} *)

let rec _all_pos_rec f vars acc pb t =
  match view t with
  | Var _ | BVar _ ->
    if vars then f acc t (PB.to_pos pb) else acc
  | Const _ -> f acc t (PB.to_pos pb)
  | TyApp (l, _) -> f acc t (PB.to_pos pb)
  | App (_, tl) ->
    let acc = f acc t (PB.to_pos pb) in  (* apply to term itself *)
    _all_pos_rec_list f vars acc pb tl 0
and _all_pos_rec_list f vars acc pb l i = match l with
  | [] -> acc
  | t::l' when LogtkType.is_type t ->
    _all_pos_rec_list f vars acc pb l' (i+1)
  | t::l' ->
    assert (is_term t);
    let acc = _all_pos_rec f vars acc (PB.arg i pb) t in
    _all_pos_rec_list f vars acc pb l' (i+1)

let all_positions ?(vars=false) ?(pos=LogtkPosition.stop) t acc f =
  _all_pos_rec f vars acc (PB.of_pos pos) t

(** {2 Some AC-utils} *)

module type AC_SPEC = sig
  val is_ac : LogtkSymbol.t -> bool
  val is_comm : LogtkSymbol.t -> bool
end

module AC(A : AC_SPEC) = struct
  let flatten f l =
    let rec flatten acc l = match l with
    | [] -> acc
    | x::l' when LogtkType.is_type x -> flatten acc l' (* ignore type args *)
    | x::l' -> flatten (deconstruct acc x) l'
    and deconstruct acc t = match T.view t with
    | T.App (f', l') when LogtkSymbol.eq (head_exn f') f ->
      flatten acc l'
    | _ -> t::acc
    in flatten [] l

  let normal_form t =
    LogtkUtil.enter_prof prof_ac_normal_form;
    let rec normalize t = match T.view t with
      | _ when LogtkType.is_type t -> t
      | T.Var _ -> t
      | T.BVar _ -> t
      | T.App (f, l) when A.is_ac (head_exn f) ->
        let l = flatten (head_exn f) l in
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
      | T.App (f, [a;b]) when A.is_comm (head_exn f) ->
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
    LogtkUtil.exit_prof prof_ac_normal_form;
    t'

  let eq t1 t2 =
    let t1' = normal_form t1
    and t2' = normal_form t2 in
    eq t1' t2'

  let symbols seq =
    Sequence.flatMap Seq.symbols seq
      |> Sequence.filter A.is_ac
      |> LogtkSymbol.Seq.add_set LogtkSymbol.Set.empty
end

(** {2 Conversions} *)

let to_prolog ?(depth=0) t =
  let module PT = LogtkPrologTerm in
  let rec to_prolog t =
    let ty = ty t in
    match view t with
    | Var i -> PT.column (PT.var (LogtkUtil.sprintf "X%d" i)) (LogtkType.Conv.to_prolog ~depth ty)
    | BVar i -> PT.var (LogtkUtil.sprintf "Y%d" (depth-i-1))
    | TyApp _
    | App _ -> gather_left [] t
    | Const f -> PT.const f
  and gather_left acc t = match view t with
    | TyApp (f, ty) -> gather_left (LogtkType.Conv.to_prolog ~depth ty :: acc) f
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
    if not (List.exists (fun hook -> hook !depth pp_rec buf t) hooks)
    then begin match view t with
    | BVar i -> Printf.bprintf buf "Y%d" (!depth - i - 1)
    | TyApp (f, ty) ->
        pp_rec buf f;
        Buffer.add_char buf ' ';
        LogtkType.pp_depth !depth buf ty
    | App (f, args) ->
        assert (args <> []);
        pp_rec buf f;
        Buffer.add_char buf ' ';
        LogtkUtil.pp_list ~sep:" " pp_inner buf args
    | Const s -> LogtkSymbol.pp buf s
    | Var i ->
      if not !print_all_types && not (LogtkType.eq (ty t) LogtkType.TPTP.i)
        then Printf.bprintf buf "X%d:%a" i (LogtkType.pp_depth !depth) (ty t)
        else Printf.bprintf buf "X%d" i
    end;
    (* print type of term? *)
    if !print_all_types
      then Printf.bprintf buf ":%a" (LogtkType.pp_depth !depth) (ty t)
  and pp_inner buf t = match view t with
    | TyApp _
    | App _ -> Buffer.add_char buf '('; pp_rec buf t; Buffer.add_char buf ')'
    | _ -> pp_rec buf t
  in
  pp_rec buf t

let __hooks = ref []
let add_hook h = __hooks := h :: !__hooks
let default_hooks () = !__hooks

let pp buf t = pp_depth ~hooks:!__hooks 0 buf t

let to_string = LogtkUtil.on_buffer pp

let fmt fmt t = Format.pp_print_string fmt (to_string t)

let rec debug fmt t =
  begin match view t with
  | Var i -> Format.fprintf fmt "X%d" i
  | BVar i -> Format.fprintf fmt "Y%d" i
  | Const s -> LogtkSymbol.fmt fmt s
  | TyApp (f, ty) ->
    Format.fprintf fmt "(%a %a)" debug f LogtkType.fmt ty
  | App (s, l) ->
    Format.fprintf fmt "(%a %a)" debug s (CCList.print debug) l
  end;
  Format.fprintf fmt ":%a" LogtkType.fmt (ty t)

(** {2 TPTP} *)

module TPTP = struct
  let true_ = const ~ty:LogtkType.TPTP.o LogtkSymbol.Base.true_
  let false_ = const ~ty:LogtkType.TPTP.o LogtkSymbol.Base.false_

  let pp_depth ?(hooks=[]) depth buf t =
    let depth = ref depth in
    (* recursive printing *)
    let rec pp_rec buf t = match view t with
    | BVar i ->
        Printf.bprintf buf "Y%d" (!depth - i - 1);
        (* print type of term *)
        if !print_all_types || not (LogtkType.eq (ty t) LogtkType.TPTP.i)
          then Printf.bprintf buf ":%a" (LogtkType.TPTP.pp_depth !depth) (ty t)
    | Const s -> LogtkSymbol.TPTP.pp buf s
    | App _
    | TyApp _ ->
        let f, tyargs, args = open_app t in
        Printf.bprintf buf "%a(" pp_rec f;
        LogtkUtil.pp_list (LogtkType.TPTP.pp_depth !depth) buf tyargs;
        begin match tyargs, args with
          | _::_, _::_ -> Buffer.add_string buf ", "
          | _ -> ();
        end;
        LogtkUtil.pp_list pp_rec buf args;
        Buffer.add_string buf ")"
    | Var i ->
        Printf.bprintf buf "X%d" i;
        (* print type of term *)
        if !print_all_types || not (LogtkType.eq (ty t) LogtkType.TPTP.i)
          then Printf.bprintf buf ":%a" (LogtkType.TPTP.pp_depth !depth) (ty t)
    in
    pp_rec buf t

  let pp buf t = pp_depth 0 buf t
  let to_string = LogtkUtil.on_buffer pp
  let fmt fmt t = Format.pp_print_string fmt (to_string t)

  module Arith = struct
    let term_pp_depth = pp_depth

    open LogtkType.TPTP

    let x = LogtkType.var 0

    let ty1 = LogtkType.(forall [x] (int <=. x))

    let floor = const ~ty:ty1 LogtkSymbol.TPTP.Arith.floor
    let ceiling = const ~ty:ty1 LogtkSymbol.TPTP.Arith.ceiling
    let truncate = const ~ty:ty1 LogtkSymbol.TPTP.Arith.truncate
    let round = const ~ty:ty1 LogtkSymbol.TPTP.Arith.round

    let prec = const ~ty:LogtkType.(int <=. int) LogtkSymbol.TPTP.Arith.prec
    let succ = const ~ty:LogtkType.(int <=. int) LogtkSymbol.TPTP.Arith.succ

    let ty2 = LogtkType.(forall [x] (x <== [x;x]))
    let ty2i = LogtkType.(int <== [int;int])

    let sum = const ~ty:ty2 LogtkSymbol.TPTP.Arith.sum
    let difference = const ~ty:ty2 LogtkSymbol.TPTP.Arith.difference
    let uminus = const ~ty:ty2 LogtkSymbol.TPTP.Arith.uminus
    let product = const ~ty:ty2 LogtkSymbol.TPTP.Arith.product
    let quotient = const ~ty:ty2 LogtkSymbol.TPTP.Arith.quotient

    let quotient_e = const ~ty:ty2i LogtkSymbol.TPTP.Arith.quotient_e
    let quotient_t = const ~ty:ty2i LogtkSymbol.TPTP.Arith.quotient_t
    let quotient_f = const ~ty:ty2i LogtkSymbol.TPTP.Arith.quotient_f
    let remainder_e = const ~ty:ty2i LogtkSymbol.TPTP.Arith.remainder_e
    let remainder_t = const ~ty:ty2i LogtkSymbol.TPTP.Arith.remainder_t
    let remainder_f = const ~ty:ty2i LogtkSymbol.TPTP.Arith.remainder_f

    let ty2o = LogtkType.(forall [x] (o <== [x;x]))

    let less = const ~ty:ty2o LogtkSymbol.TPTP.Arith.less
    let lesseq = const ~ty:ty2o LogtkSymbol.TPTP.Arith.lesseq
    let greater = const ~ty:ty2o LogtkSymbol.TPTP.Arith.greater
    let greatereq = const ~ty:ty2o LogtkSymbol.TPTP.Arith.greatereq

    (* hook that prints arithmetic expressions *)
    let arith_hook depth pp_rec buf t =
      let module SA = LogtkSymbol.TPTP.Arith in
      let pp_surrounded buf t = match Classic.view t with
      | Classic.App (s, _, [_;_]) when
        LogtkSymbol.eq s SA.sum ||
        LogtkSymbol.eq s SA.product ||
        LogtkSymbol.eq s SA.difference ||
        LogtkSymbol.eq s SA.quotient ->
        Buffer.add_char buf '(';
        pp_rec buf t;
        Buffer.add_char buf ')'
      | _ -> pp_rec buf t
      in
      match Classic.view t with
      | Classic.Var i when LogtkType.eq (ty t) LogtkType.TPTP.int ->
        Printf.bprintf buf "I%d" i; true
      | Classic.Var i when LogtkType.eq (ty t) LogtkType.TPTP.rat ->
        Printf.bprintf buf "Q%d" i; true
      | Classic.App (s, _,[a; b]) when LogtkSymbol.eq s SA.less ->
        Printf.bprintf buf "%a < %a" pp_surrounded a pp_surrounded b; true
      | Classic.App (s, _,[a; b]) when LogtkSymbol.eq s SA.lesseq ->
        Printf.bprintf buf "%a ≤ %a" pp_surrounded a pp_surrounded b; true
      | Classic.App (s, _,[a; b]) when LogtkSymbol.eq s SA.greater ->
        Printf.bprintf buf "%a > %a" pp_surrounded a pp_surrounded b; true
      | Classic.App (s, _,[a; b]) when LogtkSymbol.eq s SA.greatereq ->
        Printf.bprintf buf "%a ≥ %a" pp_surrounded a pp_surrounded b; true
      | Classic.App (s, _,[a; b]) when LogtkSymbol.eq s SA.sum ->
        Printf.bprintf buf "%a + %a" pp_surrounded a pp_surrounded b; true
      | Classic.App (s, _,[a; b]) when LogtkSymbol.eq s SA.difference ->
        Printf.bprintf buf "%a - %a" pp_surrounded a pp_surrounded b; true
      | Classic.App (s, _,[a; b]) when LogtkSymbol.eq s SA.product ->
        Printf.bprintf buf "%a × %a" pp_surrounded a pp_surrounded b; true
      | Classic.App (s, _,[a; b]) when LogtkSymbol.eq s SA.quotient ->
        Printf.bprintf buf "%a / %a" pp_surrounded a pp_surrounded b; true
      | Classic.App (s, _,[a; b]) when LogtkSymbol.eq s SA.quotient_e ->
        Printf.bprintf buf "%a // %a" pp_surrounded a pp_surrounded b; true
      | Classic.App (s, _,[a]) when LogtkSymbol.eq s SA.uminus ->
        Printf.bprintf buf "-%a" pp_surrounded a; true;
      | Classic.App (s, _,[a;b]) when LogtkSymbol.eq s SA.remainder_e ->
        Printf.bprintf buf "%a mod %a" pp_surrounded a pp_surrounded b; true;
      | _ -> false  (* default *)

    let pp_debug buf t =
      term_pp_depth ~hooks:(arith_hook:: !__hooks) 0 buf t
  end
end

(** {2 Misc} *)


(*
let bij =
  let open Bij in
  let (!!!) = Lazy.force in
  fix
    (fun bij ->
      let bij_node = lazy (triple LogtkSymbol.bij (list_ LogtkType.bij) (list_ !!!bij)) in
      let bij_var = pair int_ LogtkType.bij in
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
