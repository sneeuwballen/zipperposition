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

(** {1 First-order terms} *)

module PB = Position.Build
module T = ScopedTerm

let prof_mk_node = Util.mk_profiler "Term.mk_node"
let prof_ac_normal_form = Util.mk_profiler "ac_normal_form"

type symbol = Symbol.t

(** {2 Typed Symbol} *)

module Cst = struct
  type t = T.t

  let make ~ty sym = T.const ~kind:T.Kind.Const ~ty:(ty:>T.t) sym

  let ty c = T.ty c

  let sym c = match T.view c with
    | T.Const s -> s
    | _ -> assert false

  let is_const t = match T.kind t with
    | T.Kind.Const -> true
    | _ -> false

  let of_term t = if is_const t then Some t else None
  let of_term_exn t = if is_const t
  then t else raise (Invalid_argument "T.Cst.of_term_exn")

  let hash = T.hash
  let eq = T.eq
  let cmp = T.cmp

  let print buf c = Symbol.pp buf (sym c)
  let to_string c = Symbol.to_string (sym c)
  let fmt fmt c = Symbol.fmt fmt (sym c)

  module TPTP = struct
    let true_ = make ~ty:Type.TPTP.o Symbol.Base.true_
    let false_ = make ~ty:Type.TPTP.o Symbol.Base.false_
  end
end

module TS = Cst

(** {2 Term} *)

type t = T.t

type term = t

type sourced_term =
  t * string * string           (** Term + file,name *)

type view =
  | Var of int                (** Term variable *)
  | BVar of int               (** Bound variable (De Bruijn index) *)
  | App of Cst.t * Type.t list * t list (** Function application *)

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
  | T.App (hd, args) ->
      begin match T.view hd with
      | T.Const s ->
          let ty = Type.of_term_exn (T.ty hd) in
          let ts = Cst.make ~ty s in
          (* split arguments into type arguments + term arguments *)
          let tyargs, args = _split_types args in
          App (ts, tyargs, args)
      | _ -> assert false
      end
  | _ -> assert false

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
let hash = T.hash
let cmp = T.cmp
let ty t = Type.of_term_exn (T.ty t)

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

let cast ~ty t =
  match T.view t with
  | T.Var _ | T.BVar _ -> T.cast ~ty:(ty :> T.t) t
  | T.App _ -> raise (Invalid_argument "FOTerm.cast")
  | _ -> assert false

(** {2 Smart constructors} *)

(** In this section, term smart constructors are defined. They perform
    hashconsing, and precompute some properties (flags).
    
    TODO: flag_monomorphic *)

let kind = T.Kind.FOTerm

let var ~ty i =
  assert (i >= 0);
  T.var ~kind ~ty:(ty :> T.t) i

let bvar ~ty i =
  assert (i >= 0);
  T.bvar ~kind ~ty:(ty :> T.t) i

(* compute type of s(tyargs  *)
let _compute_ty ty_fun tyargs l =
  let ty' = Type.apply ty_fun tyargs in
  Type.apply ty' (List.map ty l)

let app ?(tyargs=[]) ts args =
  Util.enter_prof prof_mk_node;
  (* first; compute type *)
  let ty = _compute_ty ts.TS.ty tyargs args in
  (* create constant *)
  let cst = T.const ~kind ~ty:(ts.TS.ty :> T.t) ts.TS.sym in
  (* apply constant to type args and args *)
  let res = T.app ~kind ~ty:(ty:>T.t) cst ((tyargs :> T.t list) @ args) in
  Util.exit_prof prof_mk_node;
  res

let const ?(tyargs=[]) ts =
  match tyargs with
  | [] ->
    let ty = _compute_ty ts.TS.ty tyargs [] in
    T.const ~kind ~ty:(ts.TS.ty :> T.t) ts.TS.sym
  | _::_ ->  app ~tyargs ts []

let is_var t = match T.view t with
  | T.Var _ -> true
  | _ -> false

let is_bvar t = match T.view t with
  | T.BVar _ -> true
  | _ -> false

let is_const t = match T.view t with
  | T.Const _ -> true
  | _ -> false

let is_node t = match T.view t with
  | T.Const _
  | T.App _ -> true
  | _ -> false

let of_term t = match T.kind t with
  | T.Kind.FOTerm _ -> Some t
  | _ -> None

let of_term_exn t = match T.kind t with
  | T.Kind.FOTerm _ -> t
  | _ -> raise (Invalid_argument "Term.of_term_exn")

let is_term t = match T.kind t with
  | T.Kind.FOTerm _ -> true
  | _ -> false

module TPTP = struct
  let true_ = const TS.TPTP.true_
  let false_ = const TS.TPTP.false_
end

(** {2 Subterms and positions} *)

module Pos = struct
  let at t pos = of_term_exn (T.Pos.at (t :> T.t) pos)

  let replace t pos ~by = of_term_exn (T.Pos.replace (t:>T.t) pos ~by:(by:>T.t))
end

let replace t ~old ~by =
  of_term_exn (T.replace (t:>T.t) ~old:(old:>T.t) ~by:(by:>T.t))

module Seq = struct
  let rec vars t k =
    match T.kind t, T.view t with
    | T.Kind.FOTerm, T.Var _ -> k t
    | T.Kind.FOTerm, T.BVar _
    | T.Kind.FOTerm, T.App (_, []) -> ()
    | T.Kind.FOTerm, T.App (_, l) -> _vars_list l k
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
      | T.App (_, l) -> List.iter (fun t' -> subterms t' k) l
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
    | T.Kind.FOTerm, T.App (T.Const s, []) -> k s
    | T.Kind.FOTerm, T.App (T.Const s, l) ->
      k s;
      _symbols_list l k
  and _symbols_list l k = match l with
    | [] -> ()
    | t::l' -> symbols t k; _symbols_list l' k

  let max_var seq =
    let r = ref 0 in
    seq (function {term=Var i} -> r := max i !r | _ -> ());
    !r

  let min_var seq =
    let r = ref max_int in
    seq (function {term=Var i} -> r := min i !r | _ -> ());
    !r
end

(* FIXME: symbols are counted? beware of T.Const *)
let size t = T.size (t:>T.t)

let rec ty_vars set t = match t.term with
  | Var _
  | BoundVar _ -> Type.free_vars_set set t.ty
  | Node (_, tyargs, l) ->
    let set = List.fold_left Type.free_vars_set set tyargs in
    List.fold_left ty_vars set l

(** get subterm by its position *)
let at_cpos t pos = 
  let rec recurse t pos =
    match t.term, pos with
    | _, 0 -> t
    | Node (_, _, l), _ -> get_subpos l (pos - 1)
    | _ -> failwith "bad compact position"
  and get_subpos l pos =
    match l, pos with
    | t::l', _ when size t > pos -> recurse t pos  (* search inside the term *)
    | t::l', _ -> get_subpos l' (pos - size t) (* continue to next term *)
    | [], _ -> assert false
  in recurse t pos

let max_cpos t = size t - 1

let is_ground t = get_flag flag_ground t

(* TODO: use a flag to avoid computing it here? Or compute on demand? *)
let rec monomorphic t =
  Type.is_ground t.ty &&
  match t.term with
  | Var _ | BoundVar _ | Node (_, [], []) -> true
  | Node (_, tyargs, l) ->
    List.for_all Type.is_ground tyargs &&
    List.for_all monomorphic l

let var_occurs ~var t = Sequence.exists (fun t' -> eq var t') (Seq.vars t)

let max_var vars =
  let rec aux idx = function
  | [] -> idx
  | ({term=Var i}::vars) -> aux (max i idx) vars
  | _::vars -> assert false
  in
  aux 0 vars

let min_var vars =
  let rec aux idx = function
  | [] -> idx
  | ({term=Var i}::vars) -> aux (min i idx) vars
  | _ -> assert false
  in
  aux 0 vars

(** add variables of the term to the set *)
let add_vars set t =
  Sequence.iter (fun v -> Tbl.replace set v ()) (Seq.vars t)

(** compute variables of the term *)
let vars t =
  let set = Tbl.create 5 in
  add_vars set t;
  Tbl.fold (fun t () acc -> t :: acc) set []

(** Compute variables of terms in the list *)
let vars_list l =
  let set = Tbl.create 5 in
  List.iter (add_vars set) l;
  Tbl.fold (fun t () acc -> t :: acc) set []

(** Compute variables of terms in the sequence *)
let vars_seq seq =
  let set = Tbl.create 5 in
  Sequence.iter (add_vars set) seq;
  Tbl.fold (fun t () acc -> t :: acc) set []

let vars_prefix_order t =
  let rec traverse acc t = match t.term with
  | Var _ when List.memq t acc -> acc
  | Var _ -> t :: acc
  | BoundVar _ | Node (_, _, []) -> acc
  | Node (_, _, l) -> List.fold_left traverse acc l
  in List.rev (traverse [] t)

(** depth of term *)
let depth t =
  let rec depth t = match t.term with
  | Var _ | BoundVar _ -> 1
  | Node (_, _, l) -> 1 + depth_list 0 l
  and depth_list m l = match l with
  | [] -> m
  | t::l' -> depth_list (max m (depth t)) l'
  in depth t

let head t = match t.term with
  | Node (s, _, _) -> s
  | Var _ | BoundVar _ -> raise (Invalid_argument "Term.head")

let symbols ?(init=Symbol.Set.empty) t =
  Sequence.fold (fun set s -> Symbol.Set.add s set) init (Seq.symbols t)

(** Does t contains the symbol f? *)
let contains_symbol f t =
  Sequence.exists (Symbol.eq f) (Seq.symbols t)

(** {2 De Bruijn Indexes manipulations} *)

module DB = struct
  (* term is closed (ie all {!BoundVar} are properly scoped *)
  let closed ?(depth=0) t =
    let rec recurse depth t = match t.term with
    | BoundVar i -> i < depth
    | Node (_, _, [])
    | Var _ -> true
    | Node (_, _, l) -> recurse_list depth l
    and recurse_list depth l = match l with
    | [] -> true
    | x::l' -> recurse depth x && recurse_list depth l'
    in
    recurse depth t

  (* check whether t contains the De Bruijn symbol n *)
  let rec contains t n = match t.term with
    | BoundVar i -> i = n
    | Node (_, _, l) -> List.exists (fun t' -> contains t' n) l
    | Var _ -> false

  (* shift the non-captured De Bruijn indexes in the term by n *)
  let shift ?(depth=0) n t =
    let rec recurse t = 
      match t.term with
      | _ when is_ground t -> t  (* closed. *)
      | BoundVar i when i >= depth ->
        __mk_bound_var ~ty:t.ty (i+n) (* lift by n, term not captured *)
      | Node (_, _, []) | Var _ | BoundVar _ -> t
      | Node (s, tyargs, l) ->
        mk_node ~tyargs s (List.map recurse l)
    in
    assert (n >= 0);
    if n = 0 then t else recurse t

  (* unshift the term (decrement indices of all free De Bruijn variables inside) *)
  let unshift ?(depth=0) n t =
    (* only unlift DB symbol that are free. [depth] is the number of binders
       on the path from the root term. *)
    let rec recurse t =
      match t.term with
      | _ when is_ground t -> t
      | BoundVar i -> if i >= depth then __mk_bound_var ~ty:t.ty (i-n) else t
      | Node (_, _, []) | Var _ -> t
      | Node (s, tyargs, l) ->
        mk_node ~tyargs s (List.map recurse l)
    in recurse t

  (** Replace [sub] by a fresh De Bruijn index in [t]. *)
  let replace ?(depth=0) t ~sub =
    (* recurse and replace [sub]. *)
    let rec replace t = match t.term with
    | _ when eq t sub -> __mk_bound_var ~ty:t.ty depth
    | Var _
    | Node (_, _, [])
    | BoundVar _ -> t
    | Node (s, tyargs, l) ->
      mk_node ~tyargs s (List.map replace l)
    in
    replace t

  let from_var ?depth t ~var =
    assert (is_var var);
    replace ?depth t ~sub:var

  (* evaluate t in the given environment. *)
  let eval ?(depth=0) env t =
    let rec eval t = match t.term with
    | _ when is_ground t -> t
    | Var _
    | Node (_, _, []) -> t
    | BoundVar i ->
      begin match DBEnv.find env i with
        | None -> t  (* not bound *)
        | Some t' ->
          (if not (Type.eq t.ty t'.ty) then
            let msg = Util.sprintf "T.DB.eval at %d: distinct type %a and %a"
              i Type.pp t.ty Type.pp t'.ty in
            raise (Type.Error msg));
          shift depth t'
      end
    | Node (s, tyargs, l) ->
      mk_node ~tyargs s (List.map eval l)
    in
    eval t
end

(** {2 Fold} *)

let rec _all_pos_rec f vars acc pb t = match t.term with
  | Var _ | BoundVar _ ->
    if vars then f acc t (PB.to_pos pb) else acc
  | Node (hd, _, tl) ->
    let acc = f acc t (PB.to_pos pb) in  (* apply to term itself *)
    _all_pos_rec_list f vars acc pb tl 0
and _all_pos_rec_list f vars acc pb l i = match l with
  | [] -> acc
  | t::l' ->
    let acc = _all_pos_rec f vars acc (PB.add pb i) t in
    _all_pos_rec_list f vars acc pb l' (i+1)

let all_positions ?(vars=false) ?(pos=[]) t acc f =
  _all_pos_rec f vars acc (PB.of_pos pos) t

(** {2 Some AC-utils} *)

(** [flatten_ac f l] flattens the list of terms [l] by deconstructing all its
    elements that have [f] as head symbol. For instance, if l=[1+2; 3+(4+5)]
    with f="+", this will return [1;2;3;4;5], perhaps in a different order *)
let flatten_ac f l =
  let rec flatten acc l = match l with
  | [] -> acc
  | x::l' -> flatten (deconstruct acc x) l'
  and deconstruct acc t = match t.term with
  | Node (f', _, l') when Symbol.eq f f' ->
    flatten acc l'
  | _ -> t::acc
  in flatten [] l

(** normal form of the term modulo AC *)
let ac_normal_form ?(is_ac=fun s -> Symbol.has_flag Symbol.flag_ac s)
                   ?(is_com=fun s -> Symbol.has_flag Symbol.flag_commut s)
                   t =
  Util.enter_prof prof_ac_normal_form;
  let rec normalize t = match t.term with
    | Var _ -> t
    | BoundVar _ -> t
    | Node (f, tyargs, ([_;_] as l)) when is_ac f ->
      let l = flatten_ac f l in
      let l = List.map normalize l in
      let l = List.sort compare l in
      (match l with
        | x::l' -> List.fold_left
          (fun subt x -> mk_node ~tyargs f [x;subt])
          x l'
        | [] -> assert false)
    | Node (f, tyargs, [a;b]) when is_com f ->
      let a = normalize a in
      let b = normalize b in
      if compare a b > 0
        then mk_node ~tyargs f [b; a]
        else t
    | Node (f, tyargs, l) ->
      let l = List.map normalize l in
      mk_node ~tyargs f l
  in
  let t' = normalize t in
  Util.exit_prof prof_ac_normal_form;
  t'

(** Check whether the two terms are AC-equal. Optional arguments specify
    which symbols are AC or commutative (by default by looking at
    flag_ac and flag_commut) *)
let ac_eq ?(is_ac=fun s -> Symbol.has_flag Symbol.flag_ac s)
          ?(is_com=fun s -> Symbol.has_flag Symbol.flag_commut s)
          t1 t2 =
  let t1' = ac_normal_form ~is_ac ~is_com t1
  and t2' = ac_normal_form ~is_ac ~is_com t2 in
  t1' == t2'

let ac_symbols ~is_ac seq =
  let rec find set t = match t.term with
  | Var _
  | BoundVar _ -> set
  | Node (s, _, l) ->
    let set = if is_ac s then Symbol.Set.add s set else set in
    List.fold_left find set l
  in
  Sequence.fold find Symbol.Set.empty seq

(** {2 Printing/parsing} *)

let print_var_types = ref false
let print_all_types = ref false

type print_hook = (Buffer.t -> t -> unit) -> Buffer.t -> t -> bool

let pp_tstp_depth depth buf t =
  let depth = ref depth in
  (* recursive printing *)
  let rec pp_rec buf t = match t.term with
  | BoundVar i -> Printf.bprintf buf "Y%d" (!depth - i - 1)
  | Node (s, _, [a;b]) when Symbol.has_flag Symbol.flag_infix s ->
    Printf.bprintf buf "%a %a %a" pp_surrounded a Symbol.pp s pp_surrounded b
  | Node (s, _, body1::((_::_) as body)) when Symbol.has_flag Symbol.flag_infix s ->
    let sep = Util.sprintf " %a " Symbol.pp s in
    Printf.bprintf buf "%a%s%a" pp_surrounded body1 sep
      (Util.pp_list ~sep pp_surrounded) body
  | Node (s, [], []) -> Symbol.pp_tstp buf s
  | Node (s, tyargs, args) ->
    Printf.bprintf buf "%a(" Symbol.pp_tstp s;
    if not (Symbol.has_flag Symbol.flag_ad_hoc_poly s) then begin
      Util.pp_list Type.pp_tstp buf tyargs;
      (match tyargs, args with | _::_, _::_ -> Buffer.add_string buf ", " | _ -> ());
      end;
    Util.pp_list pp_rec buf args;
    Buffer.add_string buf ")"
  | Var i -> Printf.bprintf buf "X%d" i
  and pp_surrounded buf t = match t.term with
  | Node (s, _, _::_::_) when Symbol.has_flag Symbol.flag_infix s ->
    Buffer.add_char buf '('; pp_rec buf t; Buffer.add_char buf ')'
  | _ -> pp_rec buf t
  in
  pp_rec buf t

(* lightweight printing *)
let rec pp_depth ?(hooks=[]) depth buf t =
  let depth = ref depth in
  (* recursive printing *)
  let rec pp_rec buf t =
    begin match t.term with
    | BoundVar i -> Printf.bprintf buf "Y%d" (!depth - i - 1)
    | Node (s, _, [a;b]) when Symbol.has_flag Symbol.flag_infix s ->
      Printf.bprintf buf "%a %a %a" pp_surrounded a Symbol.pp s pp_surrounded b
    | Node (s, _, body1::((_::_) as body)) when Symbol.has_flag Symbol.flag_infix s ->
      let sep = Util.sprintf " %a " Symbol.pp s in
      Printf.bprintf buf "%a%s%a" pp_surrounded body1 sep
        (Util.pp_list ~sep pp_surrounded) body
    | Node (s, tyargs, args) ->
      (* try to use some hook *)
      if List.exists (fun hook -> hook pp_rec buf t) hooks
      then ()
      else (* default case for nodes *)
        begin match tyargs, args with
        | [], [] -> Symbol.pp buf s
        | _ ->
          Printf.bprintf buf "%a(" Symbol.pp_tstp s;
          if not (Symbol.has_flag Symbol.flag_ad_hoc_poly s) then begin
            Util.pp_list Type.pp_tstp buf tyargs;
            (match tyargs, args with | _::_, _::_ -> Buffer.add_string buf ", " | _ -> ());
            end;
          Util.pp_list pp_rec buf args;
          Buffer.add_string buf ")";
        end
    | Var i ->
      if not !print_all_types && !print_var_types && not (Type.eq t.ty Type.i)
        then Printf.bprintf buf "X%d:%a" i Type.pp t.ty
        else Printf.bprintf buf "X%d" i
    end;
    (* print type of term *)
    if !print_all_types
      then Printf.bprintf buf ":%a" Type.pp t.ty
  and pp_surrounded buf t = match t.term with
  | Node (s, _, _::_::_) when Symbol.has_flag Symbol.flag_infix s ->
    Buffer.add_char buf '('; pp_rec buf t; Buffer.add_char buf ')'
  | _ -> pp_rec buf t
  in
  pp_rec buf t

(* hook that prints arithmetic expressions *)
let arith_hook pp_rec buf t =
  let pp_surrounded buf t = match t.term with
  | Node (s, _, [_;_]) when
    Symbol.eq s Symbol.Arith.sum ||
    Symbol.eq s Symbol.Arith.product ||
    Symbol.eq s Symbol.Arith.difference ||
    Symbol.eq s Symbol.Arith.quotient ->
    Buffer.add_char buf '('; pp_rec buf t; Buffer.add_char buf ')'
  | _ -> pp_rec buf t
  in
  match t.term with
  | Node (s, _, [a; b]) when Symbol.eq s Symbol.Arith.less ->
    Printf.bprintf buf "%a < %a" pp_surrounded a pp_surrounded b; true
  | Node (s, _, [a; b]) when Symbol.eq s Symbol.Arith.lesseq ->
    Printf.bprintf buf "%a ≤ %a" pp_surrounded a pp_surrounded b; true
  | Node (s, _, [a; b]) when Symbol.eq s Symbol.Arith.greater ->
    Printf.bprintf buf "%a > %a" pp_surrounded a pp_surrounded b; true
  | Node (s, _, [a; b]) when Symbol.eq s Symbol.Arith.greatereq ->
    Printf.bprintf buf "%a ≥ %a" pp_surrounded a pp_surrounded b; true
  | Node (s, _, [a; b]) when Symbol.eq s Symbol.Arith.sum ->
    Printf.bprintf buf "%a + %a" pp_surrounded a pp_surrounded b; true
  | Node (s, _, [a; b]) when Symbol.eq s Symbol.Arith.difference ->
    Printf.bprintf buf "%a - %a" pp_surrounded a pp_surrounded b; true
  | Node (s, _, [a; b]) when Symbol.eq s Symbol.Arith.product ->
    Printf.bprintf buf "%a × %a" pp_surrounded a pp_surrounded b; true
  | Node (s, _, [a; b]) when Symbol.(eq s Arith.quotient) ->
    Printf.bprintf buf "%a / %a" pp_surrounded a pp_surrounded b; true
  | Node (s, _, [a; b]) when Symbol.(eq s Arith.quotient_e) ->
    Printf.bprintf buf "%a // %a" pp_surrounded a pp_surrounded b; true
  | Node (s, _, [a]) when Symbol.eq s Symbol.Arith.uminus ->
    Printf.bprintf buf "-%a" pp_surrounded a; true;
  | Node (s, _, [a;b]) when Symbol.eq s Symbol.Arith.remainder_e ->
    Printf.bprintf buf "%a mod %a" pp_surrounded a pp_surrounded b; true;
  | _ -> false  (* default *)

let pp_debug buf t = pp_depth 0 buf t

let pp_tstp buf t = pp_tstp_depth 0 buf t

let pp_arith buf t =
  pp_depth ~hooks:[arith_hook] 0 buf t

let __default_pp = ref pp_debug

let pp buf t = !__default_pp buf t

let set_default_pp pp = __default_pp := pp

let to_string t = Util.sprintf "%a" pp t

let fmt fmt t = Format.pp_print_string fmt (to_string t)

let rec debug fmt t =
  begin match t.term with
  | Var i -> Format.fprintf fmt "X%d" i
  | BoundVar i -> Format.fprintf fmt "Y%d" i
  | Node (s, [], []) ->
    Format.pp_print_string fmt (Symbol.to_string s)
  | Node (s, tyargs, l) ->
    Format.fprintf fmt "(%s %a %a)" (Symbol.to_string s)
      (Sequence.pp_seq Type.fmt) (Sequence.of_list tyargs)
      (Sequence.pp_seq debug) (Sequence.of_list l)
  end;
  Format.fprintf fmt ":%a" Type.fmt t.ty

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
