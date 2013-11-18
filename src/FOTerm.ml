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

let prof_mk_node = Util.mk_profiler "Term.mk_node"
let prof_ac_normal_form = Util.mk_profiler "ac_normal_form"

type t = {
  term : term_cell;       (** the term itself *)
  mutable ty : Type.t;    (** type of the term *)
  mutable tsize : int;    (** size (number of subterms) *)
  mutable flags : int;    (** boolean flags about the term *)
  mutable tag : int;      (** hashconsing tag *)
}

and term_cell =
  | Var of int                  (** variable *)
  | BoundVar of int             (** bound variable (De Bruijn index) *)
  | Node of Symbol.t * Type.t list * t list   (** term application *)
and sourced_term =
  t * string * string           (** Term + file,name *)

type term = t

type varlist = t list

let rec hash_novar t =
  let h_type = Type.hash t.ty in
  let h_term = match t.term with
  | Var _ -> 42
  | BoundVar _ -> 43
  | Node (s, _, l) ->
    let h = Symbol.hash s in
    Hash.hash_list hash_novar h l
  in
  Hash.combine h_type h_term

(** {2 Comparison, equality, containers} *)

let subterm ~sub t =
  let rec check t =
    sub == t ||
    match t.term with
    | Var _ | BoundVar _ | Node (_, _, []) -> false
    | Node (_, _, subterms) -> List.exists check subterms
  in
  check t

let eq x y = x == y  (* because of hashconsing *)

let compare x y = x.tag - y.tag

let hash x = x.tag

let ty t = t.ty

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

module TCache = Cache.Replacing(TermHASH)
module T2Cache = Cache.Replacing2(TermHASH)(TermHASH)

(** {2 Global terms table (hashconsing)} *)

(* structural hash *)
let hash_term t =
  match t.term with
  | Var i -> Hash.hash_int i
  | BoundVar i -> Hash.hash_int2 22 (Hash.hash_int i)
  | Node (s, tys, []) -> Hash.hash_list Type.hash (Symbol.hash s) tys
  | Node (s, tys, l) ->
    let h = Hash.hash_list Type.hash (Symbol.hash s) tys in
    Hash.hash_list (fun x -> x.tag) h l

(* structural eq *)
let hashcons_equal x y =
  (* pairwise comparison of subterms *)
  let rec eq_subterms a b = match a, b with
    | [], [] -> true
    | a::a1, b::b1 -> a == b && eq_subterms a1 b1
    | _ -> false
  and eq_types a b = match a, b with
    | [], [] -> true
    | xa::a', xb::b' -> Type.eq xa xb && eq_types a' b'
    | _ -> false
  in
  (* compare type args and subterms, if same structure *)
  match x.term, y.term with
  | Var i, Var j
  | BoundVar i, BoundVar j -> i = j && Type.eq x.ty y.ty
  | Node (sa, [], []), Node (sb, [], []) -> Symbol.eq sa sb
  | Node (_, _, []), Node (_, _, _::_)
  | Node (_, _, _::_), Node (_, _, []) -> false
  | Node (sa, tysa, la), Node (sb, tysb, lb) ->
    Symbol.eq sa sb && eq_subterms la lb && eq_types tysa tysb
  | _ -> false

(** hashconsing for terms *)
(* module H  = Hashcons.Make(struct *)
module H = Hashcons.Make(struct
  type t = term

  let equal x y = hashcons_equal x y

  let hash t = hash_term t

  let tag i t = (assert (t.tag = -1); t.tag <- i)
end)

(** {2 Boolean flags} *)

let __gen = Util.Flag.create ()
let new_flag () = Util.Flag.get_new __gen

let flag_ground = new_flag ()
let flag_monomorphic = new_flag ()

let set_flag flag t truth =
  if truth
    then t.flags <- t.flags lor flag
    else t.flags <- t.flags land (lnot flag)

let get_flag flag t = (t.flags land flag) != 0

(** {2 Typing} *)

let cast t ty =
  begin match t.term with
    | Var _ | BoundVar _ -> ()
    | _ -> raise (Invalid_argument "cast")
  end;
  H.hashcons { t with ty ; tag = ~-1; }

(** {2 Smart constructors} *)

(** In this section, term smart constructors are defined. They perform
    hashconsing, and precompute some properties (flags).
    
    TODO: flag_monomorphic *)

let mk_var ~ty idx =
  assert (idx >= 0);
  let my_v = {term = Var idx; ty; tsize = 1;
              flags= 0; tag= -1} in
  H.hashcons my_v

let __mk_bound_var ~ty idx =
  assert (idx >= 0);
  let my_v = {term = BoundVar idx; ty; tsize = 1;
              flags=0; tag= -1} in
  H.hashcons my_v

let rec compute_is_ground l = match l with
  | [] -> true
  | x::l' -> (get_flag flag_ground x) && compute_is_ground l'

let rec compute_tsize l = match l with
  | [] -> 1  (* with the initial symbol! *)
  | x::l' -> x.tsize + compute_tsize l'

(* compute type of s(tyargs @ l) *)
let _compute_ty s tyargs l =
  match tyargs, l with
    | [], [] -> Symbol.ty s
    | _ ->
      let all_ty_args = tyargs @ (List.map ty l) in
      Type.apply (Symbol.ty s) all_ty_args

let mk_node ?(tyargs=[]) s l =
  Util.enter_prof prof_mk_node;
  let ty = _compute_ty s tyargs l in
  (* hashcons term *)
  let t = match l with
  | [] ->
    let my_t = {term=Node (s, tyargs, l); ty; flags=flag_ground;
               tsize=1; tag= -1} in
    let t = H.hashcons my_t in
    t
  | _::_ ->
    let my_t = {term=Node (s, tyargs, l); ty; flags=0;
               tsize=0; tag= -1} in
    let t = H.hashcons my_t in
    if t == my_t
      then begin
        (* compute meta-data: groundness, size *)
        set_flag flag_ground t (compute_is_ground l);
        t.tsize <- compute_tsize l;
      end;
    t
  in
  Util.exit_prof prof_mk_node;
  t

let mk_const ?(tyargs=[]) s = mk_node ~tyargs s []

let true_term = mk_const Symbol.true_symbol
let false_term = mk_const Symbol.false_symbol

(** {2 Subterms and positions} *)

let is_var t = match t.term with
  | Var _ -> true
  | _ -> false

let is_bound_var t = match t.term with
  | BoundVar _ -> true
  | _ -> false

let is_const t = match t.term with
  | Node (s, _, []) -> true
  | _ -> false

let is_node t = match t.term with
  | Node _ -> true
  | _ -> false

let rec at_pos t pos = match t.term, pos with
  | _, [] -> t
  | Var _, _::_ -> invalid_arg "wrong position in term"
  | Node (_, _, l), i::subpos when i < List.length l ->
    at_pos (List.nth l i) subpos
  | _ -> invalid_arg "index too high for subterm"

let rec replace_pos t pos new_t = match t.term, pos with
  | _, [] -> new_t
  | (Var _ | BoundVar _), _::_ -> invalid_arg "wrong position in term"
  | Node (s, tyargs, l), i::subpos when i < List.length l ->
    let new_subterm = replace_pos (Util.list_get l i) subpos new_t in
    mk_node ~tyargs s (Util.list_set l i new_subterm)
  | _ -> invalid_arg "index too high for subterm"

(** [replace t ~old ~by] syntactically replaces all occurrences of [old]
    in [t] by the term [by]. *)
let rec replace t ~old ~by = match t.term with
  | _ when eq t old -> by
  | Var _ | BoundVar _ -> t
  | Node (s, tyargs, l) ->
    let l' = List.map (fun t' -> replace t' ~old ~by) l in
    mk_node ~tyargs s l'

(** Size of the term (number of subterms) *)
let size t = t.tsize

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

let rec var_occurs x t = match t.term with
  | Var _ | BoundVar _ -> x == t
  | Node (s, _, []) -> false
  | Node (s, _, l) -> List.exists (var_occurs x) l

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
  let rec add set t = match t.term with
  | BoundVar _ | Node (_, _, []) -> ()
  | Var _ -> Tbl.replace set t ()
  | Node (_, _, l) -> add_list set l
  and add_list set l = match l with
  | [] -> ()
  | x::l' -> add set x; add_list set l'
  in
  add set t

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
  | Var _ | BoundVar _ -> raise (Invalid_argument "Term.head: variable")

let symbols ?(init=Symbol.Set.empty) seq =
  let rec symbols set t = match t.term with
    | Var _ | BoundVar _ -> set
    | Node (s, _, l) ->
      let set = Symbol.Set.add s set in
      List.fold_left symbols set l
  in
  Sequence.fold symbols init seq

(** Does t contains the symbol f? *)
let rec contains_symbol f t =
  match t.term with
  | Var _ | BoundVar _ -> false
  | Node (g, _, ts) -> Symbol.eq g f || List.exists (contains_symbol f) ts

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
    Util.pp_list Type.pp_tstp buf tyargs;
    (match tyargs, args with | _::_, _::_ -> Buffer.add_string buf ", " | _ -> ());
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
          Util.pp_list Type.pp_tstp buf tyargs;
          (match tyargs, args with | _::_, _::_ -> Buffer.add_string buf ", " | _ -> ());
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
  | Node (s, _, [a; b]) when Symbol.eq s Symbol.Arith.quotient ->
    Printf.bprintf buf "%a / %a" pp_surrounded a pp_surrounded b; true
  | Node (s, _, [a]) when Symbol.eq s Symbol.Arith.uminus ->
    Printf.bprintf buf "-%a" pp_surrounded a; true;
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
