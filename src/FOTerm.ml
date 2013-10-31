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

(** term *)
type t = {
  term : term_cell;       (** the term itself *)
  ty : Type.t;            (** type of the term *)
  mutable tsize : int;    (** size (number of subterms) *)
  mutable flags : int;    (** boolean flags about the term *)
  mutable tag : int;      (** hashconsing tag *)
}
(** content of the term *)
and term_cell =
  | Var of int                  (** variable *)
  | BoundVar of int             (** bound variable (De Bruijn index) *)
  | Node of Symbol.t * t list   (** term application *)
and sourced_term =
  t * string * string           (** Term + file,name *)

type term = t

(** list of variables *)
type varlist = t list

let hash_term t =
  let h_type = Type.hash t.ty in
  let h_term = match t.term with
  | Var i -> Hash.hash_int i
  | BoundVar i -> Hash.hash_int2 22 (Hash.hash_int i)
  | Node (s, []) -> Symbol.hash s
  | Node (s, l) -> Hash.hash_list (fun x -> x.tag) (Symbol.hash s) l
  in
  Hash.combine h_type h_term

let rec hash_novar t =
  let h_type = Type.hash t.ty in
  let h_term = match t.term with
  | Var _ -> 42
  | BoundVar _ -> 43
  | Node (s, l) ->
    let h = Symbol.hash s in
    Hash.hash_list hash_novar h l
  in
  Hash.combine h_type h_term

(** {2 Comparison, equality, containers} *)

let subterm ~sub t =
  let rec check t =
    sub == t ||
    match t.term with
    | Var _ | BoundVar _ | Node (_, []) -> false
    | Node (_, subterms) -> List.exists check subterms
  in
  check t

let eq x y = x == y  (* because of hashconsing *)

let compare x y = x.tag - y.tag

let hash x = x.tag

let get_type t = t.ty

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

let hashcons_equal x y =
  (* pairwise comparison of subterms *)
  let rec eq_subterms a b = match a, b with
    | [], [] -> true
    | a::a1, b::b1 -> a == b && eq_subterms a1 b1
    | _, _ -> false
  in
  (* compare types *)
  Type.eq x.ty y.ty &&
  (* compare types and subterms, if same structure *)
  match x.term, y.term with
  | Var i, Var j
  | BoundVar i, BoundVar j -> i = j
  | Node (sa, []), Node (sb, []) -> Symbol.eq sa sb
  | Node (_, []), Node (_, _::_)
  | Node (_, _::_), Node (_, []) -> false
  | Node (sa, la), Node (sb, lb) -> Symbol.eq sa sb && eq_subterms la lb
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

let mk_bound_var ~ty idx =
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

let mk_node ~ty s l =
  Util.enter_prof prof_mk_node;
  let t = match l with
  | [] ->
    let my_t = {term=Node (s, l); ty; flags=flag_ground;
               tsize=1; tag= -1} in
    H.hashcons my_t
  | _::_ ->
    let my_t = {term=Node (s, l); ty; flags=0;
               tsize=0; tag= -1} in
    let t = H.hashcons my_t in
    if t == my_t
      then begin
        (* compute ground-ness of term and size *)
        set_flag flag_ground t (compute_is_ground l);
        t.tsize <- compute_tsize l;
      end;
    t
  in
  Util.exit_prof prof_mk_node;
  t

let mk_const ~ty s = mk_node ~ty s []

let true_term = mk_const ~ty:Type.o Symbol.true_symbol
let false_term = mk_const ~ty:Type.o Symbol.false_symbol

(** {2 Subterms and positions} *)

let is_var t = match t.term with
  | Var _ -> true
  | _ -> false

let is_bound_var t = match t.term with
  | BoundVar _ -> true
  | _ -> false

let is_const t = match t.term with
  | Node (s, []) -> true
  | _ -> false

let is_node t = match t.term with
  | Node _ -> true
  | _ -> false

let rec at_pos t pos = match t.term, pos with
  | _, [] -> t
  | Var _, _::_ -> invalid_arg "wrong position in term"
  | Node (_, l), i::subpos when i < List.length l ->
    at_pos (List.nth l i) subpos
  | _ -> invalid_arg "index too high for subterm"

let rec replace_pos t pos new_t = match t.term, pos with
  | _, [] -> new_t
  | (Var _ | BoundVar _), _::_ -> invalid_arg "wrong position in term"
  | Node (s, l), i::subpos when i < List.length l ->
    let new_subterm = replace_pos (Util.list_get l i) subpos new_t in
    mk_node ~ty:t.ty s (Util.list_set l i new_subterm)
  | _ -> invalid_arg "index too high for subterm"

(** [replace t ~old ~by] syntactically replaces all occurrences of [old]
    in [t] by the term [by]. *)
let rec replace t ~old ~by = match t.term with
  | _ when t == old -> by
  | Var _ | BoundVar _ -> t
  | Node (s, l) ->
    let l' = List.map (fun t' -> replace t' ~old ~by) l in
    mk_node ~ty:t.ty s l'

(** Size of the term (number of subterms) *)
let size t = t.tsize

(** get subterm by its position *)
let at_cpos t pos = 
  let rec recurse t pos =
    match t.term, pos with
    | _, 0 -> t
    | Node (_, l), _ -> get_subpos l (pos - 1)
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
  | Var _ | BoundVar _ | Node (_, []) -> true
  | Node (_, l) -> List.for_all monomorphic l

let rec var_occurs x t = match t.term with
  | Var _ | BoundVar _ -> x == t
  | Node (s, []) -> false
  | Node (s, l) -> List.exists (var_occurs x) l

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
  | BoundVar _ | Node (_, []) -> ()
  | Var _ -> Tbl.replace set t ()
  | Node (_, l) -> add_list set l
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
  | BoundVar _ | Node (_, []) -> acc
  | Node (_, l) -> List.fold_left traverse acc l
  in List.rev (traverse [] t)

(** depth of term *)
let depth t =
  let rec depth t = match t.term with
  | Var _ | BoundVar _ -> 1
  | Node (_, l) -> 1 + depth_list 0 l
  and depth_list m l = match l with
  | [] -> m
  | t::l' -> depth_list (max m (depth t)) l'
  in depth t

let rec head t = match t.term with
  | Node (s, _) -> s
  | Var _ | BoundVar _ -> raise (Invalid_argument "Term.head: variable")

let symbols seq =
  let rec symbols set t = match t.term with
    | Var _ | BoundVar _ -> set
    | Node (s, l) ->
      let set = Symbol.Set.add s set in
      List.fold_left symbols set l
  in
  Sequence.fold symbols Symbol.Set.empty seq

(** Does t contains the symbol f? *)
let rec contains_symbol f t =
  match t.term with
  | Var _ | BoundVar _ -> false
  | Node (g, ts) -> Symbol.eq g f || List.exists (contains_symbol f) ts

(** {2 De Bruijn Indexes manipulations} *)

let db_closed ?(depth=0) t =
  let rec recurse depth t = match t.term with
  | BoundVar i -> i < depth
  | Node (_, [])
  | Var _ -> true
  | Node (_, l) -> recurse_list depth l
  and recurse_list depth l = match l with
  | [] -> true
  | x::l' -> recurse depth x && recurse_list depth l'
  in
  recurse depth t

(* check whether t contains the De Bruijn symbol n *)
let rec db_contains t n = match t.term with
  | BoundVar i -> i = n
  | Node (_, l) -> List.exists (fun t' -> db_contains t' n) l
  | Var _ -> false

(* lift the non-captured De Bruijn indexes in the term by n *)
let db_lift ?(depth=0) n t =
  (* traverse the term, looking for non-captured DB indexes.
     [depth] is the number of binders on the path from the root of the
     term, to the current position. *)
  let rec recurse depth t = 
    match t.term with
    | _ when is_ground t -> t  (* closed. *)
    | BoundVar i when i >= depth ->
      mk_bound_var ~ty:t.ty (i+n) (* lift by n, term not captured *)
    | Node (_, []) | Var _ | BoundVar _ -> t
    | Node (s, l) ->
      mk_node ~ty:t.ty s (List.map (recurse depth) l)
  in
  assert (n >= 0);
  if depth=0 && n = 0 then t else recurse depth t

(* replace 0 by [by] into [into] *)
let db_replace ?(depth=0) ~into ~by =
  (* replace db by s in t *)
  let rec replace depth s t = match t.term with
  | BoundVar n ->
    if n = depth
      then db_lift depth s   (* free vars must be lifted *)
      else t
  | Node (_, [])
  | Var _ -> t
  | Node (f, l) ->
    mk_node ~ty:t.ty f (List.map (replace depth s) l)
  in
  replace depth by into

(* unlift the term (decrement indices of all free De Bruijn variables inside *)
let db_unlift ?(depth=0) t =
  (* only unlift DB symbol that are free. [depth] is the number of binders
     on the path from the root term. *)
  let rec recurse depth t =
    match t.term with
    | _ when is_ground t -> t
    | BoundVar i -> if i >= depth then mk_bound_var ~ty:t.ty (i-1) else t
    | Node (_, []) | Var _ -> t
    | Node (s, l) ->
      mk_node ~ty:t.ty s (List.map (recurse depth) l)
  in recurse depth t

(** Replace [t'] by a fresh De Bruijn index in [t]. *)
let db_from_term ?(depth=0) t t' =
  (* recurse and replace [t']. *)
  let rec replace depth t = match t.term with
  | _ when t == t' -> mk_bound_var ~ty:t.ty depth
  | Var _
  | Node (_, [])
  | BoundVar _ -> t
  | Node (s, l) ->
    mk_node ~ty:t.ty s (List.map (replace depth) l)
  in
  replace depth t

(** [db_from_var t v] replace v by a De Bruijn symbol in t.
  Same as db_from_term. *)
let db_from_var ?depth t v =
  assert (is_var v);
  db_from_term ?depth t v

(** {2 Fold} *)

let rec _all_pos_rec f vars acc pb t = match t.term with
  | Var _ | BoundVar _ ->
    if vars then f acc t (PB.to_pos pb) else acc
  | Node (hd, tl) ->
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
  | Node (f', l') when Symbol.eq f f' ->
    flatten acc l'
  | _ -> t::acc
  in flatten [] l

(** normal form of the term modulo AC *)
let ac_normal_form ?(is_ac=fun s -> Symbol.has_attr Symbol.attr_ac s)
                   ?(is_com=fun s -> Symbol.has_attr Symbol.attr_commut s)
                   t =
  Util.enter_prof prof_ac_normal_form;
  let rec normalize t = match t.term with
    | Var _ -> t
    | BoundVar _ -> t
    | Node (f, ([_;_] as l)) when is_ac f ->
      let l = flatten_ac f l in
      let l = List.map normalize l in
      let l = List.sort compare l in
      (match l with
        | x::l' -> List.fold_left
          (fun subt x -> mk_node ~ty:t.ty f [x;subt])
          x l'
        | [] -> assert false)
    | Node (f, [a;b]) when is_com f ->
      let a = normalize a in
      let b = normalize b in
      if compare a b > 0
        then mk_node ~ty:t.ty f [b; a]
        else t
    | Node (f, l) ->
      let l = List.map normalize l in
      mk_node ~ty:t.ty f l
  in
  let t' = normalize t in
  Util.exit_prof prof_ac_normal_form;
  t'

(** Check whether the two terms are AC-equal. Optional arguments specify
    which symbols are AC or commutative (by default by looking at
    attr_ac and attr_commut) *)
let ac_eq ?(is_ac=fun s -> Symbol.has_attr Symbol.attr_ac s)
          ?(is_com=fun s -> Symbol.has_attr Symbol.attr_commut s)
          t1 t2 =
  let t1' = ac_normal_form ~is_ac ~is_com t1
  and t2' = ac_normal_form ~is_ac ~is_com t2 in
  t1' == t2'

let ac_symbols ~is_ac seq =
  let rec find set t = match t.term with
  | Var _
  | BoundVar _ -> set
  | Node (s, l) ->
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
  | Node (s, [{term=Node (s', [a;b])}]) when Symbol.eq s Symbol.not_symbol
    && Symbol.eq s' Symbol.equiv_symbol ->
    Printf.bprintf buf "%a <~> %a" pp_surrounded a pp_surrounded b
  | Node (s, [{term=Node (s', [a; b])}])
    when Symbol.eq s Symbol.not_symbol && Symbol.eq s' Symbol.eq_symbol ->
    Printf.bprintf buf "%a != %a" pp_surrounded a pp_surrounded b
  | Node (s, [t]) when Symbol.eq s Symbol.not_symbol ->
    Printf.bprintf buf "%a%a" Symbol.pp s pp_rec t
  | Node (s, [a;b]) when Symbol.has_attr Symbol.attr_infix s ->
    Printf.bprintf buf "%a %a %a" pp_surrounded a Symbol.pp s pp_surrounded b
  | Node (s, body1::((_::_) as body)) when Symbol.has_attr Symbol.attr_infix s ->
    let sep = Util.sprintf " %a " Symbol.pp s in
    Printf.bprintf buf "%a%s%a" pp_surrounded body1 sep
      (Util.pp_list ~sep pp_surrounded) body
  | Node (s, []) -> Symbol.pp_tstp buf s
  | Node (s, args) ->
    Printf.bprintf buf "%a(%a)" Symbol.pp_tstp s (Util.pp_list ~sep:", " pp_rec) args
  | Var i -> Printf.bprintf buf "X%d" i
  and pp_surrounded buf t = match t.term with
  | Node (s, _::_::_) when Symbol.has_attr Symbol.attr_infix s ->
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
    | Node (s, [{term=Node (s', [a;b])}]) when Symbol.eq s Symbol.not_symbol
      && Symbol.eq s' Symbol.equiv_symbol ->
      Printf.bprintf buf "%a <~> %a" pp_surrounded a pp_surrounded b
    | Node (s, [{term=Node (s', [a; b])}])
      when Symbol.eq s Symbol.not_symbol && Symbol.eq s' Symbol.eq_symbol ->
      Printf.bprintf buf "%a ≠ %a" pp_surrounded a pp_surrounded b
    | Node (s, [t]) when Symbol.eq s Symbol.not_symbol ->
      Printf.bprintf buf "%a%a" Symbol.pp s pp_rec t
    | Node (s, [a;b]) when Symbol.has_attr Symbol.attr_infix s ->
      Printf.bprintf buf "%a %a %a" pp_surrounded a Symbol.pp s pp_surrounded b
    | Node (s, body1::((_::_) as body)) when Symbol.has_attr Symbol.attr_infix s ->
      let sep = Util.sprintf " %a " Symbol.pp s in
      Printf.bprintf buf "%a%s%a" pp_surrounded body1 sep
        (Util.pp_list ~sep pp_surrounded) body
    | Node (s, args) ->
      (* try to use some hook *)
      if List.exists (fun hook -> hook pp_rec buf t) hooks
      then ()
      else begin
        (* default case for nodes *)
        match args with
        | [] -> Symbol.pp buf s
        | _::_ ->
          Printf.bprintf buf "%a(%a)" Symbol.pp s (Util.pp_list ~sep:", " pp_rec) args
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
  | Node (s, _::_::_) when Symbol.has_attr Symbol.attr_infix s ->
    Buffer.add_char buf '('; pp_rec buf t; Buffer.add_char buf ')'
  | _ -> pp_rec buf t
  in
  pp_rec buf t

(* hook that prints arithmetic expressions *)
let arith_hook pp_rec buf t =
  let pp_surrounded buf t = match t.term with
  | Node (s, [_;_]) when
    Symbol.eq s Symbol.Arith.sum ||
    Symbol.eq s Symbol.Arith.product ||
    Symbol.eq s Symbol.Arith.difference ||
    Symbol.eq s Symbol.Arith.quotient ->
    Buffer.add_char buf '('; pp_rec buf t; Buffer.add_char buf ')'
  | _ -> pp_rec buf t
  in
  match t.term with
  | Node (s, [a; b]) when Symbol.eq s Symbol.Arith.less ->
    Printf.bprintf buf "%a < %a" pp_surrounded a pp_surrounded b; true
  | Node (s, [a; b]) when Symbol.eq s Symbol.Arith.lesseq ->
    Printf.bprintf buf "%a ≤ %a" pp_surrounded a pp_surrounded b; true
  | Node (s, [a; b]) when Symbol.eq s Symbol.Arith.greater ->
    Printf.bprintf buf "%a > %a" pp_surrounded a pp_surrounded b; true
  | Node (s, [a; b]) when Symbol.eq s Symbol.Arith.greatereq ->
    Printf.bprintf buf "%a ≥ %a" pp_surrounded a pp_surrounded b; true
  | Node (s, [a; b]) when Symbol.eq s Symbol.Arith.sum ->
    Printf.bprintf buf "%a + %a" pp_surrounded a pp_surrounded b; true
  | Node (s, [a; b]) when Symbol.eq s Symbol.Arith.difference ->
    Printf.bprintf buf "%a - %a" pp_surrounded a pp_surrounded b; true
  | Node (s, [a; b]) when Symbol.eq s Symbol.Arith.product ->
    Printf.bprintf buf "%a × %a" pp_surrounded a pp_surrounded b; true
  | Node (s, [a; b]) when Symbol.eq s Symbol.Arith.quotient ->
    Printf.bprintf buf "%a / %a" pp_surrounded a pp_surrounded b; true
  | Node (s, [a]) when Symbol.eq s Symbol.Arith.uminus ->
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
  | Node (s, []) ->
    Format.pp_print_string fmt (Symbol.to_string s)
  | Node (s, l) ->
    Format.fprintf fmt "(%s %a)" (Symbol.to_string s)
      (Sequence.pp_seq debug) (Sequence.of_list l)
  end;
  Format.fprintf fmt ":%a" Type.fmt t.ty

let bij =
  let open Bij in
  fix
    (fun bij ->
      let bij_node = lazy (triple Symbol.bij (list_ (Lazy.force bij)) Type.bij) in
      let bij_var = pair int_ Type.bij in
      switch
        ~inject:(fun t -> match t.term with
        | BoundVar i -> "bv", BranchTo (bij_var, (i,t.ty))
        | Var i -> "v", BranchTo (bij_var, (i, t.ty))
        | Node (s, l) -> "n", BranchTo (Lazy.force bij_node, (s, l, t.ty)))
        ~extract:(function
        | "bv" -> BranchFrom (bij_var, fun (i,ty) -> mk_bound_var ~ty i)
        | "v" -> BranchFrom (bij_var, fun (i,ty) -> mk_var ~ty i)
        | "n" -> BranchFrom (Lazy.force bij_node, fun (s,l,ty) -> mk_node ~ty s l)
        | _ -> raise (DecodingError "expected Term")))

module UT = Untyped.FO

let erase_types ?(depth=0) t =
  let rec erase t = match t.term with
  | Var i -> UT.var ~ty:(Type.to_parsed t.ty) (Util.sprintf "X%d" i)
  | BoundVar i -> UT.var ~ty:(Type.to_parsed t.ty) (Util.sprintf "Y%d" (depth-i-1))
  | Node (s, l) -> UT.app s (List.map erase l)
  in
  erase t
