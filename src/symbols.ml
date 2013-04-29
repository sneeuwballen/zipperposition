(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(** {1 Symbols and signature} *)

type symbol_attribute = int

(** {2 Boolean attributes} *)

let attr_skolem = 1 lsl 0
let attr_split = 1 lsl 1
let attr_binder = 1 lsl 2
let attr_infix = 1 lsl 3
let attr_ac = 1 lsl 4
let attr_multiset = 1 lsl 5
let attr_fresh_const = 1 lsl 6
let attr_commut = 1 lsl 7
let attr_polymorphic = 1 lsl 8
let attr_num = 1 lsl 9

(** {2 Definition of symbols and sorts} *)

type symbol = {
  symb_val : symbol_val;
  mutable symb_id : int;
  mutable symb_attrs : int;
} (** A symbol is a string, a unique ID, and some attributes *)
and symbol_val =
  | Const of string
  | Distinct of string
  | Num of Num.num
  | Real of float
  (** A symbol value is a string, a quoted string, or a number *)

let compare_symbol_val sv1 sv2 = match sv1, sv2 with
  | Const s1, Const s2 -> String.compare s1 s2
  | Distinct s1, Distinct s2 -> String.compare s1 s2
  | Num n1, Num n2 -> Num.compare_num n1 n2
  | Real f1, Real f2 -> compare f1 f2
  | Const _, _ -> 1
  | Distinct _, _ -> 1
  | Num _, _ -> 1
  | Real _, _ -> -1

let compare_symbols s1 s2 = s1.symb_id - s2.symb_id
  (** Comparison after hashconsing *)

let hash_symbol s = s.symb_id

(** weak hash table for symbols *)
module HashSymbol = Hashcons.Make(
  struct
    type t = symbol
    let equal s1 s2 = compare_symbol_val s1.symb_val s2.symb_val = 0
    let hash s = Hashtbl.hash s.symb_val
    let tag t s = (s.symb_id <- t; s)
  end)

let mk_symbol ?(attrs=0) s =
  let s = {
    symb_val = Const s;
    symb_id = 0;
    symb_attrs = attrs;
  } in
  HashSymbol.hashcons s

let mk_distinct ?(attrs=0) s =
  let s = {
    symb_val = Distinct s;
    symb_id = 0;
    symb_attrs = attrs;
  } in
  HashSymbol.hashcons s

let mk_num ?(attrs=0) n =
  let s = {
    symb_val = Num n;
    symb_id = 0;
    symb_attrs = attrs lor attr_polymorphic lor attr_num;
  } in
  HashSymbol.hashcons s

let parse_num ?attrs str =
  let n = Num.num_of_string str in
  mk_num ?attrs n

let mk_int ?(attrs=0) i =
  let s = {
    symb_val = Num (Num.num_of_int i);
    symb_id = 0;
    symb_attrs = attrs lor attr_polymorphic lor attr_num;
  } in
  HashSymbol.hashcons s

let mk_real ?(attrs=0) f =
  let s = {
    symb_val = Real f;
    symb_id = 0;
    symb_attrs = attrs lor attr_polymorphic lor attr_num;
  } in
  HashSymbol.hashcons s

let is_const s = match s.symb_val with
  | Const _ -> true | _ -> false

let is_int s = match s.symb_val with
  | Num n -> Num.is_integer_num n | _ -> false

let is_rat s = match s.symb_val with
  | Num n -> not (Num.is_integer_num n) | _ -> false

let is_real s = match s.symb_val with
  | Real f -> true | _ -> false

let is_numeric s = match s.symb_val with
  | Real _ | Num _ -> true | _ -> false

let is_distinct s = match s.symb_val with
  | Distinct _ -> true | _ -> false

(** Arithmetic (assumes the symbols verify {!is_numeric}) *)
module Arith = struct
  exception TypeMismatch

  open Num

  let sign s = match s.symb_val with
  | Num n -> sign_num n
  | Real f when f > 0. -> 1
  | Real f when f < 0. -> 1
  | Real f -> 0
  | _ -> raise TypeMismatch

  let floor s = match s.symb_val with
  | Num n -> mk_num (floor_num n)
  | Real f -> mk_real (floor f)
  | _ -> raise TypeMismatch

  let ceiling s = match s.symb_val with
  | Num n -> mk_num (ceiling_num n)
  | Real f -> mk_real (ceil f)
  | _ -> raise TypeMismatch

  let truncate s = match s.symb_val with
  | Num n when sign_num n >= 0 -> mk_num (floor_num n)
  | Num n -> mk_num (minus_num (floor_num (abs_num n)))
  | Real f -> mk_num (num_of_int (truncate f))
  | _ -> raise TypeMismatch

  let round s = match s.symb_val with
  | Num n -> mk_num (round_num n)
  | Real f ->
    let f' = Pervasives.floor f in
    let i = if f -. f' > 0.5 then int_of_float f' else (int_of_float f') + 1 in
    mk_num (num_of_int i)
  | _ -> raise TypeMismatch

  let prec s = match s.symb_val with
  | Num n -> mk_num (pred_num n)
  | Real f -> mk_real (f -. 1.)
  | _ -> raise TypeMismatch

  let succ s = match s.symb_val with
  | Num n -> mk_num (succ_num n)
  | Real f -> mk_real (f +. 1.)
  | _ -> raise TypeMismatch

  let one_i = mk_num (num_of_int 1)
  let zero_i = mk_num (num_of_int 0)
  let one_f = mk_real 1.
  let zero_f = mk_real 0.

  let is_zero s = match s.symb_val with
  | Num n -> Num.sign_num n = 0
  | Real f -> f = 0.
  | _ -> raise TypeMismatch

  let sum s1 s2 = match s1.symb_val, s2.symb_val with
  | Num n1, Num n2 -> mk_num (n1 +/ n2)
  | Real f1, Real f2 -> mk_real (f1 +. f2)
  | _ -> raise TypeMismatch

  let difference s1 s2 = match s1.symb_val, s2.symb_val with
  | Num n1, Num n2 -> mk_num (n1 -/ n2)
  | Real f1, Real f2 -> mk_real (f1 -. f2)
  | _ -> raise TypeMismatch

  let uminus s = match s.symb_val with
  | Num n -> mk_num (minus_num n)
  | Real f -> mk_real (~-. f)
  | _ -> raise TypeMismatch

  let product s1 s2 = match s1.symb_val, s2.symb_val with
  | Num n1, Num n2 -> mk_num (n1 */ n2)
  | Real f1, Real f2 -> mk_real (f1 *. f2)
  | _ -> raise TypeMismatch

  let quotient s1 s2 = match s1.symb_val, s2.symb_val with
  | Num n1, Num n2 ->
    (try mk_num (n1 // n2) with Failure _ -> raise Division_by_zero)
  | Real f1, Real f2 ->
    let f = f1 /. f2 in if f == infinity then raise Division_by_zero else mk_real f
  | _ -> raise TypeMismatch

  let quotient_e s1 s2 =
    if sign s2 >= 0 then floor (quotient s1 s2) else ceiling (quotient s1 s2)

  let quotient_t s1 s2 = truncate (quotient s1 s2)

  let quotient_f s1 s2 = floor (quotient s1 s2)

  let remainder_e s1 s2 = difference s1 (product (quotient_e s1 s2) s2)

  let remainder_t s1 s2 = difference s1 (product (quotient_t s1 s2) s2)

  let remainder_f s1 s2 = difference s1 (product (quotient_f s1 s2) s2)

  let to_int s = floor s

  let to_rat s = s  (* XXX not fully specified *)

  let to_real s = match s.symb_val with
  | Num n -> mk_real (float_of_num n)
  | Real _ -> s
  | _ -> raise TypeMismatch

  let less s1 s2 = match s1.symb_val, s2.symb_val with
  | Num n1, Num n2 -> n1 </ n2
  | Real f1, Real f2 -> f1 < f2
  | _ -> raise TypeMismatch

  let lesseq s1 s2 = match s1.symb_val, s2.symb_val with
  | Num n1, Num n2 -> n1 <=/ n2
  | Real f1, Real f2 -> f1 <= f2
  | _ -> raise TypeMismatch

  let greater s1 s2 = match s1.symb_val, s2.symb_val with
  | Num n1, Num n2 -> n1 >/ n2
  | Real f1, Real f2 -> f1 > f2
  | _ -> raise TypeMismatch

  let greatereq s1 s2 = match s1.symb_val, s2.symb_val with
  | Num n1, Num n2 -> n1 >=/ n2
  | Real f1, Real f2 -> f1 >= f2
  | _ -> raise TypeMismatch
end

let is_used s = HashSymbol.mem {symb_val=Const s; symb_id=0; symb_attrs=0;}

(** Printable form of a symbol *)
let name_symbol s = match s.symb_val with
  | Const s -> s
  | Distinct s -> s
  | Num n -> Num.string_of_num n
  | Real f -> string_of_float f

let get_val s = s.symb_val

let tag_symbol s = s.symb_id

let attrs_symbol s = s.symb_attrs

(** does the symbol have this attribute? *)
let has_attr attr s = (s.symb_attrs land attr) <> 0

module SHashtbl = Hashtbl.Make(
  struct
    type t = symbol
    let equal = (==)
    let hash = hash_symbol
  end)

module SMap = Map.Make(struct type t = symbol let compare = compare_symbols end)
module SMapSeq = Sequence.Map.Adapt(SMap)

module SSet = Set.Make(struct type t = symbol let compare = compare_symbols end)
module SSetSeq = Sequence.Set.Adapt(SSet)

(** {2 connectives} *)

let true_symbol = mk_symbol "$true"
let false_symbol = mk_symbol "$false"
let eq_symbol = mk_symbol ~attrs:(attr_infix lor attr_multiset lor
                                  attr_commut lor attr_polymorphic) "="
let exists_symbol = mk_symbol ~attrs:attr_binder "?"
let forall_symbol = mk_symbol ~attrs:attr_binder "!"
let lambda_symbol = mk_symbol ~attrs:attr_binder "^"
let not_symbol = mk_symbol "~"
let imply_symbol = mk_symbol ~attrs:attr_infix "=>"
let and_symbol = mk_symbol ~attrs:(attr_infix lor attr_ac lor attr_multiset) "&"
let or_symbol = mk_symbol ~attrs:(attr_infix lor attr_ac lor attr_multiset) "|"

(** {2 Magic symbols} *)

(** higher order curryfication symbol *)
let at_symbol = mk_symbol ~attrs:(attr_infix lor attr_polymorphic) "@"

(** pseudo symbol kept for locating bound vars in precedence. Bound
    vars are grouped in the precedence together w.r.t other symbols,
    but compare to each other by their index. *)
let db_symbol = mk_symbol "$$db_magic_cookie"

(** pseudo symbol for locating split symbols in precedence. Split
    symbols compare lexicographically with other split symbols,
    but are in a fixed location in precedence w.r.t other kinds of
    symbols. *)
let split_symbol = mk_symbol "$$split_magic_cookie"

(** pseudo symbol for locating magic constants in precedence.
    This is useful for keeping the precedence finite while managing
    an infinite set of fresh constants, that are used for
    testing terms for ground joinability (replacing variables
    with such constants) *)
let const_symbol = mk_symbol "$$const_magic_cookie"

(** pseudo symbol to locate numbers in the precedence *)
let num_symbol = mk_symbol "$$num_magic_cookie"

let pp_symbol formatter s = match s with
  | _ when s == db_symbol -> Format.pp_print_string formatter "[db]"
  | _ when s == split_symbol -> Format.pp_print_string formatter "[split]"
  | _ when s == num_symbol -> Format.pp_print_string formatter "[num]"
  | _ when s == const_symbol -> Format.pp_print_string formatter "[const]"
  | _ -> Format.pp_print_string formatter (name_symbol s) (* default *)

(** {2 sorts} *)

type sort =
  | Sort of string  (** Atomic sort *)
  | Fun of sort * sort list (** Function sort *)
  (** simple types *)

let rec pp_sort formatter sort = match sort with
  | Sort s -> Format.pp_print_string formatter s
  | Fun (s, [s']) ->
    Format.fprintf formatter "%a > %a" pp_sort s' pp_sort s
  | Fun (s, l) ->
    Format.fprintf formatter "(%a) > %a"
      (Sequence.pp_seq ~sep:" * " pp_sort) (Sequence.of_list l) pp_sort s

(** exception raised when sorts are mismatched *)
exception SortError of string

let bool_symbol = "$o"
let type_symbol = "$tType"
let univ_symbol = "$i"
let int_symbol = "$int"
let rat_symbol = "$rat"
let real_symbol = "$real"

let rec compare_sort s1 s2 = match s1, s2 with
  | Sort a, Sort b -> String.compare a b
  | Fun (a, la), Fun (b, lb) ->
    let cmp = compare_sort a b in
    if cmp <> 0 then cmp else compare_sorts la lb
  | Sort _, Fun _ -> -1
  | Fun _, Sort _ -> 1
and compare_sorts l1 l2 = match l1, l2 with
  | [], [] -> 0
  | x1::l1', x2::l2' ->
    let cmp = compare_sort x1 x2 in
    if cmp <> 0 then cmp else compare_sorts l1' l2'
  | [], _ -> -1
  | _, [] -> 1

let rec hash_sort s = match s with
  | Sort s -> Hash.hash_string s
  | Fun (s, l) -> hash_sorts (hash_sort s) l
and hash_sorts h l = match l with
  | [] -> h
  | x::l' -> hash_sorts (Hash.hash_int2 (hash_sort x) h) l'

(** weak hash table for sorts *)
module HashSort = Hashcons.Make(
  struct
    type t = sort
    let equal a b = compare_sort a b = 0
    let hash s = hash_sort s
    let tag t s = s  (* ignore tag *)
  end)

let mk_sort s = HashSort.hashcons (Sort s)

let (<==) s l = match l with
  | [] -> s  (* collapse 0-ary functions *)
  | _ -> HashSort.hashcons (Fun (s, l))

let (<=.) s1 s2 = s1 <== [s2]

let rec can_apply l args =
  match l, args with
  | [], [] -> true
  | [], _
  | _, [] -> false
  | s1::l', s2::args' -> s1 == s2 && can_apply l' args'

(** [s @@ args] applies the sort [s] to arguments [args]. Basic must match *)
let (@@) s args = match s, args with
  | _, [] -> s
  | Fun (s', l), _ when can_apply l args -> s'
  | _ -> raise (SortError "cannot apply sort")

let type_ = mk_sort type_symbol
let bool_ = mk_sort bool_symbol
let univ_ = mk_sort univ_symbol
let int_ = mk_sort int_symbol
let rat_ = mk_sort rat_symbol
let real_ = mk_sort real_symbol

(** Arity of a sort, ie nunber of arguments of the function, or 0 *)
let arity = function
  | Sort _ -> 0
  | Fun (_, l) -> List.length l

(** Infinite set of symbols, accessed by index, that will not collide with
    the signature of the problem *)
let mk_fresh_const i =
  mk_symbol ~attrs:attr_fresh_const ("$$c_" ^ string_of_int i)

(** {2 Signature of a set of symbols} *)

(** A signature maps symbols to their sort *)
type signature = sort SMap.t

let empty_signature = SMap.empty

(** Add a symbol to the signature, failing if it is incompatible *)
let add_signature signature symb sort =
  try let sort' = SMap.find symb signature in
      if sort == sort'
        then signature
        else failwith (
          let b = Buffer.create 20 in
          Format.bprintf b "incompatible sorts %a, %a for %a"
            pp_sort sort pp_sort sort' pp_symbol symb;
          Buffer.contents b)
  with Not_found -> SMap.add symb sort signature

let sig_to_seq signature = SMapSeq.to_seq signature

let sig_of_seq ?(signature=empty_signature) seq =
  Sequence.fold
    (fun s (symb,sort) -> add_signature s symb sort)
    signature seq

let pp_signature formatter signature =
  Format.fprintf formatter "@[<h>%a@]"
    (Sequence.pp_seq
      (fun formatter (s, sort) ->
        Format.fprintf formatter "%a:%a" pp_symbol s pp_sort sort))
    (sig_to_seq signature)

let pp_precedence formatter symbols =
  Format.fprintf formatter "@[<h>%a@]"
    (Sequence.pp_seq ~sep:" > " pp_symbol) (Sequence.of_list symbols)

let table =
  [true_symbol, bool_;
   false_symbol, bool_;
   eq_symbol, bool_ <== [univ_; univ_];
   exists_symbol, bool_ <=. (bool_ <=. univ_);
   forall_symbol, bool_ <=. (bool_ <=. univ_);
   lambda_symbol, univ_ <=. (univ_ <=. univ_);
   not_symbol, bool_ <=. bool_;
   imply_symbol, bool_ <== [bool_; bool_];
   and_symbol, bool_ <== [bool_; bool_];
   or_symbol, bool_ <== [bool_; bool_];
   at_symbol, univ_ <== [univ_; univ_];   (* FIXME: this really ought to be polymorphic *)
   db_symbol, univ_;
   split_symbol, bool_;
   const_symbol, univ_;
   num_symbol, univ_;
   ]

(** default signature, containing predefined symbols with their arities and sorts *)
let base_signature =
  List.fold_left
    (fun signature (symb,sort) -> SMap.add symb sort signature)
    empty_signature table

(** Set of base symbols *)
let base_symbols = List.fold_left (fun set (s, _) -> SSet.add s set) SSet.empty table

let is_base_symbol s = SSet.mem s base_symbols

(** extract the list of symbols from the complete signature *)
let symbols_of_signature signature =
  SMap.fold (fun s _ l -> s :: l) signature []

let set_of_signature signature =
  SMap.fold (fun s _ set -> SSet.add s set) signature SSet.empty

(** Merge two signatures. raises Failure if they are incompatible. *)
let merge_signatures s1 s2 =
  SMap.merge
    (fun s sort1 sort2 -> match sort1, sort2 with
      | None, None -> None (* ?? *)
      | Some s1, Some s2 ->
        if s1 == s2 then Some s1 else failwith "merge_signatures: incompatible sorts"
      | Some s1, None -> Some s1
      | None, Some s2 -> Some s2)
    s1 s2

(** {2 Conversions and printing} *)

let to_json s = match s.symb_val with
  | Const s -> Json.String s
  | Distinct s -> Json.List [Json.String "distinct"; Json.String s]
  | Num n -> Json.List [Json.String "num"; Json.String (Num.string_of_num n)]
  | Real f -> Json.List [Json.String "real"; Json.String (string_of_float f)]

let of_json json = match json with
  | Json.String s -> mk_symbol s
  | Json.List [Json.String "distinct"; Json.String s] -> mk_distinct s
  | Json.List [Json.String "num"; Json.String n] -> mk_num (Num.num_of_string n)
  | Json.List [Json.String "real"; Json.String f] -> mk_real (float_of_string f)
  | _ -> Json.type_error "expected symbol" json

let rec sort_to_json = function
  | Sort s -> Json.String s
  | Fun (s,l) -> Json.List (sort_to_json s :: List.map sort_to_json l)

let rec sort_of_json json = match json with
  | Json.String s -> mk_sort s
  | Json.List (s::l) -> (sort_of_json s) <== (List.map sort_of_json l)
  | _ -> Json.type_error "expected sort" json

let sig_to_json signature =
  let items = Sequence.map
    (fun (s,sort) -> Json.List [to_json s; sort_to_json sort])
    (sig_to_seq signature)
  in
  Json.mk_list_seq items

let sig_of_json ?(signature=empty_signature) json =
  let pair_of_json json =
    match json with
    | Json.List [a;b] ->
      (of_json a, sort_of_json b)
    | _ -> let msg = "expected signature pair" in
          Json.type_error msg json
  in
  let l = Json.to_list json in
  let seq = Sequence.map pair_of_json (Sequence.of_list l) in
  sig_of_seq ~signature seq
