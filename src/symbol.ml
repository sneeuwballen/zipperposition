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
let attr_num = 1 lsl 8

(** {2 Definition of symbols and sorts} *)

type t = {
  symb_val : symbol_val;
  mutable symb_attrs : int;
} (** A symbol is a string, a unique ID, and some attributes *)
and symbol_val =
  | Const of string
  | Distinct of string
  | Num of Num.num
  | Real of float
  (** A symbol value is a string, a quoted string, or a number *)

type symbol = t

let compare_symbol_val sv1 sv2 = match sv1, sv2 with
  | Const s1, Const s2 -> String.compare s1 s2
  | Distinct s1, Distinct s2 -> String.compare s1 s2
  | Num n1, Num n2 -> Num.compare_num n1 n2
  | Real f1, Real f2 -> compare f1 f2
  | Const _, _ -> 1
  | Distinct _, _ -> 1
  | Num _, _ -> 1
  | Real _, _ -> -1

let eq a b = a == b

let compare s1 s2 = compare_symbol_val s1.symb_val s2.symb_val
  (** Comparison after hashconsing *)

let hash s = match s.symb_val with
  | Distinct s
  | Const s -> Hash.hash_string s
  | Real f -> int_of_float f
  | Num n -> Num.int_of_num n

(** weak hash table for symbols *)
module HashSymbol = Hashcons.Make(struct
  type t = symbol
  let equal s1 s2 = compare_symbol_val s1.symb_val s2.symb_val = 0
  let hash s = Hashtbl.hash s.symb_val
  let tag t s = ()
end)

let mk_symbol ?(attrs=0) s =
  let s = {
    symb_val = Const s;
    symb_attrs = attrs;
  } in
  HashSymbol.hashcons s

let mk_distinct ?(attrs=0) s =
  let s = {
    symb_val = Distinct s;
    symb_attrs = attrs;
  } in
  HashSymbol.hashcons s

let mk_num ?(attrs=0) n =
  let s = {
    symb_val = Num n;
    symb_attrs = attrs lor attr_num;
  } in
  HashSymbol.hashcons s

let parse_num ?attrs str =
  let n = Num.num_of_string str in
  mk_num ?attrs n

let mk_int ?(attrs=0) i =
  let s = {
    symb_val = Num (Num.num_of_int i);
    symb_attrs = attrs lor attr_num;
  } in
  HashSymbol.hashcons s

let mk_real ?(attrs=0) f =
  let s = {
    symb_val = Real f;
    symb_attrs = attrs lor attr_num;
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

(** Printable form of a symbol *)
let name_symbol s = match s.symb_val with
  | Const s -> s
  | Distinct s -> s
  | Num n -> Num.string_of_num n
  | Real f -> string_of_float f

let get_val s = s.symb_val

let attrs_symbol s = s.symb_attrs

(** does the symbol have this attribute? *)
let has_attr attr s = (s.symb_attrs land attr) <> 0

module SHashtbl = Hashtbl.Make(struct
  type t = symbol
  let equal = eq
  let hash = hash
end)

module SMap = Map.Make(struct type t = symbol let compare = compare end)
module SMapSeq = Sequence.Map.Adapt(SMap)

module SSet = Set.Make(struct type t = symbol let compare = compare end)
module SSetSeq = Sequence.Set.Adapt(SSet)

(** {2 connectives} *)

let true_symbol = mk_symbol "$true"
let false_symbol = mk_symbol "$false"
let eq_symbol = mk_symbol ~attrs:(attr_infix lor attr_multiset lor
                                  attr_commut) "="
let exists_symbol = mk_symbol ~attrs:attr_binder "?"
let forall_symbol = mk_symbol ~attrs:attr_binder "!"
let lambda_symbol = mk_symbol ~attrs:attr_binder "^"
let not_symbol = mk_symbol "~"
let imply_symbol = mk_symbol ~attrs:attr_infix "=>"
let equiv_symbol = mk_symbol ~attrs:(attr_infix lor attr_commut) "<=>"
let and_symbol = mk_symbol ~attrs:(attr_infix lor attr_ac lor attr_multiset) "&"
let or_symbol = mk_symbol ~attrs:(attr_infix lor attr_ac lor attr_multiset) "|"

(** {2 Magic symbols} *)

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

(** Infinite set of symbols, accessed by index, that will not collide with
    the signature of the problem *)
let mk_fresh_const i =
  mk_symbol ~attrs:attr_fresh_const ("$$c_" ^ string_of_int i)

let table =
  [true_symbol;
   false_symbol;
   eq_symbol;
   equiv_symbol;
   exists_symbol;
   forall_symbol;
   lambda_symbol;
   not_symbol;
   imply_symbol;
   and_symbol;
   or_symbol;
   ]

(** Set of base symbols *)
let base_symbols =
  List.fold_left (fun set s -> SSet.add s set) SSet.empty table

let is_base_symbol s =
  SSet.mem s base_symbols

(** {2 IO} *)

let pp buf s = Buffer.add_string buf (name_symbol s)

let pp_tstp buf s = match s with
  | _ when s == not_symbol -> Buffer.add_string buf "~"
  | _ when s == eq_symbol -> Buffer.add_string buf "="
  | _ when s == lambda_symbol -> failwith "^"
  | _ when s == exists_symbol -> Buffer.add_string buf "?"
  | _ when s == forall_symbol -> Buffer.add_string buf "!"
  | _ when s == and_symbol -> Buffer.add_string buf "&"
  | _ when s == or_symbol -> Buffer.add_string buf "|"
  | _ when s == imply_symbol -> Buffer.add_string buf "=>"
  | _ -> Buffer.add_string buf (name_symbol s) (* default *)

let to_string s = name_symbol s

let fmt fmt s = Format.pp_print_string fmt (to_string s)

let bij =
  let open Bij in
  switch
    ~inject:(fun s -> match s.symb_val with
      | Const s -> 'c', BranchTo (string_, s)
      | Distinct s -> 's', BranchTo (string_, s)
      | Num n -> 'n', BranchTo (string_, Num.string_of_num n)
      | Real f -> 'f', BranchTo (float_, f))
    ~extract:(fun c -> match c with
      | 'c' -> BranchFrom (string_, mk_symbol)
      | 's' -> BranchFrom (string_, mk_distinct)
      | 'n' -> BranchFrom (string_, (fun n -> mk_num (Num.num_of_string n)))
      | 'f' -> BranchFrom (float_, mk_real)
      | c -> raise (DecodingError "expected symbol"))

(** {2 Generation of symbols} *)

module Gensym = struct
  type t = {
    prefix : string;
    mutable count : int;
  }

  let create ?(prefix="hyst__") () =
    assert (prefix <> "");
    { prefix;
      count = 0;
    }

  let new_ gensym =
    let n = gensym.count in
    gensym.count <- n + 1;
    let s = Util.sprintf "%s%d" gensym.prefix n in
    mk_symbol s
end
