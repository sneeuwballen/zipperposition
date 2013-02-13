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

(** A symbol is a string, a unique ID, and some attributes *)
type symbol = {
  symb_name: string;
  mutable symb_id : int;
  mutable symb_attrs : int;
}

(** A sort is just a symbol *)
type sort = symbol

let compare_symbols s1 s2 = s1.symb_id - s2.symb_id

let hash_symbol s = Hash.hash_int s.symb_id

(** weak hash table for symbols *)
module HashSymbol = Hashcons.Make(
  struct
    type t = symbol
    let equal s1 s2 = String.compare s1.symb_name s2.symb_name = 0
    let hash s = Hash.hash_string s.symb_name
    let tag t s = (s.symb_id <- t; s)
  end)

(** counter for symbols *)
let symb_count = ref 0

let mk_symbol ?(attrs=0) s =
  let s = {
    symb_name = s;
    symb_id = 0;
    symb_attrs = attrs;
  } in
  HashSymbol.hashcons s

let is_used s = HashSymbol.mem {symb_name=s; symb_id=0; symb_attrs=0;}

let name_symbol s = s.symb_name

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

(** {2 connectives} *)

let true_symbol = mk_symbol "$true"
let false_symbol = mk_symbol "$false"
let eq_symbol = mk_symbol ~attrs:(attr_infix lor attr_multiset) "="
let exists_symbol = mk_symbol ~attrs:attr_binder "$$exists"
let forall_symbol = mk_symbol ~attrs:attr_binder "$$forall"
let lambda_symbol = mk_symbol ~attrs:attr_binder "$$lambda"
let not_symbol = mk_symbol "$$not"
let imply_symbol = mk_symbol ~attrs:attr_infix "$$imply"
let and_symbol = mk_symbol ~attrs:(attr_infix lor attr_ac) "$$and"
let or_symbol = mk_symbol ~attrs:(attr_infix lor attr_ac) "$$or"

(** {2 Magic symbols} *)

(** higher order curryfication symbol *)
let at_symbol = mk_symbol ~attrs:attr_infix "@"

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

(** {2 sorts} *)
let type_sort = mk_symbol "$tType"
let bool_sort = mk_symbol "$o"
let univ_sort = mk_symbol "$i"

(** Infinite set of symbols, accessed by index, that will not collide with
    the signature of the problem *)
let mk_fresh_const i =
  mk_symbol ~attrs:attr_fresh_const ("$$const_" ^ string_of_int i)

(** A signature maps symbols to (sort, arity) *)
type signature = (int * sort) SMap.t

let empty_signature = SMap.empty

let table =
  [true_symbol, bool_sort, 0;
   false_symbol, bool_sort, 0;
   eq_symbol, bool_sort, 2;
   exists_symbol, bool_sort, 1;
   forall_symbol, bool_sort, 1;
   lambda_symbol, univ_sort, 1;
   not_symbol, bool_sort, 1;
   imply_symbol, bool_sort, 2;
   and_symbol, bool_sort, 2;
   or_symbol, bool_sort, 2;
   at_symbol, univ_sort, 2;   (* FIXME: this really ought to be polymorphic *)
   db_symbol, univ_sort, 0;
   split_symbol, bool_sort, 0;
   const_symbol, univ_sort, 0;
   ]

(** default signature, containing predefined symbols with their arities and sorts *)
let base_signature =
  List.fold_left
    (fun signature (symb,sort,arity) -> SMap.add symb (arity, sort) signature)
    empty_signature table

(** Set of base symbols *)
let base_symbols = List.fold_left (fun set (s, _, _) -> SSet.add s set) SSet.empty table

(** extract the list of symbols from the complete signature *)
let symbols_of_signature signature =
  SMap.fold (fun s _ l -> s :: l) signature []

(** {2 Conversions and printing} *)

let sig_to_seq signature =
  Sequence.map
    (fun (symb, (i,sort)) -> (symb,i,sort))
    (SMapSeq.to_seq signature)

let sig_of_seq seq =
  SMapSeq.of_seq
    (Sequence.map (fun (symb,i,sort) -> (symb,(i,sort))) seq)

module Json = Yojson.Basic

let to_json s : Json.json = `String (name_symbol s)

let of_json json =
  let s = Json.Util.to_string json in
  mk_symbol s

let sig_to_json signature =
  let items = Sequence.map
    (fun (s,i,sort) -> `List [to_json s; `Int i; to_json sort])
    (sig_to_seq signature)
  in
  `List (Sequence.to_list items)

let sig_of_json json =
  let triple_of_json json =
    match json with
    | `List [a;b;c] ->
      (of_json a, Json.Util.to_int b, of_json c)
    | _ -> let msg = "expected signature triple" in
         raise (Json.Util.Type_error (msg, json))
  in
  let l = Json.Util.to_list json in
  let seq = Sequence.map triple_of_json (Sequence.of_list l) in
  sig_of_seq seq
