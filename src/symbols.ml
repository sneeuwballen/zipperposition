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

(** Symbols and signature *)

type symbol_attribute = int

let attr_skolem = 1 lsl 0
let attr_split = 1 lsl 1
let attr_binder = 1 lsl 2
let attr_infix = 1 lsl 3
let attr_ac = 1 lsl 4

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

module SSet = Set.Make(struct type t = symbol let compare = compare_symbols end)

(** A signature maps symbols to (sort, arity) *)
type signature = (int * sort) SMap.t

let empty_signature = SMap.empty

(* connectives *)
let true_symbol = mk_symbol "$true"
let false_symbol = mk_symbol "$false"
let eq_symbol = mk_symbol ~attrs:attr_infix "="
let exists_symbol = mk_symbol ~attrs:attr_binder "$$exists"
let forall_symbol = mk_symbol ~attrs:attr_binder "$$forall"
let lambda_symbol = mk_symbol ~attrs:attr_binder "$$lambda"
let not_symbol = mk_symbol "$$not"
let imply_symbol = mk_symbol ~attrs:attr_infix "$$imply"
let and_symbol = mk_symbol ~attrs:(attr_infix lor attr_ac) "$$and"
let or_symbol = mk_symbol ~attrs:(attr_infix lor attr_ac) "$$or"

(** pseudo symbol kept for locating bound vars in precedence *)
let db_symbol = mk_symbol "$$db_magic_cookie"

(* default sorts *)
let type_sort = mk_symbol "$tType"
let bool_sort = mk_symbol "$o"
let univ_sort = mk_symbol "$i"

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
   db_symbol, univ_sort, 0;
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

(** Serialize the signature as a string, with '~' as separators *)
let dump_signature signature =
  let b = Buffer.create 128 in
  let formatter = Format.formatter_of_buffer b in
  SMap.iter
    (fun s (arity, sort) ->
      Format.fprintf formatter "%s:%d:%s~@?" (name_symbol s) arity (name_symbol sort))
    signature;
  (* get the whole buffer, except the last '~' *)
  if Buffer.length b = 0 then "" else Buffer.sub b 0 (Buffer.length b - 1)

(** Deserialize the signature from the string, or raise Invalid_argument *)
let load_signature str =
  (* how to parse a single triplet, that describes a symbol *)
  let parse_triplet signature triplet =
    match Str.split (Str.regexp ":") triplet with
    | [symbol; arity; sort] ->
      let arity = int_of_string arity in
      let symbol = mk_symbol symbol in
      let sort = mk_symbol sort in
      SMap.add symbol (arity, sort) signature
    | _ -> failwith "bad triplet"
  in
  try
    let triplets = Str.split (Str.regexp "~") str in
    List.fold_left parse_triplet empty_signature triplets
  with _ ->
    raise (Invalid_argument ("failed load_signature of " ^ str))
