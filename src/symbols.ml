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

let attr_skolem = 0x1
let attr_split = 0x2

type symbol = (string * int)

let compare_symbols (s1,_) (s2,_) = String.compare s1 s2

let hash_symbol (s, _) = Hashtbl.hash s

(** weak hash table for symbols *)
module HashSymbol = Weak.Make(
  struct
    type t = symbol
    let equal (s1,_) (s2,_) = s1 = s2
    let hash = hash_symbol
  end)

(** the global symbol table *)
let symb_table = HashSymbol.create 7

let sig_version = ref 0

let mk_symbol ?(attrs=0) s =
  let s = (s, attrs) in
  let s' = HashSymbol.merge symb_table s in
  (if s' == s then incr sig_version); (* update signature *)
  s'

let is_used s = HashSymbol.mem symb_table s

let name_symbol (s, _) = s

let attrs_symbol (_, attr) = attr

module SHashtbl = Hashtbl.Make(
  struct
    type t = symbol
    let equal = (==)
    let hash s = hash_symbol s
  end)

module SHashSet =
  struct
    type t = unit SHashtbl.t
    let create () = SHashtbl.create 7
    let member t s = SHashtbl.mem t s
    let add t s = SHashtbl.replace t s ()
    let from_list ss =
      let t = create () in
      List.iter (add t) ss;
      t
  end


(* connectives *)
let true_symbol = mk_symbol "$true"
let false_symbol = mk_symbol "$false"
let eq_symbol = mk_symbol "="
let exists_symbol = mk_symbol "$$exists"
let forall_symbol = mk_symbol "$$forall"
let lambda_symbol = mk_symbol "$$lambda"
let not_symbol = mk_symbol"$$not"
let imply_symbol = mk_symbol "$$imply"
let and_symbol = mk_symbol "$$and"
let or_symbol = mk_symbol "$$or"

(* De Bruijn *)
let db_symbol = mk_symbol "$$db"
let succ_db_symbol = mk_symbol "$$s"

type sort = symbol

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
   succ_db_symbol, univ_sort, 1;
   ]

(** default signature, containing predefined symbols with their arities and sorts *)
let base_signature () =
  let sorts = SHashtbl.create 23
  and arities  = SHashtbl.create 23
  and symbols = ref [] in
  (* update the tables *)
  List.iter
    (fun (symb, sort, arity) ->
      SHashtbl.add sorts symb sort;
      SHashtbl.add arities symb arity;
      symbols := symb :: !symbols)
    table;
  sorts, arities, !symbols
