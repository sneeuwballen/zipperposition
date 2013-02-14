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

type symbol
  (** abstract type of a symbol *)

type sort =
  | Sort of symbol          (** Atomic sort *)
  | Fun of sort * sort list (** Function sort (first is return type) *)
  (** simple types *)

(** exception raised when sorts are mismatched *)
exception SortError of string

val compare_symbols : symbol -> symbol -> int
  (** total ordering on symbols (equality is ==) *)

val hash_symbol : symbol -> int
  (** hash the symbol *)

val hash_sort : sort -> int

type symbol_attribute = int
  (** attributes of the symbol *)

val attr_skolem : symbol_attribute      (** skolem symbol? *)
val attr_split : symbol_attribute       (** symbol used for splitting? *)
val attr_binder : symbol_attribute      (** is the symbol a binding symbol? *)
val attr_infix : symbol_attribute       (** symbol is binary infix? *)
val attr_ac : symbol_attribute          (** symbol is associative-commutative? *)

val mk_symbol : ?attrs:symbol_attribute -> string -> symbol
  (** construction of a symbol *)

val is_used : string -> bool
  (** is the symbol already used? *)

val tag_symbol : symbol -> int
  (** unique ID of the symbol *)

val attrs_symbol : symbol -> symbol_attribute
  (** access attributes of a symbol *)

val has_attr : symbol_attribute -> symbol -> bool
  (** does the symbol have this attribute? *)

val name_symbol : symbol -> string
  (** deconstruction of a symbol *)

module SHashtbl : Hashtbl.S with type key = symbol

module SMap : Map.S with type key = symbol

module SSet : Set.S with type elt = symbol

(* connectives *)
val true_symbol : symbol
val false_symbol : symbol
val eq_symbol : symbol
val exists_symbol : symbol
val forall_symbol : symbol
val lambda_symbol : symbol
val not_symbol : symbol
val imply_symbol : symbol
val and_symbol : symbol
val or_symbol : symbol

(** {2 Magic symbols} *)

val at_symbol : symbol    (** higher order curryfication symbol *)

val db_symbol : symbol    (** pseudo symbol kept for locating bound vars in precedence *)
val split_symbol : symbol (** pseudo symbol for locating split symbols in precedence *)

(** {2 sorts} *)

val bool_symbol : symbol
val type_symbol : symbol
val univ_symbol : symbol

(** Sorts are simple types. Note that equality on sorts is (==),
    because sorts are hashconsed (to save memory) *)

val compare_sort : sort -> sort -> int

val hash_sort : sort -> int

val mk_sort : symbol -> sort

val (<==) : sort -> sort list -> sort
  (** [s <== args] build the sort of functions that take arguments
      of sort [args], and has return type [s] *)

val (<=.) : sort -> sort -> sort
  (** Helper for unary function sort. Left-associative, ie
      [a <=. b <=. c] is like [(a <=. b) <=. c] *)

val (@@) : sort -> sort list -> sort
  (** [s @@ args] applies the sort [s] to arguments [args]. Types must match *)

val bool_ : sort
val type_ : sort
val univ_ : sort

val arity : sort -> int
  (** Arity of a sort, ie nunber of arguments of the function, or 0 *)

(** {2 Signature} *)

(** A signature maps symbols to their sort *)
type signature = sort SMap.t

val empty_signature : signature
  (** The empty signature *)

val base_signature : signature
  (** the signature composed of predefined symbols *)

val base_symbols : SSet.t
  (** Set of base symbols *)

val symbols_of_signature : signature -> symbol list
  (** extract the list of symbols from the complete signature *)
