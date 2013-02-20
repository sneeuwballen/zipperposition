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

val compare_symbols : symbol -> symbol -> int
  (** total ordering on symbols (equality is ==) *)

val hash_symbol : symbol -> int
  (** hash the symbol *)

type symbol_attribute = int
  (** attributes of the symbol *)

(** {2 Boolean attributes} *)

(** Boolean attributes are flags that can be attached to symbols. Since
    symbols are perfectly shared, a flag is system-wide. Flags can
    be combined using the {s lor} operator. *)

val attr_skolem : symbol_attribute      (** skolem symbol? *)
val attr_split : symbol_attribute       (** symbol used for splitting? *)
val attr_binder : symbol_attribute      (** is the symbol a binding symbol? *)
val attr_infix : symbol_attribute       (** symbol is binary infix? *)
val attr_ac : symbol_attribute          (** symbol is associative-commutative? *)
val attr_multiset : symbol_attribute    (** symbol has multiset status for RPO *)
val attr_fresh_const : symbol_attribute (** symbol that is a fresh constant *)
val attr_commut : symbol_attribute      (** symbol that is commutative (not ac) *)

val mk_symbol : ?attrs:symbol_attribute -> string -> symbol
val mk_distinct : ?attrs:symbol_attribute -> string -> symbol
val mk_num : ?attrs:symbol_attribute -> Num.num -> symbol
val mk_int : ?attrs:symbol_attribute -> int -> symbol
val mk_real : ?attrs:symbol_attribute -> float -> symbol

val is_used : string -> bool
  (** is the symbol already used? *)

val tag_symbol : symbol -> int
  (** unique ID of the symbol *)

val attrs_symbol : symbol -> symbol_attribute
  (** access attributes of a symbol *)

val has_attr : symbol_attribute -> symbol -> bool
  (** does the symbol have this attribute? *)

val name_symbol : symbol -> string
  (** Printable form of a symbol *)

module SHashtbl : Hashtbl.S with type key = symbol

module SMap : Map.S with type key = symbol
module SMapSeq : Sequence.Map.S with type key = symbol and type 'a map = 'a SMap.t

module SSet : Set.S with type elt = symbol
module SSetSeq : Sequence.Set.S with type elt = symbol and type set = SSet.t

(** {2 connectives} *)

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
val const_symbol : symbol (** pseudo symbol for locating magic constants in precedence *)
val num_symbol : symbol   (** pseudo symbol to locate numbers in the precedence *)

val mk_fresh_const : int -> symbol
  (** Infinite set of symbols, accessed by index, that will not collide with
      the signature of the problem *)

(** {2 sorts} *)

type sort =
  | Sort of string (** Atomic sort *)
  | Fun of sort * sort list (** Function sort (first is return type) *)
  (** simple types *)

(** exception raised when sorts are mismatched *)
exception SortError of string

val bool_symbol : string
val type_symbol : string
val univ_symbol : string
val int_symbol : string
val rat_symbol : string
val real_symbol : string

(** Sorts are simple types. Note that equality on sorts is (==),
    because sorts are hashconsed (to save memory) *)

val compare_sort : sort -> sort -> int

val hash_sort : sort -> int

val mk_sort : string -> sort
  (** Build an atomic sort *)

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
val int_ : sort
val rat_ : sort
val real_ : sort

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

val merge_signatures : signature -> signature -> signature
  (** Merge two signatures. raises Failure if they are incompatible. *)

(** {2 Conversions and printing} *)

val sig_to_seq : signature -> (symbol * sort) Sequence.t
val sig_of_seq : (symbol * sort) Sequence.t -> signature

val to_json : symbol -> Yojson.Basic.json
val of_json : Yojson.Basic.json -> symbol

val sort_to_json : sort -> Yojson.Basic.json
val sort_of_json : Yojson.Basic.json -> sort

val sig_to_json : signature -> Yojson.Basic.json
val sig_of_json : Yojson.Basic.json -> signature
