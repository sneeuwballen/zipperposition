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

(** {2 Definition of symbols and sorts} *)

type t = private {
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

val compare : t -> t -> int
  (** total ordering on symbols (equality is ==) *)

val eq : t -> t -> bool

val hash : t -> int
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
val attr_num : symbol_attribute         (** symbol that is numeric *)

val mk_symbol : ?attrs:symbol_attribute -> string -> t
val mk_distinct : ?attrs:symbol_attribute -> string -> t
val mk_num : ?attrs:symbol_attribute -> Num.num -> t
val parse_num : ?attrs:symbol_attribute -> string -> t
val mk_int : ?attrs:symbol_attribute -> int -> t
val mk_real : ?attrs:symbol_attribute -> float -> t

val is_const : t -> bool
val is_int : t -> bool
val is_rat : t -> bool
val is_real : t -> bool
val is_numeric : t -> bool  (* any of the 3 above *)
val is_distinct : t -> bool

val attrs_symbol : t -> symbol_attribute
  (** access attributes of a symbol *)

val has_attr : symbol_attribute -> t -> bool
  (** does the symbol have this attribute? *)

val name_symbol : t -> string
  (** Printable form of a symbol *)

val get_val : t -> symbol_val
  (** Access the definition of this symbol *)

module SHashtbl : Hashtbl.S with type key = t

module SMap : Map.S with type key = t
module SMapSeq : Sequence.Map.S with type key = t and type 'a t = 'a SMap.t

module SSet : Set.S with type elt = t
module SSetSeq : Sequence.Set.S with type elt = t and type t = SSet.t

(** {2 connectives} *)

val true_symbol : t
val false_symbol : t
val eq_symbol : t
val exists_symbol : t
val forall_symbol : t
val lambda_symbol : t
val not_symbol : t
val imply_symbol : t
val equiv_symbol : t
val and_symbol : t
val or_symbol : t


(** {2 Magic symbols} *)

val db_symbol : symbol    (** pseudo symbol kept for locating bound vars in precedence *)
val split_symbol : symbol (** pseudo symbol for locating split symbols in precedence *)
val const_symbol : symbol (** pseudo symbol for locating magic constants in precedence *)
val num_symbol : symbol   (** pseudo symbol to locate numbers in the precedence *)

val mk_fresh_const : int -> t
  (** Infinite set of symbols, accessed by index, that will not collide with
      the signature of the problem *)

(** {2 Base symbols} *)

(** A set of specific symbols, representing FOL special operators
    and values (truth, connectives, etc.) *)

val table : t list
  (** Table of base symbols *)

val base_symbols : SSet.t
  (** Set of base symbols *)

val is_base_symbol : t -> bool

(* TODO: simple and handy arithmetic system; need to change the representation
         of symbols *)

(** {2 IO} *)

val pp : Buffer.t -> t -> unit
val to_string : t -> string
val fmt : Format.formatter -> t -> unit

val bij : t Bij.t

(** {2 Generation of symbols} *)

module Gensym : sig
  type t
    (** Generator of fresh symbols *)

  val create : ?prefix:string -> unit -> t
    (** New generator of fresh symbols *)

  val new_ : t -> symbol
    (** Fresh symbol *)
end
