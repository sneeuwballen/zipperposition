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

(** {1 Symbols} *)

(** {2 Definition of symbols} *)

(** Symbols have a mixed representation, with string constants being
    hashconsed, but not numeric literals. *)

(** A symbol of TPTP *)
type t = private
  | Const of string * const_info
  | Int of Big_int.big_int
  | Rat of Ratio.ratio
  | Real of float
and const_info = private {
  mutable tag : int;
  mutable attrs : int;
} (** Additional information for hashconsed symbols *)

type symbol = t

val compare : t -> t -> int
  (** total ordering on symbols *)

val eq : t -> t -> bool
  (** Equality of symbols *)

val hash : t -> int
  (** hash the symbol *)

(** {2 Boolean attributes} *)

(** Boolean attributes are flags that can be attached to symbols. Since
    symbols are perfectly shared, a flag is system-wide. Flags can
    be combined using the {s lor} operator. *)

type symbol_attribute = int

val attr_skolem : symbol_attribute      (** skolem symbol? *)
val attr_split : symbol_attribute       (** symbol used for splitting? *)
val attr_binder : symbol_attribute      (** is the symbol a binding symbol? *)
val attr_infix : symbol_attribute       (** symbol is binary infix? *)
val attr_ac : symbol_attribute          (** symbol is associative-commutative? *)
val attr_multiset : symbol_attribute    (** symbol has multiset status for RPO *)
val attr_fresh_const : symbol_attribute (** symbol that is a fresh constant *)
val attr_commut : symbol_attribute      (** symbol that is commutative (not ac) *)
val attr_distinct : symbol_attribute    (** distinct element (between "") *)

(** {2 Constructors} *)

val mk_const : ?attrs:symbol_attribute -> string -> t
val mk_distinct : ?attrs:symbol_attribute -> string -> t
val mk_bigint : Big_int.big_int -> t
val mk_int : int -> t
val mk_rat : int -> int -> t
val mk_ratio : Ratio.ratio -> t
val mk_real : float -> t

val parse_num : string -> t             (** Parse an Int or a Rat *)

val is_const : t -> bool
val is_distinct : t -> bool
val is_int : t -> bool
val is_rat : t -> bool
val is_real : t -> bool
val is_numeric : t -> bool  (* any of the 3 above *)

val attrs : t -> symbol_attribute
  (** access the attributes of a symbol *)

val has_attr : symbol_attribute -> t -> bool
  (** does the symbol have this attribute? *)

module SHashtbl : Hashtbl.S with type key = t
module SMap : Sequence.Map.S with type key = t
module SSet : Sequence.Set.S with type elt = t

(** {2 connectives} *)

(** Those symbols are useful to embed formulas in terms. This may be useful
    in non first-order logic, or to apply some term-specific operation
    such as unification, rewriting or AC-matching to formulas *)

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

val connectives : t list  (** List of the connectives *)
val is_connective : t -> bool

(** {2 Arithmetic} *)

(** Arithmetic (assumes the symbols verify {!is_numeric}).
    {!Arith} contains the symbols, and {!Arith.Op} contains the computational
    part of arithmetic (compute the result of operations) *)

module Arith : sig
  exception TypeMismatch of string
    (** This exception is raised when Arith functions are called
        on non-numeric values (Const). *)

  (** some functions may raise Division_by_zero *)

  val sign : t -> int   (* -1, 0 or 1 *)

  val floor : t
  val ceiling : t
  val truncate : t
  val round : t

  val prec : t
  val succ : t

  val one_i : t
  val zero_i : t
  val one_rat : t
  val zero_rat : t
  val one_f : t
  val zero_f : t

  val typeof : t -> Type.t        (** Type of a constant *)

  val zero_of_ty : Type.t -> t
  val one_of_ty : Type.t -> t

  val is_zero : t -> bool
  val is_one : t -> bool

  val sum : t
  val difference : t
  val uminus : t
  val product : t
  val quotient : t

  val quotient_e : t
  val quotient_t : t
  val quotient_f : t
  val remainder_e : t
  val remainder_t : t
  val remainder_f : t

  val to_int : t
  val to_rat : t
  val to_real : t

  val less : t
  val lesseq : t
  val greater : t
  val greatereq : t

  val set : SSet.t
    (** Set of arithmetic symbols *)

  val is_arith : t -> bool
    (** Is the symbol an arithmetic symbol? *)

  (** The module {!Op} deals only with numeric constants, i.e., all symbols
      must verify {!is_numeric} (and most of the time, have the same type).
      The semantics of operations follows
      {{: http://www.cs.miami.edu/~tptp/TPTP/TR/TPTPTR.shtml#Arithmetic} TPTP}.
    *)

  module Op : sig
    val floor : t -> t
    val ceiling : t -> t
    val truncate : t -> t
    val round : t -> t

    val prec : t -> t
    val succ : t -> t

    val sum : t -> t -> t
    val difference : t -> t -> t
    val uminus : t -> t
    val product : t -> t -> t
    val quotient : t -> t -> t

    val quotient_e : t -> t -> t
    val quotient_t : t -> t -> t
    val quotient_f : t -> t -> t
    val remainder_e : t -> t -> t
    val remainder_t : t -> t -> t
    val remainder_f : t -> t -> t

    val to_int : t -> t
    val to_rat : t -> t
    val to_real : t -> t

    val abs : t -> t (* absolute value *)
    val divides : t -> t -> bool (* [divides a b] returns true if [a] divides [b] *)
    val gcd : t -> t -> t  (* gcd of two ints, 1 for other types *)

    val less : t -> t -> bool
    val lesseq : t -> t -> bool
    val greater : t -> t -> bool
    val greatereq : t -> t -> bool
  end
end


(** {2 "Magic" symbols} *)

val db_symbol : t    (** pseudo symbol kept for locating bound vars in precedence *)
val split_symbol : t (** pseudo symbol for locating split symbols in precedence *)
val const_symbol : t (** pseudo symbol for locating magic constants in precedence *)
val num_symbol : t   (** pseudo symbol to locate numbers in the precedence *)

val mk_fresh_const : int -> t
  (** Infinite set of symbols, accessed by index, that will not collide with
      the signature of the problem *)

(** {2 IO} *)

val to_string : t -> string
val pp_debug : Buffer.t -> t -> unit
val to_string_tstp : t -> string
val pp_tstp : Buffer.t -> t -> unit

val pp : Buffer.t -> t -> unit   (* uses default printer *)
val fmt : Format.formatter -> t -> unit

val set_default_pp : (Buffer.t -> t -> unit) -> unit (* change default printer *)

val bij : t Bij.t
val arbitrary : t QCheck.Arbitrary.t

(** {2 Generation of symbols} *)

module Gensym : sig
  type t
    (** Generator of fresh symbols *)

  val create : ?prefix:string -> unit -> t
    (** New generator of fresh symbols *)

  val new_ : t -> symbol
    (** Fresh symbol *)
end
