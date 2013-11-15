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

(** {1 Symbols}

Symbols are either numeric constants, or hashconsed, typed strings
representing logical functions, constants and predicates.
Two symbols with the same name but distinct types are distinct.
*)

(** {2 Definition of symbols} *)

(** Symbols have a mixed representation, with string constants being
    hashconsed, but not numeric literals. *)

type const_info
  (** Additional information for hashconsed symbols. Contains type,
      etc. but remains opaque. *)

(** A symbol of TPTP *)
type t = private
  | Const of string * const_info
  | Int of Big_int.big_int
  | Rat of Ratio.ratio
  | Real of float

type symbol = t

val compare : t -> t -> int
  (** total ordering on symbols *)

val eq : t -> t -> bool
  (** Equality of symbols *)

val hash : t -> int
  (** hash the symbol *)

(** {2 Boolean flags}
Boolean flags are flags that can be attached to symbols. Since
symbols are perfectly shared, a flag is system-wide. Flags can
be combined using the {s lor} operator.
*)

type flag = int

val flag_skolem : flag          (** skolem symbol? *)
val flag_split : flag           (** symbol used for splitting? *)
val flag_binder : flag          (** is the symbol a binding symbol? *)
val flag_infix : flag           (** symbol is binary infix? *)
val flag_ac : flag              (** symbol is associative-commutative? *)
val flag_multiset : flag        (** symbol has multiset status for RPO *)
val flag_fresh_const : flag     (** symbol that is a fresh constant *)
val flag_commut : flag          (** symbol that is commutative (not ac) *)
val flag_distinct : flag        (** distinct element (between "") *)

(** {2 Constructors} *)

val mk_const : ?flags:flag -> ty:Type.t -> string -> t
  (** Build a typed symbol with the given name and type.
      @raise Invalid_argument if the type contains free variables *)

val mk_distinct : ?flags:flag -> ?ty:Type.t -> string -> t
  (** default type: $i *)

val mk_bigint : Big_int.big_int -> t
val mk_int : int -> t
val mk_rat : int -> int -> t
val mk_ratio : Ratio.ratio -> t
val mk_real : float -> t

val parse_num : string -> t             (** Parse an Int or a Rat *)

val of_basic : ?ty:Type.t -> Basic.Sym.t -> t
  (** Convert the basic symbol into a typed symbol *)

val to_basic : t -> Basic.Sym.t
  (** Forget the type *)

val is_const : t -> bool
val is_distinct : t -> bool
val is_int : t -> bool
val is_rat : t -> bool
val is_real : t -> bool
val is_numeric : t -> bool  (* any of the 3 above *)

val flags : t -> flag
  (** access the attributes of a symbol *)

val has_flag : flag -> t -> bool
  (** does the symbol have this attribute? *)

val ty : t -> Type.t
  (** Access the type of this symbol. *)

module Map : Sequence.Map.S with type key = t
module Set : Sequence.Set.S with type elt = t
module Tbl : Hashtbl.S with type key = t

(** {2 connectives} *)

(** Those symbols are useful to embed formulas in terms. This may be useful
    in non first-order logic, or to apply some term-specific operation
    such as unification, rewriting or AC-matching to formulas *)

val true_symbol : t
val false_symbol : t
val eq_symbol : t
val exists_symbol : t
val forall_symbol : t
val not_symbol : t
val imply_symbol : t
val equiv_symbol : t
val and_symbol : t
val or_symbol : t

val connectives : t list  (** List of the connectives *)
val is_connective : t -> bool

val wildcard_symbol : t   (** $_ for type inference *)

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

  val zero_of_ty : Type.t -> t
  val one_of_ty : Type.t -> t

  val is_zero : t -> bool
  val is_one : t -> bool
  val is_minus_one : t -> bool

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

  val is_int : t
  val is_rat : t
  val is_real : t

  val to_int : t
  val to_rat : t
  val to_real : t

  val less : t
  val lesseq : t
  val greater : t
  val greatereq : t

  val set : Set.t
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

val mk_fresh_const : int -> ty:Type.t -> t
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

(** {2 Generation of symbols} *)

module Gensym : sig
  type t
    (** Generator of fresh symbols *)

  val create : ?prefix:string -> unit -> t
    (** New generator of fresh symbols *)

  val new_ : t -> ty:Type.t -> symbol
    (** Fresh symbol with given type. *)
end
