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

Symbols are abstract, but must be constructible from strings, printable,
comparable and hashable.
*)

type connective =
  | Not
  | And
  | Or
  | Imply
  | Equiv
  | Xor
  | Eq
  | Neq
  | HasType
  | True
  | False
  | Exists
  | Forall
  | ForallTy
  | Lambda
  | Arrow
  | Wildcard
  | Multiset  (* type of multisets *)
  | FreshVar of int  (* special symbol to generate fresh vars *)
  | TType (* type of types *)

type const_symbol = private {
  mutable cs_id : int;
  cs_name : string;
}

type t =
  | Conn of connective
  | Cst of const_symbol
  | Int of Z.t
  | Rat of Q.t

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t
include Interfaces.PRINT with type t := t
(*
include Interfaces.SERIALIZABLE with type t := t
include Interfaces.PRINT_OVERLOAD with type t := t
*)

module Map : Sequence.Map.S with type key = t
module Set : Sequence.Set.S with type elt = t
module Tbl : Hashtbl.S with type key = t

val ty : t -> [ `Int | `Rat | `Other ]

(** {2 Base Constructors} *)

val of_string : string -> t

val mk_int : Z.t -> t
val of_int : int -> t
val int_of_string : string -> t
val mk_rat : Q.t -> t
val of_rat : int -> int -> t
val rat_of_string : string -> t

val is_int : t -> bool
val is_rat : t -> bool
val is_numeric : t -> bool

module Seq : sig
  val add_set : Set.t -> t Sequence.t -> Set.t
end

module Base : sig
  val true_ : t
  val false_ : t
  val eq : t
  val neq : t
  val exists : t
  val forall : t
  val imply : t
  val equiv : t
  val xor : t
  val lambda : t

  val not_ : t
  val and_ : t
  val or_ : t

  val forall_ty : t
  val arrow : t
  val tType : t

  val wildcard : t    (** $_ for type inference *)
  val multiset : t    (** type of multisets *)

  val fresh_var : unit -> t (** New, unique symbol (cycles after 2^63 calls...) *)
end

(** {2 Generation of symbols} *)

val gensym : ?prefix:string -> unit -> t
  (** Fresh symbol (with unique name) *)

(** {2 TPTP Interface}
Creates symbol and give them properties. *)

module TPTP : sig
  val i : t
  val o : t
  val int : t
  val rat : t
  val real : t

  val connectives : Set.t
  val is_connective : t -> bool

  include Interfaces.PRINT with type t := t
  (** printer for TPTP *)

  (** {3 Arith Symbols} *)

  module Arith : sig

    val floor : t
    val ceiling : t
    val truncate : t
    val round : t

    val prec : t
    val succ : t

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

    val to_int : t
    val to_rat : t

    val less : t
    val lesseq : t
    val greater : t
    val greatereq : t

    val symbols : t Sequence.t

    val is_arith : t -> bool
      (** Is the symbol a TPTP arithmetic symbol? *)
  end
end

(** The module {!ArithOp} deals only with numeric constants, i.e., all symbols
    must verify {!is_numeric} (and most of the time, have the same type).
    The semantics of operations follows
    {{: http://www.cs.miami.edu/~tptp/TPTP/TR/TPTPTR.shtml#Arithmetic} TPTP}.
  *)

module ArithOp : sig
  exception TypeMismatch of string
  (** This exception is raised when Arith functions are called
      on non-numeric values *)

  type arith_view =
    [ `Int of Z.t
    | `Rat of Q.t
    | `Other of t
    ]

  val view : t -> arith_view
  (** Arith centered view of symbols *)

  val parse_num : string -> t

  val sign : t -> int   (* -1, 0 or 1 *)

  val one_i : t
  val zero_i : t
  val one_rat : t
  val zero_rat : t

  val zero_of_ty : [<`Int | `Rat ] -> t
  val one_of_ty : [<`Int | `Rat ] -> t

  val is_zero : t -> bool
  val is_one : t -> bool
  val is_minus_one : t -> bool

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

  val abs : t -> t (* absolute value *)
  val divides : t -> t -> bool (* [divides a b] returns true if [a] divides [b] *)
  val gcd : t -> t -> t  (* gcd of two ints, 1 for other types *)
  val lcm : t -> t -> t   (* lcm of two ints, 1 for other types *)

  val less : t -> t -> bool
  val lesseq : t -> t -> bool
  val greater : t -> t -> bool
  val greatereq : t -> t -> bool

  val divisors : Z.t -> Z.t list
    (** List of non-trivial strict divisors of the int.
        @return [] if int <= 1, the list of divisors otherwise. Empty list
          for prime numbers, obviously. *)
end

