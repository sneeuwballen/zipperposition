
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Builtin Objects} *)

(** Most objects that have a special meaning in logic are represented
    by a {b builtin}. A builtin is a value of type {!t}; it might
    correspond to different names in different input syntaxes.

    Builtins cover numbers, connectives, and builtin types, among others.

    @since 1.5 *)

type t =
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
  | Arrow
  | Wildcard
  | Multiset  (* type of multisets *)
  | TType (* type of types *)
  | Prop
  | Term
  | ForallConst (** constant for simulating forall *)
  | ExistsConst (** constant for simulating exists *)
  | Grounding (** used for inst-gen *)
  | TyInt
  | TyRat
  | TyReal
  | Int of Z.t
  | Rat of Q.t
  | Real of string (* for now… *)
  | Floor
  | Ceiling
  | Truncate
  | Round
  | Prec
  | Succ
  | Sum
  | Difference
  | Uminus
  | Product
  | Quotient
  | Quotient_e
  | Quotient_t
  | Quotient_f
  | Remainder_e
  | Remainder_t
  | Remainder_f
  | Is_int
  | Is_rat
  | To_int
  | To_rat
  | Less
  | Lesseq
  | Greater
  | Greatereq
  | Box_opaque (** hint not to open this formula *)
  | Pseudo_de_bruijn of int (** magic to embed De Bruijn indices in normal terms *)

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t
include Interfaces.PRINT with type t := t

type fixity =
  | Infix_binary
  | Infix_nary
  | Prefix

val fixity : t -> fixity

val is_prefix : t -> bool
(** [is_infix s] returns [true] if the way the symbol is printed should
    be used in a prefix way if applied to 1 argument *)

val is_infix : t -> bool
(** [is_infix s] returns [true] if the way the symbol is printed should
    be used in an infix way if applied to two arguments *)

val ty : t -> [ `Int | `Rat | `Other ]

val mk_int : Z.t -> t
val of_int : int -> t
val int_of_string : string -> t
val mk_rat : Q.t -> t
val of_rat : int -> int -> t
val rat_of_string : string -> t

val is_int : t -> bool
val is_rat : t -> bool
val is_numeric : t -> bool
val is_not_numeric : t -> bool

val is_arith : t -> bool
(** Any arithmetic operator, or constant *)

val true_ : t
val false_ : t
val eq : t
val neq : t
val imply : t
val equiv : t
val xor : t

val not_ : t
val and_ : t
val or_ : t

val arrow : t
val tType : t
val prop : t
val term : t
val ty_int : t
val ty_rat : t
val has_type : t

val wildcard : t    (** $_ for type inference *)
val multiset : t    (** type of multisets *)

val grounding : t

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
end

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t
include Interfaces.PRINT with type t := t

module Map : Sequence.Map.S with type key = t
module Set : Sequence.Set.S with type elt = t
module Tbl : Hashtbl.S with type key = t

(** Each tag describes an extension of FO logic *)
module Tag : sig
  type t =
    | T_lia (** integer arith *)
    | T_lra (** rational arith *)
    | T_ho (** higher order *)
    | T_ext (** extensionality *)
    | T_ind (** induction *)
    | T_data (** datatypes *)
    | T_distinct (** distinct constants *)
    | T_ac of ID.t (** AC symbol *)

  val compare : t -> t -> int
  val pp : t CCFormat.printer
end

(** {2 TPTP Interface}
    Creates symbol and give them properties. *)

module TPTP : sig
  val connectives : Set.t
  val is_connective : t -> bool

  val fixity : t -> fixity

  val is_infix : t -> bool
  val is_prefix : t -> bool

  val of_string : string -> t option
  (** Parse a $word into a builtin *)

  include Interfaces.PRINT with type t := t
  (** printer for TPTP *)
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

(** {2 ZF} *)

module ZF : sig
  include Interfaces.PRINT with type t := t
end
