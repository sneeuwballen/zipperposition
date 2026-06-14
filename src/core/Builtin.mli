(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Builtin Objects} *)

(** Most objects that have a special meaning in logic are represented by a
    {b builtin}. A builtin is a hashconsed string; it might correspond to
    different names in different input syntaxes.

    Builtins cover numbers, connectives, and builtin types, among others.

    The type is [private Hstring.t], meaning any [Builtin.t] can be coerced to
    [Hstring.t] (e.g. [(b :> Hstring.t)]), but not the reverse. Use
    {!of_hstring} to promote an [Hstring.t] to a [Builtin.t].

    @since 1.5 *)

val _t_bigger_false : bool ref

type t = private Hstring.t
(** A builtin tag. Internally a hashconsed string. Known builtins are recognized
    via {!view}; unknown ones (e.g. deserialized) are still valid [t] values but
    {!view} returns [None]. *)

(** {2 View types for pattern matching} *)

type view_t = Builtin_gen.view_t

type payload_view =
  | Int of Z.t
  | Rat of Q.t
  | Real of string
  | Pseudo_de_bruijn of int

type fixity =
  | Infix_binary
  | Infix_nary
  | Prefix

val view : t -> view_t option
(** Recognize a fixed builtin by its Hstring. Returns [None] for payload
    builtins or unrecognized strings. *)

val is_payload : t -> payload_view option
(** Recognize a payload-bearing builtin (Int, Rat, Real, Pseudo_de_bruijn). *)

val make_view : view_t -> t
(** Build a fixed builtin from a view_t constructor. *)

val make_payload : payload_view -> t
(** Build a payload-bearing builtin. *)

val of_hstring : Hstring.t -> t
(** Promote an Hstring to a builtin. *)

(** {2 Well-known builtin constants} *)

val not_ : t
val and_ : t
val or_ : t
val imply : t
val equiv : t
val xor : t
val eq : t
val neq : t
val has_type : t
val true_ : t
val false_ : t
val arrow : t
val wildcard : t
val multiset : t
val tType : t
val prop : t
val term : t
val forallConst : t
val existsConst : t
val choiceConst : t
val grounding : t
val ty_int : t
val ty_rat : t
val ty_real : t
val floor_ : t
val ceiling_ : t
val truncate_ : t
val round_ : t
val prec_ : t
val succ_ : t
val sum_ : t
val difference_ : t
val uminus_ : t
val product_ : t
val quotient_ : t
val quotient_e : t
val quotient_t : t
val quotient_f : t
val remainder_e : t
val remainder_t : t
val remainder_f : t
val is_int_ : t
val is_rat_ : t
val to_int_c : t
val to_rat_c : t
val less_ : t
val lesseq_ : t
val greater_ : t
val greatereq_ : t
val box_opaque : t
val bComb : t
val cComb : t
val iComb : t
val kComb : t
val sComb : t
val distinct : t

(** {2 Comparison and hashing} *)

include Interfaces.HASH with type t := t
include Interfaces.ORD with type t := t
include Interfaces.PRINT with type t := t

(** {2 Classification predicates} *)

val fixity : t -> fixity
val is_prefix : t -> bool
val is_infix : t -> bool
val ty : t -> [ `Int | `Rat | `Other ]

val mk_int : Z.t -> t
(** Numeric constants *)

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
val is_logical_op : t -> bool
val is_logical_binop : t -> bool
val is_flattened_logical : t -> bool
val is_quantifier : t -> bool
val is_combinator : t -> bool

val as_int : t -> int
(** [as_int] is the internal integer code used for ordering *)

module Map : Iter.Map.S with type key = t
module Set : Iter.Set.S with type elt = t
module Tbl : Hashtbl.S with type key = t

(** {2 Tag module} *)

(** Each tag describes an extension of FO logic *)
module Tag : sig
  type t =
    | T_lia  (** integer arith *)
    | T_lra  (** rational arith *)
    | T_ho  (** higher order *)
    | T_live_cnf  (** live cnf *)
    | T_ho_norm  (** higher-order normalization *)
    | T_dont_increase_depth  (** don't increase depth *)
    | T_ext  (** extensionality *)
    | T_ind  (** induction *)
    | T_data  (** datatypes *)
    | T_distinct  (** distinct constants *)
    | T_ac of Name.t  (** AC symbol *)
    | T_cannot_orphan

  val compare : t -> t -> int
  val pp : t CCFormat.printer
end

(** {2 Arithmetic constructor constants} *)

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

(** {2 Arithmetic operations} *)

module ArithOp : sig
  exception TypeMismatch of string

  type arith_view =
    [ `Int of Z.t
    | `Rat of Q.t
    | `Other of t
    ]

  val view : t -> arith_view
  val parse_num : string -> t
  val sign : t -> int
  val one_i : t
  val zero_i : t
  val one_rat : t
  val zero_rat : t
  val zero_of_ty : [ `Rat | `Int ] -> t
  val one_of_ty : [ `Rat | `Int ] -> t
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
  val abs : t -> t
  val divides : t -> t -> bool
  val gcd : t -> t -> t
  val lcm : t -> t -> t
  val less : t -> t -> bool
  val lesseq : t -> t -> bool
  val greater : t -> t -> bool
  val greatereq : t -> t -> bool
  val divisors : Z.t -> Z.t list
end

(** {2 TPTP Interface} *)

module TPTP : sig
  val to_string : t -> string
  val pp : t CCFormat.printer
  val of_string : string -> t option
  val of_string_exn : string -> t

  val fixity : view_t -> fixity
  (** use view constructors for matching *)

  val is_prefix : view_t -> bool
  val is_infix : view_t -> bool
  val connectives : Set.t
  val is_connective : t -> bool
end

(** {2 ZF Interface} *)

module ZF : sig
  val to_string : t -> string
  val pp : t CCFormat.printer
end
