
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Precedence (total ordering) on symbols} *)

type symbol_status =
  | Multiset
  | Lexicographic
  | LengthLexicographic

(** {2 Weight of Symbols} *)
module Weight : sig
  type t

  val int : int -> t
  val zero : t
  val one : t
  val omega : t
  val omega_plus : int -> t

  val sign : t -> int

  val add : t -> t -> t
  val diff : t -> t -> t
  val mult : int -> t -> t

  module Infix : sig
    val ( + ) : t -> t -> t
    val ( - ) : t -> t -> t
  end
  include module type of Infix

  include Interfaces.ORD with type t := t
  include Interfaces.PRINT with type t := t
end

(** {2 Constraints} *)
module Constr : sig
  type 'a t = private ID.t -> ID.t -> int
    constraint 'a = [< `partial | `total]
  (** A partial order on symbols, used to make the precedence more
      precise.
      ['a] encodes the kind of ordering: partial or total
      {b NOTE}: the ordering must partition the set of ALL symbols into
        equivalence classes, within which all symbols are equal, but
        symbols of distinct equivalence classes are always ordered. *)

  (* TODO: sth based on order of the type. Higher-order functions should
     be bigger than first-order functions, so that ghd() works fine
     with types in KBO *)

  val arity : (ID.t -> int) -> [`partial] t
  (** decreasing arity constraint (big arity => high in precedence) *)

  val invfreq : ID.t Sequence.t -> [`partial] t
  (** symbols with high frequency are smaller. Elements of unknown
      frequency are assumed to have a frequency of 0. *)

  val max : ID.t list -> [`partial] t
  (** maximal symbols, in decreasing order *)

  val min : ID.t list -> [`partial] t
  (** minimal symbols, in decreasing order *)

  val alpha : [`total] t
  (** alphabetic ordering on symbols, themselves bigger than builtin *)

  val compose : [`partial] t -> ([<`partial | `total] as 'a) t -> 'a t
  (** [compose a b] uses [a] to compare symbols; if [a] cannot decide,
      then we use [b]. *)

  val compose_sort : (int * [`partial] t) list -> [`partial] t
  (** [compose_sort l] sorts the list by increasing priority (the lower,
      the earlier an ordering is applied, and therefore the more
      impact it has) before composing *)

  val make : (ID.t -> ID.t -> int) -> [`partial] t
  (** Create a new partial order.
      {b CAUTION}, this order must respect some properties (see {!'a t}) *)
end

type t
(** Total Ordering on a finite number of symbols, plus a few more
    data (weight for KBO, status for RPC) *)

type precedence = t

val equal : t -> t -> bool
(** Check whether the two precedences are equal (same snapshot) *)

(* TODO: use a set of IDs, in increasing order w.r.t precedence, internally,
   and [snapshot] should return a sequence to hide it.
   The rationale is that this way, inserting a new symbol is [O(ln n)] rather
   than [O(n)] of number of symbols. *)

val snapshot : t -> ID.t list
(** Current list of symbols, in increasing order *)

val compare : t -> ID.t -> ID.t -> int
(** Compare two symbols using the precedence *)

val mem : t -> ID.t -> bool
(** Is the ID.t part of the precedence? *)

val status : t -> ID.t -> symbol_status
(** Status of the symbol *)

val weight : t -> ID.t -> Weight.t
(** Weight of a symbol (for KBO). *)

val arg_coeff : t -> ID.t -> int -> int
(** Nth argument coefficient of a symbol (for KBO with argument coefficients). *)

val add_list : t -> ID.t list -> unit
(** Update the precedence with the given symbols *)

val add_seq : t -> ID.t Sequence.t -> unit

val declare_status : t -> ID.t -> symbol_status -> unit
(** Change the status of the given precedence
    @raise Error if the symbol is not in the the precedence already *)

module Seq : sig
  val symbols : t -> ID.t Sequence.t
end

val pp_snapshot : ID.t list CCFormat.printer
val pp_debugf : t CCFormat.printer
include Interfaces.PRINT with type t := t

type weight_fun = ID.t -> Weight.t
type arg_coeff_fun = ID.t -> int list

val weight_modarity : arity:(ID.t -> int) -> weight_fun

val weight_constant : weight_fun

val set_weight : t -> weight_fun -> unit
(** Change the weight function of the precedence
    @since 0.5.3 *)

(** {2 Creation of a precedence from constraints} *)

val create : ?weight:weight_fun -> ?arg_coeff:arg_coeff_fun -> [`total] Constr.t -> ID.t list -> t
(** make a precedence from the given constraints. Constraints near
    the head of the list are {b more important} than constraints close
    to the tail. Only the very first constraint is assured to be totally
    satisfied if constraints do not agree with one another. *)

val default : ID.t list -> t
(** default precedence. Default status for symbols is {!Lexicographic}. *)

val default_seq : ID.t Sequence.t -> t
(** default precedence on the given sequence of symbols *)

val constr : t -> [`total] Constr.t
(** Obtain the constraint *)
