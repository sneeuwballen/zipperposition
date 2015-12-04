
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Interface of Precedence} *)

type symbol_status =
  | Multiset
  | Lexicographic

module type S = sig
  type symbol

  type t
  (** Total Ordering on a finite number of symbols, plus a few more
      data (weight for KBO, status for RPC) *)

  type precedence = t

  val equal : t -> t -> bool
  (** Check whether the two precedences are equal (same snapshot) *)

  val snapshot : t -> symbol list
  (** Current list of symbols, in decreasing order *)

  val compare : t -> symbol -> symbol -> int
  (** Compare two symbols using the precedence *)

  val mem : t -> symbol -> bool
  (** Is the symbol part of the precedence? *)

  val status : t -> symbol -> symbol_status
  (** Status of the symbol *)

  val weight : t -> symbol -> int
  (** Weight of a symbol (for KBO). Strictly positive int. *)

  val add_list : t -> symbol list -> t
  (** Update the precedence with the given symbols *)

  val add_seq : t -> symbol Sequence.t -> t

  val declare_status : t -> symbol -> symbol_status -> t
  (** Change the status of the given precedence *)

  module Seq : sig
    val symbols : t -> symbol Sequence.t
  end

  val pp_snapshot : symbol list CCFormat.printer
  val pp_debugf : t CCFormat.printer
  include Interfaces.PRINT with type t := t

  (** {2 Builtin constraints} *)

  module Constr : sig
    type 'a t = private symbol -> symbol -> int
    constraint 'a = [< `partial | `total]
    (** A partial order on symbols, used to make the precedence more
        precise.
        ['a] encodes the kind of ordering: partial or total
        {b NOTE}: the ordering must partition the set of ALL symbols into
          equivalence classes, within which all symbols are equal, but
          symbols of distinct equivalence classes are always ordered. *)

    val arity : (symbol -> int) -> [`partial] t
    (** decreasing arity constraint (big arity => high in precedence) *)

    val invfreq : symbol Sequence.t -> [`partial] t
    (** symbols with high frequency are smaller. Elements of unknown
        frequency are assumed to have a frequency of 0. *)

    val max : symbol list -> [`partial] t
    (** maximal symbols, in decreasing order *)

    val min : symbol list -> [`partial] t
    (** minimal symbols, in decreasing order *)

    val alpha : [`total] t
    (** alphabetic ordering on symbols *)

    val compose : [`partial] t -> ([<`partial | `total] as 'a) t -> 'a t
    (** [compose a b] uses [a] to compare symbols; if [a] cannot decide,
        then we use [b]. *)

    val compose_sort : (int * [`partial] t) list -> [`partial] t
    (** [compose_sort l] sorts the list by decreasing priority (the higher,
        the earlier an ordering is applied) before composing *)
  end

  type weight_fun = symbol -> int

  val weight_modarity : arity:(symbol -> int) -> weight_fun
  val weight_constant : weight_fun

  val set_weight : t -> weight_fun -> t
  (** Change the weight function of the precedence
      @since 0.5.3 *)

  (** {2 Creation of a precedence from constraints} *)

  val create : ?weight:weight_fun -> [`total] Constr.t -> symbol list -> t
  (** make a precedence from the given constraints. Constraints near
      the head of the list are {b more important} than constraints close
      to the tail. Only the very first constraint is assured to be totally
      satisfied if constraints do not agree with one another. *)

  val default : symbol list -> t
  (** default precedence. Default status for symbols is {!Lexicographic}. *)

  val default_seq : symbol Sequence.t -> t
  (** default precedence on the given sequence of symbols *)

  val constr_list : t -> [`total] Constr.t
  (** Obtain the constraint
      @since 0.6.1 *)
end

