
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Interface} *)

type symbol = Symbol.t
type type_ = Type.t

module type S = sig
  type term
  type term_set

  type t = private ScopedTerm.t

  type form = t

  type view = private
    | True
    | False
    | Atom of term
    | And of t list
    | Or of t list
    | Not of t
    | Imply of t * t
    | Equiv of t * t
    | Xor of t * t
    | Eq of term * term
    | Neq of term * term
    | Forall of type_ * t  (** Quantified formula, with De Bruijn index *)
    | Exists of type_ * t
    | ForallTy of t  (** quantification on type variable *)

  val view : t -> view
    (** View of the formula *)

  include Interfaces.HASH with type t := t
  include Interfaces.ORD with type t := t

  (** {2 Containers} *)

  module Tbl : Hashtbl.S with type key = t
  module Set : Sequence.Set.S with type elt = t
  module Map : Sequence.Map.S with type key = t

  (** {b Caution}: constructors can raise {!Type.Error} if the types are not
      as expected.  In particular, {!Base.eq} and {!Base.neq} expect two terms
      of the exact same type. *)

  module Base : sig
    val true_ : t
    val false_ : t
    val atom : term -> t
    val not_ : t -> t
    val and_ : t list -> t
    val or_ : t list -> t
    val imply : t -> t -> t
    val equiv : t -> t -> t
    val xor : t -> t -> t
    val eq : term -> term -> t
    val neq : term -> term -> t

    val mk_eq : bool -> term -> term -> t
    val mk_atom : bool -> term -> t

    (** Quantifiers: the term list must be a list of free variables. *)

    val forall : term list -> t -> t
    val exists : term list -> t -> t
    val forall_ty : type_ list -> t -> t

    val __mk_forall : varty:type_ -> t -> t
    val __mk_exists : varty:type_ -> t -> t
    val __mk_forall_ty : t -> t
  end

  (** {2 Sequence} *)

  module Seq : sig
    val terms : t -> term Sequence.t
    val vars : t -> term Sequence.t
    val atoms : t -> t Sequence.t
    val symbols : t -> symbol Sequence.t
  end

  (** {2 Combinators} *)

  val map_leaf : (t -> t) -> t -> t
    (** Call the function on leaves (atom,equal,true,false) and replace the
        leaves by their image. The image formulas should {b not} contain
        free De Bruijn indexes (ie, should verify {! db_closed}). *)

  val map : (term -> term) -> t -> t    (** Map on terms *)
  val fold : ('a -> term -> 'a) -> 'a -> t -> 'a  (** Fold on terms *)
  val iter : (term -> unit) -> t -> unit

  val map_shallow : (t -> t) -> t -> t
    (** Apply the function to the immediate subformulas *)

  val map_depth: ?depth:int ->
                  (int -> term -> term) ->
                  t -> t
    (** Map each term to another term. The number of binders from the root
        of the formula to the term is given to the function. *)

  val map_leaf_depth : ?depth:int ->
                        (int -> t -> t) ->
                        t -> t

  val fold_depth : ?depth:int ->
                ('a -> int -> term -> 'a) ->
                'a -> t -> 'a
    (** Fold on terms, but with an additional argument, the number of
        De Bruijn indexes bound on through the path to the term *)

  val weight : t -> int
    (** Number of 'nodes' of the formula, including the terms *)

  val subterm : term -> t -> bool
    (** [subterm t f] true iff [t] occurs in some term of [f] *)

  val is_atomic : t -> bool   (** No connectives? *)
  val is_ground : t -> bool   (** No variables? *)
  val is_closed : t -> bool   (** All variables bound? *)

  val contains_symbol : Symbol.t -> t -> bool

  val symbols : ?init:Symbol.Set.t -> t -> Symbol.Set.t
    (** Set of symbols occurring in the formula *)

  (** {2 High level operations} *)

  val free_vars_set : t -> term_set (** Set of free variables *)
  val free_vars : t -> term list (** Set of free vars *)

  val close_forall : t -> t   (** Bind all free variables with forall *)
  val close_exists : t -> t   (** Bind all free variables with exists *)

  val open_forall : ?offset:int -> t -> t
    (** Remove outer forall binders, using fresh vars instead of DB symbols *)

  val open_and : t -> t list
    (** If the formula is an outer conjunction, return the list of elements of
        the conjunction *)

  val open_or : t -> t list

  (** {2 Simplifications} *)

  val flatten : t -> t        (** Flatten AC connectives (or/and) *)
  val simplify : t -> t       (** Simplify the formula *)

  val is_trivial : t -> bool  (** Trivially true formula? *)

  val ac_normal_form : t -> t (** Normal form modulo AC of "or" and "and" *)
  val ac_eq : t -> t -> bool  (** Equal modulo AC? *)

  (** {2 Conversions} *)

  val to_simple_term : ?depth:int -> t -> STerm.t

  val of_term_unsafe : ScopedTerm.t -> t

  (** {2 IO} *)

  include Interfaces.PRINT with type t := t
  include Interfaces.PRINT_DE_BRUIJN with type t := t
      and type term := term

  module TPTP : sig
    include Interfaces.PRINT with type t := t
    include Interfaces.PRINT_DE_BRUIJN with type t := t
      and type term := term
      and type print_hook := print_hook
  end
end

module type TERM = sig
  type t = private ScopedTerm.t

  val of_term_unsafe : ScopedTerm.t -> t

  val ty : t -> type_

  val size : t -> int

  include Interfaces.HASH with type t := t
  include Interfaces.ORD with type t := t

  module Seq : sig
    val vars : t -> t Sequence.t
  end

  module Set : Sequence.Set.S with type elt = t

  val to_simple_term : ?depth:int -> t -> STerm.t

  include Interfaces.PRINT_DE_BRUIJN with type t := t
      and type term := t

  val default_hooks : unit -> print_hook list

  module TPTP : sig
    include Interfaces.PRINT_DE_BRUIJN with type t := t
      and type term := t
      and type print_hook := print_hook
  end
end
