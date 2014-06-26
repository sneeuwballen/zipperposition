
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

(** {1 First-order Formulas}
Hashconsed formulas for first-order logic. This provides many useful
functions, and smart constructors that perform some basic
simplifications

TODO: attributes, to speed up simplification/flattening/closeness
checking
*)

type symbol = Symbol.t

module type S = sig
  type term
  type term_set

  type t = private ScopedTerm.t

  type form = t

  type sourced_form = t * string * string
    (** form, filename, axiom name *)

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
    | Forall of Type.t * t  (** Quantified formula, with De Bruijn index *)
    | Exists of Type.t * t
    | ForallTy of t  (** quantification on type variable *)

  val view : t -> view
    (** View of the formula *)

  val kind : ScopedTerm.Kind.t
  val of_term : ScopedTerm.t -> t option
  val of_term_exn : ScopedTerm.t -> t
    (** @raise Invalid_argument if the term isn't a formula *)
  val is_form : ScopedTerm.t -> bool

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
    val forall_ty : Type.t list -> t -> t

    val __mk_forall : varty:Type.t -> t -> t
    val __mk_exists : varty:Type.t -> t -> t
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
  val de_bruijn_set : t -> Type.Set.t * term_set
    (** Set of De Bruijn indices that are not bound for types and terms *)

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

  val to_prolog : ?depth:int -> t -> PrologTerm.t

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

  (* TODO
  include Interfaces.SERIALIZABLE with type t := t
  *)
end

module type TERM = sig
  type t = private ScopedTerm.t

  val of_term : ScopedTerm.t -> t option
  val of_term_exn : ScopedTerm.t -> t

  val ty : t -> Type.t

  val size : t -> int

  val kind : ScopedTerm.Kind.t

  include Interfaces.HASH with type t := t
  include Interfaces.ORD with type t := t

  module Seq : sig
    val vars : t -> t Sequence.t
  end

  module Set : Sequence.Set.S with type elt = t

  val to_prolog : ?depth:int -> t -> PrologTerm.t

  include Interfaces.PRINT_DE_BRUIJN with type t := t
      and type term := t
  module TPTP : sig
    include Interfaces.PRINT_DE_BRUIJN with type t := t
      and type term := t
      and type print_hook := print_hook
  end
end

module Make(MyT : TERM) : S
  with type term = MyT.t
  and type term_set = MyT.Set.t

module FO : sig
  include S with type term = FOTerm.t and type term_set = FOTerm.Set.t

  (** {2 Conversion to higher-order term} *)

  val to_hoterm : t -> HOTerm.t
  val of_hoterm : HOTerm.t -> t option
end

(** Functor to translate terms *)
module Map(From:S)(To:S) : sig
  val map : (From.term -> To.term) -> From.t -> To.t
end
