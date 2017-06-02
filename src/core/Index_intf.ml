
(* This file is free software, part of Logtk. See file "license" for more details. *)

type term = Term.t
type subst = Subst.t

module type LEAF = sig
  type t
  type elt

  val empty : t
  val add : t -> term -> elt -> t
  val remove : t -> term -> elt -> t
  val is_empty : t -> bool
  val iter : t -> (term -> elt -> unit) -> unit
  val fold : t -> 'a -> ('a -> term -> elt -> 'a) -> 'a
  val size : t -> int

  val fold_unify :
    ?subst:Unif_subst.t -> t Scoped.t -> term Scoped.t ->
    (term * elt * Unif_subst.t) Sequence.t

  val fold_match :
    ?subst:subst ->
    t Scoped.t -> term Scoped.t ->
    (term * elt * subst) Sequence.t
  (** Match the indexed terms against the given query term *)

  val fold_matched :
    ?subst:subst ->
    t Scoped.t -> term Scoped.t ->
    (term * elt * subst) Sequence.t
    (** Match the query term against the indexed terms *)
end

module type TERM_IDX = sig
  type t
  type elt

  module Leaf : LEAF with type elt = elt

  val name : string

  val empty : unit -> t

  val is_empty : t -> bool

  val size : t -> int

  val add : t -> term -> elt -> t
  val add_seq : t -> (term * elt) Sequence.t -> t
  val add_list : t -> (term * elt) list -> t

  val remove : t -> term -> elt -> t
  val remove_seq : t -> (term * elt) Sequence.t -> t
  val remove_list : t -> (term * elt) list -> t

  val iter : t -> (term -> elt -> unit) -> unit

  val fold : t -> ('a -> term -> elt -> 'a) -> 'a -> 'a

  val retrieve_unifiables : ?subst:Unif_subst.t ->
    t Scoped.t -> term Scoped.t ->
    (term * elt * Unif_subst.t) Sequence.t

  val retrieve_generalizations : ?subst:subst ->
    t Scoped.t -> term Scoped.t ->
    (term * elt * subst) Sequence.t

  val retrieve_specializations : ?subst:subst ->
    t Scoped.t -> term Scoped.t ->
    (term * elt * subst) Sequence.t

  val to_dot : elt CCFormat.printer -> t CCFormat.printer
  (** print oneself in DOT into the given file *)
end

type lits = term SLiteral.t Sequence.t
(** Sequence of literals, as a cheap abstraction on query clauses *)

type labels = Util.Int_set.t

module type CLAUSE = sig
  type t

  val compare : t -> t -> int
  (** Compare two clauses *)

  val to_lits : t -> lits
  (** Iterate on literals of the clause *)

  val labels : t -> labels
  (** Some integer labels. We assume that if [c] to subsume [d],
      then [labels c] is a subset of [labels d] *)
end

module type SUBSUMPTION_IDX = sig
  type t

  module C : CLAUSE

  val name : string

  val empty : unit -> t
  (** Empty index *)

  val add : t -> C.t -> t
  (** Index the clause *)

  val add_seq : t -> C.t Sequence.t -> t
  val add_list : t -> C.t list -> t

  val remove : t -> C.t -> t
  (** Un-index the clause *)

  val remove_seq : t -> C.t Sequence.t -> t

  val retrieve_subsuming : t -> lits -> labels -> C.t Sequence.t
  (** Fold on a set of indexed candidate clauses, that may subsume
      the given clause. *)

  val retrieve_subsuming_c : t -> C.t -> C.t Sequence.t

  val retrieve_subsumed : t -> lits -> labels -> C.t Sequence.t
  (** Fold on a set of indexed candidate clauses, that may be subsumed by
      the given clause *)

  val retrieve_subsumed_c : t -> C.t -> C.t Sequence.t

  val retrieve_alpha_equiv : t -> lits -> labels -> C.t Sequence.t
  (** Retrieve clauses that are potentially alpha-equivalent to the given clause *)

  val retrieve_alpha_equiv_c : t -> C.t -> C.t Sequence.t
  (** Retrieve clauses that are potentially alpha-equivalent to the given clause *)

  val iter : t -> C.t Sequence.t

  val fold : ('a -> C.t -> 'a) -> 'a -> t -> 'a
end

module type EQUATION = sig
  type t

  type rhs
  (** An equation can have something other than a term as a right-hand
      side, for instance a formula. *)

  val compare : t -> t -> int
  (** Total order on equations *)

  val extract : t -> (term * rhs * bool)
  (** Obtain a representation of the (in)equation. The sign indicates
      whether it is an equation [l = r] (if true) or an inequation
      [l != r] (if false) *)

  val priority : t -> int
  (** How "useful" this equation is. Can be safely ignored by
      always returning the same number. *)
end

module type UNIT_IDX = sig
  type t

  module E : EQUATION
  (** Module that describes indexed equations *)

  type rhs = E.rhs
  (** Right hand side of equation *)

  val empty : unit -> t

  val is_empty : t -> bool

  val add : t -> E.t -> t
  (** Index the given (in)equation *)

  val add_seq : t -> E.t Sequence.t -> t
  val add_list : t -> E.t list -> t

  val remove : t -> E.t -> t

  val remove_seq : t -> E.t Sequence.t -> t

  val size : t -> int
  (** Number of indexed (in)equations *)

  val iter : t -> (term -> E.t -> unit) -> unit
  (** Iterate on indexed equations *)

  val retrieve :
    ?subst:subst -> sign:bool ->
    t Scoped.t -> term Scoped.t ->
    (term * rhs * E.t * subst) Sequence.t
  (** [retrieve ~sign (idx,si) (t,st) acc] iterates on
      (in)equations l ?= r of given [sign] and substitutions [subst],
      such that subst(l, si) = t.
      It therefore finds generalizations of the query term. *)

  val to_dot : t CCFormat.printer
  (** print the index in the DOT format *)
end
