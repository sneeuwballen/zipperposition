
(* This file is free software, part of Logtk. See file "license" for more details. *)

type term = Term.t
type subst = Subst.t

module type LEAF = sig
  type t
  type elt

  val empty : t
  val add : t -> term -> elt -> t
  val remove : t -> term -> elt -> t
  val update_leaf : t -> term -> (elt -> bool) -> t
  val is_empty : t -> bool
  val iter : t -> (term -> elt -> unit) -> unit
  val fold : t -> 'a -> ('a -> term -> elt -> 'a) -> 'a
  val size : t -> int

  val fold_unify :
    t Scoped.t -> term Scoped.t ->
    (term * elt * Unif_subst.t) Iter.t

  val fold_unify_complete :
    unif_alg : (Term.t Scoped.t -> Term.t Scoped.t -> Unif_subst.t option OSeq.t) ->
    t Scoped.t -> term Scoped.t ->
    (term * elt * Unif_subst.t option OSeq.t) Iter.t

  val fold_match :
    ?subst:subst ->
    t Scoped.t -> term Scoped.t ->
    (term * elt * subst) Iter.t
  (** Match the indexed terms against the given query term *)

  val fold_matched :
    ?subst:subst ->
    t Scoped.t -> term Scoped.t ->
    (term * elt * subst) Iter.t
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
  val add_seq : t -> (term * elt) Iter.t -> t
  val add_list : t -> (term * elt) list -> t

  val remove : t -> term -> elt -> t
  val update_leaf : t -> term -> (elt -> bool) -> t
  val remove_seq : t -> (term * elt) Iter.t -> t
  val remove_list : t -> (term * elt) list -> t

  val iter : t -> (term -> elt -> unit) -> unit

  val fold : t -> ('a -> term -> elt -> 'a) -> 'a -> 'a

  (** Retrieves a decidable fragment of unifiables. Only one unifier per subterm. *)
  val retrieve_unifiables :
    t Scoped.t -> term Scoped.t ->
    (term * elt * Unif_subst.t) Iter.t

  (** Retrieves all unifiables. The set of unifiers is potentially infinite.
      Because HO unification is undecidable, the sequence is intersperced with `None`s to ensure termination for each element of the sequence. *)
  val retrieve_unifiables_complete :
    ?unif_alg:(Term.t Scoped.t -> Term.t Scoped.t -> Unif_subst.t option OSeq.t)->
    t Scoped.t -> term Scoped.t ->
    (term * elt * Unif_subst.t option OSeq.t) Iter.t

  val retrieve_generalizations : ?subst:subst ->
    t Scoped.t -> term Scoped.t ->
    (term * elt * subst) Iter.t

  val retrieve_specializations : ?subst:subst ->
    t Scoped.t -> term Scoped.t ->
    (term * elt * subst) Iter.t

  val to_dot : elt CCFormat.printer -> t CCFormat.printer
  (** print oneself in DOT into the given file *)
end

type lits = term SLiteral.t Iter.t
(** Iter of literals, as a cheap abstraction on query clauses *)

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

module type GENERAL_IDX = sig
  (** An index is a set from which it is fast to query potential generalizations, specializations and/or equivalents to a given element. Meaning of generalization is defined by the concrete instance. Specialization is the opposite of generalization, and equivalence means to both generalize and specialize. A feature vector index particularly, is usually imperfect, so that queries may return excess elements. See FV_tree.ml for an implementation. This interface currently omits unifiability because feature vectors are ineffective filters to that. *)

  type t
  type element
  
  val name : string
  (** Informal name of index type *)

  val add : t -> element -> t
  (** Add element to index. It won't be duplicated. Some index may decide to ignore this operation! Ignoring is useful to filter clauses that can never participate to the inference, which the index is for, or to save memory if the inference is optional. *)

  val add_seq : t -> element Iter.t -> t
  val add_list : t -> element list -> t
  (** Repeated addition. Equivalent to Iter.fold add / List.fold_left add *)

  val remove : t -> element -> t
  (** Remove element from index, if present. *)

  val remove_seq : t -> element Iter.t -> t
  (** Repeated removal. Equivalent to Iter.fold remove *)

  val retrieve_generalizations : t -> element -> element Iter.t
  (** Find at least all generalizations but usually also some non-generalizations. Meaning of generalization and (im)perfectness of retrieval depend on the concrete index instance. *)

  val retrieve_specializations : t -> element -> element Iter.t
  (** Find at least all specializations but usually also some non-specializations. Meaning of specialization and (im)perfectness of retrieval depend on the concrete index instance. *)

  val retrieve_equivalents : t -> element -> element Iter.t
  (** Find at least all equivalent elements but usually also some more. Meaning of equivalence and (im)perfectness of retrieval depend on the concrete index instance. *)

  val iter : t -> element Iter.t

  val fold : ('a -> element -> 'a) -> 'a -> t -> 'a
end

module type SUBSUMPTION_IDX = sig
  (* type t *)

  module C : CLAUSE

  include GENERAL_IDX with type element = C.t

  val empty : unit -> t
  (** Empty index *)

  val retrieve_subsuming : t -> lits -> labels -> C.t Iter.t
  (** Fold on a set of indexed candidate clauses, that may subsume
      the given clause. *)

  val retrieve_subsuming_c : t -> C.t -> C.t Iter.t
  (** Alias of retrieve_generalizations *)

  val retrieve_subsumed : t -> lits -> labels -> C.t Iter.t
  (** Fold on a set of indexed candidate clauses, that may be subsumed by
      the given clause *)

  val retrieve_subsumed_c : t -> C.t -> C.t Iter.t
  (** Alias of retrieve_specializations *)

  val retrieve_alpha_equiv : t -> lits -> labels -> C.t Iter.t
  (** Retrieve clauses that are potentially alpha-equivalent to the given clause *)

  val retrieve_alpha_equiv_c : t -> C.t -> C.t Iter.t
  (** Retrieve clauses that are potentially alpha-equivalent to the given clause. Alias of retrieve_equivalents *)
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

  val add_seq : t -> E.t Iter.t -> t
  val add_list : t -> E.t list -> t

  val remove : t -> E.t -> t

  val remove_seq : t -> E.t Iter.t -> t

  val size : t -> int
  (** Number of indexed (in)equations *)

  val iter : t -> (term -> E.t -> unit) -> unit
  (** Iterate on indexed equations *)

  val retrieve :
    ?subst:subst -> sign:bool ->
    t Scoped.t -> term Scoped.t ->
    (term * rhs * E.t * subst) Iter.t
  (** [retrieve ~sign (idx,si) (t,st) acc] iterates on
      (in)equations l ?= r of given [sign] and substitutions [subst],
      such that subst(l, si) = t.
      It therefore finds generalizations of the query term. *)

  val to_dot : t CCFormat.printer
  (** print the index in the DOT format *)
end
