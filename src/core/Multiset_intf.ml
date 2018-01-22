
(* This file is free software, part of Logtk. See file "license" for more details. *)

module type S = sig
  type elt
  (** Elements of the multiset *)

  type t
  (** A multiset of elements of type 'a *)

  val size : t -> int
  (** Number of distinct elements. *)

  val cardinal : t -> Z.t
  (** Number of unique occurrences of elements (the multiplicity of each
      element is considered) *)

  val empty : t
  (** Empty multiset *)

  val is_empty : t -> bool
  (** Is the multiset empty? *)

  val mem : t -> elt -> bool
  (** Is the element part of the multiset? *)

  val find : t -> elt -> Z.t
  (** Return the multiplicity of the element within the multiset.
      Will return [Z.zero] if the element is not part of the multiset *)

  val singleton : elt -> t

  val doubleton : elt -> elt -> t

  val add : t -> elt -> t
  (** Add one occurrence of the element *)

  val add_coeff : t -> elt -> Z.t -> t
  (** Add several occurrences of the element *)

  val union : t -> t -> t
  (** Union of multisets (max of multiplicies) *)

  val intersection : t -> t -> t
  (** Intersection of multisets (min of multiplicies) *)

  val sum : t -> t -> t
  (** Sum of multiplicies *)

  val difference : t -> t -> t
  (** Difference of multisets. If [x] has a bigger multiplicity in the
      second argument it won't appear in the result *)

  val product : Z.t -> t -> t
  (** Multiply all multiplicities with the given coefficient *)

  val filter : (elt -> Z.t -> bool) -> t -> t
  (** Filter out elements that don't satisfy the predicate *)

  val map : (elt -> elt) -> t -> t
  (** Apply a function to all elements *)

  val map_coeff : (elt -> Z.t -> Z.t) -> t -> t
  (** Apply a function to all coefficients. *)

  val filter_map : (elt -> Z.t -> (elt * Z.t) option) -> t -> t
  (** More powerful mapping *)

  val flat_map : (elt -> t) -> t -> t
  (** replace each element by a multiset in its own *)

  module Seq : sig
    val of_seq : t -> elt Sequence.t -> t
    val to_seq : t -> elt Sequence.t

    val of_coeffs : t -> (elt * Z.t) Sequence.t -> t
    val to_coeffs : t -> (elt * Z.t) Sequence.t
  end

  val iter : (elt -> unit) -> t -> unit
  (** Iterate on distinct occurrences of elements *)

  val fold : ('a -> elt -> 'a) -> 'a -> t -> 'a
  (** fold on occurrences of elements *)

  val iter_coeffs : (elt -> Z.t -> unit) -> t -> unit
  (** Iterate on elements with their multiplicity *)

  val fold_coeffs : ('a -> elt -> Z.t -> 'a) -> 'a -> t -> 'a
  (** Fold on elements with their multiplicity *)

  val for_all : (elt -> bool) -> t -> bool

  val exists : (elt -> bool) -> t -> bool

  val choose : t -> elt
  (** Chose one element, or
      @raise Not_found if the multiset is empty *)

  val of_list : elt list -> t
  (** Multiset from list *)

  val of_coeffs : (elt * Z.t) list -> t
  (** From list of elements with multiplicities. Multiplicities lower
      than 0 will not count. *)

  val of_iarray : elt IArray.t -> t
  (** From immutable array *)

  val of_array : elt array -> t

  val to_list : t -> (elt * Z.t) list
  (** List of elements with their coefficients *)

  val equal : t -> t -> bool
  (** Check equality of two multisets *)

  val cancel : t -> t -> t * t
  (** Remove common elements from the multisets. For instance,
      on [{1,1,2}] and [{1,2,2,3}], [cancel] will return [({1}, {2,3})] *)

  (** {6 Comparisons}

      In the following, the comparison function must be equality-compatible
      with [E.compare]. In other words, if [E.compare x y = 0] then
      [f x y = Comparison.Eq] must hold. *)

  val compare : t -> t -> int
  (** Compare two multisets with the multiset extension of {!E.compare} *)

  val compare_partial : (elt -> elt -> Comparison.t) -> t -> t -> Comparison.t
  (** Compare two multisets with the multiset extension of the
      given ordering. This ordering is total iff the element
      ordering is. *)

  val is_max : (elt -> elt -> Comparison.t) -> elt -> t -> bool
  (** Is the given element maximal (ie not dominated by any
      other element) within the multiset? *)

  val max : (elt -> elt -> Comparison.t) -> t -> t
  (** Maximal elements of the multiset, w.r.t the given ordering. *)

  val max_seq : (elt -> elt -> Comparison.t) -> t -> (elt * Z.t) Sequence.t
  (** Fold on maximal elements *)

  val max_l : (elt -> elt -> Comparison.t) -> elt list -> elt list
  (** Maximal elements of a list *)

  val compare_partial_l :
    (elt -> elt -> Comparison.t) ->
    elt list -> elt list -> Comparison.t
  (** Compare two multisets represented as list of elements *)

  val pp : elt CCFormat.printer -> t CCFormat.printer
end
