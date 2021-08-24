(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Feature Vector indexing} *)

(** Feature Vector indexing (see Schulz 2004) for efficient forward
    and backward subsumption on Horn Clauses.

    This allows to retrieve clauses that (potentially) subsume
    or are subsumed by a given query clause.

    A version for any type, once its features are provided, is also included.
*)

type labels = Index_intf.labels

type feature =
  | N of int
  | S of ID.Set.t
  | M of int ID.Map.t
  | L of labels

type feature_vector = feature IArray.t
(** a vector of feature *)

module type HasFeatures = sig
  include Set.OrderedType (* order is only for usage in sets *)
  type feature_func
  val compute_feature : feature_func -> t -> feature
  (** HasFeatures: This module allows base feature vector index FV_IDX to be parameterized by internal type of the feature functions it uses. This in turn allows to simultaneously reuse retrieval logic from FV_IDX and change the query type to be different from the type of the indexed elements. For example subsuming clauses may be queried directly by literals and labels bypassing the clause construction. (Note that modules HasFeatures and CLAUSE provide only a destructive view to their data. ) *)
end


(* Generic feature vector index *)
module FV_IDX(Element: HasFeatures) : sig
  include Index_intf.GENERAL_IDX with type element=Element.t

  type named_feature = {name: string; f: Element.feature_func}
  type feature_funs = named_feature IArray.t
  (** types for feature functions *)
  
  val empty_with : feature_funs -> t
  (** Create index with custom features. They are tried in given order so place coarse and cheap ones first. (There is no default features for general feature vector index.) *)

  val compute_fv : feature_funs -> element -> feature_vector
  (** Given feature functions and an element, compute its feature vector. *)

  val feature_funs : t -> feature_funs
  (** feature functions used by the index *)
  
  val empty_with' : (string * Element.feature_func) list -> t
  (** Convenience version of empty_with: Create an index using given feature functions. *)
end


(* Subsumption index *)
module Make(C:Index_intf.CLAUSE) : sig
  (** {2 Feature Functions for Clauses} *)
  module Feature_fun : sig
    type t

    val name : t -> string
    val compute : t -> C.t -> feature
    include Interfaces.PRINT with type t := t

    val size_plus : t (** size of positive clause *)

    val size_minus : t (** size of negative clause *)

    val labels : t

    val weight_plus : t

    val weight_minus : t

    val set_sym_plus : t (** set of positive symbols *)

    val set_sym_minus : t (** set of negative symbols *)

    val depth_sym_plus : t (** max depth of positive symbols *)

    val depth_sym_minus : t (** max depth of negative symbols *)

    val multiset_sym_plus : t (** multiset of positive symbols *)

    val multiset_sym_minus : t (** multiset of negative symbols *)
  end

  (** {2 Index} *)

  include Index.SUBSUMPTION_IDX with module C = C

  type feature_funs = Feature_fun.t IArray.t
  (** Type of feature functions specific to clause subsumption *)
  
  val empty_with : feature_funs -> t
  (** Create index with custom features. They are tried in given order so place coarse and fast ones first. Use empty() instead to initiate with default features. *)

  val compute_fv : feature_funs -> element -> feature_vector
  (** Given feature functions and an element, compute its feature vector. *)

  val feature_funs : t -> feature_funs
  (** Feature functions used by the index *)
end