
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Feature Vector indexing} *)

(** Feature Vector indexing (see Schulz 2004) for efficient forward
    and backward subsumption on Horn Clauses.

    This allows to retrieve clauses that (potentially) subsume
    or are subsumed by a given query clause.

    This module is a modified version of {!FeatureVector},
    with full-signature features that encompass all symbols at the
    same time.
*)

type labels = Index_intf.labels

type feature =
  | N of int
  | S of ID.Set.t
  | M of int ID.Map.t
  | L of labels

type feature_vector = feature IArray.t
(** a vector of feature *)

module Make(C:Index_intf.CLAUSE) : sig
  (** {2 Feature Functions} *)
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

  type feature_funs = Feature_fun.t IArray.t

  val compute_fv_c : feature_funs -> C.t -> feature_vector

  (** {2 Index} *)

  include Index.SUBSUMPTION_IDX with module C = C

  val empty_with : feature_funs -> t

  val default_feature_funs : feature_funs

  val feature_funs : t -> feature_funs
end
