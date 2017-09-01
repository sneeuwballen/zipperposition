
(* This file is free software, part of Logtk. See file "license" for more details. *)

(** {1 Feature Vector indexing} *)

(** Feature Vector indexing (see Schulz 2004) for efficient forward
    and backward subsumption.

    This allows to retrieve clauses that (potentially) subsume
    or are subsumed by a given query clause.
*)

type lits = Index_intf.lits

(* TODO: mutable version? *)

(* TODO:
   keep a pool of all possible features (with functions to register new symbols),
   and maintain an histogram of possible values for each feature.
   Then, regularly, replace the [n] currently used features by the [n] top useful
    features. See paper.
*)

module Make(C : Index.CLAUSE) : sig
  type feature_vector = int list
  (** a vector of feature *)

  (** {2 Features} *)

  module Feature : sig
    type t = {
      name : string;
      f : lits -> int;
    } (** a function that computes a given feature on clauses *)

    val name : t -> string
    val compute : t -> lits -> int
    include Interfaces.PRINT with type t := t

    val sum_of_depths : t                 (** sum of depths of symbols *)
    val size_plus : t                     (** size of positive clause *)
    val size_minus : t                    (** size of negative clause *)
    val count_symb_plus : ID.t -> t   (** occurrences of ID.t in positive clause *)
    val count_symb_minus : ID.t -> t  (** occurrences of ID.t in negative clause *)
    val max_depth_plus : ID.t -> t    (** maximal depth of symb in positive clause *)
    val max_depth_minus : ID.t -> t   (** maximal depth of symb in negative clause *)
  end

  val compute_fv : Feature.t list -> lits -> feature_vector

  (** {2 Index} *)

  include Index.SUBSUMPTION_IDX with module C = C

  val empty_with : Feature.t list -> t

  val default_features : Feature.t list

  val features_of_signature : ?ignore:(ID.t -> bool) ->
    Signature.t -> Feature.t list
  (** Build a set of features from the given signature. IDs
      that satisfy [ignore] are not considered (default ignores
      connectives) *)

  val of_signature : Signature.t -> t

  val features : t -> Feature.t list
end
