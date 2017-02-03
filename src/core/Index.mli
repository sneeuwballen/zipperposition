
(* This file is free software, part of Libzipperposition. See file "license" for more details. *)

(** {1 Generic term indexing} *)

type term = FOTerm.t
type subst = Subst.t

(** {2 Leaf} *)

(** A leaf maps terms to a set of elements *)

module type LEAF = Index_intf.LEAF

module MakeLeaf(X : Set.OrderedType) : LEAF with type elt = X.t

(** {2 FOTerm index} *)

module type TERM_IDX = Index_intf.TERM_IDX

(** {2 Subsumption Index} *)

module type CLAUSE = Index_intf.CLAUSE

(** A subsumption index (non perfect!) *)

module type SUBSUMPTION_IDX = Index_intf.SUBSUMPTION_IDX

(** {2 Specialized rewriting index} *)

module type EQUATION = Index_intf.EQUATION

module BasicEquation : EQUATION
  with type t = FOTerm.t * FOTerm.t
   and type rhs = FOTerm.t

module type UNIT_IDX = Index_intf.UNIT_IDX

