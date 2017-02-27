
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Non-Perfect Discrimination Tree} *)

(** This module provides a simplification index and a term index, based on a
    non-perfect discrimination tree (see "the handbook of automated reasoning",
    chapter "term indexing", for instance). It should be more compact
    in memory than {!Dtree}, and maybe more optimized too.
*)

module Make(E : Index.EQUATION) : Index.UNIT_IDX with module E = E

module MakeTerm(X : Set.OrderedType) : Index.TERM_IDX with type elt = X.t
