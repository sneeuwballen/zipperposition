
(* This file is free software, part of Zipperposition. See file "license" for more details. *)

(** {1 Perfect Discrimination Tree} *)

(** This module provides a simplification index, based on a
    perfect discrimination tree (see "the handbook of automated reasoning",
    chapter "term indexing", for instance).

    The simplification index can only retrieve {b generalizations}
    of a given query term, but it does it efficiently.

    It is used for demodulation (rewriting) in Superposition.
*)

module Make(E : Index.EQUATION) : Index.UNIT_IDX with module E = E

module Default : Index.UNIT_IDX with module E = Index.BasicEquation
